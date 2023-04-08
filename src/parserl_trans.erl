%%%-----------------------------------------------------------------------------
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @copyright 2023 William Fank Thomé
%%% @doc Module with helpers functions.
%%% @end
%%%-----------------------------------------------------------------------------
-module(parserl_trans).

%% API
-export([ quote/1, quote/2, unquote/1, unquote/2, insert_above/2, insert_below/2
        , insert_attribute/2, insert_attribute/3, remove_attribute/2
        , find_attribute/2, find_all_attributes/2, attribute_exists/2
        , insert_function/2, insert_function/3, insert_function/4
        , insert_function/5, replace_function/2, replace_function/3
        , replace_function/5, export_function/3, export_function/4
        , unexport_function/2, unexport_function/3, unexport_function/4
        , is_function_exported/2, is_function_exported/3, find_function/3
        , function_exists/3, debug/1, restore/1, write_file/2, get_module/1
        , module_prefix/2, module_suffix/2, eval/1, eval/2, log/3, log/4
        , log_or_raise/4, log_or_raise/5, is_attribute/2, is_function/3 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

quote(Text) ->
    quote(Text, []).

quote(Text0, Env0) when is_list(Text0); is_binary(Text0) ->
    Text = flatten_text(Text0),
    Env = env_to_abstract(Env0),
    try
        case Env =:= [] of
            true ->
                merl:quote(Text);

            false ->
                merl:qquote(Text, Env)
        end
    catch
        _:Reason:Stack ->
            quote_error(Text, Env, Reason, Stack)
    end.

unquote(Form) ->
    unquote(Form, []).

%% @see https://www.erlang.org/doc/man/erl_prettypr.html#format-2
unquote(Form, Opts) when is_tuple(Form) ->
    erl_prettypr:format(Form, Opts);
unquote(Forms, Opts) when is_list(Forms) ->
    flatten_text(lists:map(fun(Form) -> unquote(Form, Opts) end, Forms)).

insert_above(Form, Forms0) ->
    Forms1 = normalize(Forms0),
    Context = parse_trans_context(Forms1),
    {Forms, _} = parse_trans:do_transform( fun(function, F, _, false) ->
                                                  {Form, F, [], false, true};

                                              (_, F, _, Acc) ->
                                                  {F, false, Acc} end
                                          , false
                                          , Forms1
                                          , Context ),
    Forms.

insert_below(Form, [H | T] = Forms) ->
    case erl_syntax:type(H) of
        eof_marker ->
            case Form of
                Form when is_list(Form) ->
                    Form ++ Forms;

                Form when is_tuple(Form) ->
                    [Form] ++ Forms
            end;

        _ ->
            [H | insert_below(Form, T)]
    end;
insert_below(Form, []) ->
    [Form];
insert_below(Form, Forms) when is_tuple(Forms) ->
    insert_below(Form, [Forms]).

insert_attribute(Text, Forms) ->
    insert_attribute(Text, Forms, []).

insert_attribute(Text, Forms, Opts) ->
    Abstract = quote(Text, get_env(Opts)),
    insert_above(Abstract, Forms).

remove_attribute(Name, Forms) ->
    lists:filter(
        fun(Form) ->
            case is_attribute(Name, Form) of
                true ->
                    attribute_name(Form) =/= Name;

                false ->
                    true
            end
        end,
        normalize(Forms)
    ).

find_attribute(_, [{eof, _}]) ->
    false;
find_attribute(Name, [Form | T]) ->
    case is_attribute(Name, Form) of
        true ->
            {true, Form};

        false ->
            find_attribute(Name, T)
    end;
find_attribute(_, []) ->
    false;
find_attribute(Name, Forms) when is_tuple(Forms) ->
    find_attribute(Name, [Forms]).

find_all_attributes(Names, Forms) when is_list(Names) ->
    find_all_attributes(Names, Forms, []);
find_all_attributes(Name, Forms) when is_atom(Name) ->
    find_all_attributes([Name], Forms).

attribute_exists(Name, Forms) ->
    case find_attribute(Name, Forms) of
        {true, _} ->
            true;

        false ->
            false
    end.

insert_function(Text, Forms) ->
    insert_function(Text, Forms, []).

insert_function(Text0, Forms, Opts) ->
    Text = flatten_text(Text0),
    Name = guess_fun_name(Text, Opts),
    Arity = guess_fun_arity(Text),
    AST = quote(Text, get_env(Opts)),
    insert_function(Name, Arity, AST, Forms, Opts).

insert_function(Name, Arity, AST, Forms) ->
    insert_function(Name, Arity, AST, Forms, []).

insert_function(Name, Arity, AST, Forms0, Opts) ->
    Forms1 = normalize(Forms0),
    case function_exists(Name, Arity, Forms1) of
        true ->
            lists:map(
                fun(Form) ->
                    case is_function(Name, Arity, Form) of
                        true ->
                            Fun = erl_syntax:revert(Form),
                            Tmp = erl_syntax:revert(AST),
                            NewClauses = erl_syntax:function_clauses(Tmp),
                            attach_clauses(Fun, NewClauses, Opts);

                        false ->
                            Form
                    end
                end,
                Forms1
            );

        false ->
            Forms = insert_below(AST, Forms1),
            case get_value(export, Opts, false) of
                true ->
                    export_function(Name, Arity, Forms, Opts);

                false ->
                    Forms
            end
    end.

replace_function(Text, Forms) ->
    replace_function(Text, Forms, []).

replace_function(Text, Forms, Opts) ->
    Body = flatten_text(Text),
    Name = guess_fun_name(Body, Opts),
    Arity = get_value(arity, Opts, guess_fun_arity(Body)),
    replace_function(Name, Arity, Text, Forms, Opts).

replace_function(Name, Arity, Text, Forms, Opts) when is_list(Text); is_binary(Text) ->
    AST = quote(Text, get_env(Opts)),
    replace_function(Name, Arity, AST, Forms, Opts);
replace_function(Name, Arity, AST, Forms0, Opts) ->
    Forms1 = normalize(Forms0),
    case function_exists(Name, Arity, Forms1) of
        true ->
            Forms2 = parse_trans:replace_function( Name
                                                 , Arity
                                                 , AST
                                                 , Forms1
                                                 , proplist(Opts) ),
            NewName = erl_syntax:atom_value(erl_syntax:function_name(AST)),
            NewArity = erl_syntax:function_arity(AST),
            ShouldUnexportOldFun = is_function_exported(Name, Arity, Forms0) andalso
                                   (Name =/= NewName orelse Arity =/= NewArity),
            Forms = case ShouldUnexportOldFun of
                        true ->
                            unexport_function(Name, Arity, Forms2);

                        false ->
                            Forms2
                    end,
            case ShouldUnexportOldFun orelse get_value(export, Opts, false) of
                true ->
                    export_function(NewName, NewArity, Forms, Opts);

                false ->
                    Forms
            end;

        false ->
            log_or_raise( warning
                        , function_not_defined
                        , #{ text => <<"Function not defined">>
                        , name => Name
                        , arity => Arity }
                        , Opts ),
            Forms1
    end.

export_function(Name, Arity, Forms) ->
    export_function(Name, Arity, Forms, #{}).

export_function(Name, Arity, Forms0, Opts) ->
    Forms = normalize(Forms0),
    case not is_function_exported(Name, Arity, Forms) of
        true ->
            parse_trans:export_function(Name, Arity, Forms);

        false ->
            log_or_raise( warning
                        , function_already_exported
                        , #{ text => <<"Function already exported">>
                           , name => Name
                           , arity => Arity }
                        , Opts ),
            Forms
    end.

unexport_function(Name, Forms) ->
    unexport_function(Name, Forms, #{}).

unexport_function(Name, Arity, Forms) when is_integer(Arity) ->
    unexport_function(Name, Arity, Forms, #{});
unexport_function(Name, Forms0, Opts) ->
    Forms = normalize(Forms0),
    case is_function_exported(Name, Forms) of
        true ->
            lists:filtermap(
                fun(Form) ->
                    case is_attribute(Name, Form) of
                        true ->
                            %% TODO: Match using erl_syntax
                            {attribute, _, export, Funs0} = Form,
                            case lists:filter( fun({N, _}) -> N =/= Name end
                                             , Funs0 )
                            of
                                [] ->
                                    false;

                                Funs ->
                                    Pos = erl_syntax:get_pos(Form),
                                    %% TODO: Construct export attr using erl_syntax
                                    {true, {attribute, Pos, export, Funs}}
                            end;

                        false ->
                            {true, Form}
                    end
                end,
                Forms
            );

        false ->
            log_or_raise( warning
                        , function_not_exported
                        , #{ text => <<"Function not exported">>
                           , name => Name }
                        , Opts ),
            Forms
    end.

unexport_function(Name, Arity, Forms0, Opts) ->
    Forms = normalize(Forms0),
    case is_function_exported(Name, Arity, Forms) of
        true ->
            lists:filtermap(
                fun(Form) ->
                    case is_attribute(export, Form) of
                        true ->
                            %% TODO: Match using erl_syntax
                            {attribute, _, export, Funs0} = Form,
                            case lists:filter( fun({N, A}) -> N =/= Name orelse
                                                              A =/= Arity end
                                             , Funs0 )
                            of
                                [] ->
                                    false;

                                Funs ->
                                    Pos = erl_syntax:get_pos(Form),
                                    %% TODO: Construct export attr using erl_syntax
                                    {true, {attribute, Pos, export, Funs}}
                            end;

                        false ->
                            {true, Form}
                    end
                end,
                Forms
            );

        false ->
            log_or_raise( warning
                        , function_not_exported
                        , #{ text => <<"Function not exported">>
                           , name => Name
                           , arity => Arity }
                        , Opts ),
            Forms
    end.

is_function_exported(_, [{eof, _}]) ->
    false;
is_function_exported(Name, [Form | Forms]) ->
    case is_attribute(export, Form) of
        true ->
            %% TODO: Match using erl_syntax
            {attribute, _, export, Funs} = Form,
            case lists:any(fun({N, _}) -> N =:= Name end, Funs)
            of
                true ->
                    true;

                false ->
                    is_function_exported(Name, Forms)
            end;

        false ->
            is_function_exported(Name, Forms)
    end;
is_function_exported(_, []) ->
    false;
is_function_exported(Name, Forms) when is_tuple(Forms) ->
    is_function_exported(Name, [Forms]).

is_function_exported(_, _, [{eof, _}]) ->
    false;
is_function_exported(Name, Arity, [Form | Forms]) ->
    case is_attribute(export, Form) of
        true ->
            %% TODO: Match using erl_syntax
            {attribute, _, export, Funs} = Form,
            case lists:any( fun({N, A}) -> N =:= Name andalso A =:= Arity end
                          , Funs )
            of
                true ->
                    true;

                false ->
                    is_function_exported(Name, Arity, Forms)
            end;

        false ->
            is_function_exported(Name, Arity, Forms)
    end;
is_function_exported(_, _, []) ->
    false;
is_function_exported(Name, Arity, Forms) when is_tuple(Forms) ->
    is_function_exported(Name, Arity, [Forms]).

find_function(_, _, [{eof, _}]) ->
    false;
find_function(Name, Arity, [Form | T]) ->
    case is_function(Name, Arity, Form) of
        true ->
            {true, Form};

        false ->
            find_function(Name, Arity, T)
        end;
find_function(_, _, []) ->
    false;
find_function(Name, Arity, Forms) when is_tuple(Forms) ->
    find_function(Name, Arity, [Forms]).

function_exists(Name, Arity, Forms) ->
    case find_function(Name, Arity, Forms) of
        {true, _} ->
            true;

        false ->
            false
    end.

pprint(Forms) ->
    unicode:characters_to_binary(io_lib:format("~s~n", [
        lists:flatten([erl_pp:form(F) || F <- restore(Forms)])
    ])).

debug(Forms) ->
    io:format("~s~n", [pprint(Forms)]),
    Forms.

normalize(Forms) when is_list(Forms) ->
    Forms;
normalize(Forms) when is_tuple(Forms) ->
    [Forms].

restore(Forms) ->
    epp:restore_typed_record_fields(
        [erl_syntax:revert(T) || T <- lists:flatten(normalize(Forms))]).

write_file(Filename0, Forms) ->
    Filename = normalize_filename(Filename0),
    Bin = pprint(Forms),
    case file:write_file(Filename, Bin) of
        ok ->
            {ok, {Filename, Bin}};

        {error, Reason} ->
            {error, {Filename, Reason}}
    end.

get_module(Forms) ->
    parse_trans:get_module(normalize(Forms)).

module_prefix(Prefix, Forms) when is_list(Forms) ->
    module_prefix(Prefix, parse_trans:get_module(Forms));
module_prefix(Prefix, Module) when is_list(Prefix); is_binary(Prefix) ->
    unicode:characters_to_binary([Prefix, atom_to_binary(Module)]);
module_prefix(Prefix, Module) when is_atom(Prefix) ->
    module_prefix(atom_to_binary(Prefix), Module).

module_suffix(Forms, Suffix) when is_list(Forms) ->
    module_suffix(parse_trans:get_module(Forms), Suffix);
module_suffix(Module, Suffix) when is_list(Suffix); is_binary(Suffix) ->
    unicode:characters_to_binary([atom_to_binary(Module), Suffix]);
module_suffix(Module, Suffix) when is_atom(Suffix) ->
    module_suffix(Module, atom_to_binary(Suffix)).

eval(Forms) ->
    eval(Forms, []).

eval(Forms, Bindings) ->
    {value, Value, _NewBindings} = erl_eval:exprs(restore(Forms), Bindings),
    Value.

log(Level, String, Opts) when is_list(String) ->
    log(Level, String, [], Opts);
log(Level, Report, Opts) ->
    log(Level, Report, #{}, Opts).

log(_, _, _, #{log := false}) ->
    ok;
log(Level, StringOrReport, Metadata, _) ->
    logger:log(Level, StringOrReport, Metadata).

log_or_raise(Level, Reason, String, Opts) when is_list(String) ->
    log_or_raise(Level, Reason, String, [], Opts);
log_or_raise(Level, Reason, Report, Opts) ->
    log_or_raise(Level, Reason, Report, #{}, Opts).

log_or_raise(_, _, _, _, #{log := false}) ->
    ok;
log_or_raise(warning, Reason, StringOrReport, _, #{warnings_as_errors := true}) ->
    error({Reason, StringOrReport});
log_or_raise(Level, _, StringOrReport, Metadata, Opts) ->
    log(Level, StringOrReport, Metadata, Opts).

is_attribute(Name, Form) ->
    erl_syntax:type(Form) =:= attribute
    andalso attribute_name(Form) =:= Name.

is_function(Name, Arity, Form) ->
    erl_syntax:type(Form) =:= function
    andalso function_name(Form) =:= Name
    andalso erl_syntax:function_arity(Form) =:= Arity.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_all_attributes(Names, [H | T], Acc) ->
    find_all_attributes(Names, T, lists:foldl( fun(Name, Acc1) ->
                                                   case is_attribute(Name, H) of
                                                       true -> [H | Acc1];

                                                       false -> Acc1
                                                   end
                                               end, Acc, Names ));
find_all_attributes(_, [], Acc) ->
    lists:reverse(Acc).

flatten_text([L | _] = Lines) when is_list(L) ->
    lists:foldr(fun(S, T) -> S ++ [$\n | T] end, "", Lines);
flatten_text([B | _] = Lines) when is_binary(B) ->
    lists:foldr(fun(S, T) -> binary_to_list(S) ++ [$\n | T] end, "", Lines);
flatten_text(Text) when is_binary(Text) ->
    binary_to_list(Text);
flatten_text(Text) ->
    Text.

get_env(Opts) ->
    get_value(env, Opts, []).

env_to_abstract(Env) ->
    env_to_abstract(Env, []).

env_to_abstract(Env, Acc0) when is_list(Env) ->
    lists:foldl( fun({K, {abstract, V}}, Acc1) ->
                        [{K, V} | Acc1];

                    ({K, V}, Acc1) ->
                        [{K, to_abstract(V)} | Acc1];

                    (F, Acc1) when is_function(F, 0) ->
                        env_to_abstract(F(), Acc1);

                    (E, Acc1) when is_list(E) ->
                        env_to_abstract(E, Acc1) end
               , Acc0
               , Env );
env_to_abstract(Env, Acc) when is_map(Env) ->
    env_to_abstract(proplists:from_map(Env), Acc).

to_abstract(Term) ->
    erl_syntax:abstract(Term).

quote_error(Text, Env, Reason, Stacktrace) ->
    error({quote, [ {text, Text}
                  , {env, Env}
                  , {reason, Reason}
                  , {stacktrace, Stacktrace} ]}).

guess_fun_name([$', $@ | FunName], Opts) ->
    Env = get_env(Opts),
    lookup(guess_env_fun_name(FunName, []), Env);
guess_fun_name(Text, _) ->
    do_guess_fun_name(Text, []).

guess_env_fun_name([$' | _], Acc) ->
    erlang:list_to_existing_atom(lists:reverse(Acc));
guess_env_fun_name([H | T], Acc) ->
    guess_env_fun_name(T, [H | Acc]).

% TODO: Check if should use list_to_existing_atom
do_guess_fun_name([$( | _], Acc) ->
    erlang:list_to_atom(lists:reverse(Acc));
do_guess_fun_name([32 | T], []) ->
    do_guess_fun_name(T, []);
do_guess_fun_name([H | T], Acc) ->
    do_guess_fun_name(T, [H | Acc]).

guess_fun_arity(Text) ->
    guess_fun_arity(Text, 0, 0).

guess_fun_arity([$) | _], 1, Arity) ->
    Arity;
guess_fun_arity([$( | T], Depth, 0) ->
    case maybe_arity_zero(T) of
        true -> 0;
        false -> guess_fun_arity(T, Depth + 1, 1)
    end;
guess_fun_arity([$( | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$# | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$} | T], Depth, Arity) ->
    guess_fun_arity(T, Depth - 1, Arity);
guess_fun_arity([$[ | T], Depth, Arity) ->
    guess_fun_arity(T, Depth + 1, Arity);
guess_fun_arity([$] | T], Depth, Arity) ->
    guess_fun_arity(T, Depth - 1, Arity);
guess_fun_arity([$, | T], 1, Arity) ->
    guess_fun_arity(T, 1, Arity + 1);
guess_fun_arity([_ | T], Depth, Arity) ->
    guess_fun_arity(T, Depth, Arity).

maybe_arity_zero([32 | T]) ->
    maybe_arity_zero(T);
maybe_arity_zero([$) | _]) ->
    true;
maybe_arity_zero(_) ->
    false.

get_value(Key, Proplist, Default) when is_list(Proplist) ->
    proplists:get_value(Key, Proplist, Default);
get_value(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default).

lookup(Key, Proplist) when is_list(Proplist) ->
    {Key, Value} = proplists:lookup(Key, Proplist),
    Value;
lookup(Key, Map) when is_map(Map) ->
    maps:get(Key, Map).

safe_lookup(Key, Proplist) when is_list(Proplist) ->
    case proplists:lookup(Key, Proplist) of
        {Key, Value} ->
            {ok, Value};

        none ->
            {error, none}
    end;
safe_lookup(Key, Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            {ok, Value};

        error ->
            {error, none}
    end.

proplist(Proplist) when is_list(Proplist) ->
    proplists:unfold(Proplist);
proplist(Map) when is_map(Map) ->
    proplists:from_map(Map).

parse_trans_context(Forms) ->
    parse_trans_context(Forms, []).

parse_trans_context(Forms, Options) ->
    parse_trans:initial_context(normalize(Forms), Options).

attribute_name(Form) ->
    erl_syntax:atom_value(erl_syntax:attribute_name(Form)).

function_name(Form) ->
    erl_syntax:atom_value(erl_syntax:function_name(Form)).

attach_clauses({function, Pos, Name, Arity, OldClauses}, NewClauses, Opts) ->
    Clauses =
        case safe_lookup(if_function_exists, Opts) of
            {ok, append} ->
                OldClauses ++ NewClauses;

            {ok, prepend} ->
                NewClauses ++ OldClauses;

            {error, none} ->
                log_or_raise( warning
                            , function_already_defined
                            , #{ text => <<"Remove the function or set "
                                           "'append' or 'prepend' to "
                                           "'if_function_exists' option">>
                               , name => Name
                               , arity => Arity }
                            , Opts ),
                OldClauses
        end,
    {function, Pos, Name, Arity, Clauses}.

normalize_filename(Filename) when is_binary(Filename) ->
    Filename;
normalize_filename(Filename) when is_list(Filename) ->
    case io_lib:char_list(Filename) of
        true ->
            Filename;

        false ->
            filename:join(Filename)
    end.

%%%=============================================================================
%%% Test functions
%%%=============================================================================

-ifdef(TEST).

quote_test() ->
    ?assertEqual( {function,1,foo,1,[{clause,1,[{var,1,'Foo'}],[],[{var,1,'Foo'}]}]}
                , quote("foo(Foo) -> Foo.") ).

unquote_test() ->
    ?assertEqual( "foo(Foo) -> Foo."
                , unquote({function,1,foo,1,[{clause,1,[{var,1,'Foo'}],[],[{var,1,'Foo'}]}]}) ).

-endif.

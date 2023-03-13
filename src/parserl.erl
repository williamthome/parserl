-module(parserl).

%% API
-export([ quote/1, quote/2, unquote/1, unquote/2, insert_above/2, insert_below/2
        , insert_attribute/2, insert_attribute/3, remove_attribute/2
        , find_attribute/2, insert_function/2, insert_function/3
        , replace_function/2 , replace_function/3, export_function/3
        , unexport_function/2, unexport_function/3, find_function/3
        , function_exists/3, debug/1, revert/1, write_file/2, get_module/1
        , module_prefix/2, module_suffix/2 ]).

-include_lib("syntax_tools/include/merl.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

quote(Text) ->
    quote(Text, []).

quote(Text0, Env0) when is_list(Text0); is_binary(Text0) ->
    Text = flatten_text(Text0),
    Env = env_to_abstract(Env0),
    case Env =:= [] of
        true ->
            try ?Q(Text)
            catch
                _:Reason:Stack ->
                    quote_error(Text, Env, Reason, Stack)
            end;

        false ->
            try ?Q(Text, Env)
            catch
                _:Reason:Stack ->
                    quote_error(Text, Env, Reason, Stack)
            end
    end.

unquote(Abstract) ->
    unquote(Abstract, []).

%% @see https://www.erlang.org/doc/man/erl_prettypr.html#format-2
unquote(Abstract, Opts) ->
    erl_prettypr:format(Abstract, Opts).

insert_above(Form, Forms0) when is_list(Form) ->
    Context = parse_trans_context(Forms0),
    {Forms, _} = parse_trans:do_transform(
        fun(function, F, _, false) ->
                {Form, F, [], false, true};

            (_, F, _, Acc) ->
                {F, false, Acc} end,
        false, Forms0, Context),
    Forms;
insert_above(Form, Forms0) when is_tuple(Form) ->
    insert_above([Form], Forms0).

insert_below(Form, [F | Rest]) ->
    case erl_syntax:type(F) of
        eof_marker ->
            case Form of
                Form when is_list(Form) ->
                    Form ++ [F | Rest];

                Form when is_tuple(Form) ->
                    [Form, F | Rest]
            end;

        _ ->
            [F | insert_below(Form, Rest)]
    end.

insert_attribute(Text, Forms) ->
    insert_attribute(Text, Forms, []).

insert_attribute(Text, Forms, Opts) ->
    Abstract = quote(Text, get_env(Opts)),
    insert_above(Abstract, Forms).

remove_attribute(Name, Forms) ->
    lists:filter(
        fun({attribute, _, N, _}) -> N =/= Name end,
        Forms
    ).

find_attribute(Name, [{attribute, _, Name, _} = Attr | _]) ->
    {true, Attr};
find_attribute(Name, [_ | T]) ->
    find_attribute(Name, T);
find_attribute(_, []) ->
    false.

insert_function(Text, Forms) ->
    insert_function(Text, Forms, []).

insert_function(Text0, Forms0, Opts) ->
    Text = flatten_text(Text0),
    Name = guess_fun_name(Text),
    Arity = guess_fun_arity(Text),
    Abstract = quote(Text, get_env(Opts)),
    Forms = insert_below(Abstract, Forms0),
    case get_value(export, Opts, false) of
        true ->
            export_function(Name, Arity, Forms);

        false ->
            Forms
    end.

replace_function(Text, Forms) ->
    replace_function(Text, Forms, []).

replace_function(Text, Forms0, Opts) ->
    Body = flatten_text(Text),
    Name = guess_fun_name(Body),
    Arity = get_value(arity, Opts, guess_fun_arity(Body)),
    Form = quote(Text, get_env(Opts)),
    Forms = parse_trans:replace_function(Name, Arity, Form,
                                         Forms0, proplist(Opts)),
    case get_value(export, Opts, false) of
        true ->
            export_function(Name, Arity, Forms);

        false ->
            Forms
    end.

export_function(Name, Arity, Forms) ->
    parse_trans:export_function(Name, Arity, Forms).

unexport_function(Name, Forms) ->
    lists:filtermap(
        fun({attribute, Pos, export, Export0}) ->
                case lists:filter(fun({N, _}) -> N =/= Name end, Export0) of
                    [] ->
                        false;

                    Export ->
                        {true, {attribute, Pos, export, Export}}
                end;

            (Form) ->
                {true, Form}
        end,
        Forms
    ).

unexport_function(Name, Arity, Forms) ->
    lists:filtermap(
        fun({attribute, Pos, export, Export0}) ->
                case lists:filter(fun({N, A}) -> N =/= Name orelse
                                                 A =/= Arity end, Export0)
                of
                    [] ->
                        false;

                    Export ->
                        {true, {attribute, Pos, export, Export}}
                end;

            (Form) ->
                {true, Form}
        end,
        Forms
    ).

find_function(Name, Arity, [{function, _, Name, Arity, _} = Fun | _]) ->
    {true, Fun};
find_function(Name, Arity, [_ | T]) ->
    find_function(Name, Arity, T);
find_function(_, _, []) ->
    false.

function_exists(Name, Arity, Forms) ->
    case find_function(Name, Arity, Forms) of
        {true, _} ->
            true;

        false ->
            false
    end.

pprint(Forms) ->
    unicode:characters_to_nfc_binary(io_lib:format("~s~n", [
        lists:flatten([erl_pp:form(F) || F <- revert(Forms)])
    ])).

debug(Forms) ->
    io:format("~s~n", [pprint(Forms)]),
    Forms.

revert(Forms) when is_list(Forms) ->
    epp:restore_typed_record_fields(
        [erl_syntax:revert(T) || T <- lists:flatten(Forms)]);
revert(Forms) when is_tuple(Forms) ->
    revert([Forms]).

write_file(Filename, Forms) ->
    file:write_file(Filename, pprint(Forms)).

get_module(Forms) ->
    parse_trans:get_module(Forms).

module_prefix(Prefix, Forms) when is_list(Prefix); is_binary(Prefix) ->
    iolist_to_binary([Prefix, atom_to_binary(parse_trans:get_module(Forms))]);
module_prefix(Prefix, Forms) when is_atom(Prefix) ->
    module_prefix(atom_to_binary(Prefix), Forms).

module_suffix(Suffix, Forms) when is_list(Suffix); is_binary(Suffix) ->
    iolist_to_binary([atom_to_binary(parse_trans:get_module(Forms)), Suffix]);
module_suffix(Suffix, Forms) when is_atom(Suffix) ->
    module_suffix(atom_to_binary(Suffix), Forms).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

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
    lists:foldl(
        fun({K, {abstract, V}}, Acc1) ->
               [{K, V} | Acc1];

           ({K, V}, Acc1) ->
               [{K, to_abstract(V)} | Acc1];

           (F, Acc1) when is_function(F, 0) ->
               env_to_abstract(F(), Acc1);

           (E, Acc1) when is_list(E) ->
               env_to_abstract(E, Acc1)
        end, Acc0, Env);
env_to_abstract(Env, Acc) when is_map(Env) ->
    env_to_abstract(proplists:from_map(Env), Acc).

to_abstract(Term) ->
    erl_syntax:abstract(Term).

quote_error(Text, Env, Reason, Stacktrace) ->
    error({quote, [ {text, Text}
                  , {env, Env}
                  , {reason, Reason}
                  , {stacktrace, Stacktrace} ]}).

guess_fun_name(Text) ->
    guess_fun_name(Text, []).

guess_fun_name([$( | _], Acc) ->
    erlang:list_to_atom(lists:reverse(Acc));
guess_fun_name([32 | T], []) ->
    guess_fun_name(T, []);
guess_fun_name([H | T], Acc) ->
    guess_fun_name(T, [H | Acc]).

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

proplist(Proplist) when is_list(Proplist) ->
    proplists:unfold(Proplist);
proplist(Map) when is_map(Map) ->
    proplists:from_map(Map).

parse_trans_context(Forms) ->
    parse_trans_context(Forms, []).

parse_trans_context(Forms, Options) ->
    parse_trans:initial_context(Forms, Options).

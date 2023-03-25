-module(parserl_trans).

%% API
-export([ form/2, form/3, form/4, insert_above/1, insert_below/1
        , insert_attribute/1, insert_attribute/2, remove_attribute/1
        , attribute_exists/1, insert_function/1, insert_function/2
        , replace_function/1, replace_function/2, export_function/1
        , export_function/2, unexport_function/1, unexport_function/2
        , function_exists/1, debug/0, write_file/1, if_true/2, if_false/2
        , if_else/3, restore/0, foreach/2 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

form(Forms, TransFuns) ->
    form(Forms, #{}, TransFuns).

form(Forms, GlobalOpts, TransFuns) ->
    form(Forms, GlobalOpts, undefined, TransFuns).

form(Forms, GlobalOpts, Context, TransFuns) ->
    lists:foldl( fun(T, {F, C}) when is_function(T, 3) -> T(F, C, GlobalOpts) end
               , {Forms, Context}, TransFuns ++ [restore()]).

insert_above(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {Form, Context} = Fun(Context0),
        {parserl:insert_above(Form, Forms), Context}
    end;
insert_above(Form) ->
    fun(Forms, Context, _) ->
        {parserl:insert_above(Form, Forms), Context}
    end.

insert_below(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {Form, Context} = Fun(Context0),
        {parserl:insert_below(Form, Forms), Context}
    end;
insert_below(Form) ->
    fun(Forms, Context, _) ->
        {parserl:insert_below(Form, Forms), Context}
    end.

insert_attribute(FunOrText) ->
    insert_attribute(FunOrText, []).

insert_attribute(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0, GlobalOpts) ->
        {Text, Context} = Fun(Context0),
        {parserl:insert_attribute( Text
                                 , Forms
                                 , merge_opts(Opts, GlobalOpts) ), Context}
    end;
insert_attribute(Text, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:insert_attribute( Text
                                 , Forms
                                 , merge_opts(Opts, GlobalOpts) ), Context}
    end.

remove_attribute(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {Name, Context} = Fun(Context0),
        {parserl:remove_attribute(Name, Forms), Context}
    end;
remove_attribute(Name) ->
    fun(Forms, Context, _) ->
        {parserl:remove_attribute(Name, Forms), Context}
    end.

attribute_exists(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {Name, Context} = Fun(Context0),
        {parserl:attribute_exists(Name, Forms), Context}
    end;
attribute_exists(Name) ->
    fun(Forms, Context, _) ->
        {parserl:attribute_exists(Name, Forms), Context}
    end.

insert_function(FunOrText) ->
    insert_function(FunOrText, []).

insert_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0, GlobalOpts) ->
        {Text, Context} = Fun(Context0),
        {parserl:insert_function( Text
                                , Forms
                                , merge_opts(Opts, GlobalOpts) ), Context}
    end;
insert_function(Text, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:insert_function( Text
                                , Forms
                                , merge_opts(Opts, GlobalOpts) ), Context}
    end.

replace_function(FunOrText) ->
    replace_function(FunOrText, []).

replace_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0, GlobalOpts) ->
        {Text, Context} = Fun(Context0),
        {parserl:replace_function( Text
                                 , Forms
                                 , merge_opts(Opts, GlobalOpts) ), Context}
    end;
replace_function(Text, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:replace_function( Text
                                 , Forms
                                 , merge_opts(Opts, GlobalOpts) ), Context}
    end.

export_function(Fun) ->
    export_function(Fun, #{}).

export_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0, GlobalOpts) ->
        {{Name, Arity}, Context} = Fun(Context0),
        {parserl:export_function( Name
                                 , Arity
                                 , Forms
                                 , merge_opts(Opts, GlobalOpts) ), Context}
    end;
export_function({Name, Arity}, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:export_function( Name
                                , Arity
                                , Forms
                                , merge_opts(Opts, GlobalOpts) ), Context}
    end.

unexport_function(Fun) ->
    unexport_function(Fun, #{}).

unexport_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0, GlobalOpts) ->
        case Fun(Context0) of
            {{Name, Arity}, Context} ->
                {parserl:unexport_function( Name
                                          , Arity
                                          , Forms
                                          , merge_opts(Opts, GlobalOpts) ), Context};
            {Name, Context} ->
                {parserl:unexport_function( Name
                                          , Forms
                                          , merge_opts(Opts, GlobalOpts) ), Context}
        end
    end;
unexport_function({Name, Arity}, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:unexport_function( Name
                                  , Arity
                                  , Forms
                                  , merge_opts(Opts, GlobalOpts) ), Context}
    end;
unexport_function(Name, Opts) ->
    fun(Forms, Context, GlobalOpts) ->
        {parserl:unexport_function( Name
                                  , Forms
                                  , merge_opts(Opts, GlobalOpts) ), Context}
    end.

function_exists(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {{Name, Arity}, Context} = Fun(Context0),
        {parserl:function_exists(Name, Arity, Forms), Context}
    end;
function_exists({Name, Arity}) ->
    fun(Forms, Context, _) ->
        {parserl:function_exists(Name, Arity, Forms), Context}
    end;
function_exists(Name) ->
    fun(Forms, Context, _) ->
        {parserl:function_exists(Name, Forms), Context}
    end.

debug() ->
    fun(Forms, Context, _) ->
        {parserl:debug(Forms), Context}
    end.

write_file(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0, _) ->
        {Filename, Context} = Fun(Context0),
        {parserl:write_file(Filename, Forms), Context}
    end;
write_file(Filename) ->
    fun(Forms, Context, _) ->
        {parserl:write_file(Filename, Forms), Context}
    end.

if_true(true, Unresolved) ->
    fun(Forms, Context, GlobalOpts) ->
        resolve(Forms, Context, GlobalOpts, Unresolved)
    end;
if_true(false, _) ->
    fun(Forms, Context, _) ->
        {Forms, Context}
    end;
if_true(BoolFun, Unresolved) ->
    fun(Forms, Context0, GlobalOpts) ->
        case BoolFun(Forms, Context0, GlobalOpts) of
            {true, Context} ->
                resolve(Forms, Context, GlobalOpts, Unresolved);

            {false, Context} ->
                {Forms, Context}
        end
    end.

if_false(true, _) ->
    fun(Forms, Context, _) ->
        {Forms, Context}
    end;
if_false(false, Unresolved) ->
    fun(Forms, Context, GlobalOpts) ->
        resolve(Forms, Context, GlobalOpts, Unresolved)
    end;
if_false(BoolFun, Unresolved) ->
    fun(Forms, Context0, GlobalOpts) ->
        case BoolFun(Forms, Context0, GlobalOpts) of
            {false, Context} ->
                resolve(Forms, Context, GlobalOpts, Unresolved);

            {true, Context} ->
                {Forms, Context}
        end
    end.

if_else(true, Unresolved, _) ->
    fun(Forms, Context, GlobalOpts) ->
        resolve(Forms, Context, GlobalOpts, Unresolved)
    end;
if_else(false, _, Unresolved) ->
    fun(Forms, Context, GlobalOpts) ->
        resolve(Forms, Context, GlobalOpts, Unresolved)
    end;
if_else(BoolFun, IfTrue, IfFalse) ->
    fun(Forms, Context0, GlobalOpts) ->
        case BoolFun(Forms, Context0, GlobalOpts) of
            {true, Context} ->
                resolve(Forms, Context, GlobalOpts, IfTrue);

            {false, Context} ->
                resolve(Forms, Context, GlobalOpts, IfFalse)
        end
    end.

restore() ->
    fun(Forms, _, _) ->
        parserl:restore(Forms)
    end.

foreach(Predicate, List) ->
    fun(Forms, Context, GlobalOpts) ->
        lists:foldl( fun(Term, Acc) ->
                         Unresolved = Predicate(Term),
                         resolve(Acc, Context, GlobalOpts, Unresolved) end
                   , Forms
                   , List )
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


resolve({Forms, Context}, _, GlobalOpts, TransFun) ->
    % Resolves foreach result
    resolve(Forms, Context, GlobalOpts, TransFun);
resolve(Forms, Context, GlobalOpts, TransFun) when is_function(TransFun, 3) ->
    TransFun(Forms, Context, GlobalOpts);
resolve(Forms, Context, GlobalOpts, TransFuns) when is_list(TransFuns) ->
    lists:foldl( fun(TransFun, {F, C}) -> resolve(F, C, GlobalOpts, TransFun) end
               , {Forms, Context}
               , TransFuns ).

%% TODO: Improve merge

merge_opts(Opts, GlobalOpts) when is_list(Opts) ->
    merge_opts(proplists:to_map(Opts), GlobalOpts);
merge_opts(Opts, GlobalOpts) when is_list(GlobalOpts) ->
    merge_opts(Opts, proplists:to_map(GlobalOpts));
merge_opts(Opts, GlobalOpts) ->
    maps:merge(maps:merge(Opts, GlobalOpts), #{
        env => maps:merge( maps:get(env, GlobalOpts, #{})
                         , maps:get(env, Opts, #{}) )
    }).

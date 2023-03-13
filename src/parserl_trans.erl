-module(parserl_trans).

%% API
-export([ form/2, form/3, insert_above/1, insert_below/1
        , insert_attribute/1, insert_attribute/2, remove_attribute/1
        , attribute_exists/1, insert_function/1, insert_function/2
        , replace_function/1, replace_function/2, export_function/1
        , unexport_function/1, function_exists/1, debug/0, write_file/1
        , if_true/2, if_false/2, if_else/3, normalize/0, foreach/2 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

form(Forms, TransFuns) ->
    form(Forms, undefined, TransFuns).

form(Forms, Context, TransFuns) ->
    lists:foldl(fun(T, {F, C}) when is_function(T, 2) -> T(F, C) end,
                {Forms, Context}, TransFuns ++ [normalize()]).

insert_above(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Form, Context} = Fun(Context0),
        {parserl:insert_above(Form, Forms), Context}
    end;
insert_above(Form) ->
    fun(Forms, Context) ->
        {parserl:insert_above(Form, Forms), Context}
    end.

insert_below(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Form, Context} = Fun(Context0),
        {parserl:insert_below(Form, Forms), Context}
    end;
insert_below(Form) ->
    fun(Forms, Context) ->
        {parserl:insert_below(Form, Forms), Context}
    end.

insert_attribute(FunOrText) ->
    insert_attribute(FunOrText, []).

insert_attribute(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Text, Context} = Fun(Context0),
        {parserl:insert_attribute(Text, Forms, Opts), Context}
    end;
insert_attribute(Text, Opts) ->
    fun(Forms, Context) ->
        {parserl:insert_attribute(Text, Forms, Opts), Context}
    end.

remove_attribute(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Name, Context} = Fun(Context0),
        {parserl:remove_attribute(Name, Forms), Context}
    end;
remove_attribute(Name) ->
    fun(Forms, Context) ->
        {parserl:remove_attribute(Name, Forms), Context}
    end.

attribute_exists(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Name, Context} = Fun(Context0),
        {parserl:attribute_exists(Name, Forms), Context}
    end;
attribute_exists(Name) ->
    fun(Forms, Context) ->
        {parserl:attribute_exists(Name, Forms), Context}
    end.

insert_function(FunOrText) ->
    insert_function(FunOrText, []).

insert_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Text, Context} = Fun(Context0),
        {parserl:insert_function(Text, Forms, Opts), Context}
    end;
insert_function(Text, Opts) ->
    fun(Forms, Context) ->
        {parserl:insert_function(Text, Forms, Opts), Context}
    end.

replace_function(FunOrText) ->
    replace_function(FunOrText, []).

replace_function(Fun, Opts) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Text, Context} = Fun(Context0),
        {parserl:replace_function(Text, Forms, Opts), Context}
    end;
replace_function(Text, Opts) ->
    fun(Forms, Context) ->
        {parserl:replace_function(Text, Forms, Opts), Context}
    end.

export_function(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {{Name, Arity}, Context} = Fun(Context0),
        {parserl:export_function(Name, Arity, Forms), Context}
    end;
export_function({Name, Arity}) ->
    fun(Forms, Context) ->
        {parserl:export_function(Name, Arity, Forms), Context}
    end.

unexport_function(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        case Fun(Context0) of
            {{Name, Arity}, Context} ->
                {parserl:unexport_function(Name, Arity, Forms), Context};
            {Name, Context} ->
                {parserl:unexport_function(Name, Forms), Context}
        end
    end;
unexport_function({Name, Arity}) ->
    fun(Forms, Context) ->
        {parserl:unexport_function(Name, Arity, Forms), Context}
    end;
unexport_function(Name) ->
    fun(Forms, Context) ->
        {parserl:unexport_function(Name, Forms), Context}
    end.

function_exists(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {{Name, Arity}, Context} = Fun(Context0),
        {parserl:function_exists(Name, Arity, Forms), Context}
    end;
function_exists({Name, Arity}) ->
    fun(Forms, Context) ->
        {parserl:function_exists(Name, Arity, Forms), Context}
    end.

debug() ->
    fun(Forms, Context) ->
        {parserl:debug(Forms), Context}
    end.

write_file(Fun) when is_function(Fun, 1) ->
    fun(Forms, Context0) ->
        {Filename, Context} = Fun(Context0),
        {parserl:write_file(Filename, Forms), Context}
    end;
write_file(Filename) ->
    fun(Forms, Context) ->
        {parserl:write_file(Filename, Forms), Context}
    end.

if_true(true, Unresolved) ->
    fun(Forms, Context) ->
        resolve(Forms, Context, Unresolved)
    end;
if_true(false, _) ->
    fun(Forms, Context) ->
        {Forms, Context}
    end;
if_true(BoolFun, Unresolved) ->
    fun(Forms, Context0) ->
        case BoolFun(Forms, Context0) of
            {true, Context} ->
                resolve(Forms, Context, Unresolved);

            {false, Context} ->
                {Forms, Context}
        end
    end.

if_false(true, _) ->
    fun(Forms, Context) ->
        {Forms, Context}
    end;
if_false(false, Unresolved) ->
    fun(Forms, Context) ->
        resolve(Forms, Context, Unresolved)
    end;
if_false(BoolFun, Unresolved) ->
    fun(Forms, Context0) ->
        case BoolFun(Forms, Context0) of
            {false, Context} ->
                resolve(Forms, Context, Unresolved);

            {true, Context} ->
                {Forms, Context}
        end
    end.

if_else(true, Unresolved, _) ->
    fun(Forms, Context) ->
        resolve(Forms, Context, Unresolved)
    end;
if_else(false, _, Unresolved) ->
    fun(Forms, Context) ->
        resolve(Forms, Context, Unresolved)
    end;
if_else(BoolFun, IfTrue, IfFalse) ->
    fun(Forms, Context0) ->
        case BoolFun(Forms, Context0) of
            {true, Context} ->
                resolve(Forms, Context, IfTrue);

            {false, Context} ->
                resolve(Forms, Context, IfFalse)
        end
    end.

normalize() ->
    fun(Forms, _) ->
        parserl:revert(Forms)
    end.

foreach(Predicate, List) ->
    fun(Forms, Context) ->
        lists:foldl(
            fun(Term, Acc) ->
                Unresolved = Predicate(Term),
                resolve(Acc, Context, Unresolved)
            end,
            Forms,
            List
        )
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

resolve(Forms, Context, TransFun) when is_function(TransFun, 2) ->
    TransFun(Forms, Context);
resolve(Forms, Context, TransFuns) when is_list(TransFuns) ->
    form(Forms, Context, TransFuns).

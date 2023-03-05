-module(parserl_trans).

%% API
-export([transform/2, insert_above/1, insert_below/1, insert_attribute/1,
         insert_attribute/2, remove_attribute/1, insert_function/1,
         insert_function/2, insert_function/3, replace_function/1,
         replace_function/2, replace_function/3, export_function/2,
         unexport_function/1, unexport_function/2, function_exists/2,
         debug/0, write_file/1, if_true/2, if_false/2]).

%%%=============================================================================
%%% API
%%%=============================================================================

transform(Forms, TransFuns) ->
    parserl:foldl(TransFuns ++ [revert()], Forms).

insert_above(Form) ->
    fun(Forms) -> parserl:insert_above(Form, Forms) end.

insert_below(Form) ->
    fun(Forms) -> parserl:insert_below(Form, Forms) end.

insert_attribute(Text) ->
    insert_attribute(Text, []).

insert_attribute(Text, Env) ->
    fun(Forms) -> parserl:insert_attribute(Text, Env, Forms) end.

remove_attribute(Name) ->
    fun(Forms) -> parserl:remove_attribute(Name, Forms) end.

insert_function(Text) ->
    insert_function(Text, []).

insert_function(Text, Env) ->
    insert_function(Text, Env, []).

insert_function(Text, Env, Opts) ->
    fun(Forms) -> parserl:insert_function(Text, Env, Forms, Opts) end.

replace_function(Text) ->
    replace_function(Text, []).

replace_function(Text, Env) ->
    replace_function(Text, Env, []).

replace_function(Text, Env, Opts) ->
    fun(Forms) -> parserl:replace_function(Text, Env, Forms, Opts) end.

export_function(Name, Arity) ->
    fun(Forms) -> parserl:export_function(Name, Arity, Forms) end.

unexport_function(Name) ->
    fun(Forms) -> parserl:unexport_function(Name, Forms) end.

unexport_function(Name, Arity) ->
    fun(Forms) -> parserl:unexport_function(Name, Arity, Forms) end.

function_exists(Name, Arity) ->
    fun(Forms) -> parserl:function_exists(Name, Arity, Forms) end.

debug() ->
    fun(Forms) -> parserl:debug(Forms) end.

write_file(Filename) ->
    fun(Forms) -> parserl:write_file(Filename, Forms) end.

if_true(true, Unresolved) ->
    fun(Forms) -> resolve(Unresolved, Forms) end;
if_true(false, _) ->
    fun(Forms) -> Forms end;
if_true(BoolFun, Unresolved) ->
    fun(Forms) ->
        case BoolFun(Forms) of
            true ->
                resolve(Unresolved, Forms);

            false ->
                Forms
        end
    end.

if_false(true, _) ->
    fun(Forms) -> Forms end;
if_false(false, Unresolved) ->
    fun(Forms) -> resolve(Unresolved, Forms) end;
if_false(BoolFun, Unresolved) ->
    fun(Forms) ->
        case BoolFun(Forms) of
            false ->
                resolve(Unresolved, Forms);

            true ->
                Forms
        end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

revert() ->
    fun(Forms) -> parserl:revert(Forms) end.

resolve(TransFun, Forms) when is_function(TransFun, 1) ->
    TransFun(Forms);
resolve(TransFuns, Forms) when is_list(TransFuns) ->
    parserl:foldl(TransFuns, Forms).

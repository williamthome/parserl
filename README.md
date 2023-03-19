# parserl

A helper library to simplify modules transformation.

## Examples

### #1

```erlang
-module(mymodule).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    EElModule = binary_to_atom(parserl:module_suffix("_eel", Forms)),
    parserl_trans:form(Forms, [
        parserl_trans:insert_attribute("-on_load(compile/0)."),

        parserl_trans:insert_function(
            ["compile() ->",
             "    {ok, _@eel_module} =",
             "        case render_defs(#{}) of",
             "            {{html, Html0, Opts}, _} ->",
             "                Html = unicode:characters_to_nfc_binary(string:trim(Html0)),",
             "                eel:compile_to_module(Html, _@eel_module, Opts);",
             "            {{html, Html0}, _} ->",
             "                Html = unicode:characters_to_nfc_binary(string:trim(Html0)),",
             "                eel:compile_to_module(Html, _@eel_module);",
             "            {{file, Filename, Opts}, _} ->",
             "                eel:compile_file_to_module(Filename, _@eel_module, Opts);",
             "            {{file, Filename}, _} ->",
             "                eel:compile_file_to_module(Filename, _@eel_module)",
             "        end,",
             "    ok."],
            #{env => #{eel_module => EElModule}}),

        parserl_trans:replace_function(
            "render(Bindings) -> render(Bindings, #{}).",
            #{rename_original => render_defs}),

        parserl_trans:unexport_function(render_defs),

        parserl_trans:insert_function(
            ["render(Bindings0, Opts) when is_map(Opts) ->",
             "    {_, Bindings} = render_defs(Bindings0),",
             "    _@eel_module:render(Bindings, Opts);",
             "render(Bindings, Snapshot) ->",
             "    render(Bindings, Snapshot, #{})."],
            #{env => #{eel_module => EElModule}, export => true}),

        parserl_trans:insert_function(
            ["render(Bindings0, Snapshot, Opts) ->",
             "    {_, Bindings} = render_defs(Bindings0),",
             "    _@eel_module:render(Bindings, Snapshot, Opts)."],
            #{env => #{eel_module => EElModule}, export => true}),

        parserl_trans:insert_function(
            ["static() ->",
             "    _@eel_module:static()."],
            #{env => #{eel_module => EElModule}, export => true}),

        parserl_trans:if_false(
            parserl_trans:function_exists({mount, 2}),
            parserl_trans:insert_function(
                "mount(_Params, Socket) -> {ok, Socket}.",
                [export])
        ),

        parserl_trans:if_false(
            parserl_trans:function_exists({handle_params, 2}),
            parserl_trans:insert_function(
                "handle_params(_Params, Socket) -> {noreply, Socket}.",
                [export])
        ),

        parserl_trans:debug()
    ]).
```

### #2

```erlang
-module(eel_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    case parserl:find_all_attributes(eel_fun, Forms) of
        [] ->
            logger:warning("No eel attribute found in ~p", [parserl:get_module(Forms)]),
            Forms;

        Attrs ->
            GlobalOpts = #{ if_fun_exists => append },
            parserl_trans:form(Forms, GlobalOpts, [
                parserl_trans:remove_attribute(eel_fun),
                parserl_trans:foreach(
                    fun(Attr) ->
                        Args = parserl:eval(erl_syntax:attribute_arguments(Attr)),
                        {FunName, Kind, FunOpts} = Args,
                        {ok, {Static, AST}} =
                            case Kind of
                                {bin, Bin, CompOpts} ->
                                    eel:compile(Bin, CompOpts);

                                {file, Filename, CompOpts} ->
                                    eel:compile_file(Filename, CompOpts)
                            end,
                        Vars = eel_compiler:ast_vars(AST),
                        Opts = #{ env => #{fun_name => FunName}
                                , export => maps:get(export, FunOpts, true) },
                        [
                            parserl_trans:insert_function(
                                "static('@fun_name') -> _@static.",
                                #{ env => #{ fun_name => FunName
                                           , static => Static } }),
                            parserl_trans:insert_function(
                                "ast('@fun_name') -> _@ast.",
                                #{ env => #{ fun_name => FunName
                                           , ast => AST } }),
                            parserl_trans:insert_function(
                                "vars('@fun_name') -> _@vars.",
                                #{ env => #{ fun_name => FunName
                                           , vars => Vars } }),
                            parserl_trans:if_else(
                                Vars =:= [],
                                [
                                    parserl_trans:if_else(
                                        maps:get(eval, FunOpts, false),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'() ->",
                                             "    eel_evaluator:eval(eel_renderer:render(",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    ))."], Opts),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'() ->",
                                             "    eel_renderer:render(",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    )."], Opts)
                                    )
                                ],
                                [
                                    parserl_trans:if_else(
                                        maps:get(eval, FunOpts, false),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'(Bindings) ->",
                                             "    eel_evaluator:eval(eel_renderer:render(",
                                             "        Bindings,",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    ))."], Opts),
                                        parserl_trans:insert_function(
                                            ["'@fun_name'(Bindings) ->",
                                             "    eel_renderer:render(",
                                             "        Bindings,",
                                             "        #{ static => static('@fun_name')",
                                             "         , ast => ast('@fun_name')",
                                             "         , vars => vars('@fun_name') }",
                                             "    )."], Opts)
                                    )
                                ]
                            )
                        ]
                    end,
                    Attrs
                )
            ])
    end.
```

## Credits

This lib uses [merl](https://www.erlang.org/doc/man/merl.html) and [parse_trans](https://github.com/uwiger/parse_trans) under the hood, so, many thanks to [Richard Carlsson](https://github.com/richcarl) and [Ulf Wiger](https://github.com/uwiger).

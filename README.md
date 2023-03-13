# parserl

A helper library to simplify modules transformation.

Example:

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

## Credits

This lib uses [merl](https://www.erlang.org/doc/man/merl.html) and [parse_trans](https://github.com/uwiger/parse_trans) under the hood, so, many thanks to [Richard Carlsson](https://github.com/richcarl) and [Ulf Wiger](https://github.com/uwiger).

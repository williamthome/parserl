# parserl

A helper library to simplify modules transformation.

## Introduction

In a nutshell, `parserl` is a recursive function.
The transform function receives `ASTs` (Abstract Syntax Trees), and the output is also ASTs but transformed along the way. It can also receive a module name and build the module from scratch, which is used in the [example](#example) below, but it isn't usual.

## Disclaimer

This is a work in progress lib.

## Installation

Add `parserl` to rebar.config deps:

```erlang
{deps, [{ parserl, { git, "https://github.com/williamthome/parserl.git"
                   , {branch, "main"} }}]}.
```

Run `rebar3 compile`.

## Example

```erlang
-module(example).

-export([parse_transform/2]).
-export([write_module_from_scratch/0]).

% Use as parse transform ...
parse_transform(Forms, _Options) ->
    do_transform(Forms).

% ... or start a module from scratch
write_module_from_scratch() ->
    Module = parserl_example,
    do_transform(Module),
    ok.

do_transform(ModuleOrForms) ->
    Module = get_module(ModuleOrForms),
    % Global options are optional.
    GlobalOpts = #{ env => #{module => Module}
                  , log => true
                  , if_function_exists => append },
    % Context is optional and can be anything.
    Context = #{replace_temp_fun_by => "bar() -> bar."},
    parserl:transform(ModuleOrForms, GlobalOpts, Context, [
        parserl:insert_attribute("-on_load(init/0)."),
        parserl:insert_function(
            [ "init() ->"
            , "    io:format(\"Module ~p loaded.\", [_@module])." ]),
            % The _@module can looks weird, but it's a metavariable.
            % parserl uses merl under the hood, this gives the
            % power to do metavariable substitution.
            % See the merl documentation for more information:
            %     https://www.erlang.org/doc/man/merl.html
        parserl:insert_function(
            [ "foo(Foo, _) when is_atom(Foo) ->"
            , "    foo." ]
            , [export]),
        parserl:insert_function(
            [ "foo(Foo, Bar) ->"
            , "    case Foo == Bar of"
            , "        true -> equal;"
            , "        false -> not_equal"
            , "    end." ]),
        parserl:insert_function("temp() -> deleteme."),
        parserl:if_true(
            parserl:function_exists(temp, 0),
            % if statements also accepts lists
            parserl:replace_function(
                fun(#{replace_temp_fun_by := FunBody} = Ctx0) ->
                    % Context can be transformed.
                    Ctx = maps:without([replace_temp_fun_by], Ctx0),
                    {temp, 0, FunBody, Ctx}
                end,
                [export]
            )
            % The function above can be written as
            %
            %     parserl:replace_function(temp, 0, "bar() -> bar.", [export])
            %
        ),
        parserl:write_file(["/tmp", parserl_trans:module_suffix(Module, ".erl")])
        % =NOTICE REPORT==== 25-Mar-2023::01:27:53.989528 ===
        % File saved in /tmp/parserl_example.erl
        % ---
        % -module(parserl_example).
        % -export([foo/2]).
        % -export([bar/0]).
        % init() ->
        %     io:format("Module ~p loaded.", [parserl_example]).
        % foo(Foo, _) when is_atom(Foo) ->
        %     foo;
        % foo(Foo, Bar) ->
        %     case Foo == Bar of
        %         true ->
        %             equal;
        %         false ->
        %             not_equal
        %     end.
        % bar() ->
        %     bar.
        %
        %
        % ---
    ]).

get_module(Module) when is_atom(Module) ->
    Module;
get_module(Forms) when is_list(Forms) ->
    parserl_trans:get_module(Forms).
```

## TODO

- Improve this README file
- Functions documentations
- Functions specs
- Test everything

## License

`parserl` is under the Apache 2.0 License. Please refer to the included [LICENSE](LICENSE.md) file for more information.

## Credits

This lib uses [merl](https://www.erlang.org/doc/man/merl.html) and [parse_trans](https://github.com/uwiger/parse_trans) under the hood, so, many thanks to [Richard Carlsson](https://github.com/richcarl) and [Ulf Wiger](https://github.com/uwiger).

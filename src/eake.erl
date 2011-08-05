%%%'   HEADER
%% @author    andrzej.sliwa@i-tool.eu
%% @copyright 2011 Andrzej Sliwa
%%
%% @doc eake - clone of rake for erlang
%%
%% eake is released under the MIT license.
%%
%% Copyright (c) 2011 Andrzej Sliwa
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% @end
-module(eake).

%% export API
%-ifdef(TEST).
-compile(export_all).
%-endif.

-export([main/1, task/3, namespace/2, invoke_target/2]).


%% helper macro
%-define(CMD, filename:basename(escript:script_name())).

-include("eake.hrl").

%%%.
%%%'   PUBLIC API
-spec main(Args :: [string()]) -> ok.
main(Args)->
    io:format("Eake~n"),
    process_commands(Args).

-spec task(Name :: atom(), Desc :: string(), Code :: fun()) -> eake_task().
task(Name, Desc, Code) ->
    #eake_task{name = Name, desc = Desc, code = Code}.

-spec namespace(Name :: atom(), Tasks :: [eake_task()]) -> eake_namespace().
namespace(Name, Tasks) ->
    #eake_namespace{name = Name, tasks = Tasks}.

-spec process_commands(Args :: [string()]) -> ok | error.
process_commands(Args) ->
    EakeFile = "eakefile.erl",
    case {file_exists(EakeFile), prepare_module(EakeFile)} of
        {true, ok} ->
            [FirstParam | _] = Args,
            io:format("  !!!!!!!! ~s", [FirstParam]),
            [prepare_for_target(Target) || Target <- Args],
            ok
    end.

-spec file_exists(FileName :: string()) -> boolean().
file_exists(FileName) ->
    filelib:is_regular(FileName).

-spec prepare_module(FileName :: string()) -> ok.
prepare_module(FileName) ->
    load_module(build_module(FileName)).

-spec build_module(FileName :: string()) -> {ok, ModuleName :: atom()}.
build_module(FileName) ->
    case compile:file(FileName) of
        {ok, ModuleName} -> {ok, ModuleName};
        {ok, ModuleName, _Warnings} -> {ok, ModuleName}
    end.

-spec load_module({ok, ModuleName :: atom()}) -> ok.
load_module({ok, ModuleName}) ->
    code:purge(ModuleName),
    case code:load_file(ModuleName) of
        {module, ModuleName} -> ok
    end.

-spec get_tokens(String :: string()) -> {ok, term()}.
get_tokens(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _} -> {ok, Tokens}
    end.

-spec get_terms({ok, Tokens :: term()}) -> Terms :: term().
get_terms({ok, Tokens}) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Terms} -> Terms
    end.

-spec prepare_for_target(TargetWithParams :: string()) -> ok.
prepare_for_target(TargetWithParams) ->
    [Target | TargetParams] = re:split(TargetWithParams, "[=]", [{return, list}]),
    case length(TargetParams) of
        0 -> invoke_target(Target, []);
        _GreaterThan ->
            [Params| _] = TargetParams,
            invoke_target(
                Target, get_terms(get_tokens(string:concat(Params, "."))))
    end.

-spec invoke_target(Target :: string(), Params :: term()) -> ok.
invoke_target(Target, Params) ->
    TargetsScopes = string:tokens(Target, ":"),
    case find_target(TargetsScopes, eakefile:execute()) of
        {_, _, Fun} ->
            Fun(Params),
            ok;
        missing_target -> io:format("Missing '~s' target~n", [Target])
    end.

-spec find_target(TargetScopes :: [string()], List :: [string()]) ->
    missing_target | eake_task().
find_target(TargetScopes, List) ->
    [Current| Rest] = TargetScopes,
    Search = fun
        ({_, Name, _, _}) -> atom_to_list(Name) =:= Current;
        ({_, Name, _}) -> atom_to_list(Name) =:= Current
    end,
    case lists:filter(Search, List) of
        [ {eake_task, Name, Desc, Code} | _ ] ->
			{Name, Desc, Code};
		[ {eake_namespace, _, TaskList} | _ ] ->
			find_target(Rest, TaskList);
		[] -> missing_target
    end.

-spec run(Command :: string()) -> ok.
run(Command) ->
    io:format("~s", [os:cmd(Command)]).
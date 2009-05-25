-module(eake).
-define(VERSION, "1.0").
-vsn(?VERSION).
-export([run/1, task/3, namespace/2, process_command/1, process_command/0,run_task/2]).

task(Name, Desc, Code) ->
	{task, Name, Desc, Code}.

namespace(Name, TaskList) ->
	{namespace, Name, TaskList}.

run(Command) ->
	io:format("~s",[os:cmd(Command)]).

show_info() ->
	io:format("Eake ver. ~s ~n", [?VERSION]),
	{ok, CurrentDir} = file:get_cwd(),
	io:format("(in ~s)~n", [CurrentDir]).

process_command() ->
	io:format("eake [target]~n"),
	init:stop().
	
process_command(Args) ->
	show_info(),
	case Args of 
		[TaskName | Params] -> 
			Files = ["eakefile"],
			[Eakefile | _] = Files, 
			case filelib:is_regular([Eakefile | ".erl"]) of
				true ->
					prepare_files(Files),
					run_task(TaskName, Params);
				false ->
					io:format("Missing eakefile!~n")
			end;
		[] -> io:format("Missing arguments!~n")
	end,
	init:stop().

run_task(TaskName, Params) ->
	case find_task(TaskName) of
		{ok, Code} -> 
			io:format("~nRunning target: ~n  \"~s\"~n~n", [TaskName]),
		 	Code(Params);
		error -> io:format("~nUnknown Task!~n")
	end.
	
exist_file(FileName) ->
	case filelib:is_regular(FileName) of
		true -> {exist, FileName};
		false -> {not_exist, FileName}
	end.
		
prepare_files(Files) ->
	io:format("~nBuilding:~n"),
	lists:foreach(fun(File) ->
		case load_file(build_file(delete_file(File))) of
			{ok, FileName} -> 
				io:format("  ~s: ok", [FileName]);
			{error, FileName} ->
				io:format("  ~s: faild", [FileName])
		end
	end, Files).
		
find_task(TaskName) ->
	Find = fun({task, Name, _, _}) ->
		Name =:= TaskName end,
	case lists:filter(Find, eakefile:execute()) of
		[ {_, _, _, Code} | _] ->
			{ok, Code};
	 	[] -> error
	end.


delete_file({error, FileName}) ->
	{error, FileName};
				
delete_file({ok, FileName}) ->
	delete_file(FileName);

delete_file(FileName) ->
	BeamFileName = [FileName | ".beam"],
	case exist_file(BeamFileName) of 
		{exist, BeamFileToDelete} ->
			case file:delete(BeamFileToDelete) of
				ok -> {ok, FileName};
				{error, _} -> {error, FileName}
			end;
		{not_exist, _} -> {ok, FileName}
	end.


build_file({error, FileName}) ->
	{error, FileName};
		
build_file({ok, FileName}) ->
	build_file(FileName);
	
build_file(FileName) ->
	case compile:file([FileName | ".erl"]) of
		{ok, _ModuleName} -> {ok, FileName};
		{ok, _ModuleName, _} -> {ok, FileName};
		error -> {error, FileName};
		{error, _Errors, _Warnings} -> {error, FileName}
	end.


load_file({error, FileName}) ->
	{error, FileName};

load_file({ok, FileName}) ->
	load_file(FileName);
		
load_file(FileName) ->
	case code:load_file(list_to_atom(FileName)) of
		{module, _Module} -> {ok, FileName};
		{error, _What} -> {error, FileName}
	end.


-module(eake).
-export([run/1, task/3, namespace/2, process_command/1, run_task/2]).

task(Name, Desc, Code) ->
	{task, Name, Desc, Code}.

namespace(Name, TaskList) ->
	{namespace, Name, TaskList}.

run(Command) ->
	io:format("~s",[os:cmd(Command)]).

process_command(Args) ->
	case Args of 
		[TaskName | Params] -> 
			Files = [ "eake", "eakefile"],
			io:format("Preparing EAKE:~n"),
			prepare_files(Files),
			io:format("Running   TASK: \"~s\"~n~n", [TaskName]),
			run_task(list_to_atom(TaskName), Params);
		[] -> io:format("Missing arguments!")
	end.

run_task(TaskName, Params) ->
	case find_task(TaskName) of
		{ok, Code} -> Code(Params);
		error -> io:format("Unknown Task!")
	end.
	
prepare_files(Files) ->
	lists:foreach(fun(File) ->
		case load_file(build_file(delete_file(File))) of
			{ok, FileName} -> 
				io:format("  ~s: ok~n", [FileName]);
			{error, FileName} ->
				io:format("  ~s: faild~n", [FileName])
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
	case file:delete([FileName | ".beam"]) of
		ok -> {ok, FileName};
		{error, _} -> {error, FileName}
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


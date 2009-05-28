-module(eake).
-define(VERSION, "1.0").
-vsn(?VERSION).

-export ([task/3, namespace/3, process_command/1, run_target/2, run/1]).


-ifdef(TEST).
-export([test_all/0, assert_equal/3, run_tests/1]).
-define(LOG(X), io:format("~nLOGGER : [~s]~n",[X])).
-define(assertEqual(Expected, Value), assert_equal(Expected, Value, ?LINE)).
-else.
-define(LOG(X),).
-endif.


%%{-----------------------------------------------------------------}
%% BUILD_FILE
task(Name, Desc, Code) ->
	{eake_task, Name, Desc, Code}.

namespace(Name, Desc, TaskList) ->
	{eake_namespace, Name, Desc, TaskList}.

process_command(Args) ->
	EakeFile = "eakefile",
	case exist_file([EakeFile | ".erl"]) of
		{exist, _ } ->
			prepare_file(EakeFile),
			[ prepare_target(Target) || Target <- Args ];
		_Oops -> io:format("Missing ~s~n", [EakeFile])
	end,
	init:stop().
	
prepare_target(Target) ->
	Result = re:split(atom_to_list(Target), "[=]", [{return, list}]),
	case length(Result) of
		1 -> run_target(Target, []);
		2 ->
			[NewTarget, TermsString] = Result,
			TargetParams = string:concat(TermsString, "."),
			case get_terms(get_tokens(TargetParams)) of
				{ok, Params} -> run_target(list_to_atom(NewTarget), Params);
				{error} -> io:format("Wrong Target Params.")
			end
	end.


run(Command) ->
	io:format("~s", [os:cmd(Command)]).
	
% run target by string name ex. "db:migrate"
run_target(Target, Params) ->
	TargetAsList = atom_to_list(Target),
	Targets = string:tokens(TargetAsList, ":"),
	case find_target(Targets, eakefile:execute()) of % move to module name dynamic
		{_, _, Code} ->
			io:format("Running Target: ~n \"~s\"~n~n", [TargetAsList]),
			Code(Params),
			{ok};
		{missing_target} ->
			io:format("Unknown Target! ~n"),
			{missing_target}
	end.
%%{-----------------------------------------------------------------}
get_tokens(String) ->
	case erl_scan:string(String) of
		{ok, Tokens, _} -> {ok, Tokens};
		_Oops -> {error}
	end.

get_terms({error}) ->
	{error};
	
get_terms({ok, Tokens}) ->
	case erl_parse:parse_term(Tokens) of
		{ok, Terms} -> {ok, Terms};
		_Oops -> {error}
	end.


prepare_file(File) ->
	io:format("~nBuilding:~n"),
	case load_file(build_file(delete_file(File))) of
		{ok, FileName} -> 
			io:format("  ~s: ok", [FileName]);
		{error, FileName} ->
			io:format("  ~s: faild", [FileName])
	end.

%%{-----------------------------------------------------------------}

%% find target in namespaces
%% Targets is List of strings
find_target(Targets, List) ->
	[ Head | Rest ] = Targets,
	Find_fun = fun( {_, Name, _, _} ) -> 
		Name =:= list_to_atom(Head) 
	end,
	case lists:filter(Find_fun, List) of 
		[ {eake_task, Name, Desc, Code} | _ ] ->
			{Name, Desc, Code};
		[ {eake_namespace, _, _, TaskList} | _ ] ->
			find_target(Rest, TaskList);
		[] -> {missing_target}
	end.

%%{-----------------------------------------------------------------}
%% check if file exist
exist_file(FileName) ->
	case filelib:is_regular(FileName) of
		true -> {exist, FileName};
		false -> {not_exist, FileName}
	end.

%%{-----------------------------------------------------------------}
%% BUILD_FILE

%% handle and pass error forward
build_file({error, FileName}) ->
	{error, FileName};

%% handle ok status and call 
%% buliding file
build_file({ok, FileName}) ->
	build_file(FileName);

%% build erl to beam file
build_file(FileName) ->
	File = [FileName | ".erl"],
	case exist_file(File) of
		{exist, _} -> 
			case compile:file([FileName | ".erl"]) of
				{ok, _ModuleName} -> {ok, FileName};
				{ok, _ModuleName, _} -> {ok, FileName};
				_Oops -> {error, FileName}
			end;
		_Oops -> {error, FileName}
	end.


%%{-----------------------------------------------------------------}
%% LOAD_FILE

%% handle and pass error forward
load_file({error, FileName}) ->
	{error, FileName};

%% handle ok status and call 
%% loading file
load_file({ok, FileName}) ->
	load_file(FileName);

%% load beam file in runtime
load_file(FileName) ->
	File = list_to_atom(FileName),
	code:purge(File),
	case code:load_file(File) of
		{module, _Module} -> {ok, FileName};
		{error, _What} -> {error, FileName}
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

-ifdef(TEST).
%%{-----------------------------------------------------------------}
%% TEST HELPERS
%% test runner with preety printer for tests output
run_tests(TestList) ->
	io:format("[Running Tests:]~n"),
	lists:foreach(fun({Name, Fun}) ->
		io:format("~n TEST CASE: [~s] :",[Name]),
		Fun()
	end, TestList),
	io:format("~n").

%% equal assertion
assert_equal(Expected, Value, Line) ->
	case Expected of 
		Value -> io:format(".");
		_Oops -> 
			io:format("~n   FAILD:~n   EXPECTED ~w,~n   IS       ~w - [LINE: ~B]~n", 
				[Expected, Value, Line])
	end.



%%{-----------------------------------------------------------------}
%% All Data for Tests & Tests
%% TEST DATA
get_migration_fun() -> fun(_) -> io:format("in migration") end.
get_migration_desc() -> "That is migration".
get_example_task(Fun) -> task(migrate, get_migration_desc(), Fun).
get_another_task() -> task(rollback, "That is rollback", fun(_) -> io:format("in rollback") end).
get_example_task_list(Fun) -> [get_example_task(Fun), get_another_task()].
get_example_namespace(Fun) -> namespace(db, "desc", get_example_task_list(Fun)).
get_example_namespace_list(Fun) -> [get_example_namespace(Fun)].
get_example_nested_namespace(Fun) -> namespace(next, "desc", [get_example_namespace(Fun)]).
get_example_nested_namespace_list(Fun) -> [get_example_nested_namespace(Fun)].

%% All Tests

get_terms_test() ->
	?assertEqual({ok, [1,2]}, get_terms(get_tokens("[1,2]."))),
	?assertEqual({ok, 1}, get_terms(get_tokens("1."))),
	?assertEqual({ok, {name, "name"}}, get_terms(get_tokens("{name, \"name\"}."))),
	?assertEqual({error}, get_terms(get_tokens("[1,2]"))).
	
get_tokens_test() ->
	?assertEqual({ok,[{'[',1},{integer,1,1},{',',1},{integer,1,2},{']',1}]}, get_tokens("[1,2]")).

prepare_file_test() ->
	FileName = "test_example",
	BeamFileName = [FileName | ".beam"],
	prepare_file(FileName),
	?assertEqual({exist, BeamFileName}, exist_file(BeamFileName)),
	WrongFileName = "test_ex",
	BeamWrongFileName = [WrongFileName | ".beam"],
	prepare_file(WrongFileName),
	?assertEqual({not_exist, BeamWrongFileName}, exist_file(BeamWrongFileName)).

find_target_test() ->
	Fun = get_migration_fun(),
	Targets = [ "db", "migrate" ],
	?assertEqual({migrate, get_migration_desc(), Fun},find_target(Targets, get_example_namespace_list(Fun))),
	NestedTargets = [ "next", "db", "migrate" ],
	?assertEqual({migrate, get_migration_desc(), Fun},find_target(NestedTargets, get_example_nested_namespace_list(Fun))),
	WrongTargets = [ "db", "wrong"],
	?assertEqual({missing_target},find_target(WrongTargets, get_example_namespace_list(Fun))).

exist_file_test() ->
	FileName = "test_example.erl",
	?assertEqual({exist, FileName}, exist_file(FileName)),
	?assertEqual({not_exist, "unknown"}, exist_file("unknown")).

delete_file_test() ->
	FileName = "test_example",
	?assertEqual({ok, FileName}, build_file(FileName)),
	?assertEqual({ok, FileName}, delete_file(FileName)),
	?assertEqual({ok, FileName}, delete_file({ok, FileName})),
	?assertEqual({ok, "unknown"}, delete_file("unknown")),
	?assertEqual({ok, "unknown"}, delete_file({ok, "unknown"})),
	?assertEqual({error, FileName}, delete_file({error, FileName})).

build_file_test() ->
	FileName = "test_example",
	?assertEqual({ok, FileName}, build_file(FileName)),
	?assertEqual({ok, FileName}, build_file({ok, FileName})),
	?assertEqual({error, "unknown"}, build_file("unknown")),
	?assertEqual({error, "unknown"}, build_file({ok, "unknown"})),
	?assertEqual({error, FileName}, build_file({error, FileName})).

load_file_test() ->
	FileName = "test_example",
	?assertEqual({ok, FileName}, build_file(FileName)),
	?assertEqual({ok, FileName}, load_file(FileName)),
	?assertEqual({ok, FileName}, load_file({ok, FileName})),
	?assertEqual({error, "unknown"}, load_file("unknown")),
	?assertEqual({error, "unknown"}, load_file({ok, "unknown"})),
	?assertEqual({error, FileName}, load_file({error, FileName})).


%% pulbic function for runing all tests
test_all() -> 
	run_tests([
		{get_terms_test, fun get_terms_test/0},
		{get_tokens_test, fun get_tokens_test/0},
		{load_file_test, fun load_file_test/0},
		{build_file_test, fun build_file_test/0},
		{delete_file_test, fun delete_file_test/0},
		{exist_file_test, fun exist_file_test/0},
		{find_target_test, fun find_target_test/0},
		{prepare_file_test, fun prepare_file_test/0}
	]), init:stop().
-endif.
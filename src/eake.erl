-module(eake).
-define(VERSION, "1.0").
-vsn(?VERSION).

-export ([
	task/3,
	namespace/3,
	test_all/0
]).
	
-define(LOG(X), io:format("~nLOGGER : [~s]~n",[X])).

%%{-----------------------------------------------------------------}
%% BUILD_FILE
task(Name, Desc, Code) ->
	{eake_task, Name, Desc, Code}.

namespace(Name, Desc, TaskList) ->
	{eake_namespace, Name, Desc, TaskList}.


%%{-----------------------------------------------------------------}


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


prepare_file_test() ->
	FileName = "test_example",
	BeamFileName = [FileName | ".beam"],
	prepare_file(FileName),
	assert_equal({exist, BeamFileName}, exist_file(BeamFileName)),
	WrongFileName = "test_ex",
	BeamWrongFileName = [WrongFileName | ".beam"],
	prepare_file(WrongFileName),
	assert_equal({not_exist, BeamWrongFileName}, exist_file(BeamWrongFileName)).
	
find_target_test() ->
	Fun = get_migration_fun(),
	Targets = [ "db", "migrate" ],
	assert_equal({migrate, get_migration_desc(), Fun},find_target(Targets, get_example_namespace_list(Fun))),
	NestedTargets = [ "next", "db", "migrate" ],
	assert_equal({migrate, get_migration_desc(), Fun},find_target(NestedTargets, get_example_nested_namespace_list(Fun))),
	WrongTargets = [ "db", "wrong"],
	assert_equal({missing_target},find_target(WrongTargets, get_example_namespace_list(Fun))).
	
exist_file_test() ->
	FileName = "test_example.erl",
	assert_equal({exist, FileName}, exist_file(FileName)),
	assert_equal({not_exist, "unknown"}, exist_file("unknown")).

delete_file_test() ->
	FileName = "test_example",
	assert_equal({ok, FileName}, build_file(FileName)),
	assert_equal({ok, FileName}, delete_file(FileName)),
	assert_equal({ok, FileName}, delete_file({ok, FileName})),
	assert_equal({ok, "unknown"}, delete_file("unknown")),
	assert_equal({ok, "unknown"}, delete_file({ok, "unknown"})),
	assert_equal({error, FileName}, delete_file({error, FileName})).

build_file_test() ->
	FileName = "test_example",
	assert_equal({ok, FileName}, build_file(FileName)),
	assert_equal({ok, FileName}, build_file({ok, FileName})),
	assert_equal({error, "unknown"}, build_file("unknown")),
	assert_equal({error, "unknown"}, build_file({ok, "unknown"})),
	assert_equal({error, FileName}, build_file({error, FileName})).

load_file_test() ->
	FileName = "test_example",
	assert_equal({ok, FileName}, build_file(FileName)),
	assert_equal({ok, FileName}, load_file(FileName)),
	assert_equal({ok, FileName}, load_file({ok, FileName})),
	assert_equal({error, "unknown"}, load_file("unknown")),
	assert_equal({error, "unknown"}, load_file({ok, "unknown"})),
	assert_equal({error, FileName}, load_file({error, FileName})).



%% pulbic function for runing all tests
test_all() -> run_tests([
		{load_file_test, fun load_file_test/0},
		{build_file_test, fun build_file_test/0},
		{delete_file_test, fun delete_file_test/0},
		{exist_file_test, fun exist_file_test/0},
		{find_target_test, fun find_target_test/0},
		{prepare_file_test, fun prepare_file_test/0}
	]).

%% test runner with preety printer for tests output
run_tests(TestList) ->
	io:format("Running Tests:~n"),
	lists:foreach(fun({Name, Fun}) ->
		io:format("~n  - ~s :",[Name]),
		Fun()
	end, TestList),
	io:format("~n").

%% equal assertion
assert_equal(Expected,Value) ->
	case Expected of 
		Value -> io:format(".");
		_Oops -> io:format("FAILD~n      EXPECTED ~w,~n      IS       ~w~n", [Expected, Value])
	end.
	


	
	
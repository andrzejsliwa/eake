-module(eakefile).
-compile([export_all]).
-import(eake, [task/3, namespace/3, execute_target/2, run/1]).

execute() -> [

	namespace(db, "test", [
		task(migrate, "That is migration", fun(Params) ->
			io:format("in migration params: ~w", [Params]),
			execute_target('db:rollback', [])
		end),

		task(rollback, "That is rollback", fun(_) ->
			io:format("in rollback"),
			run("ls")
		end)
	])
].


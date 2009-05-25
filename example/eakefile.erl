-module(eakefile).
-compile([export_all]).
-import(eake, [task/3, run_task/2, run/1]).

execute() -> [
	task(migrate, "That is migration", fun(_) ->
		io:format("in migration"),
		run_task(rollback, [])
	end),

	task(rollback, "That is rollback", fun(_) ->
		io:format("in rollback"),
		run("ls")
	end)
].

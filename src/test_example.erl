-module (test_example).
-compile([export_all]).
-import(eake, [task/3, namespace/3]).

execute() -> [
	namespace(db, "test", [
		task(migrate, "That is migration", fun(Params) ->
			io:format("in migration params: ~w", [Params])
		end),

		task(rollback, "That is rollback", fun(_) ->
			io:format("in rollback")
		end)
	])
].
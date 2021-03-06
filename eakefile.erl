-module(eakefile).
-compile(export_all).
-import(eake, [task/3, namespace/2, invoke_target/2, run/1]).

execute() -> [
    namespace(db, [
      task(migrate, "That is migration", fun(Params) ->
        io:format("in migration params: ~w~n", [Params]),
        invoke_target("db:rollback", [])
      end),

      task(rollback, "That is rollback", fun(_) ->
        io:format("in rollback~n"),
        run("ls")
      end)
    ])
].
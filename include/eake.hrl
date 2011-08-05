-record(eake_task, {
  name                    :: atom(),
  desc = ""               :: string(),
  code = fun(_) -> ok end :: fun()
}).
-type eake_task() :: #eake_task{name :: atom(), desc :: string(), code :: fun()}.

-record(eake_namespace, {
  name       :: atom(),
  tasks = [] :: [eake_task()]
}).
-type eake_namespace() :: #eake_namespace{name :: atom(),tasks :: [eake_task()]}.

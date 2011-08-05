%%%'   HEADER
%%
%% @author    Andrzej Sliwa <andrzej.sliwa@i-tool.eu>
%% @copyright 2011 Andrzej Sliwa
%% @doc       EUnit test suite module eake.
%% @end
-module(eake_test).
-author('Andrzej Sliwa <andrzej.sliwa@i-tool.eu>').

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").

-define(MODNAME, eake).

%% make compiler shut up.
-spec test() -> ok.

%%%.
%%%' TESTS

formats_test_() ->
    Fun = fun(_) -> ok end,
    { "should be correct format of", [
        { "task",
            ?_assertEqual({eake_task, migrate, "database migration", Fun},
                eake:task(migrate, "database migration", Fun))
        },
        { "namespace",
            ?_assertEqual({eake_namespace, db, []},
                eake:namespace(db, []))
        }
    ]}.

file_exists_test_() ->
    { "file should", [
        { "be",
            ?_assert(eake:file_exists("eake_test.erl"))
        },
        { "not be",
            ?_assertNot(eake:file_exists("unexist.erl"))
        }
    ]}.
    %% add your asserts in the returned list, e.g.:
    %% [
    %%   ?_assert(?MODNAME:double(2) =:= 4),
    %%   ?_assertMatch({ok, Pid}, ?MODNAME:spawn_link()),
    %%   ?_assertEqual("ba", ?MODNAME:reverse("ab")),
    %%   ?_assertError(badarith, ?MODNAME:divide(X, 0)),
    %%   ?_assertExit(normal, ?MODNAME:exit(normal)),
    %%   ?_assertThrow({not_found, _}, ?MODNAME:func(unknown_object))
    %% ]
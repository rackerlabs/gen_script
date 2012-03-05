-module(gen_script_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SCRIPT, "../test/test_script.rb").

basic_smoke_test() ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    ?assertMatch(ok, gen_script:cast(Pid, ping, [])),
    ?assertMatch(<<"pong">>, gen_script:call(Pid, ping, [])),
    ?assertMatch({error, testing}, gen_script:call(Pid, error, [])),
    gen_script:stop(Pid).

timeout_test() ->
    error_logger:tty(false),
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    ?assertExit({timeout, _}, gen_script:call(Pid, timeout, [], 500)),
    ?assertNot(is_process_alive(Pid)).

exception_test() ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    ?assertMatch({error, {user, 0, <<"RuntimeError">>, <<"Testing">>, _}},
                 gen_script:call(Pid, exception, [])),
    ?assertMatch(<<"pong">>, gen_script:call(Pid, ping, [])),
    ?assert(is_process_alive(Pid)),
    gen_script:stop(Pid).

exit_test() ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    ?assertExit({{error, 255}, _}, gen_script:call(Pid, die, [])),
    ?assertNot(is_process_alive(Pid)).

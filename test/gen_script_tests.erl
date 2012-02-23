-module(gen_script_tests).
-include_lib("eunit/include/eunit.hrl").

basic_smoke_test() ->
    {ok, Pid} = gen_script:start(test_script, "../test/test_script.rb"),
    ?assertMatch(ok, gen_script:cast(Pid, ping, [])),
    ?assertMatch(<<"pong">>, gen_script:call(Pid, ping, [])),
    gen_script:stop(Pid).


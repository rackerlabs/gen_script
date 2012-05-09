-module(gen_script_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/test.hrl").

-define(SCRIPT, "../../test/test_script.rb").

-export([all/0]).
-export([basic_smoke_test/1, exit_test/1, timeout_test/1, exception_test/1]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

basic_smoke_test(_Config) ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    ok = gen_script:cast(Pid, ping, []),
    <<"pong">> = gen_script:call(Pid, ping, []),
    {error, testing} = gen_script:call(Pid, error, []),
    gen_script:stop(Pid).

timeout_test(_Config) ->
    error_logger:tty(false),
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    {'EXIT', {timeout, _}} = (catch gen_script:call(Pid, timeout, [], 500)),
    false = is_process_alive(Pid).

exception_test(__Config) ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    {error, {user, 0, <<"RuntimeError">>, <<"Testing">>, _}} =
    	gen_script:call(Pid, exception, []),
    <<"pong">> = gen_script:call(Pid, ping, []),
    true = is_process_alive(Pid),
    gen_script:stop(Pid).

exit_test(_Config) ->
    {ok, Pid} = gen_script:start(test_script, {ruby, ?SCRIPT}),
    {'EXIT', {{error, 255}, _}} = (catch gen_script:call(Pid, die, [])),
    false = is_process_alive(Pid).

-module(gen_script).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start/2, start_link/2, stop/1, call/3, call/4, cast/3]).

-record(state, { port :: port(),
                 mod_name :: atom(),
                 from :: {pid(), atom()} }).

-type error()    :: {error, term()}.
-type fun_name() :: atom().
-type fun_args() :: [list() | binary() | atom()].

-type filepath() :: string().
-type command()  :: string().
-type source_lang() :: ruby.
-type source_file() :: { source_lang(), filepath() } | filepath().

%%%===================================================================
%%% API
%%%===================================================================

-spec start(atom(), string()) -> {ok, pid()} | ignore | error().
start(ModName, ScriptFile) ->
    gen_server:start(?MODULE, [ModName, ScriptFile], []).

-spec start_link(atom(), string()) -> {ok, pid()} | ignore | error().
start_link(ModName, ScriptFile) ->
    gen_server:start_link(?MODULE, [ModName, ScriptFile], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec call(pid(), fun_name(), fun_args()) -> any().
call(Pid, Fun, Args) ->
    call(Pid, Fun, Args, infinity).

-spec call(pid(), fun_name(), fun_args(), timeout()) -> any().
call(Pid, Fun, Args, Timeout) ->
    gen_server:call(Pid, {Fun, Args, Timeout}, infinity).

-spec cast(pid(), fun_name(), fun_args()) -> ok.
cast(Pid, Fun, Args) ->
    gen_server:cast(Pid, {Fun, Args}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ModName, ScriptFile]) ->
    ScriptCmd = boot_script(ScriptFile),
    Port = open_port({spawn, ScriptCmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    {ok, #state{ port = Port, mod_name = ModName }}.

handle_call({Fun, Args, Timeout}, From, S) ->
    Message = bert:encode({call, S#state.mod_name, Fun, Args}),
    port_command(S#state.port, Message),
    {noreply, S#state{ from = From }, Timeout}.

handle_cast({Fun, Args}, S) ->
    Message = bert:encode({cast, S#state.mod_name, Fun, Args}),
    port_command(S#state.port, Message),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({Port, {data, Result}}, #state{port = Port} = S) ->
    Reply = case bert:decode(Result) of
                {reply, Info} -> Info;
                {error, _} = Error -> Error
            end,
    maybe_reply(S#state.from, Reply),
    {noreply, S#state{ from = undefined }};

handle_info({Port, {exit_status, Code}}, #state{port = Port} = S) ->
    % Hard and unanticipated crash
    error_logger:error_msg("Port ~p crashed ~p~n", [Port, Code]),
    {stop, {error, Code}, S};

handle_info(timeout, S) ->
    error_logger:error_msg("Port ~p timed out~n", [S#state.port]),
    {stop, timeout, S};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, S) ->
    try
        port_close(S#state.port)
    catch
        error:badarg ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec boot_script(source_file()) -> command().
boot_script({ruby, File}) ->
    RubyDir = filename:join([app_root(), "priv", "ruby"]),
    BertDir = filename:join([RubyDir, "bert", "lib"]),
    GSDir = filename:join(RubyDir, "gen_script"),
    lists:flatten(io_lib:format("ruby -I ~s -I ~s -r gen_script ~s", [BertDir, GSDir, File]));
boot_script(ScriptFile) ->
    ScriptFile.

-spec app_root() -> filepath().
app_root() ->
    Dir = filename:dirname(code:where_is_file("gen_script.app")),
    filename:join(Dir, "..").


maybe_reply(_, noreply) -> ok;
maybe_reply(undefined, _) -> ok;
maybe_reply(From, Reply) ->
    gen_server:reply(From, Reply).

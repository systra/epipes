-module(epipes_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, exec/2]).

-define(SERVER, ?MODULE).
-define(PORT_EOF_TIMEOUT, 30000).

-record(state, {forcer}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec exec(iolist(), iolist()) -> {ok, iolist()} | {error, term()}.
exec(Cmd, Data) ->
    gen_server:call(?SERVER, {exec, Cmd, Data}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_) ->
  process_flag(trap_exit, true),
  Forcer = get_base_dir(?MODULE) ++ "/priv/stdin_forcer",
  {ok, #state{forcer = Forcer}}.

handle_call({exec, Cmd, Data}, From, #state{forcer = Forcer} = State) ->
    % quickly spawn so we can be a non-blocking gen_server:
    spawn(fun() ->
                process_flag(trap_exit, true),
                P = open_port({spawn_executable, Forcer},
                              [stream, use_stdio, stderr_to_stdout, binary, eof,
                               {args, string:tokens(Cmd, " ")}]),
                port_connect(P, self()),   % attach port to this spawned process
                port_command(P, Data),  % send our stdin content to the wrapper
                port_command(P, <<0>>),    % tell the wrapper we're done
                gen_server:reply(From, gather_response(P)),
                port_close(P)
              end),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Other info: ~p with state ~p~n", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

gather_response(Port) ->
    gather_response(Port, []).
gather_response(Port, Accum) ->
    receive
        {Port, {data, Bin}} -> gather_response(Port, [Bin | Accum]);
        {Port, eof} -> {ok, lists:reverse(Accum)};
        {'EXIT', Port, Reason} -> {error, {port_terminated, Reason}}
    after % max 30 seconds of time for the process to send EOF (close stdout)
        ?PORT_EOF_TIMEOUT -> {error, {timeout, lists:reverse(Accum)}}
    end.

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).


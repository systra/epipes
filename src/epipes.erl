-module(epipes).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0, exec/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    epipes_sup:start_link().

stop(_State) ->
        ok.

%%%===================================================================
%%% API
%%%===================================================================

% For application start from console
start() ->
    application:start(epipes).

% Doubles epipes_server API function
-spec exec(iolist(), iolist()) -> {ok, iolist()} | {error, term()}.
exec(Cmd, Data) ->
    epipes_server:exec(Cmd, Data).

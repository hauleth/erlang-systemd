-module(mock_journald).

-behaviour(gen_server).

-export([socket_path/1]).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% This hack is needed to reduce length of the socket path which is limited to
%% 104-108 characters (depends on the OS).
-spec socket_path(PrivDir :: file:name()) -> string().
socket_path(PrivDir0) ->
    {ok, Cwd} = file:get_cwd(),
    PrivDir = string:prefix(PrivDir0, Cwd),
    unicode:characters_to_list([".", PrivDir, "journald.sock"]).

-record(state, {pid, socket, path}).

start_link(Path, Pid) ->
    gen_server:start_link(?MODULE, {Path, Pid}, []).

init({Path, Pid}) ->
    ct:log("Starting journald socket at ~p", [Path]),
    ct:log("Sending logs to ~p", [Pid]),
    Addr = {local, Path},
    {ok, Socket} = gen_udp:open(0, [local, binary, {ifaddr, Addr}]),
    {ok, #state{pid = Pid, socket = Socket, path = Path}}.

handle_call(_Msg, _Ref, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {udp, Socket, _, _, Data},
    #state{socket = Socket, pid = Pid} = State
) ->
    ct:log("Mock journald received: ~p~nSending to: ~p", [Data, Pid]),
    Pid ! {log, Data},
    {noreply, State}.

terminate(_, #state{socket = Socket, path = Path}) ->
    gen_udp:close(Socket),
    file:delete(Path),
    ok.

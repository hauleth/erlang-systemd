-module(mock_systemd).

-behaviour(gen_server).

-export([socket_path/1,
         messages/1]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% This hack is needed to reduce length of the socket path which is limited to
%% 104-108 characters (depends on the OS).
-spec socket_path(PrivDir :: file:name()) -> string().
socket_path(PrivDir0) ->
    {ok, Cwd} = file:get_cwd(),
    PrivDir = string:prefix(PrivDir0, Cwd),
    erlang:binary_to_list(iolist_to_binary([".", PrivDir, "systemd.sock"])).

-spec messages(Pid :: pid()) -> [binary()].
messages(Pid) ->
    lists:reverse(gen_server:call(Pid, messages)).

-record(state, {socket, path, messages=[]}).

start_link(Path) ->
    gen_server:start_link(?MODULE, Path, []).

init(Path) ->
    ct:log("Starting socket at ~p", [Path]),
    Addr = {local, Path},
    {ok, Socket} = gen_udp:open(0, [local, {ifaddr, Addr}]),
    {ok, #state{socket=Socket, path=Path}}.

handle_call(messages, _Ref, #state{messages=Messages}=State) ->
    {reply, Messages, State#state{messages=[]}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, _, _, Data}, #state{socket=Socket,messages=Msgs}=State) ->
    ct:log("Mock systemd received: ~p", [Data]),
    {noreply, State#state{messages=[Data | Msgs]}}.

terminate(_, #state{socket=Socket,path=Path}) ->
    gen_udp:close(Socket),
    file:delete(Path),
    ok.

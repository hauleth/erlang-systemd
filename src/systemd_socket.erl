%% @hidden

-module(systemd_socket).

-behaviour(gen_server).

-define(NAME, ?MODULE).

-include_lib("kernel/include/logger.hrl").

-export([send/1]).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% # Internal interface

send(Message) ->
    gen_server:call(?NAME, {send, string:trim(Message, trailing, "\n")}).

% # Behaviour implementation

start_link(Address) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Address, []).

init([]) -> {ok, []};
init(Address) ->
    % We never receive on this socket, so we set is as {active, false}
    {ok, Socket} = gen_udp:open(0, [binary, local, {active, false}]),
    {ok, {Socket, Address}}.

handle_call({send, Message}, _Ref, {Socket, Address}=State) ->
    ok = gen_udp:send(Socket, Address, 0, [Message, $\n]),
    {reply, ok, State};
handle_call(_Msg, _Ref, []) ->
    {reply, ok, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

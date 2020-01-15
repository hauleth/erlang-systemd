%% @hidden

-module(systemd_socket).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(NOTIFY_SOCKET, "NOTIFY_SOCKET").

-include_lib("kernel/include/logger.hrl").

-export([send/1]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% # Internal interface

send(Message) ->
    gen_server:call(?NAME, {send, Message}).

% # Behaviour implementation

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Arg) ->
    case has_env(?NOTIFY_SOCKET) of
        false ->
            {ok, []};
        Path ->
            Address = {local, normalize(Path)},
            {ok, Socket} = gen_udp:open(0, [local]),

            os:unsetenv(?NOTIFY_SOCKET),

            {ok, {Socket, Address}}
    end.

handle_call({send, Message}, _Ref, {Socket, Address}=State) ->
    case gen_udp:send(Socket, Address, 0, [Message, $\n]) of
        ok ->
            ok;
        {error, enoent} ->
            ?LOG_ERROR("NOTIFY_SOCKET not available")
    end,

    {reply, ok, State};
handle_call(_Msg, _Ref, []) ->
    {reply, {error, nosocket}, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

normalize([$@ | Rest]) -> [0 | Rest];
normalize(Value) -> Value.

has_env(Name) ->
    case os:getenv(Name) of
        false -> false;
        "" -> false;
        Value -> Value
    end.

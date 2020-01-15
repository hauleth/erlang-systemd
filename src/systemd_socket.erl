%% @hidden

-module(systemd_socket).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(NOTIFY_SOCKET, "NOTIFY_SOCKET").

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-export([send/1]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% # Internal interface

send(Message) ->
    gen_server:call(?NAME, {send, string:trim(Message, trailing, "\n")}).

% # Behaviour implementation

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Arg) ->
    State = case has_env(?NOTIFY_SOCKET) of
                false ->
                    [];
                [$@ | AbstractPath] ->
                    Address = {local, [0 | AbstractPath]},
                    {ok, Socket} = gen_udp:open(0, [local]),
                    {Socket, Address};
                Path ->
                    case file:read_file_info(Path) of
                        {error, _Error} ->
                            [];
                        {ok, #file_info{access=Access}}
                          when Access =:= write; Access =:= read_write ->
                            Address = {local, Path},
                            {ok, Socket} = gen_udp:open(0, [local]),
                            {Socket, Address}
                    end
            end,
    os:unsetenv(?NOTIFY_SOCKET),
    {ok, State}.

handle_call({send, Message}, _Ref, {Socket, Address}=State) ->
    ok = gen_udp:send(Socket, Address, 0, [Message, $\n]),
    {reply, ok, State};
handle_call(_Msg, _Ref, []) ->
    {reply, ok, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

has_env(Name) ->
    case os:getenv(Name) of
        false -> false;
        "" -> false;
        Value -> Value
    end.

%%%-------------------------------------------------------------------
%% @doc echo public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case logger:add_handlers(systemd) of
        ok ->
            logger:remove_handler(default);
        _ ->
            ok
    end,
    echo_sup:start_link(get_ports()).

stop(_State) ->
    ok.

%% internal functions

get_ports() ->
    Fds = systemd:listen_fds(),
    ?LOG_NOTICE("fds: ~p", [Fds]),
    Tcp = case lists:keyfind("tcp", 2, Fds) of
              {TcpFd, "tcp"} -> {fd, TcpFd};
              _ -> {port, 7777}
          end,
    Udp = case lists:keyfind("udp", 2, Fds) of
              {UdpFd, "udp"} -> {fd, UdpFd};
              _ -> {port, 7777}
          end,
    #{tcp => Tcp, udp => Udp}.

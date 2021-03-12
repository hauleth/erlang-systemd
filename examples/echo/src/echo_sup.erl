%%%-------------------------------------------------------------------
%% @doc echo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(echo_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    #{tcp := TcpPort, udp := UdpPort} = get_ports(),
    SupFlags = #{strategy => one_for_all},
    ChildSpecs = [
                  #{id => echo_tcp,
                    start => {echo_tcp, start_link, [TcpPort]}},
                  #{id => echo_udp,
                    start => {echo_udp, start_link, [UdpPort]}},
                  systemd:ready()
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

get_ports() ->
    Fds = systemd:listen_fds(),
    Tcp = case lists:keyfind("tcp", 2, Fds) of
              {TcpFd, "tcp"} -> {fd, TcpFd};
              _ -> {port, 7777}
          end,
    Udp = case lists:keyfind("udp", 2, Fds) of
              {UdpFd, "udp"} -> {fd, UdpFd};
              _ -> {port, 7777}
          end,
    #{tcp => Tcp, udp => Udp}.

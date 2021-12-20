%%%-------------------------------------------------------------------
%% @doc echo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(echo_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(#{tcp := TcpPort, udp := UdpPort}) ->
    SupFlags = #{strategy => one_for_all, intensity => 0},
    ChildSpecs = [
                  #{id => echo_tcp,
                    start => {echo_tcp, start_link, [TcpPort]}},
                  #{id => echo_udp,
                    start => {echo_udp, start_link, [UdpPort]}},
                  systemd:ready()
                 ],
    {ok, {SupFlags, ChildSpecs}}.

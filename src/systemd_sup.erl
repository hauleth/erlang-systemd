%% @hidden

-module(systemd_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

init(_Opts) ->
    SupFlags = #{
      strategy => one_for_one
     },
    SocketServer = #{id => socket,
                     start => {systemd_socket, start_link, []}
                    },
    Watchdog = #{id => watchdog,
                 start => {systemd_watchdog, start_link, []}
                },

    {ok, {SupFlags, [SocketServer, Watchdog]}}.

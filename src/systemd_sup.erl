-module(systemd_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

init(_Opts) ->
    SupFlags = #{
      strategy => one_for_one
     },
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.

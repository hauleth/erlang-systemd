%% @hidden

-module(systemd_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2,
         prep_stop/1,
         stop/1]).

start(_Type, _Opts) ->
    systemd_sup:start_link([]).

prep_stop(State) ->
    ok = systemd:notify(stopping),
    State.

stop(_State) ->
    ok.

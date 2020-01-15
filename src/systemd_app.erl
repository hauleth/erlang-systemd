%% @hidden

-module(systemd_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2,
         start_phase/3,
         prep_stop/1,
         stop/1]).

start(_Type, _Opts) ->
    systemd_sup:start_link([]).

start_phase(systemd, _Type, _Opts) ->
    ok = systemd:notify(ready).

prep_stop(State) ->
    ok = systemd:notify(stopping),
    State.

stop(_State) ->
    ok.

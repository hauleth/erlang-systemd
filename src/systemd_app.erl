-module(systemd_app).

-behaviour(application).

-export([start/2,
         start_phase/3,
         stop/1]).

start(_Type, _Opts) ->
    systemd_sup:start_link([]).

start_phase(systemd, _Type, _Opts) ->
    ok = systemd:notify(ready),

    ok.

prep_stop(_State) ->
    ok = systemd:notify(stopping).

stop(_State) ->
    ok.

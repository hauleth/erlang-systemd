-module(logger_send_back_h).

-export([log/2]).

log(Event, #{config := #{pid := Pid}}) ->
    Pid ! Event.

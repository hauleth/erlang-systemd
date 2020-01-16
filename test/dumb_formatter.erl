-module(dumb_formatter).

-export([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) ->
    unicode:chardata().
format(#{msg:={string, Msg}}, _Config) ->
    Msg.

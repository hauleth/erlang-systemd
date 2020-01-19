-module(dumb_formatter).

-export([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) ->
    unicode:chardata().
format(LogEvent, Config) ->
    logger_formatter:format(LogEvent, Config#{template => [msg]}).

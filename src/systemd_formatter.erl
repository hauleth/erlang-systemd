-module(systemd_formatter).

-include("systemd.hrl").

-export([check_config/1,
         format/2]).

-spec check_config(logger:formatter_config()) -> ok | {error, term()}.
check_config(Config0) ->
    case maps:take(parent, Config0) of
         {Formatter, Config} ->
            case erlang:function_exported(Formatter, check_config, 1) of
                true -> Formatter:check_config(Config);
                _ -> ok
            end;
        error ->
            logger_formatter:check_config(Config0)
    end.

-spec format(logger:log_event(), logger:formatter_config()) ->
    unicode:chardata().
format(#{level := Level}=LogEvent, Config0) ->
    {Formatter, Config} = case maps:take(parent, Config0) of
                              {FMod, Conf} -> {FMod, Conf};
                              error -> {logger_formatter, Config0}
                          end,
    Message = Formatter:format(LogEvent, Config),
    [format_level(Level), Message].

format_level(emergency) -> ?SD_EMERG;
format_level(alert)     -> ?SD_ALERT;
format_level(critical)  -> ?SD_CRIT;
format_level(error)     -> ?SD_ERR;
format_level(warning)   -> ?SD_WARNING;
format_level(notice)    -> ?SD_NOTICE;
format_level(info)      -> ?SD_INFO;
format_level(debug)     -> ?SD_DEBUG.

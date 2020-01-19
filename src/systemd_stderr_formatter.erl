%% @doc
%% Formatter wrapper for systemd's journal level information.
%%
%% Journald uses log-level format similar to Linux's `printk' logging function.
%% This simply adds special prefix to the message formatted by the formatter
%% specified via `parent' option.
%%
%% This can be used with `logger_std_h' handler with `type => standard_error'.
%%
%% == Example ==
%%
%% ```
%% logger:add_handler(stderr,
%%                    logger_std_h,
%%                    #{formatter => {systemd_stderr_formatter, #{}}).
%% '''
%%
%% == Options ==
%%
%% <dl>
%%      <dt>`parent'</dt>
%%      <dd>"Parent" formatter that will be used as a "main" formatter.
%%
%%      Defaults to `logger_formatter'.</dd>
%% </dl>
%%
%% Rest of the options will be passed to `parent' with this option removed.
%%
%% @end
-module(systemd_stderr_formatter).

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
    Priority = format_level(Level),
    Message0 = Formatter:format(LogEvent, Config),
    Message = string:replace(Message0, "\n", [$\n, Priority], all),
    [Priority | Message].

format_level(emergency) -> ?SD_EMERG;
format_level(alert)     -> ?SD_ALERT;
format_level(critical)  -> ?SD_CRIT;
format_level(error)     -> ?SD_ERR;
format_level(warning)   -> ?SD_WARNING;
format_level(notice)    -> ?SD_NOTICE;
format_level(info)      -> ?SD_INFO;
format_level(debug)     -> ?SD_DEBUG.

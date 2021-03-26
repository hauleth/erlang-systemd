%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc
%% Formatter wrapper for systemd's journal level information.
%%
%% Journald uses log-level format similar to Linux's `printk' logging function.
%% This simply adds special prefix to the message formatted by the formatter
%% specified via `parent' option.
%%
%% == Automatic registration ==
%%
%% This formatter will be automatically registered for all handlers that use
%% `logger_std_h' with `type' set to one of `standard_io' or `standard_error' if
%% the respective FDs point to journal that is pointed by `JOURNAL_STREAM' which
%% is autometically set by systemd if one of the options `StandardOutput' or
%% `StandardError' is set to `journal'. So if you are using default logger
%% handler then all you need to do is to set
%%
%% ```
%% StandardOutput=journal
%% '''
%%
%% In your `systemd.service(8)' file, and this library will handle the rest.
%%
%% It is important that this formatter will still work poorly with multiline
%% logs, if you are using such, then check out {@link systemd_journal_h.
%% `systemd_journal_h'} handler (which <b>SHOULD NOT</b> be used with this
%% formatter).
%%
%% To disable this behaviour set `auto_formatter' option for `systemd' to
%% `false'.
%%
%% == Example ==
%%
%% ```
%% logger:add_handler(journal_stdout,
%%                    logger_std_h,
%%                    #{formatter => {systemd_kmsg_formatter, #{}}).
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
%% @since 0.3.0
%% @end
-module(systemd_kmsg_formatter).

-include("systemd.hrl").

-export([check_config/1,
         format/2]).

-export([auto_install/0]).

%% @hidden
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

%% @hidden
-spec format(logger:log_event(), logger:formatter_config()) ->
    unicode:chardata().
format(#{level := Level}=LogEvent, Config0) ->
    {Formatter, Config} = case maps:take(parent, Config0) of
                              {FMod, Conf} -> {FMod, Conf};
                              error -> {logger_formatter, Config0}
                          end,
    Priority = format_level(Level),
    Message = Formatter:format(LogEvent, Config),
    prefix_lines(Message, Priority).

format_level(emergency) -> ?SD_EMERG;
format_level(alert)     -> ?SD_ALERT;
format_level(critical)  -> ?SD_CRIT;
format_level(error)     -> ?SD_ERR;
format_level(warning)   -> ?SD_WARNING;
format_level(notice)    -> ?SD_NOTICE;
format_level(info)      -> ?SD_INFO;
format_level(debug)     -> ?SD_DEBUG.

prefix_lines(String, Prefix) ->
    Splitted = string:split(String, "\n", all),
    lists:map(fun(Elem) -> [Prefix, Elem, $\n] end, Splitted).

%% @hidden Automatically install kmsg formatter for all handlers that use
%% logger_std_h and points to journal stream
auto_install() ->
    [auto_install(Config)
     || Config <- logger:get_handler_config()],
    ok.

auto_install(#{id := Id,
               module := logger_std_h,
               config := #{type := Type},
               formatter := {FMod, FConf}}) when
      FMod =/= ?MODULE ->
    case systemd:is_journal(Type) of
        true ->
            logger:update_handler_config(
              Id,
              #{formatter => {?MODULE, FConf#{parent => FMod}}});
        _ ->
            ok
    end;
auto_install(_Config) ->
    ok.

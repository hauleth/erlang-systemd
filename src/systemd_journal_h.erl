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
%% Logger handler for sending messages to systemd's `journal'.
%%
%% == Usage ==
%%
%% Run this after the `systemd' application is started:
%%
%% ```
%% logger:add_handler(journal, systemd_journal_h, #{}).
%% '''
%%
%% == Options ==
%%
%% <dl>
%%       <dt>`fields :: [field_definition()]'</dt>
%%       <dd>Contains list of all fields that will be passed to the `journald'.
%%
%%       Defaults to:
%%
%%       ```
%%       [syslog_timestamp,
%%        syslog_pid,
%%        priority,
%%        {"ERL_PID", pid},
%%        {"CODE_FILE", file},
%%        {"CODE_LINE", line},
%%        {"CODE_MFA", mfa}]
%%       '''
%%
%%       See {@section Fields} below.</dd>
%%
%%       <dt>`report_cb :: fun ((logger:report()) -> [field()]'</dt>
%%       <dd>Function that takes `Prefix' and Logger's report and returns list
%%       of 2-ary tuples  where first one MUST contain only uppercase ASCII
%%       letters, digits and underscore characters, and must not start with
%%       underscore. Field name and second one is field value in form of
%%       `iolist()'.  It is important to note that value can contain any data,
%%       and does not need to be in any encoding; it can even be binary.
%%
%%       === Example ===
%%
%%       ```
%%       my_formatter(Prefix, #{field := Field}) when is_integer(Field) ->
%%           [
%%            {[Prefix,"_FIELD"], io_lib:format("~.16B", [Field]}
%%           ].
%%       '''
%%
%%       Remember that all field names <b>MUST NOT</b> start with the underscore,
%%       otherwise `journald' can ignore them. Such behaviour is not enforced on
%%       data returned by `report_cb' and it is left up to the implementor to
%%       remember it.</dd>
%% </dl>
%%
%% == Fields ==
%%
%% Fields list contain definition of fields that will be presented in the log
%% message fed into `journald'. Few of them have special meaning and you can
%% see list of them in the <a href="https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html">
%% `systemd.journal-fields(7)' manpage</a>.
%%
%% Metakeys (i.e. atoms) in `fields' list will be sent to
%% the `journald' as a uppercased atom names.
%%
%% Entries in form of `{Name :: field_name(), metakey()}' will use `Name'
%% as the field name. `Name' will be checked if it is correct `journald' field
%% name (i.e. contains only ASCII letters, digits, and underscores,
%% additionally do not start with underscore).
%%
%% Entries in form of `{Name :: field_name(), Data :: iolist()}' will use
%% `Name' as field name and will contain `Data' as a literal.
%%
%% If entry data is empty or not set then it will be ommited in the output.
%%
%% === Special fields ===
%%
%% Special fields availables:
%%
%% <dl>
%%      <dt>`level'</dt>
%%      <dd>Log level presented as string.</dd>
%%      <dt>`priority'</dt>
%%      <dd>Log level presented as decimal representation of syslog level.</dd>
%%      <dt>`os_pid'</dt>
%%      <dd>OS PID for current Erlang process. This is <b>NOT Erlang PID</b>.
%%      </dd>
%%      <dt>`mfa'</dt>
%%      <dd>Calling function presented in form `Module:Function/Arity'.</dd>
%%      <dt>`time'</dt>
%%      <dd>Timestamp of log message presented in RFC3339 format in UTC.</dd>
%% </dl>
%%
%% Otherwise field is treated as a entry key where `key' is equivalent of
%% `[key]' and is used as a list of atoms to extract data from the metadata map.
%%
%% === Syslog compatibility ===
%%
%% To provide better compatibility and user convinience:
%%
%% <dl>
%%      <dt>`syslog_pid'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_PID", os_pid}'.</dd>
%%      <dt>`syslog_timestamp'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_TIMESTAMP", time}'.</dd>
%% </dl>
%%
%% @since 0.3.0
%% @end
-module(systemd_journal_h).

-behaviour(enough).

-include("systemd_internal.hrl").

-define(JOURNAL_SOCKET, <<"/run/systemd/journal/socket">>).

% logger handler callbacks
-export([adding_handler/1,
         changing_config/3,
         filter_config/1,
         removing_handler/1,
         log/2]).

% gen_server callbacks
-export([start_link/2,
         init/1,
         handle_load/2,
         handle_call/3,
         handle_cast/2]).

-define(FORMATTER, {logger_formatter, #{}}).
-define(CHILD_SPEC(Id, Args), #{id => Id,
                                start => {?MODULE, start_link, Args},
                                restart => temporary}).

-define(DEFAULT_FIELDS, [{"SYSLOG_TIMESTAMP", time},
                         {"SYSLOG_PID", os_pid},
                         {"PRIORITY", priority},
                         {"ERL_PID", pid},
                         {"CODE_FILE", file},
                         {"CODE_LINE", line},
                         {"CODE_MFA", mfa}]).

% -----------------------------------------------------------------------------
% Logger Handler

%% @hidden
-spec adding_handler(logger:handler_config()) -> {ok, logger:handler_config()} |
                                                 {error, term()}.
adding_handler(HConfig) ->
    Config0 = maps:get(config, HConfig, #{}),
    case get_path(Config0) of
        false ->
            {error, no_journal_socket};
        {Path, Config} ->
            do_add_handler(Path, Config, HConfig)
    end.

do_add_handler(Path, Config, #{id := Id} = HConfig) ->
    case validate_config(Config) of
        ok ->
            Fields = [translate_field(Field)
                      || Field <- maps:get(fields, Config, ?DEFAULT_FIELDS)],
            case start_connection(Id, Config) of
                {ok, Pid, OlpRef} ->
                    {ok, HConfig#{config => Config#{pid => Pid,
                                                    fields => Fields,
                                                    olp_ref => OlpRef,
                                                    path => Path}}};
                Err ->
                    Err
            end;
        Error -> Error
    end.

-ifdef(TEST).
get_path(Config) ->
    case maps:is_key(path, Config) of
        true -> maps:take(path, Config);
        false -> {{local, ?JOURNAL_SOCKET}, Config}
    end.
-else.
get_path(Config) ->
    case file:read_file_info(?JOURNAL_SOCKET) of
        {error, enoent} -> false;
        {ok, _} -> {{local, ?JOURNAL_SOCKET}, Config}
    end.
-endif.

%% @hidden
changing_config(update, #{config := OldHConfig}, NewConfig) ->
    NewHConfig = maps:get(config, NewConfig, #{}),
    case validate_config(NewHConfig) of
        ok ->
            Fields = case maps:is_key(fields, NewHConfig) of
                         true ->
                             NewFields = maps:get(fields, NewHConfig),
                             [translate_field(Field) || Field <- NewFields];
                         false ->
                             maps:get(fields, OldHConfig)
                     end,
            {ok, NewConfig#{config => OldHConfig#{fields := Fields}}};
        Error -> Error
    end;
changing_config(set, #{config := OldHConfig}, NewConfig) ->
    NewHConfig = maps:get(config, NewConfig, #{}),
    case validate_config(NewHConfig) of
        ok ->
            Fields = maps:get(fields, NewHConfig, ?DEFAULT_FIELDS),
            Formatted = [translate_field(Field) || Field <- Fields],
            {ok, NewConfig#{config => OldHConfig#{fields := Formatted}}};
        Error -> Error
    end.

translate_field(syslog_timestamp) -> {"SYSLOG_TIMESTAMP", time};
translate_field(syslog_pid) -> {"SYSLOG_PID", os_pid};
translate_field(Atom) when is_atom(Atom) -> {Atom, Atom};
translate_field({_Name, _Data} = Field) -> Field.

validate_config(Config0) when is_map(Config0) ->
    Config = maps:without([pid, olp_ref, path], Config0),
    do_validate(maps:to_list(Config)).

do_validate([{fields, Fields} | Rest]) ->
    case check_fields(Fields) of
        ok -> do_validate(Rest);
        Error -> Error
    end;
do_validate([{sync_mode_qlen, _} | Rest]) -> do_validate(Rest);
do_validate([{drop_mode_qlen, _} | Rest]) -> do_validate(Rest);
do_validate([{flush_qlen, _} | Rest]) -> do_validate(Rest);
do_validate([{burst_limit_enable, _} | Rest]) -> do_validate(Rest);
do_validate([{burst_limit_max_count, _} | Rest]) -> do_validate(Rest);
do_validate([{burst_limit_window_time, _} | Rest]) -> do_validate(Rest);
do_validate([{overload_kill_enable, _} | Rest]) -> do_validate(Rest);
do_validate([{overload_kill_qlen, _} | Rest]) -> do_validate(Rest);
do_validate([{overload_kill_mem_size, _} | Rest]) -> do_validate(Rest);
do_validate([{overload_kill_restart_after, _} | Rest]) -> do_validate(Rest);
do_validate([]) -> ok;
do_validate([Option | _]) ->
    {error, {invalid_option, Option}}.

-define(IS_STRING(Name), (is_binary(Name) orelse is_list(Name))).

check_fields([Atom | Rest])
  when is_atom(Atom) ->
    Name = atom_to_list(Atom),
    case check_name(Name) of
        true -> check_fields(Rest);
        false -> {error, {name_invalid, Name}}
    end;
check_fields([{Atom, _} | Rest])
  when is_atom(Atom) ->
    check_fields([Atom | Rest]);
check_fields([{Name, _} | Rest])
  when ?IS_STRING(Name) ->
    case check_name(unicode:characters_to_list(Name)) of
        true -> check_fields(Rest);
        false -> {error, {name_invalid, Name}}
    end;
check_fields([]) ->
    ok;
check_fields([Unknown | _]) ->
    {error, {invalid_field, Unknown}}.

check_name([C|Rest])
  when $A =< C, C =< $Z;
       $a =< C, C =< $z;
       $0 =< C, C =< $9 ->
    check_name_rest(Rest);
check_name(_) ->
    false.

check_name_rest([C|Rest])
  when $A =< C, C =< $Z;
       $a =< C, C =< $z;
       $0 =< C, C =< $9;
       C == $_ ->
    check_name_rest(Rest);
check_name_rest([]) ->
    true;
check_name_rest(_) ->
    false.

%% @hidden
-spec filter_config(logger:handler_config()) -> logger:handler_config().
filter_config(#{config := Config0} = HConfig) ->
    Config = maps:without([pid, olp_ref, path], Config0),
    HConfig#{config => Config}.

start_connection(Id, Config) ->
    case supervisor:start_child(?SUPERVISOR, ?CHILD_SPEC(Id, [Id, Config])) of
        {ok, Pid, OlpRef} -> {ok, Pid, OlpRef};
        {error, Error} -> {error, {spawn_error, Error}}
    end.

%% @hidden
-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(#{config := #{pid := Pid}}) ->
    ok = enough:stop(Pid),
    ok.

%% @hidden
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(LogEvent, #{config := Config} = HConfig) ->
    #{olp_ref := OlpRef, path := Path, fields := Fields} = Config,
    {FMod, FConf} = maps:get(formatter, HConfig, ?FORMATTER),
    Msg0 = FMod:format(LogEvent, FConf),
    case string:is_empty(Msg0) of
        false ->
            FieldsData = [{Name, get_field(Field, LogEvent)}
                          || {Name, Field} <- Fields],
            Msg = unicode:characters_to_binary(Msg0),
            Data = systemd_protocol:encode([{"MESSAGE", Msg} | FieldsData]),
            ok = enough:load(OlpRef, {send, Data, Path});
        true -> ok
    end.

get_field(os_pid, _LogEvent) ->
    os:getpid();
get_field(time, #{meta := #{time := Time}}) ->
    calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                           {offset, "Z"}]);
get_field(mfa, #{meta := #{mfa := {M, F, A}}}) ->
    io_lib:format("~tp:~tp/~B", [M, F, A]);
get_field(priority, #{level := Level}) ->
    level_to_char(Level);
get_field(level, #{level := Level}) ->
    atom_to_binary(Level, utf8);
get_field(Metakey, #{meta := Meta})
  when is_atom(Metakey) orelse
       (is_list(Metakey) andalso is_atom(hd(Metakey))) ->
    case get_meta(Metakey, Meta) of
        undefined -> "";
        Data -> to_string(Data)
    end;
get_field(Iolist, _) when is_list(Iolist) orelse is_binary(Iolist) ->
    Iolist.

get_meta([], Data) ->
    Data;
get_meta([Atom | Rest], Meta) when is_map(Meta) ->
    case maps:get(Atom, Meta, undefined) of
        undefined -> undefined;
        Next -> get_meta(Rest, Next)
    end;
get_meta(Atom, Meta) when is_atom(Atom) ->
    maps:get(Atom, Meta, undefined);
get_meta(_, _) ->
    undefined.

to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_string(Ref) when is_reference(Ref) ->
    ref_to_list(Ref);
to_string(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_string(List) when is_list(List) ->
    case printable_list(List) of
        true -> List;
        false -> io_lib:format("~tp", [List])
    end;
to_string(Bin) when is_binary(Bin) ->
    case printable_list(binary_to_list(Bin)) of
        true -> Bin;
        false -> io_lib:format("~tp", [Bin])
    end;
to_string(X) ->
    io_lib:format("~tp", [X]).

printable_list([]) ->
    false;
printable_list(X) ->
    io_lib:printable_list(X).

level_to_char(debug)     -> "7";
level_to_char(info)      -> "6";
level_to_char(notice)    -> "5";
level_to_char(warning)   -> "4";
level_to_char(error)     -> "3";
level_to_char(critical)  -> "2";
level_to_char(alert)     -> "1";
level_to_char(emergency) -> "0".

% -----------------------------------------------------------------------------
% Socket handler

%% @hidden
start_link(Id, Opts0) ->
    Opts = maps:with([
    sync_mode_qlen,
    drop_mode_qlen,
    flush_qlen,
    burst_limit_enable,
    burst_limit_max_count,
    burst_limit_window_time,
    overload_kill_enable,
    overload_kill_qlen,
    overload_kill_mem_size,
    overload_kill_restart_after], Opts0),
    enough:start_link(Id, ?MODULE, [], Opts).

%% @hidden
init(_Arg) ->
    % We never receive on this socket, so we set {active, false}
    {ok, Socket} = gen_udp:open(0, [binary, local, {active, false}]),
    {ok, Socket}.

%% @hidden
handle_load({send, Message, Path}, Socket) ->
    gen_udp:send(Socket, Path, Message),
    Socket.

%% @hidden
handle_call(stop, _Ref, Socket) ->
    {stop, normal, ok, Socket}.

%% @hidden
handle_cast(_Msg, Socket) ->
    {noreply, Socket}.

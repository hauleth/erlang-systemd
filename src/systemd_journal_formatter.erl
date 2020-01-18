%% @doc
%% Formatter for `systemd_journal_h'.
%%
%% This formatters formats data in the way that is digestible by the `journald'.
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
%%       [message,
%%        syslog_timestamp,
%%        syslog_pid,
%%        syslog_priority,
%%        {"ERL_PID", pid},
%%        {"CODE_FILE", file},
%%        {"CODE_LINE", line},
%%        {"CODE_MFA", mfa}]
%%       '''
%%
%%       See {@section Fields} below.</dd>
%%
%%       <dt>`report_cb :: fun ((Prefix :: field_name(), logger:report()) -> [field()]'</dt>
%%       <dd>Function that takes `Prefix' and Logger's report and returns list of
%%       values returned by {@link field/2. `systemd_journal_formatter:field/2'}.
%%
%%       === Example ===
%%
%%       ```
%%       my_formatter(Prefix, #{field := Field}) when is_integer(Field) ->
%%           [
%%            systemd_journal_formatter:field([Prefix,"_FIELD"], io_lib:format("~.16B", [Field])
%%           ].
%%       '''
%%
%%       Rememer that all field names <b>MUST</b> be uppercase and <b>MUST NOT</b>
%%       start with the underscore, otherwise `journald' can ignore them. Such
%%       behaviour is not enforced on data returned by `report_cb' and it is left
%%       up to the user to the implementor to remember about it.</dd>
%%
%%       <dt>`time_designator :: byte()'</dt>
%%       <dd>Timestamps are formatted according to RFC3339, and the time
%%       designator is the character used as date and time separator.
%%
%%       Defaults to `$T'.
%%
%%       The value of this parameter is used as the time_designator option to
%%       {@link calendar:system_time_to_rfc3339/2. `calendar:system_time_to_rfc3339/2'}.</dd>
%%
%%       <dt>`time_offset :: integer() | [byte()]'</dt>
%%       <dd>The time offset, either a string or an integer, to be used when
%%       formatting the timestamp.
%%
%%       An empty string is interpreted as local time. The values "Z", "z" or 0
%%       are interpreted as Universal Coordinated Time (UTC).
%%
%%       Strings, other than `"Z"', `"z"', or `""', must be on the form `±[hh]:[mm]',
%%       for example `"-02:00"' or `"+00:00"'.
%%
%%       Integers must be in microseconds, meaning that the offset `7200000000' is
%%       equivalent to `"+02:00"'.
%%
%%       Defaults to an `"Z"', meaning that timestamps are displayed in
%%       UTC.
%%
%%       The value of this parameter is used as the offset option to
%%       {@link calendar:system_time_to_rfc3339/2. `calendar:system_time_to_rfc3339/2'}.</dd>
%% </dl>
%%
%% == Fields ==
%%
%% Fields list contain definition of fields that will be presented in the log
%% message feeded into `journald'. Few of them have special meaning and you can
%% see list of them in the <a href="https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html">
%% `systemd.journal-fields(7)' manpage</a>.
%%
%% Metakeys (i.e. atoms and lists of atoms) in `fields' list will be sent to
%% the `journald' as a uppercased atom names, and in case of lists, joined
%% with underscores.
%%
%% Entries in form of `{Name :: field_name(), metakey()}' will use `Name'
%% as the field name. `Name' will be checked if it is correct `journald' field
%% name (i.e. contains only uppercase ASCII letters, digits, and underscores,
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
%%      <dd>Log level presented as decimal representation of
%%      syslog level.</dd>
%%      <dt>`msg' and `message'</dt>
%%      <dd>Log message.</dd>
%%      <dt>`os_pid'</dt>
%%      <dd>OS PID for current Erlang process. This is <b>NOT Erlang
%%      PID</b>.</dd>
%%      <dt>`mfa'</dt>
%%      <dd>Calling function presented in form `Module:Function/Arity'.</dd>
%%      <dt>`time'</dt>
%%      <dd>Timestamp of log message presented in RFC3339 format.</dd>
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
%%      <dt>`syslog_priority'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_PRIORITY", priority}'.</dd>
%%      <dt>`syslog_pid'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_PID", os_pid}'.</dd>
%%      <dt>`syslog_timestamp'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_TIMESTAMP", time}'.</dd>
%%      <dt>`syslog_facility'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_FACILITY", facility}'.</dd>
%%      <dt>`syslog_identifier'</dt>
%%      <dd>Will work exactly the same as `{"SYSLOG_TIMESTAMP", identifier}'.</dd>
%% </dl>
%% @end
-module(systemd_journal_formatter).

-export([format_report/2]).

-export([check_config/1,
         format/2]).

-define(DEFAULT_FIELDS, [message,
                         syslog_timestamp,
                         syslog_pid,
                         syslog_priority,
                         {"ERL_PID", pid},
                         {"CODE_FILE", file},
                         {"CODE_LINE", line},
                         {"CODE_MFA", mfa}]).

-type metakey() :: atom() | [atom()].
-type field_definition() :: metakey() | {field_name(), metakey() | iolist()}.
-type field_name() :: unicode:chardata().
-type field() :: {field_name(), iolist()}.

-export_type([metakey/0,
              field_definition/0,
              field_name/0,
              field/0]).

%% @doc
%% Default report formatter used for `systemd_journal_formatter'. Output format
%% will be similar to one provided by `logger:format_report/1', but this one
%% takes also additional parameter `Perfix' which should be used as prefix for
%% all generated fields.
%% @end
-spec format_report(Prefix :: field_name(), logger:report()) -> [field()].
format_report(Prefix, Report) ->
    {Format, Args} = format_fields(Report),
    [{Prefix, io_lib:format(Format, Args)}].

format_fields(Report) when is_map(Report) ->
    format_fields(maps:to_list(Report));
format_fields(Report)  when is_list(Report) ->
    case lists:flatten(Report) of
        [] ->
            {"~tp",[[]]};
        FlatList ->
            case string_p1(FlatList) of
                true ->
                    {"~ts",[FlatList]};
                false ->
                    format_term_list(Report,[],[])
            end
    end;
format_fields(Report) ->
    {"~tp",[Report]}.

format_term_list([{Tag,Data}|T],Format,Args) ->
    PorS = case string_p(Data) of
               true -> "s";
               false -> "p"
           end,
    format_term_list(T,["~tp: ~t"++PorS|Format],[Data,Tag|Args]);
format_term_list([Data|T],Format,Args) ->
    format_term_list(T,["~tp"|Format],[Data|Args]);
format_term_list([],Format,Args) ->
    {lists:flatten(lists:join($\n,lists:reverse(Format))),lists:reverse(Args)}.

string_p(List) when is_list(List) ->
    string_p1(lists:flatten(List));
string_p(_) ->
    false.

string_p1([]) ->
    false;
string_p1(FlatList) ->
    io_lib:printable_unicode_list(FlatList).

% -----------------------------------------------------------------------------

%% @hidden
-spec check_config(logger:formatter_config()) -> ok | {error, term()}.
check_config(Config) when is_map(Config) ->
    do_check(maps:to_list(Config)).

do_check([{fields, Fields} | Rest])
  when Fields =/= [] ->
    case check_fields(Fields) of
        ok -> do_check(Rest);
        Error -> Error
    end;
do_check([{report_cb, Cb} | Rest])
  when is_function(Cb, 2) ->
    do_check(Rest);
do_check([{time_designator, Byte} | Rest])
  when is_integer(Byte), Byte > 0, Byte < 256 ->
    do_check(Rest);
do_check([{time_offset, Offset} | Rest])
    when is_integer(Offset);
         is_list(Offset) ->
    do_check(Rest);
do_check([]) ->
    ok;
do_check([Option | _]) ->
    {error, {invalid_option, Option}}.

-define(IS_STRING(Name), (is_binary(Name) orelse is_list(Name))).

check_fields([Atom | Rest]) when is_atom(Atom) ->
    Name = string:uppercase(atom_to_list(Atom)),
    case check_name(string:uppercase(Name)) of
        true -> check_fields(Rest);
        false -> {error, {name_invalid, Name}}
    end;
check_fields([List | Rest]) when is_list(List) ->
    case check_atom_list(List) of
        ok ->
            Name0 = [atom_to_list(Atom) || Atom <- List],
            Name1 = lists:join($_, Name0),
            Name = string:uppercase(Name1),
            check_fields([{Name, List} | Rest]);
        _Error -> {error, {invalid_field, List}}
    end;
check_fields([{Name, Atom} | Rest])
  when is_atom(Atom), ?IS_STRING(Name) ->
    check_fields([{Name, [Atom]} | Rest]);
check_fields([{Name, Value} | Rest])
  when ?IS_STRING(Name), ?IS_STRING(Value) ->
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
       $0 =< C, C =< $9 ->
    check_name_rest(Rest);
check_name(_) ->
    false.

check_name_rest([C|Rest])
  when $A =< C, C =< $Z;
       $0 =< C, C =< $9;
       C == $_ ->
    check_name_rest(Rest);
check_name_rest([]) ->
    true;
check_name_rest(_) ->
    false.

check_atom_list([Atom | Rest]) when is_atom(Atom) ->
    check_atom_list(Rest);
check_atom_list([]) ->
    ok;
check_atom_list(_) ->
    error.

%% @hidden
-spec format(logger:log_event(), logger:formatter_config()) ->
    unicode:chardata().
format(LogEvent, Config) ->
    Fields0 = maps:get(fields, Config, ?DEFAULT_FIELDS),
    Fields = [build_field(Field, LogEvent, Config) || Field <- Fields0],
    build_message(Fields).


build_message([]) -> [];
build_message([List | Rest]) when is_list(List) ->
    [build_message(List) | build_message(Rest)];
build_message([{Name, Data} | Rest]) ->
    case string:is_empty(Data) of
        true -> build_message(Rest);
        false ->
            Content = case string:find(Data, "\n") of
                          nomatch -> [$=, Data];
                          _ ->
                              Len = iolist_size(Data),
                              [$\n, <<Len:64/integer-little>>, Data]
                      end,
            [Name, Content, $\n | build_message(Rest)]
    end.

build_field({Prefix, msg}, #{msg := Msg, meta := Meta}, Config) ->
    normalize_msg(Prefix, Msg, Meta, Config);
build_field({Prefix, message}, LogEvent, Config) ->
    build_field({Prefix, msg}, LogEvent, Config);
build_field({Prefix, syslog_timestamp}, LogEvent, Config) ->
    build_field({Prefix, time}, LogEvent, Config);
build_field({Prefix, syslog_pid}, LogEvent, Config) ->
    build_field({Prefix, os_pid}, LogEvent, Config);
build_field({Prefix, syslog_priority}, LogEvent, Config) ->
    build_field({Prefix, priority}, LogEvent, Config);
build_field({Prefix, syslog_facility}, LogEvent, Config) ->
    build_field({Prefix, facility}, LogEvent, Config);
build_field({Prefix, syslog_identifier}, LogEvent, Config) ->
    build_field({Prefix, identifier}, LogEvent, Config);
build_field({Name, Binary}, _LogEvent, _Config) when is_binary(Binary) ->
    {Name, Binary};
build_field({Name, Data}, LogEvent, Config) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            {Name, Data};
        false ->
            case get_field(Data, LogEvent, Config) of
                undefined -> [];
                Value ->
                    {Name, Value}
            end
    end;
build_field(Atom, LogEvent, Config) when is_atom(Atom) ->
    Name0 = atom_to_list(Atom),
    Name = string:uppercase(Name0),
    build_field({Name, Atom}, LogEvent, Config);
build_field(Metakey, LogEvent, Config) when is_list(Metakey) ->
    Name0 = [atom_to_list(Atom) || Atom <- Metakey],
    Name1 = lists:join($_, Name0),
    Name = string:uppercase(Name1),
    build_field({Name, Metakey}, LogEvent, Config).

get_field(os_pid, _LogEvent, _Config) ->
    os:getpid();
get_field(time, #{meta := #{time := Time}}, Config) ->
    TimeDesignator = maps:get(time_designator, Config, $T),
    TimeOffset = maps:get(time_offset, Config, "Z"),
    calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                           {time_designator, TimeDesignator},
                                           {offset, TimeOffset}]);
get_field(mfa, #{meta := #{mfa := {M, F, A}}}, _Config) ->
    io_lib:format("~tp:~tp/~B", [M, F, A]);
get_field(priority, #{level := Level}, _Config) ->
    level_to_char(Level);
get_field(level, #{level := Level}, _Config) ->
    to_string(Level);
get_field(Metakey, #{meta := Meta}, _Config) ->
    case get_meta(Metakey, Meta) of
        undefined -> undefined;
        Data -> to_string(Data)
    end.

normalize_msg(Prefix, {string, String}, _Meta, _Config) ->
    {Prefix, String};
normalize_msg(Prefix, {report, Report}, #{report_cb := Cb}, _Config)
  when is_function(Cb, 1) ->
    {Format0, Args} = Cb(Report),
    Format = string:trim(Format0, leading, " "),
    {Prefix, io_lib:format(Format, Args)};
normalize_msg(Prefix, {report, Report}, #{report_cb := Cb}, _Config)
  when is_function(Cb, 2) ->
    Config = #{depth => unlimited,
               chars_limit => unlimited,
               single_line => false},
    {Prefix, Cb(Report, Config)};
normalize_msg(Prefix, {report, Report}, _Meta, Config) ->
    Cb = maps:get(report_cb, Config, fun ?MODULE:format_report/2),
    Cb(Prefix, Report);
normalize_msg(Prefix, {Format, Data}, _Meta, _Config) ->
    {Prefix, io_lib:format(Format, Data)}.

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

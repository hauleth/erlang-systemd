-module(systemd_journal_formatter_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [configuration, message, fields].

configuration(_Config) ->
    ok = systemd_journal_formatter:check_config(#{}),

    ok = systemd_journal_formatter:check_config(#{fields => [msg]}),
    ok = systemd_journal_formatter:check_config(#{fields => [message]}),
    ok = systemd_journal_formatter:check_config(#{fields => [a]}),
    ok = systemd_journal_formatter:check_config(#{fields => [z]}),
    ok = systemd_journal_formatter:check_config(#{fields => ['0']}),
    ok = systemd_journal_formatter:check_config(#{fields => ['9']}),
    ok = systemd_journal_formatter:check_config(#{fields => [[foo, bar]]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{"FOO", foo}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{<<"FOO">>, foo}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{"FOO_BAR", [foo, bar]}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{<<"FOO_BAR">>, [foo, bar]}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{"BAR", "bar"}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{"BAR", <<"bar">>}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{<<"BAR">>, "bar"}]}),
    ok = systemd_journal_formatter:check_config(#{fields => [{<<"BAR">>, <<"bar">>}]}),
    {error, {name_invalid, "bar"}} = systemd_journal_formatter:check_config(#{fields => [{"bar", bar}]}),
    {error, {name_invalid, "bar"}} = systemd_journal_formatter:check_config(#{fields => [{"bar", "bar"}]}),
    {error, {name_invalid, "Bar"}} = systemd_journal_formatter:check_config(#{fields => [{"Bar", bar}]}),
    {error, {name_invalid, "B.AR"}} = systemd_journal_formatter:check_config(#{fields => [{"B.AR", bar}]}),
    {error, {invalid_option, {fields, []}}} = systemd_journal_formatter:check_config(#{fields => []}),
    {error, {name_invalid, "_A"}} = systemd_journal_formatter:check_config(#{fields => ['_a']}),
    {error, {name_invalid, "_A"}} = systemd_journal_formatter:check_config(#{fields => ['_a']}),
    {error, {invalid_field, [foo, 0]}} = systemd_journal_formatter:check_config(#{fields => [[foo, 0]]}),
    {error, {invalid_field, "foo"}} = systemd_journal_formatter:check_config(#{fields => ["foo"]}),
    {error, {invalid_field, {0, foo}}} = systemd_journal_formatter:check_config(#{fields => [{0, foo}]}),
    {error, {invalid_field, {foo, foo}}} = systemd_journal_formatter:check_config(#{fields => [{foo, foo}]}),
    {error, {invalid_field, {foo, 0}}} = systemd_journal_formatter:check_config(#{fields => [{foo, 0}]}),
    {error, {invalid_field, {foo, [1.0]}}} = systemd_journal_formatter:check_config(#{fields => [{foo, [1.0]}]}),

    ok = systemd_journal_formatter:check_config(#{report_cb => fun systemd_journal_formatter:format_report/2}),
    Fun0 = fun () -> [] end,
    Fun3 = fun (_, _, _) -> [] end,
    {error, {invalid_option, {report_cb, Fun0}}} = systemd_journal_formatter:check_config(#{report_cb => Fun0}),
    {error, {invalid_option, {report_cb, Fun3}}} = systemd_journal_formatter:check_config(#{report_cb => Fun3}),

    ok = systemd_journal_formatter:check_config(#{time_designator => $T}),
    ok = systemd_journal_formatter:check_config(#{time_designator => $\s}),
    {error, {invalid_option, {time_designator, "T"}}} = systemd_journal_formatter:check_config(#{time_designator => "T"}),
    {error, {invalid_option, {time_designator, 666}}} = systemd_journal_formatter:check_config(#{time_designator => 666}),

    ok = systemd_journal_formatter:check_config(#{time_offset => 5}),
    ok = systemd_journal_formatter:check_config(#{time_offset => 10}),
    {error, {invalid_option, {time_offset, 1.0}}} = systemd_journal_formatter:check_config(#{time_offset => 1.0}),

    {error, {invalid_option, {unknown_option, 10}}} = systemd_journal_formatter:check_config(#{unknown_option => 10}),

    ok = systemd_journal_formatter:check_config(#{fields => [{<<"BAR">>, <<"bar">>}]}),
    ok.

message(_Config) ->
    Config0 = #{fields => [message]},
    <<>> = format(debug, {string, ""}, Config0),
    <<"MESSAGE=foo\n">> = format(debug, {string, "foo"}, Config0),
    <<"MESSAGE=foo\n">> = format(debug, {string, <<"foo">>}, Config0),
    <<"MESSAGE=bar\n">> = format(debug, {"~s", ["bar"]}, Config0),
    <<"MESSAGE=foo: bar\n">> = format(debug, {report, #{foo => "bar"}}, Config0),
    <<"MESSAGE=foo: []\n">> = format(debug, {report, #{foo => []}}, Config0),
    <<"MESSAGE=foo: bar\n">> = format(debug, {report, #{foo => bar}}, Config0),
    <<"MESSAGE=foo: bar\n">> = format(debug, {report, [{foo, bar}]}, Config0),
    <<"MESSAGE=[]\n">> = format(debug, {report, []}, Config0),
    <<"MESSAGE=foo\n">> = format(debug, {report, ["foo"]}, Config0),
    <<"MESSAGE=foo\n">> = format(debug, {report, [foo]}, Config0),
    <<"MESSAGE=foo\n">> = format(debug, {report, foo}, Config0),

    <<"MESSAGE=\n", 7:64/integer-little, "\nfoo\nbar\n">> = format(debug, {string, "foo\nbar"}, Config0),

    Config1 = #{fields => [msg]},
    <<>> = format(debug, {string, ""}, Config1),
    <<"MSG=foo\n">> = format(debug, {string, "foo"}, Config1),
    <<"MSG=foo\n">> = format(debug, {string, <<"foo">>}, Config1),
    <<"MSG=bar\n">> = format(debug, {"~s", ["bar"]}, Config1),
    <<"MSG=foo: bar\n">> = format(debug, {report, #{foo => "bar"}}, Config1),
    <<"MSG=foo: bar\n">> = format(debug, {report, #{foo => bar}}, Config1),
    <<"MSG=foo: bar\n">> = format(debug, {report, [{foo, bar}]}, Config1),
    <<"MSG=[]\n">> = format(debug, {report, []}, Config1),

    ReportCb1 = fun (#{foo := Data}) -> Data end,
    <<"MSG=foo\n">> = format(debug, {report, #{foo => "foo"}}, #{report_cb => ReportCb1}, Config1),
    ReportCb2 = fun (#{foo := Data}, _Options) -> Data end,
    <<"MSG=foo\n">> = format(debug, {report, #{foo => "foo"}}, #{report_cb => ReportCb2}, Config1),

    Config2 = #{fields => [{"FOO", msg}]},
    <<>> = format(debug, {string, ""}, Config2),
    <<"FOO=foo\n">> = format(debug, {string, "foo"}, Config2),
    <<"FOO=foo\n">> = format(debug, {string, <<"foo">>}, Config2),
    <<"FOO=bar\n">> = format(debug, {"~s", ["bar"]}, Config2),
    <<"FOO=foo: bar\n">> = format(debug, {report, #{foo => "bar"}}, Config2),
    <<"FOO=foo: bar\n">> = format(debug, {report, #{foo => bar}}, Config2),
    <<"FOO=foo: bar\n">> = format(debug, {report, [{foo, bar}]}, Config2),
    <<"FOO=[]\n">> = format(debug, {report, []}, Config2),

    Config3 = #{fields => [{"BAR", message}]},
    <<>> = format(debug, {string, ""}, Config3),
    <<"BAR=foo\n">> = format(debug, {string, "foo"}, Config3),
    <<"BAR=foo\n">> = format(debug, {string, <<"foo">>}, Config3),
    <<"BAR=bar\n">> = format(debug, {"~s", ["bar"]}, Config3),
    <<"BAR=foo: bar\n">> = format(debug, {report, #{foo => "bar"}}, Config3),
    <<"BAR=foo: bar\n">> = format(debug, {report, #{foo => bar}}, Config3),
    <<"BAR=foo: bar\n">> = format(debug, {report, [{foo, bar}]}, Config3),
    <<"BAR=foo: 10\n">> = format(debug, {report, [{foo, 10}]}, Config3),
    <<"BAR=[]\n">> = format(debug, {report, []}, Config3),

    ok.

fields(_Config) ->
    <<>> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", <<>>}]}),
    <<>> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", ""}]}),
    <<>> = format(debug, {string, "foo"}, #{fields => []}),
    <<>> = format(debug, {string, "foo"}, #{fields => [line]}),
    <<>> = format(debug, {string, "foo"}, #{fields => [{"LINE", line}]}),

    <<"LINE=10\n">> = format(debug, {string, "foo"}, #{line => 10}, #{fields => [line]}),
    <<"CODE_LINE=10\n">> = format(debug, {string, "foo"}, #{line => 10}, #{fields => [{"CODE_LINE", line}]}),

    <<"FILE=foo.erl\n">> = format(debug, {string, "foo"}, #{file => "foo.erl"}, #{fields => [file]}),
    <<"CODE_FILE=foo.erl\n">> = format(debug, {string, "foo"}, #{file => "foo.erl"}, #{fields => [{"CODE_FILE", file}]}),

    Time = logger:timestamp(),
    StrTime0 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $T},
                                                                       {offset, "Z"}])),
    ?assertEqual(
       <<"TIME=",StrTime0/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [time]})
      ),
    StrTime1 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, "Z"}])),
    ?assertEqual(
       <<"TIME=",StrTime1/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [time],
                                                         time_designator => $\s})
      ),
    StrTime2 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $T},
                                                                       {offset, ""}])),
    ?assertEqual(
       <<"TIME=",StrTime2/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [time],
                                                         time_offset => ""})
      ),
    StrTime3 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, ""}])),
    ?assertEqual(
       <<"TIME=",StrTime3/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [time],
                                                         time_designator => $\s,
                                                         time_offset => ""})
      ),
    StrTime4 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, 5}])),
    ?assertEqual(
       <<"TIME=",StrTime4/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [time],
                                                         time_designator => $\s,
                                                         time_offset => 5})
      ),

    % syslog_timestamp
    StrTime5 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $T},
                                                                       {offset, "Z"}])),
    ?assertEqual(
       <<"SYSLOG_TIMESTAMP=",StrTime5/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [syslog_timestamp]})
      ),
    StrTime6 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, "Z"}])),
    ?assertEqual(
       <<"SYSLOG_TIMESTAMP=",StrTime6/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [syslog_timestamp],
                                                         time_designator => $\s})
      ),
    StrTime7 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $T},
                                                                       {offset, ""}])),
    ?assertEqual(
       <<"SYSLOG_TIMESTAMP=",StrTime7/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [syslog_timestamp],
                                                         time_offset => ""})
      ),
    StrTime8 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, ""}])),
    ?assertEqual(
       <<"SYSLOG_TIMESTAMP=",StrTime8/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [syslog_timestamp],
                                                         time_designator => $\s,
                                                         time_offset => ""})
      ),
    StrTime9 = iolist_to_binary(calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                                       {time_designator, $\s},
                                                                       {offset, 5}])),
    ?assertEqual(
       <<"SYSLOG_TIMESTAMP=",StrTime9/binary,"\n">>,
       format(debug, {string, "foo"}, #{time => Time}, #{fields => [syslog_timestamp],
                                                         time_designator => $\s,
                                                         time_offset => 5})
      ),

    <<"SYSLOG_PRIORITY=7\n">> = format(debug,     {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=6\n">> = format(info,      {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=5\n">> = format(notice,    {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=4\n">> = format(warning,   {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=3\n">> = format(error,     {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=2\n">> = format(critical,  {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=1\n">> = format(alert,     {string, "foo"}, #{fields => [syslog_priority]}),
    <<"SYSLOG_PRIORITY=0\n">> = format(emergency, {string, "foo"}, #{fields => [syslog_priority]}),

    <<"SYSLOG_IDENTIFIER=example\n">> = format(emergency, {string, "foo"}, #{identifier => example}, #{fields => [syslog_identifier]}),
    <<"SYSLOG_FACILITY=user0\n">> = format(emergency, {string, "foo"}, #{facility => user0}, #{fields => [syslog_facility]}),

    OsPid = iolist_to_binary(os:getpid()),
    ?assertEqual(<<"SYSLOG_PID=",OsPid/binary,"\n">>, format(emergency, {string, "foo"}, #{fields => [syslog_pid]})),

    <<"LEVEL=debug\n">> = format(debug, {string, "foo"}, #{fields => [level]}),
    <<"SYSLOG_LEVEL=debug\n">> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_LEVEL", level}]}),

    <<"MFA=foo:bar/6\n">> = format(debug, {string, "foo"}, #{mfa => {foo, bar, 6}}, #{fields => [mfa]}),
    <<"FUNCTION=foo:bar/6\n">> = format(debug, {string, "foo"}, #{mfa => {foo, bar, 6}}, #{fields => [{"FUNCTION", mfa}]}),

    <<"SYSLOG_IDENTIFIER=foo\n">> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", "foo"}]}),
    <<"SYSLOG_IDENTIFIER=bar\n">> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", <<"bar">>}]}),

    <<"SYSLOG_IDENTIFIER=foo\n">> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", "foo"}]}),
    <<"SYSLOG_IDENTIFIER=bar\n">> = format(debug, {string, "foo"}, #{fields => [{"SYSLOG_IDENTIFIER", <<"bar">>}]}),

    <<"FOO=foo\n">> = format(debug, {string, "foo"}, #{foo => "foo"}, #{fields => [foo]}),
    <<"FOO=10\n">> = format(debug, {string, "foo"}, #{foo => 10}, #{fields => [foo]}),
    <<"FOO=10.0\n">> = format(debug, {string, "foo"}, #{foo => 10.0}, #{fields => [foo]}),
    <<"FOO=atom\n">> = format(debug, {string, "foo"}, #{foo => atom}, #{fields => [foo]}),
    <<"FOO_BAR=baz\n">> = format(debug, {string, "foo"}, #{foo => #{bar => "baz"}}, #{fields => [[foo, bar]]}),
    StrPid = iolist_to_binary(pid_to_list(self())),
    ?assertEqual(<<"FOO=",StrPid/binary,"\n">>, format(debug, {string, "foo"}, #{foo => self()}, #{fields => [foo]})),
    Ref = make_ref(),
    StrRef = iolist_to_binary(ref_to_list(Ref)),
    ?assertEqual(<<"FOO=",StrRef/binary,"\n">>, format(debug, {string, "foo"}, #{foo => Ref}, #{fields => [foo]})),
    <<"FOO=[1000]\n">> = format(debug, {string, "foo"}, #{foo => [1000]}, #{fields => [foo]}),
    <<"FOO=[foo]\n">> = format(debug, {string, "foo"}, #{foo => [foo]}, #{fields => [foo]}),
    <<"FOO=[]\n">> = format(debug, {string, "foo"}, #{foo => []}, #{fields => [foo]}),

    <<"FOO=foo\n">> = format(debug, {string, "foo"}, #{foo => "foo", bar => "bar"}, #{fields => [foo]}),
    <<"FOO=foo\n">> = format(debug, {string, "foo"}, #{foo => "foo"}, #{fields => [foo, bar]}),

    <<"MSG=foo\n">> = format(debug, {string, "foo"}, #{foo => #{bar => "baz"}}, #{fields => [msg, [foo, baz]]}),
    <<"MSG=foo\n">> = format(debug, {string, "foo"}, #{foo => "baz"}, #{fields => [msg, [foo, baz]]}),

    ok.


% -----------------------------------------------------------------------------
% Internal

format(Level,Msg,Config) ->
    format(#{level=>Level,msg=>Msg,meta=>add_time(#{})},Config).

format(Level,Msg,Meta,Config) ->
    format(#{level=>Level,msg=>Msg,meta=>add_time(Meta)},Config).

format(Log,Config) ->
    Message = iolist_to_binary(systemd_journal_formatter:format(Log,Config)),
    ct:log("~p", [Message]),
    Message.

add_time(#{time := _}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time => logger:timestamp()}.

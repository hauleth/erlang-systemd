-module(systemd_journal_h_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [{group, config}, journal_handler, filter_config, output].

groups() ->
    [{config, [],
      [changing_config,
       {config_add, [], [config_fields]},
       {config_set, [], [config_fields]},
       {config_update, [], [config_fields]}]
     }].

init_per_group(config_add, Config) ->
    [{mode, add} | Config];
init_per_group(config_set, Config) ->
    [{mode, set} | Config];
init_per_group(config_update, Config) ->
    [{mode, update} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = mock_journald:socket_path(PrivDir),
    {ok, Pid} = mock_journald:start_link(Path, self()),
    {ok, _} = application:ensure_all_started(systemd),
    Config1 = [{path, Path}, {mock_pid, Pid} | Config0],

    case erlang:function_exported(?MODULE, Name, 2) of
        true -> ?MODULE:Name(init, Config1);
        false -> Config1
    end.

end_per_testcase(Name, Config0) ->
    Config = case erlang:function_exported(?MODULE, Name, 2) of
                 true -> ?MODULE:Name(finish, Config0);
                 false -> Config0
             end,

    _ = application:stop(systemd),
    ok = gen_server:stop(?config(mock_pid, Config)),

    Config.

check_config(add, Config) ->
    case logger:add_handler(example, systemd_journal_h, #{config => Config}) of
        ok ->
            logger:remove_handler(example);
        {error, {handler_not_added, Error}} -> {error, Error}
    end;
check_config(set, Config) ->
    ok = logger:add_handler(example, systemd_journal_h, #{}),
    Result = logger:set_handler_config(example, config, Config),
    ok = logger:remove_handler(example),
    Result;
check_config(update, Config) ->
    ok = logger:add_handler(example, systemd_journal_h, #{}),
    Result = logger:update_handler_config(example, config, Config),
    ok = logger:remove_handler(example),
    Result.

config_fields(Config) ->
    Mode = ?config(mode, Config),
    ok = check_config(Mode, #{}),
    ok = check_config(Mode, #{fields => []}),
    ok = check_config(Mode, #{fields => [msg]}),
    ok = check_config(Mode, #{fields => [message]}),
    ok = check_config(Mode, #{fields => [a]}),
    ok = check_config(Mode, #{fields => [z]}),
    ok = check_config(Mode, #{fields => [a_z]}),
    ok = check_config(Mode, #{fields => ['0']}),
    ok = check_config(Mode, #{fields => ['9']}),
    ok = check_config(Mode, #{fields => [{"foo", foo}]}),
    ok = check_config(Mode, #{fields => [{<<"foo">>, foo}]}),
    ok = check_config(Mode, #{fields => [{foo, foo}]}),
    ok = check_config(Mode, #{fields => [{"FOO", foo}]}),
    ok = check_config(Mode, #{fields => [{<<"FOO">>, foo}]}),
    ok = check_config(Mode, #{fields => [{"FOO_BAR", [foo, bar]}]}),
    ok = check_config(Mode, #{fields => [{<<"FOO_BAR">>, [foo, bar]}]}),
    ok = check_config(Mode, #{fields => [{"BAR", "bar"}]}),
    ok = check_config(Mode, #{fields => [{"BAR", <<"bar">>}]}),
    ok = check_config(Mode, #{fields => [{<<"BAR">>, "bar"}]}),
    ok = check_config(Mode, #{fields => [{<<"BAR">>, <<"bar">>}]}),
    {error, {invalid_field, [foo, bar]}} = check_config(Mode, #{fields => [[foo, bar]]}),
    {error, {name_invalid, "B.AR"}} = check_config(Mode, #{fields => [{"B.AR", bar}]}),
    {error, {name_invalid, "_a"}} = check_config(Mode, #{fields => ['_a']}),
    {error, {invalid_field, [foo, 0]}} = check_config(Mode, #{fields => [[foo, 0]]}),
    {error, {invalid_field, "foo"}} = check_config(Mode, #{fields => ["foo"]}),
    {error, {invalid_field, {0, foo}}} = check_config(Mode, #{fields => [{0, foo}]}),
    {error, {invalid_option, {unknown_option, 10}}} = check_config(Mode, #{unknown_option => 10}),
    ok.

changing_config(_Config) ->
    ct:log("Fields"),
    ok = logger:add_handler(example, systemd_journal_h, #{}),
    {ok, #{config := #{fields := DefaultFields}}} = logger:get_handler_config(example),

    ok = logger:update_handler_config(example, config, #{fields => []}),
    {ok, #{config := #{fields := []}}} = logger:get_handler_config(example),

    ok = logger:update_handler_config(example, config, #{}),
    {ok, #{config := #{fields := []}}} = logger:get_handler_config(example),

    ok = logger:set_handler_config(example, config, #{}),
    {ok, #{config := #{fields := DefaultFields}}} = logger:get_handler_config(example),

    ok = logger:set_handler_config(example, config, #{fields => []}),
    {ok, #{config := #{fields := []}}} = logger:get_handler_config(example),

    ok = logger:remove_handler(example),

    ct:log("Formatter"),
    ok = logger:add_handler(example, systemd_journal_h, #{}),
    {ok, #{formatter := DefaultFormatter}} = logger:get_handler_config(example),

    ok = logger:set_handler_config(example, formatter, {dummy_formatter, #{}}),
    {ok, #{formatter := {dummy_formatter, #{}}}} = logger:get_handler_config(example),

    ok = logger:set_handler_config(example, #{}),
    {ok, #{formatter := DefaultFormatter}} = logger:get_handler_config(example),

    ok = logger:remove_handler(example),

    ok.

journal_handler(_Config) ->
    % Journal handler do not start automatically
    {error, not_found} = supervisor:get_childspec(systemd_sup, example),

    % Starts handler process when adding handler
    ok = logger:add_handler(example, systemd_journal_h, #{}),
    {ok, _} = supervisor:get_childspec(systemd_sup, example),

    % Adding new handler works
    ok = logger:add_handler(example2, systemd_journal_h, #{}),
    {ok, _} = supervisor:get_childspec(systemd_sup, example2),

    % After removing additional handler the original still works
    ok = logger:remove_handler(example2),
    ct:sleep(10),
    {error, not_found} = supervisor:get_childspec(systemd_sup, example2),
    {ok, _} = supervisor:get_childspec(systemd_sup, example),

    % After removing handler the process is shutted down
    ok = logger:remove_handler(example),
    ct:sleep(10),
    {error, not_found} = supervisor:get_childspec(systemd_sup, example),
    ok.

output(init, Config) ->
    Path = ?config(path, Config),
    ok = logger:add_handler(example,
                            systemd_journal_h,
                            #{formatter => {dumb_formatter, #{}},
                              config => #{path => {local, Path}}}),
    OldConfig = logger:get_primary_config(),
    ok = logger:update_primary_config(#{level => all}),
    [{logger_config, OldConfig} | Config];
output(finish, Config) ->
    OldConfig = ?config(logger_config, Config),
    ok = logger:remove_handler(example),
    ok = logger:set_primary_config(OldConfig),
    Config.

output(_Config) ->
    ct:log("~p", [self()]),

    % Message should always be present
    ok = logger:update_handler_config(example, config, #{fields => []}),
    {log, <<"MESSAGE=foo\n">>} = log(debug, "foo", #{}),

    % `priority' is treated in proper way
    ok = logger:update_handler_config(example, config, #{fields => [priority]}),
    {log, <<"MESSAGE=foo\nPRIORITY=7\n">>} = log(debug,     "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=6\n">>} = log(info,      "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=5\n">>} = log(notice,    "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=4\n">>} = log(warning,   "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=3\n">>} = log(error,     "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=2\n">>} = log(critical,  "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=1\n">>} = log(alert,     "foo", #{}),
    {log, <<"MESSAGE=foo\nPRIORITY=0\n">>} = log(emergency, "foo", #{}),

    % `mfa' is formatted to Erlang-style function definition
    ok = logger:update_handler_config(example, config, #{fields => [mfa]}),
    {log, <<"MESSAGE=foo\nMFA=foo:bar/6\n">>} = log(debug, "foo", #{mfa => {foo, bar, 6}}),

    % `level' is displayed as-is
    ok = logger:update_handler_config(example, config, #{fields => [level]}),
    {log, <<"MESSAGE=foo\nLEVEL=debug\n">>} = log(debug, "foo", #{}),

    % `time' is formatted as RFC3339 in UTC
    ok = logger:update_handler_config(example, config, #{fields => [time]}),
    {log, <<"MESSAGE=foo\nTIME=1970-01-01T00:00:00.000000Z\n">>} = log(debug, "foo", #{time => 0}),
    ok = logger:update_handler_config(example, config, #{fields => [syslog_timestamp]}),
    {log, <<"MESSAGE=foo\nSYSLOG_TIMESTAMP=1970-01-01T00:00:00.000000Z\n">>} = log(debug, "foo", #{time => 0}),

    % `os_pid' returns curent OS PID
    ok = logger:update_handler_config(example, config, #{fields => [os_pid]}),
    OsPid = iolist_to_binary(os:getpid()),
    ?assertEqual({log, <<"MESSAGE=foo\nOS_PID=", OsPid/binary, "\n">>}, log(debug, "foo", #{})),
    ok = logger:update_handler_config(example, config, #{fields => [syslog_pid]}),
    ?assertEqual({log, <<"MESSAGE=foo\nSYSLOG_PID=", OsPid/binary, "\n">>}, log(debug, "foo", #{})),

    % `pid' is printed as Erlang PID
    ok = logger:update_handler_config(example, config, #{fields => [pid]}),
    Pid = iolist_to_binary(pid_to_list(self())),
    ?assertEqual({log, <<"MESSAGE=foo\nPID=", Pid/binary, "\n">>}, log(debug, "foo", #{})),

    % Simple metadata access
    ok = logger:update_handler_config(example, config, #{fields => [foo]}),
    {log, <<"MESSAGE=foo\nFOO=\n">>} = log(debug, "foo", #{}),
    {log, <<"MESSAGE=foo\nFOO=1\n">>} = log(debug, "foo", #{foo => 1}),
    {log, <<"MESSAGE=foo\nFOO=foo\n">>} = log(debug, "foo", #{foo => "foo"}),
    {log, <<"MESSAGE=foo\nFOO=foo\n">>} = log(debug, "foo", #{foo => <<"foo">>}),
    {log, <<"MESSAGE=foo\nFOO=<<0>>\n">>} = log(debug, "foo", #{foo => <<0>>}),
    {log, <<"MESSAGE=foo\nFOO=<<>>\n">>} = log(debug, "foo", #{foo => <<>>}),
    {log, <<"MESSAGE=foo\nFOO=[]\n">>} = log(debug, "foo", #{foo => []}),
    {log, <<"MESSAGE=foo\nFOO=[1000]\n">>} = log(debug, "foo", #{foo => [1000]}),
    {log, <<"MESSAGE=foo\nFOO={}\n">>} = log(debug, "foo", #{foo => {}}),
    {log, <<"MESSAGE=foo\nFOO={foo,1}\n">>} = log(debug, "foo", #{foo => {foo,1}}),
    Ref = make_ref(),
    BinRef = iolist_to_binary(ref_to_list(Ref)),
    ?assertEqual({log, <<"MESSAGE=foo\nFOO=", BinRef/binary, "\n">>}, log(debug, "foo", #{foo => Ref})),
    % Do not fail when trying to format function
    Func = fun(A) -> A + 1 end,
    {log, <<"MESSAGE=foo\nFOO=", _/binary>>} = log(debug, "foo", #{foo => Func}),

    % Nested metadata access
    ok = logger:update_handler_config(example, config, #{fields => [{"FOO", [foo, bar]}]}),
    {log, <<"MESSAGE=foo\nFOO=\n">>} = log(debug, "foo", #{}),
    {log, <<"MESSAGE=foo\nFOO=\n">>} = log(debug, "foo", #{foo => 1}),
    {log, <<"MESSAGE=foo\nFOO=1\n">>} = log(debug, "foo", #{foo => #{bar => 1}}),

    % Literal field values
    ok = logger:update_handler_config(example, config, #{fields => [{"FOO", "BAR"}]}),
    {log, <<"MESSAGE=foo\nFOO=BAR\n">>} = log(debug, "foo", #{}),
    ok = logger:update_handler_config(example, config, #{fields => [{"FOO", <<"BAR">>}]}),
    {log, <<"MESSAGE=foo\nFOO=BAR\n">>} = log(debug, "foo", #{}),
    ok = logger:update_handler_config(example, config, #{fields => [{"FOO", [$B, <<"A">>, "R"]}]}),
    {log, <<"MESSAGE=foo\nFOO=BAR\n">>} = log(debug, "foo", #{}),

    % Empty messages aren't sent at all
    ok = logger:set_handler_config(example, config, #{}),
    nolog = log(info, "", #{}),

    ok.

filter_config(_Config) ->
    % Starts handler process when adding handler
    ok = logger:add_handler(example,
                            systemd_journal_h,
                            #{formatter => {dumb_formatter, #{}}}),
    {ok, #{config := Config}} = logger:get_handler_config(example),

    ?assertNot(maps:is_key(pid, Config)),
    ?assertNot(maps:is_key(socket, Config)),
    ?assertNot(maps:is_key(path, Config)),
    ok = logger:remove_handler(example).

% -----------------------------------------------------------------------------
% Internal

log(Level,Msg,Meta) ->
    logger:log(Level, Msg, add_time(Meta)),
    receive
        {log, Data} -> {log, Data}
    after
        100 -> nolog
    end.

add_time(#{time := _}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time => logger:timestamp()}.

-module(systemd_watchdog_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(assertReceived(Message),
    receive
        Message -> ok
    after 0 ->
        ct:fail(
            "Expected message ~p",
            [Message]
        )
    end
).

-import(systemd_test_utils, [
    socket_path/1,
    start_with_socket/1,
    stop/1,
    flush/1,
    recv/1
]).

all() ->
    [
        env_usec_set,
        env_pid_mismatch,
        env_pid_match,
        invalid,
        env_usec_is_zero,
        env_usec_is_non_int,
        state_control,
        server_restart,
        unset_env,
        {group, func_check},
        {group, mfa_check}
    ].

groups() ->
    FuncTests = [
        check_func_true,
        check_func_false,
        check_func_non_bool,
        check_func_failure
    ],
    [
        {func_check, [], FuncTests},
        {mfa_check, [], FuncTests}
    ].

init_per_group(mfa_check, Config) ->
    [
        {func, fun(Pid, Ret) ->
            fun() -> check(Pid, Ret) end
        end}
        | Config
    ];
init_per_group(func_check, Config) ->
    [
        {func, fun(Pid, Ret) ->
            {?MODULE, check, [Pid, Ret]}
        end}
        | Config
    ].

check(Pid, Cb) when is_function(Cb) ->
    check(Pid, Cb());
check(Pid, Result) ->
    Pid ! {check, Result},
    Result.

end_per_group(_Name, Config) -> Config.

init_per_testcase(_Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = socket_path(PrivDir),
    Self = self(),
    Config1 =
        case ?config(func, Config0) of
            Func0 when is_function(Func0, 2) ->
                Func = fun(Ret) -> Func0(Self, Ret) end,
                [{func, Func} | Config0];
            _ ->
                Config0
        end,
    Timeout = 200,
    Timeout0 = erlang:convert_time_unit(
        Timeout,
        millisecond,
        microsecond
    ),
    os:putenv("WATCHDOG_PID", os:getpid()),
    os:putenv("WATCHDOG_USEC", integer_to_list(Timeout0)),
    {ok, Socket} = socket:open(local, dgram, default),
    case socket:bind(Socket, #{family => local, path => Path}) of
        {ok, _Port} -> ok; %% Erlang up to 23
        ok          -> ok  %% Erlang 24+
    end,
    [{socket, Socket}, {path, Path}, {timeout, Timeout} | Config1].

end_per_testcase(_Name, Config) ->
    _ = application:stop(systemd),
    _ = socket:close(?config(socket, Config)),
    _ = file:delete(?config(path, Config)),

    systemd:unset_env(notify),
    systemd:unset_env(watchdog),
    systemd:unset_env(listen_fds),

    Config.

env_usec_set(Config) ->
    Socket = ?config(socket, Config),
    Timeout = ?config(timeout, Config),

    ok = start_with_socket(Socket),
    ?assertEqual(Timeout, systemd:watchdog(state)),
    ct:sleep(300),

    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok.

env_pid_mismatch(Config) ->
    Socket = ?config(socket, Config),

    os:putenv("WATCHDOG_PID", "foo"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),

    ok = empty(Socket),
    ok.

env_pid_match(Config) ->
    Socket = ?config(socket, Config),
    Timeout = ?config(timeout, Config),

    os:putenv("WATCHDOG_PID", os:getpid()),
    ok = start_with_socket(Socket),
    Timeout = systemd:watchdog(state),
    ct:sleep(300),

    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok.

invalid(Config) ->
    Socket = ?config(socket, Config),

    os:putenv("WATCHDOG_USEC", "0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ok = empty(Socket),

    systemd:watchdog(enable),
    false = systemd:watchdog(state),
    ok = empty(Socket),
    ok.

env_usec_is_zero(Config) ->
    Socket = ?config(socket, Config),

    os:putenv("WATCHDOG_USEC", "0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok.

env_usec_is_non_int(Config) ->
    Socket = ?config(socket, Config),

    os:putenv("WATCHDOG_USEC", "foo"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok = stop(Config),

    os:putenv("WATCHDOG_USEC", "1.0"),
    ok = start_with_socket(Socket),
    false = systemd:watchdog(state),
    ct:sleep(300),
    ok = empty(Socket),
    ok.

state_control(Config) ->
    Socket = ?config(socket, Config),
    Timeout = ?config(timeout, Config),

    ok = start_with_socket(Socket),

    ct:log("-> state enabled"),
    ?assertEqual(Timeout, systemd:watchdog(state)),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> disable"),
    ok = systemd:watchdog(disable),
    ct:sleep(300),
    ok = empty(Socket),

    ct:log("-> state disabled"),
    ?assertEqual(false, systemd:watchdog(state)),
    ct:sleep(300),
    ok = empty(Socket),

    ct:log("-> ping disabled"),
    ok = systemd:watchdog(ping),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> enable"),
    ok = systemd:watchdog(enable),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    flush(Socket),

    ct:log("-> trigger"),
    ok = systemd:watchdog(trigger),
    {ok, <<"WATCHDOG=trigger\n">>} = recv(Socket),
    flush(Socket),

    ok.

server_restart(Config) ->
    Socket = ?config(socket, Config),

    ok = start_with_socket(Socket),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = empty(Socket),
    gen_server:stop(systemd_watchdog, error, 100),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ok = empty(Socket),

    ok.

unset_env(Config) ->
    Socket = ?config(socket, Config),

    ok = start_with_socket(Socket),
    false = os:getenv("WATCHDOG_PID"),
    false = os:getenv("WATCHDOG_USEC"),

    ok.

do_not_unset_when_opted_out(Config) ->
    Socket = ?config(socket, Config),

    ok = application:set_env(systemd, unset_env, false),
    ok = start_with_socket(Socket),
    ?assertEqual(os:getpid(), os:getenv("WATCHDOG_PID")),
    ?assertNotEqual(false, os:getenv("WATCHDOG_USEC")),
    ok = application:set_env(systemd, unset_env, true),

    ok.

check_func_true(Config) ->
    Func = ?config(func, Config),
    Socket = ?config(socket, Config),

    application:set_env(systemd, watchdog_check, Func(true)),
    ok = start_with_socket(Socket),
    ct:sleep(300),
    {ok, <<"WATCHDOG=1\n">>} = recv(Socket),
    ?assertReceived({check, true}),
    application:set_env(systemd, watchdog_check, undefined).

check_func_false(Config) ->
    Func = ?config(func, Config),
    Socket = ?config(socket, Config),

    application:set_env(systemd, watchdog_check, Func(false)),
    ok = start_with_socket(Socket),
    ct:sleep(300),
    ?assertReceived({check, false}),
    ok = empty(Socket),
    application:unset_env(systemd, watchdog_check).

check_func_non_bool(Config) ->
    Func = ?config(func, Config),
    Socket = ?config(socket, Config),

    application:set_env(systemd, watchdog_check, Func(10)),
    ok = start_with_socket(Socket),
    ct:sleep(300),
    ok = empty(Socket),
    application:unset_env(systemd, watchdog_check).

check_func_failure(Config) ->
    Socket = ?config(socket, Config),
    Func = ?config(func, Config),

    Cb = fun() -> error("Failed") end,

    application:set_env(systemd, watchdog_check, Func(Cb)),
    ok = start_with_socket(Socket),
    ct:sleep(300),
    ok = empty(Socket),
    application:unset_env(systemd, watchdog_check).

empty(S) ->
    case socket:recv(S, 0, 0) of
        {ok, <<>>} -> ok;
        {error, timeout} -> ok;
        {ok, Msg} -> {error, {received, Msg}};
        {error, _} = Error -> Error
    end.

-module(systemd_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-import(systemd_test_utils, [
    socket_path/1,
    start_with_socket/1,
    start_with_path/1,
    stop/1,
    flush/1,
    recvmsg/1,
    recv/1
]).

all() -> [notify, ready, ready_warning, listen_fds, socket, fds].

init_per_testcase(Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = socket_path(PrivDir),
    {ok, Socket} = socket:open(local, dgram, default),
    case socket:bind(Socket, #{family => local, path => Path}) of
        %% Erlang up to 23
        {ok, _Port} -> ok;
        %% Erlang 24+
        ok -> ok
    end,
    Config1 = [{socket, Socket}, {path, Path} | Config0],

    case erlang:function_exported(?MODULE, Name, 2) of
        true -> ?MODULE:Name(init, Config1);
        false -> Config1
    end.

end_per_testcase(Name, Config0) ->
    Config =
        case erlang:function_exported(?MODULE, Name, 2) of
            true -> ?MODULE:Name(finish, Config0);
            false -> Config0
        end,

    _ = application:stop(systemd),
    _ = socket:close(?config(socket, Config)),
    _ = file:delete(?config(path, Config)),

    systemd:unset_env(notify),
    systemd:unset_env(watchdog),
    systemd:unset_env(listen_fds),

    Config.

notify(init, Config) ->
    Socket = ?config(socket, Config),
    ok = start_with_socket(Socket),
    Config;
notify(_, Config) ->
    Config.

notify(Config) ->
    Socket = ?config(socket, Config),

    systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    systemd:notify(stopping),
    {ok, <<"STOPPING=1\n">>} = recv(Socket),
    systemd:notify(reloading),
    {ok, <<"RELOADING=1\n">>} = recv(Socket),
    systemd:notify({status, "example status"}),
    {ok, <<"STATUS=example status\n">>} = recv(Socket),
    systemd:notify({errno, 10}),
    {ok, <<"ERRNO=10\n">>} = recv(Socket),
    systemd:notify({buserror, "test.example.bus.service.Error"}),
    {ok, <<"BUSERROR=test.example.bus.service.Error\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, microsecond}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, millisecond}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5000\n">>} = recv(Socket),
    systemd:notify({extend_timeout, {5, second}}),
    {ok, <<"EXTEND_TIMEOUT_USEC=5000000\n">>} = recv(Socket),
    systemd:notify({custom, "message"}),
    {ok, <<"CUSTOM=message\n">>} = recv(Socket),

    ct:log("Connection address persists between process restarts"),
    gen_server:stop(systemd_socket, error, 100),
    ct:sleep(100),
    systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok.

ready(init, Config) ->
    ok = start_with_socket(?config(socket, Config)),
    Config;
ready(_, Config) ->
    stop(Config),
    Config.

ready(Config) ->
    Socket = ?config(socket, Config),
    {ok, _Pid} = mock_supervisor:start_link([systemd:ready()]),
    ?assertEqual(
        {ok, <<"READY=1\n">>},
        recv(Socket)
    ),

    ok.

ready_warning(init, Config) ->
    ok = logger:add_handler(send_back,
                            logger_send_back_h,
                            #{
                              config => #{
                                          pid => self()
                                         }
                             }),
    application:set_env(systemd, warn_about_readiness_message, 1000),
    Config;
ready_warning(_, Config) ->
    _ = logger:remove_handler(send_back),
    application:set_env(systemd, warn_about_readiness_message, 10000),
    stop(Config),
    Config.

ready_warning(Config) ->
    Socket = ?config(socket, Config),
    ok = start_with_socket(Socket),
    ct:sleep(2000),
    receive
        #{msg := {string, Msg}, level := warning} ->
            ?assert(string:find(Msg, "systemd haven't received READY=1") =/= nomatch),
            ok
    after
        0 -> ct:fail("Haven't received any warning message")
    end,
    stop(Config),

    ok = start_with_socket(Socket),
    systemd:notify(ready),
    ct:sleep(2000),
    receive
        #{msg := {string, _Msg}, level := warning} ->
            ct:fail("Received warning even after sending ready message")
    after
        0 -> ok
    end.

listen_fds(init, Config) ->
    Config;
listen_fds(finish, Config) ->
    os:unsetenv("LISTEN_PID"),
    os:unsetenv("LISTEN_FDS"),
    os:unsetenv("LISTEN_FDNAMES"),
    Config.

listen_fds(_Config) ->
    % -------------------------------------------------------------------------
    ct:log("When no environment variables it returns empty list"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log(
        "When no LISTEN_PID environment variable is set it returns empty list"
    ),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "3"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_PID mismatch it returns empty list"),
    os:putenv("LISTEN_PID", "foo"),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log(
        "When LISTEN_PID match returned list has LISTEN_FDS amount of entries"
    ),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    ?assertEqual(3, length(systemd:listen_fds())),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not set, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When LISTEN_FDS is not integer, it is assumed to be 0"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "foo"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "1.0"),
    ?assertEqual([], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("Returned list has PIDs with respective names"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, {_, "baz"}], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When subsequent calls will return the same data"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar:baz"),
    First = systemd:listen_fds(),
    First = systemd:listen_fds(),
    systemd:unset_env(listen_fds),
    [] = systemd:listen_fds(),
    [] = systemd:listen_fds(),

    % -------------------------------------------------------------------------
    ct:log("When name is empty it returns raw FD"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo::baz"),
    ?assertMatch([{_, "foo"}, _, {_, "baz"}], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    % -------------------------------------------------------------------------
    ct:log("When names list is shorter it returns raw FDs"),
    os:putenv("LISTEN_PID", os:getpid()),
    os:putenv("LISTEN_FDS", "3"),
    os:putenv("LISTEN_FDNAMES", "foo:bar"),
    ?assertMatch([{_, "foo"}, {_, "bar"}, _], systemd:listen_fds()),
    systemd:unset_env(listen_fds),

    ok.

socket(Config) ->
    ct:log("~p", [Config]),
    Socket = ?config(socket, Config),

    % -------------------------------------------------------------------------
    ct:log("When started without NOTIFY_SOCKET it is noop"),
    {ok, _} = application:ensure_all_started(systemd),
    ok = systemd:notify(ready),
    ok = empty(Socket),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with invalid NOTIFY_SOCKET it is noop"),
    ok = start_with_path("/non/existent"),
    ok = systemd:notify(ready),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("When started with empty NOTIFY_SOCKET it is noop"),
    ok = start_with_path(""),
    ok = systemd:notify(ready),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("By default unset NOTIFY_SOCKET"),
    ok = start_with_socket(Socket),
    ok = systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok = empty(Socket),
    false = os:getenv("NOTIFY_SOCKET"),
    ok = stop(Config),

    % -------------------------------------------------------------------------
    ct:log("Do not unset NOTIFY_SOCKET when unset_env is false"),
    ok = application:set_env(systemd, unset_env, false),
    ok = start_with_socket(Socket),
    ok = systemd:notify(ready),
    {ok, <<"READY=1\n">>} = recv(Socket),
    ok = empty(Socket),
    ?assertEqual(?config(path, Config), os:getenv("NOTIFY_SOCKET")),
    ok = stop(Config),
    ok = application:set_env(systemd, unset_env, true),

    ok.

fds(init, Config) ->
    Socket = ?config(socket, Config),
    ok = start_with_socket(Socket),
    Config;
fds(_, Config) ->
    Config.

fds(Config) ->
    Socket = ?config(socket, Config),
    ok = systemd:store_fds([1]),
    ?assertMatch(
        {ok, #{
            iov := [<<"FDSTORE=1\nFDNAMES=\n">>],
            ctrl := [#{type := rights}]
        }},
        recvmsg(Socket)
    ),
    ok = systemd:store_fds([{1, "foo"}]),
    ?assertMatch(
        {ok, #{
            iov := [<<"FDSTORE=1\nFDNAMES=foo\n">>],
            ctrl := [#{type := rights}]
        }},
        recvmsg(Socket)
    ),
    ok = systemd:store_fds([1, {2, "foo"}]),
    ?assertMatch(
        {ok, #{
            iov := [<<"FDSTORE=1\nFDNAMES=:foo\n">>],
            ctrl := [#{type := rights}]
        }},
        recvmsg(Socket)
    ),
    {error, bad_descriptor} = systemd:store_fds([999]),
    ok = empty(Socket),

    systemd:clear_fds(["foo", "bar"]),
    {ok, <<"FDSTOREREMOVE=1\nFDNAMES=foo:bar\n">>} = recv(Socket),

    ok.

empty(S) ->
    case socket:recv(S, 0, 0) of
        {ok, <<>>} -> ok;
        {error, timeout} -> ok;
        {ok, Msg} -> {error, {received, Msg}};
        {error, _} = Error -> Error
    end.

-module(systemd_test_utils).

-include_lib("common_test/include/ct.hrl").

-export([
    socket_path/1,
    start_with_socket/1,
    start_with_path/1,
    stop/1,
    flush/1,
    recv/1,
    recvmsg/1,
    app_paths/1
]).

%% This hack is needed to reduce length of the socket path which is limited to
%% 104-108 characters (depends on the OS).
-spec socket_path(PrivDir :: file:name()) -> string().
socket_path(PrivDir0) ->
    {ok, Cwd} = file:get_cwd(),
    PrivDir = string:prefix(PrivDir0, Cwd),
    erlang:binary_to_list(iolist_to_binary([".", PrivDir, "systemd.sock"])).

start_with_socket(Socket) ->
    {ok, #{path := Path}} = socket:sockname(Socket),
    start_with_path(Path).

start_with_path(Path) when is_list(Path) ->
    ct:log("Start app with socket at ~p", [Path]),
    systemd_env:clear(),
    os:putenv("NOTIFY_SOCKET", Path),
    {ok, _} = application:ensure_all_started(systemd),
    ok;
start_with_path(Path) when is_binary(Path) ->
    start_with_path(binary_to_list(Path)).

stop(Config) ->
    _ = application:stop(systemd),
    ok = flush(?config(socket, Config)),
    _ = persistent_term:erase(systemd_env),
    ok.

recv(S) ->
    Resp = socket:recv(S, 0, 1000),
    ct:log("recv/1: ~p~n", [Resp]),
    Resp.

recvmsg(S) ->
    Resp = socket:recvmsg(S, 0, 1000),
    ct:log("recvmsg/1: ~p~n", [Resp]),
    Resp.

flush(S) ->
    case socket:recv(S, 0, 0) of
        {ok, _} -> flush(S);
        _ -> ok
    end.

app_paths(AppName) ->
    Apps = lists:uniq(gather_apps(AppName)),
    [get_app_path(App) || App <- Apps].

get_app_path(AppName) ->
    FName = atom_to_list(AppName) ++ ".app",
    filename:dirname(code:where_is_file(FName)).

gather_apps(AppName) ->
    _ = application:load(AppName),
    {ok, Deps} = application:get_key(AppName, applications),
    [AppName | lists:flatten([gather_apps(Dep) || Dep <- Deps])].

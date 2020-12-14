-module(systemd_test_utils).

-include_lib("common_test/include/ct.hrl").

-export([
    socket_path/1,
    start_with_socket/1,
    start_with_path/1,
    stop/1,
    flush/1,
    recv/1,
    recvmsg/1
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

start_with_path(Path) ->
    ct:log("Start app with socket at ~p", [Path]),
    os:putenv("NOTIFY_SOCKET", Path),
    {ok, _} = application:ensure_all_started(systemd),
    ok.

stop(Config) ->
    ok = application:stop(systemd),
    ok = flush(?config(socket, Config)),
    ok.

recv(S) ->
    socket:recv(S, 0, 1000).

recvmsg(S) ->
    socket:recvmsg(S, 0, 1000).

flush(S) ->
    case socket:recv(S, 0, 0) of
        {ok, _} -> flush(S);
        _ -> ok
    end.

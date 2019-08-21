-module(systemd).

-define(NOTIFY_SOCKET, "NOTIFY_SOCKET").

-type state() :: ready | stopping | binary() | string().

-export([notify/1]).
-export_type([state/0]).

-spec notify(State :: state()) -> ok.
notify(State) ->
    case is_linux() of
        true -> do_notify(State);
        _ -> ok
    end.

do_notify(State) ->
    case has_env(?NOTIFY_SOCKET) of
        false -> ok;
        Path ->
            Address = {local, normalize_path(Path)},
            {ok, Socket} = gen_udp:open(0, [local]),
            ok = gen_udp:send(Socket, Address, 0, normalize_state(State)),
            ok = gen_udp:close(Socket),

            ok
    end.

normalize_path([$@ | Rest]) -> [0 | Rest];
normalize_path(Value) -> Value.

normalize_state(ready) -> "READY=1";
normalize_state(stopping) -> "STOPPING=1";
normalize_state(Msg) -> Msg.

has_env(Name) ->
    case os:getenv(Name) of
        false -> false;
        "" -> false;
        Value -> Value
    end.

is_linux() ->
    case os:type() of
        {unix, linux} -> true;
        _ -> false
    end.

-module(echo_tcp).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1,
         listen/1]).

start_link(Opt) ->
    Pid = spawn_link(?MODULE, listen, [Opt]),
    {ok, Pid}.

listen(Opt) ->
    Opts = [Opt,
            inet6,
            binary,
            {packet, 0},
            {active, false},
            {reuseaddr, true}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    accept(LSock).

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> loop(Sock) end),
    accept(LSock).

loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            ?LOG_NOTICE(#{data => Data, protocol => tcp}),
            gen_tcp:send(Sock, Data),
            loop(Sock);
        {error, closed} ->
            ok
    end.

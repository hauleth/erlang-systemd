-module(echo_udp).

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
            {active, false},
            {reuseaddr, true}],
    {ok, Sock} = gen_udp:open(0, Opts),
    loop(Sock).

loop(Sock) ->
    case gen_udp:recv(Sock, 1024) of
        {ok, {Addr, Port, Data}} ->
            ?LOG_NOTICE(#{data => Data, protocol => udp}),
            ok = gen_udp:send(Sock, Addr, Port, Data),
            loop(Sock);
        {ok, {Addr, Port, Data, AncData}} ->
            ?LOG_NOTICE(#{data => Data, protocol => udp}),
            ok = gen_udp:send(Sock, Addr, Port, AncData, Data),
            loop(Sock);
        _ ->
            loop(Sock)
    end.

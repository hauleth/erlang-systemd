%% @hidden

-module(systemd_watchdog).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("systemd.hrl").

-define(PID, "WATCHDOG_PID").
-define(TIMEOUT, "WATCHDOG_USEC").

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {timeout, enabled=true}).

start_link() ->
    Pid = os:getpid(),
    case {watchdog_pid(), watchdog_timeout()} of
        {Pid, Timeout} when Timeout > 0 ->
            gen_server:start_link({local, ?WATCHDOG}, ?MODULE, Timeout, []);
        _ ->
            ignore
    end.

init(Timeout) ->
    systemd:notify(watchdog),
    {ok, #state{timeout=Timeout}, Timeout}.

handle_call(trigger, _Ref, State) ->
    systemd:notify(watchdog_trigger),
    {reply, ok, State#state{enabled=false}};
handle_call(enable, _Ref, #state{timeout=Timeout}=State) ->
    systemd:notify(watchdog),
    {reply, ok, State#state{enabled=true}, Timeout};
handle_call(disable, _Ref, State) ->
    {reply, ok, State#state{enabled=false}};
handle_call(state, _Ref, State) ->
    case State of
        #state{enabled=true, timeout=Timeout} ->
            systemd:notify(watchdog),
            {reply, Timeout, State, Timeout};
        _ ->
            {reply, false, State}
    end;
handle_call(ping, _Ref, State) ->
    systemd:notify(watchdog),
    case State of
        #state{enabled=true, timeout=Timeout} ->
            {reply, ok, State, Timeout};
        _ ->
            {reply, ok, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{timeout=Timeout, enabled=true}=State) ->
    systemd:notify(watchdog),
    {noreply, State, Timeout};
handle_info(timeout, State) ->
    {noreply, State}.

watchdog_pid() ->
    Return = case os:getenv(?PID) of
                 false -> os:getpid();
                 Env -> Env
             end,
    os:unsetenv(?PID),
    Return.

watchdog_timeout() ->
    Return = case os:getenv(?TIMEOUT) of
                 false -> -1;
                 Env ->
                     case string:to_integer(Env) of
                         {Timeout, ""} -> Timeout;
                         _ -> -1
                     end
             end,
    os:unsetenv(?TIMEOUT),
    Return.

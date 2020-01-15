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
            gen_server:start_link({local, ?WATCHDOG}, ?MODULE, infinity, [])
    end.

init(Timeout) ->
    State = #state{timeout=Timeout},
    notify(State),
    {ok, State}.

handle_call(trigger, _Ref, State) ->
    systemd:notify(watchdog_trigger),
    {reply, ok, State#state{enabled=false}};
handle_call(enable, _Ref, State) ->
    NewState = State#state{enabled=true},
    notify(NewState),
    {reply, ok, NewState};
handle_call(disable, _Ref, State) ->
    {reply, ok, State#state{enabled=false}};
handle_call(state, _Ref, State) ->
    case State of
        #state{enabled=true, timeout=Timeout} ->
            {reply, Timeout, State};
        _ ->
            {reply, false, State}
    end;
handle_call(ping, _Ref, State) ->
    systemd:notify(watchdog),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(keepalive, State) ->
    notify(State),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

notify(#state{enabled=true, timeout=Timeout}) when is_integer(Timeout) andalso Timeout > 2 ->
    {ok, Scale} = application:get_env(systemd, watchdog_scale),
    systemd:notify(watchdog),
    erlang:send_after(Timeout div Scale, self(), keepalive);
notify(_State) ->
    ok.

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

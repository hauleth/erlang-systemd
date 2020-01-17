%% @doc
%% Logger handler for sending messages to systemd's `journal'.
%%
%% == Usage ==
%%
%% Run this after the `systemd' application is started:
%%
%% ```
%% logger:add_handler(journal,
%%                    systemd_journal_h,
%%                    #{formatter => {systemd_journal_formatter, #{}}).
%% '''
%%
%% It is very important to use `systemd_journal_formatter' here, otherwise the
%% messages will not be recorded.
%%
%% == Warning ==
%%
%% This logger <b>should</b> always be used with `systemd_journal_formatter' unless
%% you are completely sure what you are trying to do. Otherwise you can loose your
%% log data.
%% @end
-module(systemd_journal_h).

-behaviour(gen_server).

-include("systemd_internal.hrl").

-define(JOURNAL_SOCKET, {local, <<"/run/systemd/journal/socket">>}).

% logger handler callbacks
-export([adding_handler/1,
         % filter_config/1,
         removing_handler/1,
         log/2]).

% gen_server callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(FORMATTER, {systemd_journal_formatter, #{}}).
-define(CHILD_SPEC(Id, Args), #{id => Id,
                                start => {?MODULE, start_link, Args},
                                restart => temporary}).

% -----------------------------------------------------------------------------
% Logger Handler

%% @hidden
-spec adding_handler(logger:handler_config()) -> logger:handler_config().
adding_handler(HConfig) ->
    Config = maps:get(config, HConfig, #{}),
    Path = maps:get(path, Config, ?JOURNAL_SOCKET),
    case start_connection(HConfig) of
        {ok, Pid} ->
            {ok, Socket} = gen_server:call(Pid, get),
            {ok, HConfig#{config => Config#{pid => Pid,
                                            socket => Socket,
                                            path => Path}}};
        Err ->
            Err
    end.

start_connection(#{id := Id}) ->
    case supervisor:start_child(?SUPERVISOR, ?CHILD_SPEC(Id, [])) of
        {ok, Pid} -> {ok, Pid};
        {error, Error} -> {error, {spawn_error, Error}}
    end.

%% @hidden
-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(#{config := #{pid := Pid}}) ->
    ok = gen_server:call(Pid, stop),
    ok.

%% @hidden
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(LogEvent, #{config := #{socket := Socket, path := Path}}=Config) ->
    {FMod, FConf} = maps:get(formatter, Config, ?FORMATTER),
    Buffer = FMod:format(LogEvent, FConf),
    case string:is_empty(Buffer) of
        false ->
            ok = gen_udp:send(Socket, Path, 0, Buffer);
        true -> ok
    end.

% -----------------------------------------------------------------------------
% Socket handler

%% @hidden
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @hidden
init(_Arg) ->
    % We never receive on this socket, so we set is as {active, false}
    {ok, Socket} = gen_udp:open(0, [binary, local, {active, false}]),
    {ok, Socket}.

%% @hidden
% TODO: Implement async handler and overload protectopn
handle_call(get, _Ref, Socket) ->
    {reply, {ok, Socket}, Socket};
handle_call(stop, _Ref, Socket) ->
    {stop, normal, ok, Socket}.

%% @hidden
handle_cast(_Msg, Socket) ->
    {noreply, Socket}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%% @doc echo public API
%% @end
%%%-------------------------------------------------------------------

-module(echo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case logger:add_handlers(systemd) of
        ok ->
            logger:remove_handler(default);
        _ ->
            ok
    end,
    echo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

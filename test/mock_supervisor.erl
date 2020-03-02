-module(mock_supervisor).

-behaviour(supervisor).

-export([start_link/1,
         init/1]).

start_link(Children) ->
    supervisor:start_link(?MODULE, Children).

init(Children) ->
    {ok, {#{strategy => one_for_one}, Children}}.

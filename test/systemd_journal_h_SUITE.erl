-module(systemd_journal_h_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [journal_handler, output].

init_per_testcase(Name, Config0) ->
    PrivDir = ?config(priv_dir, Config0),
    Path = mock_journald:socket_path(PrivDir),
    {ok, Pid} = mock_journald:start_link(Path, self()),
    {ok, _} = application:ensure_all_started(systemd),
    Config1 = [{path, Path}, {mock_pid, Pid} | Config0],

    case erlang:function_exported(?MODULE, Name, 2) of
        true -> ?MODULE:Name(init, Config1);
        false -> Config1
    end.

end_per_testcase(Name, Config0) ->
    Config = case erlang:function_exported(?MODULE, Name, 2) of
                 true -> ?MODULE:Name(finish, Config0);
                 false -> Config0
             end,

    _ = application:stop(systemd),
    ok = gen_server:stop(?config(mock_pid, Config)),

    Config.

journal_handler(_Config) ->
    % Journal handler do not start automatically
    {error, not_found} = supervisor:get_childspec(systemd_sup, example),

    % Starts handler process when adding handler
    ok = logger:add_handler(example,
                            systemd_journal_h,
                            #{formatter => {dumb_formatter, #{}}}),
    {ok, _} = supervisor:get_childspec(systemd_sup, example),

    % Adding new handler works
    ok = logger:add_handler(example2, systemd_journal_h, #{}),
    {ok, _} = supervisor:get_childspec(systemd_sup, example2),

    % After removing additional handler the original still works
    ok = logger:remove_handler(example2),
    ct:sleep(10),
    {error, not_found} = supervisor:get_childspec(systemd_sup, example2),
    {ok, _} = supervisor:get_childspec(systemd_sup, example),

    % After removing handler the process is shutted down
    ok = logger:remove_handler(example),
    ct:sleep(10),
    {error, not_found} = supervisor:get_childspec(systemd_sup, example),
    ok.

output(init, Config) ->
    Path = ?config(path, Config),
    ok = logger:add_handler(example,
                            systemd_journal_h,
                            #{formatter => {dumb_formatter, #{}},
                              config => #{path => {local, Path}}}),
    OldConfig = logger:get_primary_config(),
    ok = logger:update_primary_config(#{level => all}),
    [{logger_config, OldConfig} | Config];
output(finish, Config) ->
    OldConfig = ?config(logger_config, Config),
    ok = logger:remove_handler(example),
    ok = logger:set_primary_config(OldConfig),
    Config.

output(_Config) ->
    ct:log("~p", [self()]),
    % Test that logging works
    {log, <<"foo">>} = log(debug, "foo", #{}),

    % Empty messages aren't sent at all
    nolog = log(info, "", #{}),

    ok.

% -----------------------------------------------------------------------------
% Internal

log(Level,Msg,Meta) ->
    logger:log(Level, Msg, add_time(Meta)),
    receive
        {log, Data} -> {log, Data}
    after
        1000 -> nolog
    end.

add_time(#{time := _}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time => logger:timestamp()}.

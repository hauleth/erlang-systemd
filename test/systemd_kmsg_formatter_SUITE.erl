-module(systemd_kmsg_formatter_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/file.hrl").

all() -> [default, custom_parent, log_prefix, {group, auto_install}].

groups() -> [
             {auto_install, [shuffle], [non_standard_handler,
                                        {group, auto_install_standard_error},
                                        {group, auto_install_standard_io}]},
             {auto_install_standard_error, [shuffle], [auto_install]},
             {auto_install_standard_io, [shuffle], [auto_install]}
            ].

init_per_group(auto_install_standard_error, Config) ->
    [{type, standard_error} | Config];
init_per_group(auto_install_standard_io, Config) ->
    [{type, standard_io} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) -> Config.

init_per_testcase(Test, Config) when
      Test =:= auto_install;
      Test =:= non_standard_handler ->
    OldHandlers = logger:get_handler_config(),

    [{old_handlers, OldHandlers} | Config];
init_per_testcase(_, Config) -> Config.

end_per_testcase(Test, Config) when
      Test =:= auto_install;
      Test =:= non_standard_handler ->
    os:unsetenv("JOURNAL_STREAM"),
    OldHandlers = ?config(old_handlers, Config),

    % Cleanup all handlers, so we will remove added ones
    [logger:remove_handler(HId)
     || #{id := HId} <- logger:get_handler_config()],

    % Restore all previous handlers
    [logger:add_handler(HId, HModule, HConfig)
     || #{id := HId, module := HModule} = HConfig <- OldHandlers],

    Config;
end_per_testcase(_, Config) -> Config.

default(_Config) ->
    ok = systemd_kmsg_formatter:check_config(#{}),
    ok = systemd_kmsg_formatter:check_config(#{depth => 2}),
    {error, _} = systemd_kmsg_formatter:check_config(#{unknown => 10}),

    "<0>MESSAGE=foo" = format(emergency, {string, "foo"}, #{}, #{template => ["MESSAGE=", msg]}),
    ok.

custom_parent(_Config) ->
    ok = systemd_kmsg_formatter:check_config(#{parent => logger_formatter}),
    ok = systemd_kmsg_formatter:check_config(#{parent => logger_formatter,
                                          depth => 2}),
    {error, _} = systemd_kmsg_formatter:check_config(#{parent => logger_formatter,
                                                  unknown => 10}),
    ok = systemd_kmsg_formatter:check_config(#{parent => dumb_format}),
    ok = systemd_kmsg_formatter:check_config(#{parent => dumb_format,
                                          unknown => 10}),
    ok.

log_prefix(_Config) ->
    Config0 = #{parent => dumb_formatter},

    "<0>foo" = format(emergency, {string, "foo"}, #{}, Config0),
    "<1>foo" = format(alert    , {string, "foo"}, #{}, Config0),
    "<2>foo" = format(critical , {string, "foo"}, #{}, Config0),
    "<3>foo" = format(error    , {string, "foo"}, #{}, Config0),
    "<4>foo" = format(warning  , {string, "foo"}, #{}, Config0),
    "<5>foo" = format(notice   , {string, "foo"}, #{}, Config0),
    "<6>foo" = format(info     , {string, "foo"}, #{}, Config0),
    "<7>foo" = format(debug    , {string, "foo"}, #{}, Config0),

    "<0>foo" = format(emergency, {string, <<"foo">>}, #{}, Config0),

    Config1 = Config0#{single_line => false},
    "<0>foo\n<0>bar" = format(emergency, {string, "foo\nbar"}, #{}, Config1),
    "<0>foo\n<0>bar" = format(emergency, {string, [<<"foo">>, [$\n, "bar"]]}, #{}, Config1),

    ok.

auto_install(Config) ->
    Type = ?config(type, Config),
    ok = logger:add_handler(Type, logger_std_h, #{
                                       config => #{type => Type},
                                       formatter => {logger_formatter, #{}}
                                      }),
    {ok, OrigHConfig} = logger:get_handler_config(Type),
    #{config := #{type := Type}, formatter := {Parent, _}} = OrigHConfig,
    {ok, #file_info{major_device=Dev, inode=Inode}} = file_info(Type),

    ct:log("Do nothing when there is no JOURNAL_STREAM"),
    ok = systemd_kmsg_formatter:auto_install(),
    ?assertMatch({ok, OrigHConfig}, logger:get_handler_config(Type)),

    ct:log("Do nothing when JOURNAL_STREAM is invalid"),
    os:putenv("JOURNAL_STREAM", ""),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(Type),
    os:putenv("JOURNAL_STREAM", ":"),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(Type),
    os:putenv("JOURNAL_STREAM", "A:B"),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(Type),

    ct:log("Do nothing when ~p uses different device", [Type]),
    set_stream(Dev + 1, Inode),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(Type),

    ct:log("Do nothing when ~p uses different Inode", [Type]),
    set_stream(Dev, Inode + 1),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(Type),

    ct:log("Update stdout logger"),
    set_stream(Dev, Inode),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, #{formatter := {systemd_kmsg_formatter, #{parent := Parent}}}} = logger:get_handler_config(Type),
    % Double call will be NOOP
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, #{formatter := {systemd_kmsg_formatter, #{parent := Parent}}}} = logger:get_handler_config(Type),
    ok.

non_standard_handler(_Config) ->
    {ok, #file_info{major_device=Dev, inode=Inode}} = file_info(standard_io),
    ok = logger:add_handler(non_standard_handler, logger_std_h, #{
                                       config => #{file => "/dev/null"},
                                       formatter => {logger_formatter, #{}}
                                      }),
    {ok, OrigHConfig} = logger:get_handler_config(non_standard_handler),
    set_stream(Dev, Inode),
    ok = systemd_kmsg_formatter:auto_install(),
    {ok, OrigHConfig} = logger:get_handler_config(non_standard_handler),

    ok.


% -----------------------------------------------------------------------------
% Internal

format(Level,Msg,Meta,Config) ->
    format(#{level=>Level,msg=>Msg,meta=>add_time(Meta)},Config).

format(Log,Config) ->
    unicode:characters_to_list(systemd_kmsg_formatter:format(Log,Config)).

add_time(#{time := _}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time => logger:timestamp()}.

set_stream(Dev, Inode) ->
    Data = integer_to_list(Dev) ++ ":" ++ integer_to_list(Inode),
    os:putenv("JOURNAL_STREAM", Data).

file_info(standard_io) ->
    file:read_file_info("/dev/stdout");
file_info(standard_error) ->
    file:read_file_info("/dev/stderr").

-module(systemd_stderr_formatter_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [default, custom_parent, log_prefix].

default(_Config) ->
    ok = systemd_stderr_formatter:check_config(#{}),
    ok = systemd_stderr_formatter:check_config(#{depth => 2}),
    {error, _} = systemd_stderr_formatter:check_config(#{unknown => 10}),

    "<0>MESSAGE=foo" = format(emergency, {string, "foo"}, #{}, #{template => ["MESSAGE=", msg]}),
    ok.

custom_parent(_Config) ->
    ok = systemd_stderr_formatter:check_config(#{parent => logger_formatter}),
    ok = systemd_stderr_formatter:check_config(#{parent => logger_formatter,
                                          depth => 2}),
    {error, _} = systemd_stderr_formatter:check_config(#{parent => logger_formatter,
                                                  unknown => 10}),
    ok = systemd_stderr_formatter:check_config(#{parent => dumb_format}),
    ok = systemd_stderr_formatter:check_config(#{parent => dumb_format,
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

    Config1 = Config0#{single_line => false},
    "<0>foo\n<0>bar" = format(emergency, {string, "foo\nbar"}, #{}, Config1),

    ok.

% -----------------------------------------------------------------------------
% Internal

format(Level,Msg,Meta,Config) ->
    format(#{level=>Level,msg=>Msg,meta=>add_time(Meta)},Config).

format(Log,Config) ->
    unicode:characters_to_list(systemd_stderr_formatter:format(Log,Config)).

add_time(#{time := _}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time => logger:timestamp()}.

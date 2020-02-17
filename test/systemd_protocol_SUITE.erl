-module(systemd_protocol_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [simple].

simple(_Config) ->
    ?assertEqual(<<"FOO=bar\n">>, encode("FOO", "bar")),
    ?assertEqual(<<"FOO\n", 4:64/integer-little, "\nbar\n">>, encode("FOO", "\nbar")).

encode(Name, Data) ->
    iolist_to_binary(systemd_protocol:encode(Name, Data)).

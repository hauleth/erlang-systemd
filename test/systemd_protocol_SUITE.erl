-module(systemd_protocol_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [simple].

simple(_Config) ->
    ?assertEqual(<<"FOO=bar\n">>, encode_field('FOO', [$b, "a", <<"r">>])),
    ?assertEqual(<<"FOO=bar\n">>, encode_field('FOO', "bar")),
    ?assertEqual(<<"FOO=bar\n">>, encode_field(foo, "bar")),
    ?assertEqual(<<"FOO=bar\n">>, encode_field("foo", "bar")),
    ?assertEqual(<<"FOO=bar\n">>, encode_field("FOO", "bar")),
    ?assertEqual(
        <<"FOO\n", 4:64/integer-little, "\nbar\n">>,
        encode_field("FOO", "\nbar")
    ),
    ?assertEqual(
        <<"FOO\n", 8:64/integer-little, "\nbar\nbaz\n">>,
        encode_field("FOO", "\nbar\nbaz")
    ),

    ok.

encode_field(Name, Data) ->
    iolist_to_binary(systemd_protocol:encode_field(Name, Data)).

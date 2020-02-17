-module(systemd_protocol).

-export([encode/2]).

encode(Name, Data) ->
    Content = case string:find(Data, "\n") of
                  nomatch -> [$=, Data];
                  _ ->
                      Len = iolist_size(Data),
                      [$\n, <<Len:64/integer-little>>, Data]
              end,
    [Name, Content, $\n].

%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @hidden
-module(systemd_protocol).

-export([encode/1,
         encode_field/2]).

-type field_name() :: atom() | unicode:chardata().
-type field_data() :: iodata().

%% @doc
%% Encode list of fields
%% @end
encode(Fields) when is_list(Fields) ->
    lists:flatten([encode_field(Name, Data) || {Name, Data} <- Fields]).

%% @doc
%% Encode single field
%% @end
-spec encode_field(Name::field_name(), Data::field_data()) -> erlang:iovec().
encode_field(Name, Data) ->
    Sep = case string:find(Data, "\n") of
                  nomatch -> <<"=">>;
                  _ ->
                      Len = iolist_size(Data),
                      [<<"\n">>, <<Len:64/integer-little>>]
              end,
    [unicode:characters_to_binary(field_name(Name)),
     Sep,
     unicode:characters_to_binary(Data),
     <<"\n">>].

-spec field_name(field_name()) -> unicode:chardata().
field_name(Atom) when is_atom(Atom) ->
    field_name(atom_to_binary(Atom, utf8));
field_name(Name) ->
    string:uppercase(Name).

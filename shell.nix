{ pkgs ? import <nixpkgs> {} }:

with pkgs.beam.packages.erlangR24;

pkgs.mkShell {
  buildInputs = [ erlang elixir rebar3 erlang-ls ];
}

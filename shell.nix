{ pkgs ? import <nixpkgs> {} }:

with pkgs.beam.packages.erlang;

pkgs.mkShell {
  buildInputs = [ erlang elixir rebar3 ];
}

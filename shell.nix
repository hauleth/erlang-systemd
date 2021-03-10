{ pkgs ? import <nixpkgs> {} }:

with pkgs.beam.packages.erlangR23;

pkgs.mkShell {
  buildInputs = [ erlang elixir rebar3 erlang-ls ];

  passthru = {
    inherit erlang-ls;
  };
}

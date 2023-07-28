defmodule PlugSystemdExample.MixProject do
  use Mix.Project

  def project do
    [
      app: :plug_systemd_example,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [
        plug: [
          include_executables_for: [:unix],
          strip_beams: false
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {PlugSystemdExample.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:plug, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:dialyxir, ">= 0.0.0"},
      {:bandit, ">= 0.0.0"},
      {:systemd, path: "../.."}
    ]
  end
end

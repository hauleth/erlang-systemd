defmodule PlugSystemdExample.Application do
  @moduledoc false

  use Application

  require Logger

  def start(_type, _args) do
    Logger.warning("Starting")

    # Replace default logging system with systemd if available
    case :logger.add_handlers(:systemd) do
      :ok ->
        :logger.remove_handler(:default)

      _ ->
        :logger.add_handler_filter(
          :default,
          :elixir_filter,
          {&:logger_filters.domain/2, {:log, :sub, [:elixir]}}
        )

        :ok
    end

    # Setup socket-activation for the Cowboy
    fds = :systemd.listen_fds()

    {port, transport_opts} = socket_opts(fds)
    basic_opts = [scheme: :http, plug: PlugSystemdExample.Router]

    server_env = String.downcase(System.get_env("SERVER", "bandit"))

    Logger.info("Using server #{server_env}")

    server =
      case server_env do
        "cowboy" ->
          [
            {Plug.Cowboy, basic_opts ++ [port: port] ++ transport_opts},
            {Plug.Cowboy.Drainer, refs: :all}
          ]

        "bandit" ->
          [
            {Bandit, basic_opts ++ [options: [port: port, transport_options: transport_opts]]}
          ]
      end

    children =
      server ++
        [
          # Notify systemd that application is ready for work
          :systemd.ready()
        ]

    opts = [strategy: :one_for_one, name: PlugSystemdExample.Supervisor]

    Supervisor.start_link(children, opts)
  end

  # If there is no sockets passed to the application, then start listening on
  # `PORT` environment variable
  defp socket_opts([]) do
    Logger.info("No FDs")

    {String.to_integer(System.get_env("PORT", "5000")), []}
  end

  # If there is any socket passed, then use that one
  defp socket_opts([socket | _]) do
    fd =
      case socket do
        {fd, _name} when is_integer(fd) and fd > 0 -> fd
        fd when is_integer(fd) and fd > 0 -> fd
      end

    Logger.info("Trying to use passed FD #{fd}")

    {0,
     [
       port: 0,
       net: :inet6,
       fd: fd
     ]}
  end
end

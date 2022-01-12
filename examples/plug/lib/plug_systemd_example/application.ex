defmodule PlugSystemdExample.Application do
  @moduledoc false

  use Application

  require Logger

  def start(_type, _args) do
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

    cowboy_opts =
      [
        scheme: :http,
        plug: PlugSystemdExample.Router
      ] ++ socket_opts(fds)

    children = [
      {Plug.Cowboy, cowboy_opts},
      # Notify systemd that application is ready for work
      :systemd.ready(),
      :systemd.set_status(down: [status: "drained"]),
      # # Wait for all connections to end before shutting down
      {Plug.Cowboy.Drainer, refs: :all, shutdown: 10_000},
      :systemd.set_status(down: [status: "draining"])
    ]

    opts = [strategy: :one_for_one, name: PlugSystemdExample.Supervisor]

    Supervisor.start_link(children, opts)
  end

  # If there is no sockets passed to the application, then start listening on
  # `PORT` environment variable
  defp socket_opts([]) do
    [port: String.to_integer(System.get_env("PORT", "5000"))]
  end

  # If there is any socket passed, then use that one
  defp socket_opts([socket | _]) do
    fd =
      case socket do
        {fd, _name} when is_integer(fd) and fd > 0 -> fd
        fd when is_integer(fd) and fd > 0 -> fd
      end

    case :socket.open(fd) do
      {:ok, socket} ->
        Logger.info(:socket.info(socket))

      {:error, reason} ->
        Logger.error(inspect(reason))
    end

    [
      net: :inet6,
      port: 0,
      fd: fd,
      exit_on_close: false
    ]
  end
end

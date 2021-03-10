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
      # Wait for all connections to end before shutting down
      {Plug.Cowboy.Drainer, refs: :all, shutdown: 60_000}
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

    [
      transport_options: [
        socket_opts: [:inet6, fd: fd]
      ],
      port: 0
    ]
  end
end

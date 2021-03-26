defmodule PlugSystemdExample.Router do
  use Plug.Router

  require Logger

  plug(:match)
  plug(:fetch_params)
  plug(Plug.Logger)
  plug(:dispatch)

  # Show PID of the current process
  get "/" do
    send_resp(conn, 200, ["PID: ", :os.getpid(), "\n"])
  end

  # Exit the process with given status code
  get "/exit" do
    status =
      conn.query_params
      |> Map.get("status", "0")
      |> String.to_integer()

    Task.async(fn ->
      Process.sleep(100)
      :init.stop(status)
    end)

    send_resp(conn, 200, "Shutting down whith status #{status}\n")
  end

  # Trigger watchdog
  get "/trigger" do
    Task.async(fn ->
      Process.sleep(100)
      :systemd.watchdog(:trigger)
    end)

    send_resp(conn, 204, "")
  end

  # Set systemd status with given message
  get "/set-status" do
    with %{"status" => status} <- conn.query_params do
      :systemd.notify({:status, status || ""})
    end

    send_resp(conn, 204, "")
  end

  # Simulate reload of the application
  get "/reload" do
    :systemd.notify(:reloading)

    Task.start(fn ->
      Process.sleep(5_000)
      :systemd.notify(:ready)
    end)

    send_resp(conn, 204, "")
  end

  # Slow endpoint
  get "/slow" do
    conn = send_chunked(conn, 200)

    Enum.reduce_while('I am so sloooooow', conn, fn chunk, conn ->
      case chunk(conn, [chunk, "\n"]) do
        {:ok, conn} ->
          Process.sleep(500)
          {:cont, conn}

        {:error, :closed} ->
          {:halt, conn}
      end
    end)
  end

  get _ do
    send_resp(conn, 404, "Not found\n")
  end

  defp fetch_params(conn, _opts), do: Plug.Conn.fetch_query_params(conn)
end

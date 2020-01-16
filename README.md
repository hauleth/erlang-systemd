systemd
=====

[![Hex.pm](https://img.shields.io/hexpm/v/systemd?style=flat-square)](https://hex.pm/packages/systemd)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/hauleth/erlang-systemd/Erlang%20CI?style=flat-square)](https://github.com/hauleth/erlang-systemd/actions)
[![Codecov](https://img.shields.io/codecov/c/gh/hauleth/erlang-systemd?style=flat-square)](https://codecov.io/gh/hauleth/erlang-systemd)

Simple library for notifying systemd about process state.

## Features

- `NOTIFY_SOCKET` is handled and You can notify supervisor about events
  happening in your application.
- Watchdog process will be started automatically if enabled. It will also handle
  sending keep-alive messages automatically.
- File descriptors fetching from the environment.

## Installation

Just add this to your `rebar.config`:

```erlang
{deps, [systemd]}.
```

Or in case of Mix project, to your `mix.exs`:

```elixir
defp deps do
  [
    {:systemd, "~> 0.2"}
  ]
end
```

Then call `systemd:notify(ready)` when your application is ready to work/accept
connections.

### Non-systemd systems

This application and all functions within are safe to call even in non-systemd
and non-Linux OSes. In case if there is no systemd configuration options then
all functions will simply work as (almost) no-ops.

## Usage

Assuming you have `my_app.service` unit like that

```systemd
[Unit]
Description=My Awesome App

[Service]
User=appuser
Group=appgroup
Type=notify
# Important! This need to run in foreground and not fork.
# Check documentation of your release tool.
ExecStart=/path/to/my_app start
WatchdogSec=10s
NotifyAccess=main
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

(the `Type=notify` part is important)

You can inform systemd about state of your application. To do so just call:

```erlang
systemd:notify(ready).
```

This will make `systemctl start my_app.service` to wait until application is up
and running.

If you want to restart your application you can notify systemd about it with:

```erlang
systemd:notify(reloading).
```

Message about application shutting down will be handled automatically for you.

## License

See [LICENSE](LICENSE).

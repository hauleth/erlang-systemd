systemd
=====

[![Hex.pm](https://img.shields.io/hexpm/v/systemd?style=flat-square)](https://hex.pm/packages/systemd)
[![HexDocs](https://img.shields.io/badge/HexDocs-docs-blue?style=flat-square)](https://hexdocs.pm/systemd/)
[![Hex.pm License](https://img.shields.io/hexpm/l/systemd?style=flat-square)](https://tldrlegal.com/license/apache-license-2.0-(apache-2.0))
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/hauleth/erlang-systemd/Erlang%20CI?style=flat-square)](https://github.com/hauleth/erlang-systemd/actions)
[![Codecov](https://img.shields.io/codecov/c/gh/hauleth/erlang-systemd?style=flat-square)](https://codecov.io/gh/hauleth/erlang-systemd)

Simple library for notifying systemd about process state.

## Features

- `NOTIFY_SOCKET` is handled and You can notify supervisor about events
  happening in your application.
- Watchdog process will be started automatically if enabled. It will also handle
  sending keep-alive messages automatically.
- File descriptors fetching from the environment.
- `journal` logger handler and formatters

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

### Logs

To handle logs you have 2 possible options:

- Output data to standard error with special prefixes. This approach is much
  simpler and straightforward, however do not support structured logging and
  multiline messages.
- Use datagram socket with special communication protocol. This requires a
  little bit more effort to set up, but seamlessly supports structured logging
  and multiline messages.

This library supports both formats, and it is up to You which one (or
both?) your app will decide to use.

#### Standard error

There is `systemd_stderr_formatter` which can be used with any logger that
outputs to standard error, for example `logger_std_h`. So to use this format it
is the best to define in your `sys.config`:

```erlang
[
  {kernel,
   [{logger,
     [{handler, default, logger_std_h, #{formatter => {systemd_stderr_formatter, #{}},
                                                       type => standard_error}}]
    }}}
].
```

This will add required prefixes automatically for you.

#### Datagram socket

This one requires `systemd` application to be started to spawn some processes
required for handling sockets, so the best way to handle it is to add predefined
`systemd` handlers after your application starts:

```erlang
logger:add_handlers(systemd).
```

Be aware that this one is **not** guaranteed to work on non-systemd systems, so
if You aren't sure if that application will be ran on systemd-enabled OS then
you shouldn't use it as an only logger solution in your application or you can
end with no logger attached at all.

## License

See [LICENSE](LICENSE).

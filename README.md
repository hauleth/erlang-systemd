systemd
=====

[![Hex.pm](https://img.shields.io/hexpm/v/systemd?style=flat-square)](https://hex.pm/packages/systemd)
[![HexDocs](https://img.shields.io/badge/HexDocs-docs-blue?style=flat-square)](https://hexdocs.pm/systemd/)
[![Hex.pm License](https://img.shields.io/hexpm/l/systemd?style=flat-square)](https://tldrlegal.com/license/apache-license-2.0-(apache-2.0))
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/hauleth/erlang-systemd/Erlang%20CI?style=flat-square)](https://github.com/hauleth/erlang-systemd/actions)
[![Codecov](https://img.shields.io/codecov/c/gh/hauleth/erlang-systemd?style=flat-square)](https://codecov.io/gh/hauleth/erlang-systemd)

Simple library for notifying systemd about process state.

## Features

- `NOTIFY_SOCKET` communication with supervising process.
- Watchdog process will be started automatically (if not disabled). It will also
  handle sending keep-alive messages automatically.
- Fetching file descritors passed by the supervisor.
- `journal` logger handler and formatters.

## Installation

Just add this to your `rebar.config`:

```erlang
{deps, [systemd]}.
```

Or in case of Mix project, to your `mix.exs`:

```elixir
defp deps do
  [
    {:systemd, "~> 0.6"}
  ]
end
```

Then call `systemd:notify(ready)` when your application is ready to work/accept
connections or add `systemd:ready()` as a child of your application's main supervisor.

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
# This will allow using `systemd:notify/1` for informing the system supervisor
# about application status.
Type=notify
# Application need to start in foreground instead of forking into background,
# otherwise it may be not correctly detected and system will try to start it
# again.
ExecStart=/path/to/my_app start
# Enable watchdog process, which will expect messages in given timeframe,
# otherwise it will restart the process as a defunct. It should be managed
# automatically by `systemd` application in most cases and will send messages
# twice as often as requested.
#
# You can force failure by using `systemd:watchdog(trigger)` or manually ping
# systemd watchdog via `systemd:watchdog(ping)`.
WatchdogSec=10s
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

You can inform systemd about state of your application. To do so just call:

```erlang
% Erlang
systemd:notify(ready).
```

```elixir
# Elixir
:systemd.notify(:ready)
```

This will make `systemctl start my_app.service` to wait until application is up
and running.

If you want to restart your application you can notify systemd about it with:

```erlang
% Erlang
systemd:notify(reloading).
```

```elixir
# Elixir
:systemd.notify(:reloading)
```

Message about application shutting down will be handled automatically for you.

For simplification of readiness notification there is `systemd:ready()` function
that returns child specs for temporary process that can be used as a part of
your supervision tree to mark the point when application is ready, ex.:

```erlang
% Erlang
-module(my_app_sup).

-behaviour(supervisor).

-export([start_link/1,
         init/1]).

start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(_Opts) ->
    SupFlags = #{
      strategy => one_for_one
    },
    Children = [
      my_app_db:child_spec(),
      my_app_webserver:child_spec(),
      systemd:ready(),
      my_app_periodic_job:child_spec()
    ],

    {ok, {SupFlags, Children}}.
```

```elixir
# Elixir
defmodule MyProject.Application do
  use Application

  def start(_type, _opts) do
    children = [
      MyProject.Repo,
      MyProjectWeb.Endpoint,
      :systemd.ready() # <- IMPORTANT - this is a function call (it returns the proper child spec)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

### Logs

To handle logs you have 2 possible options:

- Output data to standard output or error with special prefixes. This approach
  is much simpler and straightforward, however do not support structured logging
  and multiline messages.
- Use datagram socket with special communication protocol. This requires a
  little bit more effort to set up, but seamlessly supports structured logging
  and multiline messages.

This library supports both formats, and it is up to You which one (or
both?) your app will decide to use.

#### Erlang

##### Standard error

There is `systemd_kmsg_formatter` which formats data using `kmsg`-like level
prefixes can be used with any logger that outputs to standard output or
standard error if this is attached to the journal. By default `systemd` library
will update all handlers that use `logger_std_h` with type `standard_io` or
`standard_error` that are attached to the journal (it is automatically detected
via `JOURNAL_STREAM` environment variable). You can disable that behaviour by
setting:

```erlang
% Erlang
[
  {systemd, [{auto_formatter, false}]}
].
```

For custom loggers you can use this formatter by adding new option `parent` to
the formatter options that will be used as "upstream" formatter, ex.:

```erlang
logger:add_handler(example_handler, logger_disk_log_h, #{
  formatter => {systemd_kmsg_formatter, #{parent => logger_formatter,
                                          template => [msg]},
  config => #{
    file => "/var/log/my_app.log"
  }
}).
```

##### Datagram socket

This one requires `systemd` application to be started to spawn some processes
required for handling sockets, so the best way to handle it is to add predefined
`systemd` handlers after your application starts:

```erlang
logger:add_handlers(systemd),
logger:remove_handler(default).
```

Be aware that this one is **not** guaranteed to work on non-systemd systems, so
if You aren't sure if that application will be ran on systemd-enabled OS then
you shouldn't use it as an only logger solution in your application or you can
end with no logger attached at all.

This handler **should not** be used with `systemd_kmsg_formatter` as this will
result with pointless `kmsg`-like prefixes in the log messages.

You can also "manually" configgure handler if you want to configure formatter:

```erlang
logger:add_handler(my_handler, systemd_journal_h, #{
  formatter => {my_formatter, FormatterOpts}
}),
logger:remove_handler(default).
```

#### Elixir

This assumes Elixir 1.10+, as earlier versions do not use Erlang's `logger`
module for dispatching logs.

##### Standard error

`systemd` has Erlang's `logger` backend, which mean that you have 2 ways of
achieving what is needed:

1. Disable Elixir's backends and just rely on default Erlang's handler:
  ```elixir
  # config/config.exs
  config :logger,
    backends: [],
    handle_otp_reports: false,
    handle_sasl_reports: false
  ```
  And then allow `systemd` to make its magic that is used in "regular" Erlang
  code.

2. "Manually" add handler that will use `systemd_kmsg_formatter`:
  ```elixir
  # In application start/2 callback
  :ok = :logger.add_handler(
    :my_handler,
    :logger_std_h,
    %{formatter: {:systemd_kmsg_formatter, %{}}}
  )
  Logger.remove_backend(:console)
  ```

However remember, that currently (as Elixir 1.11) there is no "Elixir formatter"
for Erlang's `logger` implementation, so you can end with Erlang-style
formatting of the metadata in the logs.

##### Datagram socket

You can use Erlang-like approach, which is:

```elixir
# In application start/2 callback
:logger.add_handlers(:systemd)
Logger.remove_backend(:console)
```

Or you can manually configure the handler:

```elixir
# In application start/2 callback
:logger.add_handler(
  :my_handler,
  :systemd_journal_h,
  %{formatter: {MyFormatter, formatter_opts}}
)
Logger.remove_backend(:console)
```

Be aware that this one is **not** guaranteed to work on non-systemd systems, so
if You aren't sure if that application will be ran on systemd-enabled OS then
you shouldn't use it as an only logger solution in your application or you can
end with no logger attached at all.

This handler **should not** be used with `:systemd_kmsg_formatter` as this will
result with pointless `kmsg`-like prefixes in the log messages.

## License

See [LICENSE](LICENSE).

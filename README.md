systemd
=====

Simple library for notifying systemd about process state.

## Features

- `NOTIFY_SOCKET` is handled and You can notify supervisor about events
  happening in your application.
- Watchdog process will be started automatically if enabled. It will also handle
  sending keep-alive messages automatically.
- File descriptors fetching from the environment.

## Usage

Assuming you have `my_app.service` unit like that

```
[Unit]
Description=my test app

[Service]
Type=notify
ExecStart=/path/to/my_app start

[Install]
WantedBy=multi-user.target
```

(the `Type=notify` part is important)

You can inform systemd about state of your application. In Erlang apps it can be
pretty useful, as startup can take some time.

So if you add `systemd` as your dependency, and then define:

```erlang
{start_phases, {systemd, []}}
```

Or

```elixir
[
  start_phases: [systemd: []]
]
```

In your application resources file then all notifying thingy will be taken care
for you automatically. Just like that.

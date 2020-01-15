systemd
=====

![Hex.pm](https://img.shields.io/hexpm/v/systemd?style=flat-square)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/hauleth/erlang-systemd/Erlang%20CI)

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

You can inform systemd about state of your application. To do so just call:

```erlang
systemd:notify(ready).
```

After your application is ready. Message about application shutting down will be
handled automatically for you.

## License

See [LICENSE](LICENSE).

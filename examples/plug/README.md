# Plug

This is example of Plug+Cowboy application that integrates with systemd via
[systemd](https://github.com/hauleth/erlang-systemd) library features like:

- Socket activation to listen on privileged port port (80) while VM running
  as unprivileged user `www-data`
- Journal logging, which allows for structured logging and multiline logs
- Watchdog integration which allows for triggering restarts from within
  application
- Status notifications where application reports readiness when it is really
  ready and will inform when the application is in process of shutting down

ASCIICinema recording of shutdown handling:

[![asciicast](https://asciinema.org/a/jqTbUdgFkc7206vFK4AScqq5p.svg)](https://asciinema.org/a/jqTbUdgFkc7206vFK4AScqq5p)

## Installation

This project require Elixir 1.10+ for `logger` integration.

1. Clone repo.
2. Run `make` (you will need to enter `sudo` password for installation of
   systemd units).
3. Application will be available at <http://localhost/>.

## Endpoints

- `/` - return OS PID of VM
- `/exit` - gracefully exit the VM (you can use query param `status` to set exit code)
- `/slow` - response will be sent in chunks that are separated by 1s wait
- `/trigger` - trigger Watchdog
- `/set-status` - set systemd status to message given as `status` query param
- `/reload` - simulate application reload (5s)

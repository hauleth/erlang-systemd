NAME = echo
RUN_COMMAND = foreground

SOCKETS = echo-tcp.socket echo-udp.socket

include ../utils.mk

release:
	rebar3 as prod release

NAME = plug
RUN_COMMAND = start

include ../utils.mk

release:
	MIX_ENV=prod mix do deps.get, release --overwrite

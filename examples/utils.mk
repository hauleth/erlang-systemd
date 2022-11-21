SYSTEMD_TARGET ?= /usr/local/lib/systemd/system
PREFIX ?= /opt

SOCKETS ?= $(NAME).socket

UNITS = $(notdir $(wildcard systemd/*))

PORT ?= 5000

all: install start

start: systemd
	sudo systemctl start ${SOCKETS}

test: release
	systemd-socket-activate -l ${PORT} ${SYSTEMD_SOCKET_OPTS} _build/prod/rel/$(NAME)/bin/$(NAME) $(RUN_COMMAND)

install: systemd install-rel

units:
	echo ${UNITS}

systemd:
	@sudo mkdir -p ${SYSTEMD_TARGET}
	sudo cp -f systemd/* ${SYSTEMD_TARGET}
	@sudo chmod 644 $(addprefix ${SYSTEMD_TARGET}/,${UNITS})
	@sudo systemctl daemon-reload
	sudo systemctl stop ${UNITS}

uninstall:
	-sudo rm $(addprefix ${SYSTEMD_TARGET}/,${UNITS})
	-sudo rm -rf $(PREFIX)/$(NAME)

install-rel: release
	sudo mkdir -p $(PREFIX)/$(NAME)
	sudo cp -fr _build/prod/rel/$(NAME)/* $(PREFIX)/$(NAME)/
	sudo chown -R root:root $(PREFIX)/$(NAME)
	sudo find $(PREFIX)/$(NAME) -executable -exec chmod a+x '{}' +

.PHONY: release systemd uninstall

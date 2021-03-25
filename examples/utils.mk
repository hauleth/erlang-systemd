SYSTEMD_TARGET ?= /usr/local/lib/systemd/system
PREFIX ?= /opt

UNITS = $(notdir $(wildcard systemd/*))

start: install
	sudo systemctl start $(NAME).target

test: release
	systemd-socket-activate -l 8080 _build/prod/rel/$(NAME)/bin/$(NAME) $(RUN_COMMAND)

install: systemd install-rel

units:
	echo ${UNITS}

systemd:
	@sudo mkdir -p ${SYSTEMD_TARGET}
	sudo cp -f systemd/* ${SYSTEMD_TARGET}
	@sudo chmod 644 $(addprefix ${SYSTEMD_TARGET}/,${UNITS})
	@sudo systemctl daemon-reload
	-for unit in ${UNITS}; do sudo systemctl stop $$unit || true; done

uninstall:
	-sudo rm $(addprefix ${SYSTEMD_TARGET}/,${UNITS})
	-sudo rm -rf $(PREFIX)/$(NAME)

install-rel: release
	sudo mkdir -p $(PREFIX)/$(NAME)
	sudo cp -r _build/prod/rel/$(NAME)/* $(PREFIX)/$(NAME)/
	sudo chown -R root:root $(PREFIX)/$(NAME)
	sudo find $(PREFIX)/$(NAME) -executable -exec chmod a+x '{}' +

.PHONY: release systemd uninstall

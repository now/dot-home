host := $(shell hostname)

hostxdgconfighome_DATA = \
	zsh/host/profile \
	zsh/host/rc

$(foreach file,$(hostxdgconfighome_DATA),$(call FILE_template,$(or $(wildcard host/$(host)/$(file)),host/$(file)),$(XDG_CONFIG_HOME)/$(file)))

-include host/$(host)/host.mk

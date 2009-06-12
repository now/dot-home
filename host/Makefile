host := $(shell hostname)

DOTFILES = \
	   zsh/rc/host

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard host/$(host)/$(file)),host/$(file)),$(userconfdir)/.$(file)))

host := $(shell hostname)

DOTFILES = \
	   zsh/host/login \
	   zsh/host/profile \
	   zsh/host/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard host/$(host)/$(file)),host/$(file)),$(userconfdir)/.$(file)))

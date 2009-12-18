host := $(shell hostname)

DOTFILES = \
	   zsh/host/profile \
	   zsh/host/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard host/$(host)/$(file)),host/$(file)),$(userconfdir)/.$(file)))

DOTFILES = \
	   firefox/permissions.sql

$(call SQLITE_IF_EXISTS_template,$(addprefix host/$(host)/,$(DOTFILES)),$(firefoxuserconfdir),,host/$(host)/firefox/)

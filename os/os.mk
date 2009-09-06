DOTFILES = \
	   zsh/os/login \
	   zsh/os/profile \
	   zsh/os/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(userconfdir)/.$(file)))

-include $(uname)/os.mk

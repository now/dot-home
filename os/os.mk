DOTFILES = \
	   zsh/os/env \
	   zsh/os/login \
	   zsh/os/profile \
	   zsh/os/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(userconfdir)/.$(file)))

-include os/$(uname)/os.mk

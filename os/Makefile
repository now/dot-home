DOTFILES = \
	   zsh/login/os \
	   zsh/profile/os \
	   zsh/rc/os

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(userconfdir)/.$(file)))

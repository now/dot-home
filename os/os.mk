DOTFILES = \
	   zsh/os/env \
	   zsh/os/login \
	   zsh/os/profile \
	   zsh/os/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(userconfdir)/.$(file)))

BINFILES = \
	bin/a \
	bin/aa \
	bin/o \
	bin/oo

$(foreach file,$(BINFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(userconfdir)/$(file),755))

-include os/$(uname)/os.mk

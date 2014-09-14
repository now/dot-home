DOTFILES = \
	zsh/os/env \
	zsh/os/login \
	zsh/os/rc

$(foreach file,$(DOTFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(XDG_CONFIG_HOME)/$(file)))

BINFILES = \
	bin/a \
	bin/aa \
	bin/im \
	bin/o \
	bin/oo

$(foreach file,$(BINFILES),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(bindir)/$(file:bin/%=%),755))

-include os/$(uname)/os.mk

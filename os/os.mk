osxdgconfighome_DATA = \
	zsh/os/env \
	zsh/os/login \
	zsh/os/rc

$(foreach file,$(osxdgconfighome_DATA),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(XDG_CONFIG_HOME)/$(file)))

osbin_SCRIPTS = \
	bin/a \
	bin/aa \
	bin/im \
	bin/o \
	bin/oo

$(foreach file,$(osbin_SCRIPTS),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(bindir)/$(file:bin/%=%),755))

-include os/$(uname)/os.mk

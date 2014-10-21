xdgconfighomezshosdir = $(xdgconfighomezshdir)/os
xdgconfighomezshos_DATA = \
	os/zsh/env \
	os/zsh/login \
	os/zsh/rc

osbin_SCRIPTS = \
	bin/a \
	bin/aa \
	bin/im \
	bin/o \
	bin/oo

$(foreach file,$(osbin_SCRIPTS),$(call FILE_template,$(or $(wildcard os/$(uname)/$(file)),os/$(file)),$(bindir)/$(file:bin/%=%),755))

-include os/$(uname)/os.mk

$(call DIR,xdgconfighomezshos)

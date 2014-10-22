xdgconfighomezshosdir = $(xdgconfighomezshdir)/os
xdgconfighomezshos_DATA = \
	os/zsh/env \
	os/zsh/login \
	os/zsh/rc

bin_SCRIPTS = \
	os/bin/a \
	os/bin/aa \
	os/bin/im \
	os/bin/o \
	os/bin/oo

-include os/$(uname)/os.mk

$(call DIR,xdgconfighomezshos)
$(call DIR,bin)

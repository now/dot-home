xdgconfighomezshosdir = $(xdgconfighomezshdir)/os
xdgconfighomezshos_DATA = \
	os/zsh/env \
	os/zsh/login \
	os/zsh/rc

-include $(srcdir)/os/$(uname)/os.mk

$(call DIR,xdgconfighomezshos)

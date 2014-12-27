host := $(shell hostname)

xdgconfighomezshhostdir = $(xdgconfighomezshdir)/host
xdgconfighomezshhost_DATA = \
	host/zsh/profile

-include $(srcdir)/host/$(host)/host.mk

$(call DIR,xdgconfighomezshhost)

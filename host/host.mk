host := $(shell hostname)

xdgconfighomezshhostdir = $(xdgconfighomezshdir)/host
xdgconfighomezshhost_DATA = \
	host/zsh/profile \
	host/zsh/rc

-include host/$(host)/host.mk

$(call DIR,xdgconfighomezshhost)

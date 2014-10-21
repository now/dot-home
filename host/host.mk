host := $(shell hostname)

xdgconfighomezshhostdir = $(xdgconfighomezshdir)/host
xdgconfighomezshhost_DATA = \
	host/zsh/profile

-include host/$(host)/host.mk

$(call DIR,xdgconfighomezshhost)

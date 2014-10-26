userconf_DATA += \
	os/Linux/Xresources

userconf_SCRIPTS += \
	os/Linux/xsession

userconfxmonaddir = $(userconf)/xmonad

userconfxmonad_DATA = \
	os/Linux/xmonad/xmonad.hs

HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
LDLIBS = -lX11
bin_PROGRAMS += \
	os/Linux/bin/xdigraph
endif

$(call DIR,userconfxmonad)

userconf_DATA += \
	os/Linux/Xresources

userconf_SCRIPTS += \
	os/Linux/xsession

userconfxmonaddir = $(userconf)/xmonad

userconfxmonad_DATA = \
	os/Linux/xmonad/xmonad.hs

HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
oslinuxbin_programs = \
	os/Linux/bin/xdigraph

$(oslinuxbin_programs): os/Linux/bin/.dirstamp

LDLIBS = -lX11

bin_PROGRAMS += \
	$(oslinuxbin_programs)
endif

$(call DIR,userconfxmonad)

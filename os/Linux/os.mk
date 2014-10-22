userconf_DATA += \
	os/Linux/Xresources

userconfxmonaddir = $(userconf)/xmonad
userconfxmonad_DATA = \
	os/Linux/xmonad/xmonad.hs

$(call DIR,userconfxmonad)

userconf_SCRIPTS = \
	os/Linux/xsession

$(call GROUP_template,$(userconf_SCRIPTS),$(userconfdir),.,os/Linux/,755)

HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
LDLIBS = -lX11
userbin_PROGRAMS = \
	os/Linux/bin/xdigraph

$(call GROUP_template,$(userbin_PROGRAMS),$(bindir),,os/Linux/bin/,755)
endif

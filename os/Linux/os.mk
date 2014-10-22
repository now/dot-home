userconf_DATA = \
	os/Linux/Xresources \
	os/Linux/xmonad/xmonad.hs

$(call GROUP_template,$(userconf_DATA),$(userconfdir),.os/Linux/)

userconf_SCRIPTS = \
	os/Linux/xsession

bin_substitutables += \
	os/Linux/xsession

$(call GROUP_template,$(userconf_SCRIPTS),$(userconfdir),.,os/Linux/,755)

HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
LDLIBS = -lX11
userbin_PROGRAMS = \
	os/Linux/bin/xdigraph

$(call GROUP_template,$(userbin_PROGRAMS),$(bindir),,os/Linux/bin/,755)
endif

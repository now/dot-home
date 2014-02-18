DOTFILES = \
	   os/Linux/Xresources \
	   os/Linux/xmonad/xmonad.hs

$(call GROUP_template,$(DOTFILES),$(userconfdir),.os/Linux/)

BINFILES = \
	   os/Linux/xsession

bin_substitutables += $(BINFILES)

$(call GROUP_template,$(BINFILES),$(userconfdir),.,os/Linux/,755)

HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
LDLIBS = -lX11
BINFILES = \
	   os/Linux/bin/xdigraph

$(call GROUP_template,$(BINFILES),$(bindir),,os/Linux/bin/,755)
endif

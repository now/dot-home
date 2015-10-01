userconf_DATA += \
	os/Linux/Xresources

userconf_SCRIPTS += \
	os/Linux/xsession

os/Linux/xsession: os/Linux/.dirstamp

userconfxmonaddir = $(userconfdir)/xmonad

userconfxmonad_DATA = \
	os/Linux/xmonad/xmonad.hs

$(call DIR,userconfxmonad)

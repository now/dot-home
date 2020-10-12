userconfxmonaddir = $(userconfdir)/.xmonad

userconf_DATA += \
	os/Linux/Xresources

userconf_SCRIPTS += \
	os/Linux/xsession

userconfxmonad_DATA = \
	os/Linux/xmonad/xmonad.hs

xdgconfighomezsh_DATA += \
	os/Linux/zsh/zprofile

os/Linux/xsession: os/Linux/.dirstamp

os/Linux/zsh/zprofile: \
	environment.xml \
	os/Linux/zsh/zprofile.xsl \
	os/Linux/zsh/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  $(srcdir)/os/Linux/zsh/zprofile.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@

$(call DIR,userconfxmonad)

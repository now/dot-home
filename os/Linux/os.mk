HAVE_X11 := $(realpath /usr/X11)

ifneq ($(HAVE_X11),)
LDLIBS = -lX11
BINFILES = \
	os/Linux/bin/xdigraph

$(call GROUP_template,$(BINFILES),~,,os/Linux,755)
endif

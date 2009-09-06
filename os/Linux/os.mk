LDLIBS = -lX11
BINFILES = \
	os/Linux/bin/xdigraph

$(call GROUP_template,$(BINFILES),~,,os/Linux,755)

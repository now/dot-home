firefoxprofilesdir=$(appdatadir)/Mozilla/Firefox
DOTFILES = \
	   os/Cygwin/firefox/profiles.ini

$(call GROUP_template,$(DOTFILES),$(firefoxprofilesdir),,os/Cygwin/firefox/)

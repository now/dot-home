firefoxprofilesdir=$(call shell_quote,$(shell cygpath -u "$(APPDATA)")/Mozilla/Firefox)
DOTFILES = \
	   os/Cygwin/firefox/profiles.ini

$(call GROUP_template,$(DOTFILES),$(firefoxprofilesdir),,os/Cygwin/firefox/)

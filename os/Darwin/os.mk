DOTFILES = \
	   os/Darwin/Library/LaunchAgents/se.disu.org.sync.plist

LAUNCHCTL = launchctl

# 1: File
# 2: Target
define LAUNCHCTL_template_file
$(call GROUP_template_diff_file,$(1),$(2))

install: $(2)
$(2): $(1)
	$$(INSTALL) -D --mode=644 --preserve-timestamps $$< $$(call shell_quote,$$@)
	$$(LAUNCHCTL) unload $(2)
	$$(LAUNCHCTL) load $(2)

endef

# 1: Files
# 2: Parent directory
# 4: Prefix to strip
define LAUNCHCTL_template
$(eval $(foreach file,$(1),$(call LAUNCHCTL_template_file,$(file),$(2)/$(file:$(3)%=%))))
endef

$(call LAUNCHCTL_template,$(DOTFILES),$(userconfdir),os/Darwin/)

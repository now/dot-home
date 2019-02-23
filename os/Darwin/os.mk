librarydir = $(userconfdir)/Library
librarylaunchagentsdir = $(librarydir)/LaunchAgents

librarylaunchagents_DATA = \
	   os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist

$(call DIR,librarylaunchagents,,\
	$$(V_LAUNCHCTL)$$(LAUNCHCTL) unload $$@; $$(LAUNCHCTL) load $$@)

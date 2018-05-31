librarydir = $(userconfdir)/Library
librarylaunchagentsdir = $(librarydir)/LaunchAgents

librarylaunchagents_DATA = \
	   os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist \
	   os/Darwin/Library/LaunchAgents/se.disu.mbsync.daily.plist \
	   os/Darwin/Library/LaunchAgents/se.disu.mbsync.quarterly.plist \
	   os/Darwin/Library/LaunchAgents/se.disu.mbsync.hourly.plist

$(call DIR,librarylaunchagents,,\
	$$(V_LAUNCHCTL)$$(LAUNCHCTL) unload $$@; $$(LAUNCHCTL) load $$@)

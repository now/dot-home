appdir = $(prefix)/Applications
appNowmacsappContentsdir = $(appdir)/Nowmacs.app/Contents
appNowmacsappContentsMacOSdir = $(appNowmacsappContentsdir)/MacOS
appNowmacsappContentsResourcesdir = $(appNowmacsappContentsdir)/Resources
appNowmacsappContentsResourcesScriptsdir = $(appNowmacsappContentsResourcesdir)/Scripts
appsupportdir = $(prefix)/Library/Application\ Support
fontsdir = $(prefix)/Library/Fonts
librarydir = $(userconfdir)/Library
librarylaunchagentsdir = $(librarydir)/LaunchAgents
userconfaudacitydir = $(appsupportdir)/audacity
userconfmixxxdir = $(appsupportdir)/Mixxx
userconfmozillafirefoxdir = $(appsupportdir)/Firefox
xdgconfighomevlcdir = $(prefix)/Library/Preferences/org.videolan.vlc

librarylaunchagents_DATA = \
	   os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist

appNowmacsappContents_DATA = \
	os/Darwin/bin/Nowmacs.app/Contents/Info.plist \
	os/Darwin/bin/Nowmacs.app/Contents/PkgInfo

appNowmacsappContentsMacOS_SCRIPTS = \
	os/Darwin/bin/Nowmacs.app/Contents/MacOS/droplet

appNowmacsappContentsResources_DATA = \
	os/Darwin/bin/Nowmacs.app/Contents/Resources/droplet.icns \
	os/Darwin/bin/Nowmacs.app/Contents/Resources/droplet.rsrc

appNowmacsappContentsResourcesScripts_DATA = \
	os/Darwin/bin/Nowmacs.app/Contents/Resources/Scripts/main.scpt

$(appNowmacsappContents_DATA) \
$(appNowmacsappContentsMacOS_SCRIPTS) \
$(appNowmacsappContentsResources_DATA) \
$(appNowmacsappContentsResourcesScripts_DATA): os/Darwin/bin/Nowmacs.app

os/Darwin/bin/Nowmacs.app: os/Darwin/bin/Nowmacs.scpt os/Darwin/bin/.dirstamp
	$(V_GEN)osacompile -o $@ $<
	$(V_at)cp $(srcdir)/os/Darwin/data/Emacs.icns \
	  os/Darwin/bin/Nowmacs.app/Contents/Resources/droplet.icns
	$(V_at)touch $@ \
	  $(appNowmacsappContents_DATA) \
	  $(appNowmacsappContentsMacOS_SCRIPTS) \
	  $(appNowmacsappContentsResources_DATA) \
	  $(appNowmacsappContentsResourcesScripts_DATA)

fonts_DATA = \
	$(fontsdejavu_DATA)

librarylaunchagents_DATA = \
	   os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist \
	   os/Darwin/Library/LaunchAgents/se.disu.socat.plist

os/Darwin/Library/LaunchAgents/se.disu.socat.plist: os/Darwin/Library/Launchagents/.dirstamp

$(call DIR,appNowmacsappContents)
$(call DIR,appNowmacsappContentsMacOS)
$(call DIR,appNowmacsappContentsResources)
$(call DIR,appNowmacsappContentsResourcesScripts)
$(call DIR,fonts)
$(call DIR,librarylaunchagents,,\
	$$(V_LAUNCHCTL)$$(LAUNCHCTL) unload $$@; $$(LAUNCHCTL) load $$@)

appsupportdir = $(prefix)/Library/Application\ Support
fontsdir = $(prefix)/Library/Fonts
librarydir = $(userconfdir)/Library
librarylaunchagentsdir = $(librarydir)/LaunchAgents
userconfaudacitydir = $(appsupportdir)/audacity
userconfmixxxdir = $(appsupportdir)/Mixxx
userconfmozillafirefoxdir = $(appsupportdir)/Firefox
xdgconfighomevlcdir = $(prefix)/Library/Preferences/org.videolan.vlc

ELC_LOADPATH = -L emacs.d/site-lisp

fonts_DATA = \
	$(fontsdejavu_DATA)

librarylaunchagents_DATA = \
	os/Darwin/Library/LaunchAgents/se.disu.environment.editor.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.lang.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manpager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manwidth.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.pager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist \
	os/Darwin/Library/LaunchAgents/se.disu.socat.plist

sitelisp_elcs += \
	emacs.d/site-lisp/now-path.elc

emacs.d/init.elc: | emacs.d/site-lisp/now-path.el

emacs.d/site-lisp/now-path.el: \
	environment.xml \
	os/Darwin/emacs.d/site-lisp/now-path.el.xsl \
	os/Darwin/emacs.d/site-lisp/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  --stringparam home "$(prefix)" \
	  --stringparam path "$(PATH)" \
	  $(srcdir)/os/Darwin/emacs.d/site-lisp/now-path.el.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@


os/Darwin/Library/LaunchAgents/se.disu.environment.editor.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.lang.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.manpager.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.manwidth.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.pager.plist: \
	environment.xml \
	os/Darwin/Library/LaunchAgents/se.disu.environment.plist.xsl \
	os/Darwin/Library/LaunchAgents/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  --stringparam label "$(@F)" \
	  --stringparam name "$(@F:se.disu.environment.%.plist=%)" \
	  --stringparam current-value "$($(@F:se.disu.environment.%.plist=%))" \
	  $(srcdir)/os/Darwin/Library/LaunchAgents/se.disu.environment.plist.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@

os/Darwin/Library/LaunchAgents/se.disu.socat.plist: \
	os/Darwin/Library/Launchagents/.dirstamp

$(call DIR,fonts)
$(call DIR,librarylaunchagents,,\
	$$(V_LAUNCHCTL)$$(LAUNCHCTL) unload $$@; $$(LAUNCHCTL) load $$@)

# sudo launchctl config user path '~/opt/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin:/opt/local/sbin:/Library/Apple/usr/bin'


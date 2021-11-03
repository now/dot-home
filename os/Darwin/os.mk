appsupportdir = $(prefix)/Library/Application\ Support
fontsdir = $(prefix)/Library/Fonts
librarydir = $(userconfdir)/Library
librarylaunchagentsdir = $(librarydir)/LaunchAgents
userconfaudacitydir = $(appsupportdir)/audacity
userconfmixxxdir = $(appsupportdir)/Mixxx
userconfmozillafirefoxdir = $(appsupportdir)/Firefox
xdgconfighomevlcdir = $(prefix)/Library/Preferences/org.videolan.vlc

ELC_LOADPATH = -L emacs/site-lisp

fonts_DATA = \
	$(fontsdejavu_DATA) \
	$(fontsfiracode_DATA)

librarylaunchagents_DATA = \
	os/Darwin/Library/LaunchAgents/se.disu.environment.editor.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.gobin.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.gomodcache.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.lang.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manpager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manwidth.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.pager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist \
	os/Darwin/Library/LaunchAgents/se.disu.socat.plist

sitelisp_elcs += \
	emacs/site-lisp/now-path.elc

emacs/init.elc: | emacs/site-lisp/now-path.el

emacs/site-lisp/now-path.el: \
	environment.xml \
	os/Darwin/emacs/site-lisp/now-path.el.xsl \
	os/Darwin/emacs/site-lisp/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  --stringparam home "$(prefix)" \
	  --stringparam path "$(PATH)" \
	  $(srcdir)/os/Darwin/emacs/site-lisp/now-path.el.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@


os/Darwin/Library/LaunchAgents/se.disu.environment.editor.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.gobin.plist \
os/Darwin/Library/LaunchAgents/se.disu.environment.gomodcache.plist \
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

macports_PACKAGES = \
	autoconf \
	automake \
	bison \
	borgbackup \
	clojure \
	coreutils \
	curl \
	diffutils \
	emacs-app-devel \
	exif \
	findutils \
	flac \
	gawk \
	git \
	gmake \
	gnupg2 \
	go \
	gopls \
	grep \
	gsed \
	hunspell \
	iperf3 \
	jq \
	libxml2 \
	libxslt \
	miller \
	parallel \
	py39-awscli \
	rsync \
	socat \
	sqlite3 \
	vorbis-tools \
	wget \
	yarn \
	zsh

macports/$(macports_PACKAGES): macports/.dirstamp
	$(V_PORT)$(PORT) -q installed $(@F) | fgrep '  $(@F)' > /dev/null || \
	  $(SUDO) $(PORT) -N $(if $(V_PORT),-q) install $(@F)
	$(V_at): > $@.tmp
	$(V_at)mv $@.tmp $@

install: macports/$(macports_PACKAGES)

# sudo launchctl config user path '~/opt/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin:/opt/local/sbin:/Library/Apple/usr/bin'

fix-keybindings:
	map=`printf '30064771%s 30064771%s\n' 129 299 296 299 299 296 | \
	    sed -e 's,\([^ ]*\) \([^ ]*\),<dict><key>HIDKeyboardModifierMappingDst</key><integer>\2</integer><key>HIDKeyboardModifierMappingSrc</key><integer>\1</integer></dict>,'` && \
	  for m in `defaults -currentHost read | \
	      sed -n 's/ *"com\.apple\.keyboard\.modifiermapping\.\(.*\)" = .*/\1/p'`; do \
	    defaults -currentHost write -g "com.apple.keyboard.modifiermapping.$$m" -array $$map || exit 1; \
	  done

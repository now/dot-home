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
	os/Darwin/Library/LaunchAgents/se.disu.environment.aws_shared_credentials_file.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.editor.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.gobin.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.gomodcache.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.lang.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manpager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.manwidth.plist \
	os/Darwin/Library/LaunchAgents/se.disu.environment.pager.plist \
	os/Darwin/Library/LaunchAgents/se.disu.kinesis.plist \
	os/Darwin/Library/LaunchAgents/se.disu.socat.plist

paths_DATA = \
	$(sysconfdir)/paths

sitelisp_elcs += \
	emacs/site-lisp/now-path.elc

sitelispterm_elcs += \
	emacs/site-lisp/term/now-ns-win.elc

xdgconfighomezsh_DATA += \
	os/Darwin/zsh/zprofile

emacs/init.elc: | emacs/site-lisp/now-path.el

emacs/site-lisp/now-path.el: \
	environment.xml \
	$(paths_DATA) \
	os/Darwin/emacs/site-lisp/now-path.el.xsl \
	os/Darwin/emacs/site-lisp/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  --xinclude \
	  --stringparam home "$(prefix)" \
	  --stringparam path "`cat $(paths_DATA) | tr \\\\n : | sed 's/:$$//'`" \
	  $(srcdir)/os/Darwin/emacs/site-lisp/now-path.el.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@

os/Darwin/zsh/zprofile: \
	environment.xml \
	os/Darwin/zsh/zprofile.xsl \
	os/Darwin/zsh/.dirstamp \
	os/Linux/zsh/zprofile.xsl
	$(V_XSLTPROC)$(XSLTPROC) \
	  --xinclude \
	  $(srcdir)/os/Darwin/zsh/zprofile.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@

os/Darwin/Library/LaunchAgents/se.disu.environment.%.plist: \
	environment.xml \
	Makefile \
	os/Darwin/Library/LaunchAgents/se.disu.environment.plist.xsl \
	os/Darwin/Library/LaunchAgents/.dirstamp
	$(V_XSLTPROC)$(XSLTPROC) \
	  --xinclude \
	  --stringparam label "$(@F)" \
	  --stringparam name "$(@F:se.disu.environment.%.plist=%)" \
	  --stringparam current-value "$($(@F:se.disu.environment.%.plist=%))" \
	  $(srcdir)/os/Darwin/Library/LaunchAgents/se.disu.environment.plist.xsl \
	  $< > $@.tmp
	$(V_at)mv $@.tmp $@

os/Darwin/Library/LaunchAgents/se.disu.socat.plist: os/Darwin/Library/LaunchAgents/.dirstamp

$(call DIR,fonts)

macports_PACKAGES = \
	autoconf \
	autoconf-archive \
	automake \
	bison \
	clojure \
	coreutils \
	emacs-app-devel \
	exif \
	flac \
	gmake \
	gnupg2 \
	go \
	gopls \
	grep \
	hunspell \
	iperf3 \
	jdk17 \
	libxml2 \
	libxslt \
	pkgconfig \
	rsync \
	socat \
	sqlite3 \
	vorbis-tools

$(addprefix macports/,$(macports_PACKAGES)): macports/.dirstamp
	$(V_PORT)$(PORT) -q installed $(@F) | fgrep '  $(@F)' > /dev/null || \
	  $(SUDO) $(PORT) -N $(if $(V_PORT),-q) install $(@F) $($(@F)_VARIANT)
	$(V_at)$(SUDO) $(PORT) setrequested $(@F)
	$(V_at): > $@.tmp
	$(V_at)mv $@.tmp $@

install: $(addprefix macports/,$(macports_PACKAGES))

# sudo launchctl config user path '~/opt/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin'

fix-keybindings:
	map=`printf '30064771%s 30064771%s\n' 129 299 296 299 299 296 | \
	    sed -e 's,\([^ ]*\) \([^ ]*\),<dict><key>HIDKeyboardModifierMappingDst</key><integer>\2</integer><key>HIDKeyboardModifierMappingSrc</key><integer>\1</integer></dict>,'` && \
	  for m in `defaults -currentHost read | \
	      sed -n 's/ *"com\.apple\.keyboard\.modifiermapping\.\(.*\)" = .*/\1/p'`; do \
	    defaults -currentHost write -g "com.apple.keyboard.modifiermapping.$$m" -array $$map || exit 1; \
	  done

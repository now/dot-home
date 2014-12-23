uname := $(patsubst CYGWIN_%,Cygwin,$(shell uname -s))

CURL = curl
DIFF = diff
EMACS = emacs
ICONV = iconv
INSTALL = install
PATCH = patch
UNZIP = unzip
ZSHELL = /bin/zsh

empty :=
space := $(empty) $(empty)
shell_quote = $(subst $(space),\ ,$(1))

DEFAULT_VERBOSITY = 0

V_at = $(_v_at_$(V))
_v_at_ = $(_v_at_$(DEFAULT_VERBOSITY))
_v_at_0 = @
_v_at_1 =

V_GEN = $(V_GEN_$(V))
V_GEN_ = $(V_GEN_$(DEFAULT_VERBOSITY))
V_GEN_0 = @echo "  GEN     " $@;

V_CURL = $(V_CURL_$(V))
V_CURL = $(V_CURL_$(DEFAULT_VERBOSITY))
V_CURL_0 = @echo "  CURL    " $@;

V_PATCH = $(V_PATCH_$(V))
V_PATCH = $(V_PATCH_$(DEFAULT_VERBOSITY))
V_PATCH_0 = @echo "  PATCH   " $@;

V_INSTALL = $(V_INSTALL_$(V))
V_INSTALL_ = $(V_INSTALL_$(DEFAULT_VERBOSITY))
V_INSTALL_0 = @echo "  INSTALL " $@;

V_ELC = $(V_ELC_$(V))
V_ELC_ = $(V_ELC_$(DEFAULT_VERBOSITY))
V_ELC_0 = @echo "  ELC     " $@;

prefix = ~
bindir = $(prefix)/opt/bin
sharedir = $(prefix)/opt/share
userconfdir = $(prefix)
XDG_CONFIG_HOME ?= $(prefix)/.config
userconfaudacitydir = $(userconfdir)/.audacity
userconfemacsddir = $(userconfdir)/.emacs.d
userconfemacsddelayedinitsdir = $(userconfemacsddir)/delayed-inits
userconfemacsdetcschemadir = $(userconfemacsddir)/etc/schema
userconfemacsdinitsdir = $(userconfemacsddir)/inits
userconfemacsdsitelispdir = $(userconfemacsddir)/site-lisp
userconfmozillafirefoxdir = $(userconfdir)/.mozilla/firefox
userconfmozillafirefoxprofilesdefaultdir = $(userconfmozillafirefoxdir)/Profiles/default
userconfopenofficeorg3userwordbookdir = $(userconfdir)/.openoffice.org/3/user/wordbook
xdgconfighomedir = $(XDG_CONFIG_HOME)
xdgconfighomefontconfigdir = $(xdgconfighomedir)/fontconfig
xdgconfighomegitdir = $(xdgconfighomedir)/git
xdgconfighomevlcdir = $(xdgconfighomedir)/vlc
xdgconfighomezshdir = $(xdgconfighomedir)/zsh
xdgconfighomezshfunctionsdir = $(xdgconfighomezshdir)/functions
sysconfdir = /etc

-include Config/$(uname)
-include config.mk

.PHONY: all diff install

all: diff

# patch, target
define PATCH_template_file
.PHONY: $(2:.patch=)
$(2:.patch=):
	$$(V_PATCH)if $$(PATCH) -Nsg 0 --dry-run $$@ $(1) > /dev/null; then \
	  $$(PATCH) -Nsg 0 $$@ $(1); \
	fi

endef

# patches, parent-directory, prefix, prefix-to-strip
PATCH_template = $(eval $(foreach file,$(1),$(call PATCH_template_file,$(file),$(2)/$(3)$(file:$(4)%=%))))

INSTALLFLAGS = -D --preserve-timestamps
INSTALL_DATA = $(INSTALL) $(INSTALLFLAGS) -m 0644
INSTALL_PROGRAMS = $(INSTALL) $(INSTALLFLAGS)
INSTALL_SCRIPTS = $(INSTALL_PROGRAMS)

# file, target, primary
define DIR_primary
.PHONY diff: $(2).diff
$(2).diff:
	@$$(DIFF) -u $(2) $(1) || true

install: $(2)

$(2): $(1)
	$$(V_INSTALL)$$(INSTALL_$(3)) $$< $$(call shell_quote,$$@)

endef

# dir, prefix?
DIR = $(eval $(foreach primary,DATA SCRIPTS,$(foreach file,$($(1)_$(primary)),$(if $(subst $(file),,$(lastword $(filter %/$(notdir $(file)),$($(1)_$(primary))))),,$(call DIR_primary,$(file),$($(1)dir)/$(2)$(notdir $(file)),$(primary))))))

%.elc: %.el emacs.d/site-lisp/userloaddefs.el emacs.d/inits/package.el
	$(V_ELC)$(EMACS) --batch -Q -L emacs.d/site-lisp \
	  -l emacs.d/site-lisp/userloaddefs.el -l emacs.d/inits/package.el \
	  $(ELCFLAGS) -f batch-byte-compile $<

HUNSPELL_DICT_VERSION = 2014.08.11
HUNSPELL_EN_GB_DICT_ZIP = hunspell/hunspell-en_GB-ise-$(HUNSPELL_DICT_VERSION).zip
HUNSPELL_EN_US_DICT_ZIP = hunspell/hunspell-en_US-$(HUNSPELL_DICT_VERSION).zip

$(HUNSPELL_EN_GB_DICT_ZIP) $(HUNSPELL_EN_US_DICT_ZIP):
	$(V_CURL)$(CURL) -Ls http://downloads.sourceforge.net/wordlist/$(@F) > $@

hunspell/%.aff: hunspell/hunspell-%-$(HUNSPELL_DICT_VERSION).zip
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)mv $@.tmp $@
	$(V_at)$(PATCH) -sp0 $@ < $@.patch

hunspell/%.dic: hunspell/hunspell-%-$(HUNSPELL_DICT_VERSION).zip
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)sed -e "s/'/’/g" $@.tmp > $@.tmp.tmp
	$(V_at)mv $@.tmp.tmp $@.tmp
	$(V_at)mv $@.tmp $@

HUNSPELL_SV_DICT_ZIP = hunspell/addon-474623-latest.xpi

$(HUNSPELL_SV_DICT_ZIP):
	$(V_CURL)$(CURL) -Ls https://addons.mozilla.org/firefox/downloads/latest/474623/$(@F) > $@

hunspell/sv.aff: $(HUNSPELL_SV_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qojd $(@D) $< dictionaries/$(@F)
	$(V_at)touch $@

hunspell/sv.dic: $(HUNSPELL_SV_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qojd $(@D) $< dictionaries/$(@F)
	$(V_at)sed -e "s/'/’/g" $@ > $@.tmp
	$(V_at)mv $@.tmp $@

userconf_DATA = \
	editrc \
	gemrc \
	gtkrc-2.0 \
	hunspell_en_US \
	indent.pro \
	inputrc \
	mailcap \
	zshenv

userconfaudacity_DATA = \
	audacity.cfg

userconfemacsd_DATA = \
	emacs.d/init.elc \
	emacs.d/now-theme.elc

provided_elcs = \
	emacs.d/delayed-inits/calc.elc \
	emacs.d/delayed-inits/calendar.elc \
	emacs.d/delayed-inits/cc-mode.elc \
	emacs.d/delayed-inits/compile.elc \
	emacs.d/delayed-inits/css-mode.elc \
	emacs.d/delayed-inits/desktop.elc \
	emacs.d/delayed-inits/diff.elc \
	emacs.d/delayed-inits/dired.elc \
	emacs.d/delayed-inits/dired-aux.elc \
	emacs.d/delayed-inits/evil.elc \
	emacs.d/delayed-inits/flx-ido.elc \
	emacs.d/delayed-inits/grep.elc \
	emacs.d/delayed-inits/hideshow.elc \
	emacs.d/delayed-inits/holidays.elc \
	emacs.d/delayed-inits/ido.elc \
	emacs.d/delayed-inits/ispell.elc \
	emacs.d/delayed-inits/lisp-mode.elc \
	emacs.d/delayed-inits/magit.elc \
	emacs.d/delayed-inits/make-mode.elc \
	emacs.d/delayed-inits/man.elc \
	emacs.d/delayed-inits/nxml-mode.elc \
	emacs.d/delayed-inits/org-agenda.elc \
	emacs.d/delayed-inits/org-capture.elc \
	emacs.d/delayed-inits/org-clock.elc \
	emacs.d/delayed-inits/org-colview.elc \
	emacs.d/delayed-inits/org-faces.elc \
	emacs.d/delayed-inits/org-id.elc \
	emacs.d/delayed-inits/org-mobile.elc \
	emacs.d/delayed-inits/org.elc \
	emacs.d/delayed-inits/recentf.elc \
	emacs.d/delayed-inits/rng-loc.elc \
	emacs.d/delayed-inits/ruby-mode.elc \
	emacs.d/delayed-inits/scroll-bar.elc \
	emacs.d/delayed-inits/sh-script.elc \
	emacs.d/delayed-inits/solar.elc \
	emacs.d/delayed-inits/tabulated-list.elc \
	emacs.d/delayed-inits/tramp-sh.elc

unprovided_elcs = \
	emacs.d/delayed-inits/buff-menu.elc

userconfemacsddelayedinits_DATA = \
	$(provided_elcs) \
	$(unprovided_elcs)

userconfemacsdetcschema_DATA = \
	emacs.d/etc/schema/catalog.rnc \
	emacs.d/etc/schema/gtk-builder.rnc \
	emacs.d/etc/schema/PropertyList-1.0.rnc \
	emacs.d/etc/schema/schemas.xml

userconfemacsdinits_DATA = \
	emacs.d/inits/package.elc \
	emacs.d/inits/provided-delayed-inits.elc \
	emacs.d/inits/unprovided-delayed-inits.elc \

sitelisp_elcs = \
	emacs.d/site-lisp/buff-menu-ext.elc \
	emacs.d/site-lisp/evil-paredit.elc \
	emacs.d/site-lisp/hide-mode-line.elc \
	emacs.d/site-lisp/ned-info-on-file.elc \
	emacs.d/site-lisp/now-org.elc \
	emacs.d/site-lisp/project.elc \
	emacs.d/site-lisp/rnc-mode.elc

userconfemacsdsitelisp_DATA = \
	$(sitelisp_elcs) \
	emacs.d/site-lisp/userloaddefs.el

userconfmozillafirefox_DATA = \
	firefox/profiles.ini

userconfmozillafirefoxprofilesdefault_DATA = \
	firefox/user.js

userconfopenofficeorg3userwordbook_DATA = \
	hunspell/en_GB-ise.aff \
	hunspell/en_GB-ise.dic \
	hunspell/en_US.aff \
	hunspell/en_US.dic \
	hunspell/sv.aff \
	hunspell/sv.dic

xdgconfighome_DATA = \
	dircolors

xdgconfighomefontconfig_DATA = \
	fonts.conf

xdgconfighomegit_DATA = \
	git/config

xdgconfighomevlc_DATA = \
	vlcrc

xdgconfighomezsh_DATA = \
	zsh/zlogin \
	zsh/zprofile \
	zsh/zshrc

xdgconfighomezshfunctions_DATA = \
	zsh/functions/_unpack \
	zsh/functions/_up \
	zsh/functions/freload \
	zsh/functions/hc \
	zsh/functions/up \
	zsh/functions/urlify \
	zsh/functions/dynamic-directory-names \
	zsh/functions/cache-invalid \
	zsh/functions/cache-path \
	zsh/functions/cache-retrieve \
	zsh/functions/cache-store \
	zsh/functions/zle/cd-to-alternate-directory \
	zsh/functions/zle/foreground-or-list-choices \
	zsh/functions/zle/self-insert-redir \
	zsh/functions/zle/sudo-command-line \
	zsh/functions/zle/up-directory \
	zsh/functions/zle/urlify-current-argument \
	zsh/functions/zle/vi-cmd-mode-silently \
	zsh/functions/zle/yank-clipboard

bin_SCRIPTS = \
	bin/a \
	bin/aa \
	bin/asciitable \
	bin/burn \
	bin/clipboard \
	bin/create-key \
	bin/dfs \
	bin/discogs-tags \
	bin/duh \
	bin/e \
	bin/emv \
	bin/g \
	bin/im \
	bin/index-disc \
	bin/m \
	bin/mfedit \
	bin/mov-re-encode \
	bin/o \
	bin/oo \
	bin/pack \
	bin/terminal-colors \
	bin/unpack \
	bin/valgrind-ruby

include os/os.mk
include host/host.mk

ifdef INCLUDE_VIM
userconf_DATA += \
	vim/after/ftplugin/sh.vim \
	vim/after/ftplugin/vim.vim \
	vim/after/ftplugin/zsh.vim \
	vim/after/syntax/vim.vim \
	vim/colors/now.vim \
	vim/ftplugin/man.vim \
	vimrc
endif

$(provided_elcs): ELCFLAGS = --eval "(require '$(basename $(notdir $@)))"

$(unprovided_elcs): ELCFLAGS = --eval '(load "$(basename $(notdir $@))" nil t)'

emacs.d/inits/provided-delayed-inits.el: $(provided_elcs) Makefile
	$(V_GEN)echo "(dolist (feature '($(basename $(notdir $(provided_elcs))))) \
	  (eval-after-load feature \
	                   \`(load (concat user-emacs-directory \
	                                   \"delayed-inits/\" \
	                                   ,(symbol-name feature)))))" > $@

emacs.d/inits/unprovided-delayed-inits.el: $(unprovided_elcs) Makefile
	$(V_GEN)echo "(dolist (feature '($(basename $(notdir $(unprovided_elcs))))) \
	  (eval-after-load (symbol-name feature) \
			   \`(load (concat user-emacs-directory \
	                                   \"delayed-inits/\" \
	                                   ,(symbol-name feature)))))" > $@

$(sitelisp_elcs): %.elc: %.el
	$(V_ELC)$(EMACS) --batch -Q -L emacs.d/site-lisp \
	  -l emacs.d/inits/package.el $(ELCFLAGS) -f batch-byte-compile $<

emacs.d/site-lisp/userloaddefs.el: $(sitelisp_elcs)
	$(V_ELC)$(EMACS) --batch -Q --eval '(setq vc-handled-backends nil)' \
	  --eval '(setq generated-autoload-file "$(abspath $@)")' \
	  -f batch-update-autoloads emacs.d/site-lisp
	$(V_at)touch $@

$(call DIR,bin)
$(call DIR,userconf,.)
$(call DIR,userconfaudacity)
$(call DIR,userconfemacsd)
$(call DIR,userconfemacsddelayedinits)
$(call DIR,userconfemacsdetcschema)
$(call DIR,userconfemacsdinits)
$(call DIR,userconfemacsdsitelisp)
$(call DIR,userconfmozillafirefox)
$(call DIR,userconfmozillafirefoxprofilesdefault)
$(call DIR,userconfopenofficeorg3userwordbook)
$(call DIR,xdgconfighome)
$(call DIR,xdgconfighomefontconfig)
$(call DIR,xdgconfighomegit)
$(call DIR,xdgconfighomevlc)
$(call DIR,xdgconfighomezsh,.)
$(call DIR,xdgconfighomezshfunctions)

edit = sed \
	-e 's|@SHELL[@]|$(SHELL)|g' \
	-e 's|@ZSHELL[@]|$(ZSHELL)|g' \

$(bin_SCRIPTS) $(userconf_SCRIPTS): %: %.in Makefile
	$(V_GEN)rm -f $@ $@.tmp
	$(V_at)$(edit) $@.in > $@.tmp
	$(V_at)mv $@.tmp $@

DEPENDENCIES = \
	zap

environmentdir = Environment

install-dependencies:
	mkdir -p $(environmentdir) && \
	  for d in $(DEPENDENCIES); do \
	    ed=$(environmentdir)/$$d; \
	    if [[ -d $$ed ]]; then \
	      (cd $$ed && git up && make install); \
	    else \
	      (git clone myhub:$$d.git $$ed && cd $$ed && make install); \
	    fi; \
	  done

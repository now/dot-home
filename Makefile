uname := $(shell uname -s)
ifeq ($(patsubst CYGWIN_%,,$(uname)),)
  uname := Cygwin
endif

CURL = curl
DIFF = diff
EMACS = emacs
ICONV = iconv
INSTALL = install
PATCH = patch
TOUCH = touch
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

ifeq ($(origin XDG_CONFIG_HOME), undefined)
XDG_CONFIG_HOME = ~/.config
endif

prefix = ~
bindir = $(prefix)/opt/bin
sharedir = $(prefix)/opt/share
userconfdir = $(prefix)
guiuserconfdir = $(prefix)
audacityuserconfdir = $(userconfdir)/.audacity
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))
vlcuserconfdir = $(prefix)/.config/vlc

-include Config/$(uname)
-include config.mk

.PHONY: all diff install

all: diff

# file, target
define GROUP_template_diff_file
.PHONY diff: $(2).diff
$(2).diff:
	@$$(DIFF) -u $(2) $(1) || true

endef

# file, target, mode
define GROUP_template_install_file
install: $(2)

$(2): $(1)
	$$(V_INSTALL)$$(INSTALL) -D --mode=$(if $(3),$(3),644) --preserve-timestamps $$< $$(call shell_quote,$$@)

endef

# file, target, mode
define GROUP_template_file
$(call GROUP_template_diff_file,$(1),$(2))
$(call GROUP_template_install_file,$(1),$(2),$(3))
endef

# files, parent-directory, prefix, prefix-to-strip, mode
define GROUP_template
$(eval $(foreach file,$(1),$(call GROUP_template_file,$(file),$(2)/$(3)$(file:$(4)%=%),$(5))))
endef

# file, target, mode
define FILE_template
$(eval $(call GROUP_template_file,$(1),$(2),$(3)))
endef

# file, target, require, userloaddefs
define EMACS_template_file
ifneq ($4,)
emacs/site-lisp/userloaddefs.el: $(1)
endif

$(1:.el=.elc): $(1)
	$$(V_ELC)$$(EMACS) --batch -Q -L emacs/site-lisp -l emacs/site-lisp/userloaddefs.el -l emacs/inits/package.el $(if $(3),--eval "(require '$(basename $(notdir $1)))" )-f batch-byte-compile $$<

$(call GROUP_template_install_file,$(1:.el=.elc),$(2:.el=.elc))

endef

# files, parent-directory, prefix, prefix-to-strip, require, userloaddefs
define EMACS_template
$(eval $(foreach file,$(1),$(call EMACS_template_file,$(file),$(2)/$(3)$(file:$(4)%=%),$(5),$(6))))
endef

# patch, target
define PATCH_template_file
.PHONY: $(2:.patch=)
$(2:.patch=):
	$$(V_PATCH)if $$(PATCH) -Nsg 0 --dry-run $$@ $(1) > /dev/null; then \
	  $$(PATCH) -Nsg 0 $$@ $(1); \
	fi

endef

# patches, parent-directory, prefix, prefix-to-strip
define PATCH_template
$(eval $(foreach file,$(1),$(call PATCH_template_file,$(file),$(2)/$(3)$(file:$(4)%=%))))
endef

HUNSPELL_DICT_VERSION = 2014.08.11
HUNSPELL_EN_GB_DICT_ZIP = openoffice.org/3/user/wordbook/hunspell-en_GB-ise-$(HUNSPELL_DICT_VERSION).zip
HUNSPELL_EN_US_DICT_ZIP = openoffice.org/3/user/wordbook/hunspell-en_US-$(HUNSPELL_DICT_VERSION).zip

$(HUNSPELL_EN_GB_DICT_ZIP) $(HUNSPELL_EN_US_DICT_ZIP):
	$(V_CURL)$(CURL) -Ls http://downloads.sourceforge.net/wordlist/$(@F) > $@

openoffice.org/3/user/wordbook/en_GB-ise.aff: $(HUNSPELL_EN_GB_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)mv $@.tmp $@
	$(V_at)$(PATCH) -sp0 $@ < $@.patch

openoffice.org/3/user/wordbook/en_GB-ise.dic: $(HUNSPELL_EN_GB_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)mv $@.tmp $@
	$(V_at)sed -e "s/'/’/g" $@ > $@.tmp
	$(V_at)mv $@.tmp $@

openoffice.org/3/user/wordbook/en_US.aff: $(HUNSPELL_EN_US_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)mv $@.tmp $@
	$(V_at)$(PATCH) -sp0 $@ < $@.patch

openoffice.org/3/user/wordbook/en_US.dic: $(HUNSPELL_EN_US_DICT_ZIP)
	$(V_GEN)$(UNZIP) -qod $(@D) $< $(@F)
	$(V_at)$(ICONV) -f iso-8859-1 -t utf-8 $@ > $@.tmp
	$(V_at)mv $@.tmp $@
	$(V_at)sed -e "s/'/’/g" $@ > $@.tmp
	$(V_at)mv $@.tmp $@

DOTFILES = \
	editrc \
	gemrc \
	gtkrc-2.0 \
	hunspell_en_US \
	indent.pro \
	inputrc \
	mailcap \
	openoffice.org/3/user/wordbook/en_GB-ise.aff \
	openoffice.org/3/user/wordbook/en_GB-ise.dic \
	openoffice.org/3/user/wordbook/en_US.aff \
	openoffice.org/3/user/wordbook/en_US.dic \
	zshenv

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	dircolors \
	fontconfig/fonts.conf \
	git/config \
	zsh/functions/_unpack \
	zsh/functions/_up \
	zsh/functions/autoload/cd \
	zsh/functions/autoload/freload \
	zsh/functions/autoload/hc \
	zsh/functions/autoload/up \
	zsh/functions/autoload/urlify \
	zsh/functions/define-digraphs \
	zsh/functions/dynamic-directory-names \
	zsh/functions/cache/invalid \
	zsh/functions/cache/path \
	zsh/functions/cache/retrieve \
	zsh/functions/cache/store \
	zsh/functions/zle/cd-to-alternate-directory \
	zsh/functions/zle/foreground-or-list-choices \
	zsh/functions/zle/history-beginning-search-menu \
	zsh/functions/zle/insert-digraph \
	zsh/functions/zle/self-insert-redir \
	zsh/functions/zle/sudo-command-line \
	zsh/functions/zle/up-directory \
	zsh/functions/zle/up-from-menu \
	zsh/functions/zle/urlify-current-argument \
	zsh/functions/zle/util/select-match-from-menu \
	zsh/functions/zle/util/select-match-from-menu-widget \
	zsh/functions/zle/vi-cmd-mode-silently \
	zsh/functions/zle/yank-clipboard

$(call GROUP_template,$(DOTFILES),$(XDG_CONFIG_HOME))

DOTFILES = \
	emacs/etc/schema/catalog.rnc \
	emacs/etc/schema/gtk-builder.rnc \
	emacs/etc/schema/PropertyList-1.0.rnc \
	emacs/etc/schema/schemas.xml

$(call GROUP_template,$(DOTFILES),$(userconfdir),.emacs.d/,emacs/)

install: emacs/site-lisp/userloaddefs.el

emacs/site-lisp/userloaddefs.el: Makefile
	$(V_ELC)$(EMACS) --batch -Q --eval '(setq vc-handled-backends nil)' \
	  --eval '(setq generated-autoload-file "$(abspath $@)")' \
	  -f batch-update-autoloads emacs/site-lisp
	$(V_at)touch $@

$(call GROUP_template,emacs/site-lisp/userloaddefs.el,$(userconfdir),.emacs.d/,emacs/)

DOTFILES = \
	emacs/site-lisp/hide-mode-line.el \
	emacs/site-lisp/ned-info-on-file.el \
	emacs/site-lisp/now-org.el \
	emacs/site-lisp/project.el \
	emacs/site-lisp/rnc-mode.el

$(call EMACS_template,$(DOTFILES),$(userconfdir),.emacs.d/,emacs/,,userloaddefs)

DOTFILES = \
	emacs/init.el \
	emacs/now-theme.el

$(call EMACS_template,$(DOTFILES),$(userconfdir),.emacs.d/,emacs/)

DOTFILES = \
	emacs/delayed-inits/bs.el \
	emacs/delayed-inits/calc.el \
	emacs/delayed-inits/calendar.el \
	emacs/delayed-inits/cc-mode.el \
	emacs/delayed-inits/compile.el \
	emacs/delayed-inits/css-mode.el \
	emacs/delayed-inits/desktop.el \
	emacs/delayed-inits/diff-mode.el \
	emacs/delayed-inits/diff.el \
	emacs/delayed-inits/dired.el \
	emacs/delayed-inits/dired-aux.el \
	emacs/delayed-inits/evil.el \
	emacs/delayed-inits/flx-ido.el \
	emacs/delayed-inits/grep.el \
	emacs/delayed-inits/hideshow.el \
	emacs/delayed-inits/holidays.el \
	emacs/delayed-inits/ido.el \
	emacs/delayed-inits/ispell.el \
	emacs/delayed-inits/magit.el \
	emacs/delayed-inits/make-mode.el \
	emacs/delayed-inits/man.el \
	emacs/delayed-inits/nxml-mode.el \
	emacs/delayed-inits/org-agenda.el \
	emacs/delayed-inits/org-capture.el \
	emacs/delayed-inits/org-clock.el \
	emacs/delayed-inits/org-colview.el \
	emacs/delayed-inits/org-id.el \
	emacs/delayed-inits/org-mobile.el \
	emacs/delayed-inits/org.el \
	emacs/delayed-inits/paredit.el \
	emacs/delayed-inits/recentf.el \
	emacs/delayed-inits/rng-loc.el \
	emacs/delayed-inits/ruby-mode.el \
	emacs/delayed-inits/sh-script.el \
	emacs/delayed-inits/solar.el \
	emacs/delayed-inits/tabulated-list.el \
	emacs/inits/package.el

$(call EMACS_template,$(DOTFILES),$(userconfdir),.emacs.d/,emacs/,require)

DOTFILES = \
	zsh/zlogin \
	zsh/zprofile \
	zsh/zshrc

$(call GROUP_template,$(DOTFILES),$(XDG_CONFIG_HOME)/zsh,.,zsh/)

DOTFILES = \
	audacity.cfg

$(call GROUP_template,$(DOTFILES),$(audacityuserconfdir))

DOTFILES = \
	vlc/vlcrc

$(call GROUP_template,$(DOTFILES),$(vlcuserconfdir),,vlc/)

ifneq ($(firefoxuserconfdir),)
DOTFILES = \
	firefox/mimeTypes.rdf \
	firefox/searchplugins/adlibris.xml \
	firefox/searchplugins/discogs.xml \
	firefox/searchplugins/gatherer.xml \
	firefox/searchplugins/hittase-where.xml \
	firefox/searchplugins/hittase-who.xml \
	firefox/searchplugins/imdb.xml \
	firefox/searchplugins/juno-records.xml \
	firefox/searchplugins/mancx.xml \
	firefox/searchplugins/posix.xml \
	firefox/searchplugins/thepiratebayse.xml \
	firefox/searchplugins/tvragecom.xml \
	firefox/searchplugins/youtube.xml \
	firefox/user.js

$(call GROUP_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)
endif

edit = sed \
       -e 's|@SHELL[@]|$(SHELL)|g' \
       -e 's|@ZSHELL[@]|$(ZSHELL)|g'

BINFILES = \
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
	bin/index-disc \
	bin/m \
	bin/mfedit \
	bin/mov-re-encode \
	bin/terminal-colors \
	bin/pack \
	bin/unpack \
	bin/valgrind-ruby

bin_substitutables := $(BINFILES)

$(call GROUP_template,$(BINFILES),$(bindir),,bin/,755)

include os/os.mk
include host/host.mk

$(bin_substitutables): Makefile
	$(V_GEN)rm -f $@ $@.tmp
	$(V_at)$(edit) $@.in > $@.tmp
	$(V_at)chmod +x $@.tmp
	$(V_at)chmod a-w $@.tmp
	$(V_at)mv $@.tmp $@

define bin_substitutables_file
$(1): $(1).in

endef

$(eval $(foreach file,$(bin_substitutables),$(call bin_substitutables_file,$(file))))

ifdef INCLUDE_VIM
DOTFILES = \
	vim/after/ftplugin/sh.vim \
	vim/after/ftplugin/vim.vim \
	vim/after/ftplugin/zsh.vim \
	vim/after/syntax/vim.vim \
	vim/colors/now.vim \
	vim/ftplugin/man.vim \
	vimrc

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)
endif

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

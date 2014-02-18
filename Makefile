.PHONY: all diff install

all: diff

empty :=
space := $(empty) $(empty)
shell_quote = $(subst $(space),\ ,$(1))

# 1: File
# 2: Target
define GROUP_template_diff_file
GROUP_diff_target := $(2).diff
.PHONY diff: $$(GROUP_diff_target)
$$(GROUP_diff_target):
	@$$(DIFF) -u $(2) $(1) || true

endef

# 1: File
# 2: Target
# 3: Mode
define GROUP_template_file
$(call GROUP_template_diff_file,$(1),$(2))

install: $(2)
$(2): $(1)
	$$(INSTALL) -D --mode=$(if $(3),$(3),644) --preserve-timestamps $$< $$(call shell_quote,$$@)

endef

# 1: Files
# 2: Parent directory
# 3: Prefix to add
# 4: Prefix to strip
# 5: Mode
define GROUP_template
$(eval $(foreach file,$(1),$(call GROUP_template_file,$(file),$(2)/$(3)$(file:$(4)%=%),$(5))))
endef

# 1: File
# 2: Target
# 3: Mode
define FILE_template
$(eval $(call GROUP_template_file,$(1),$(2),$(3)))
endef

# 1: File
# 2: Database
define SQLITE_template_file
install: $(2)
$(2): $(1)
	cat $$^ | $$(SQLITE) $$(call shell_quote,$$@)

endef

# 1: File
# 2: Parent directory
# 3: Prefix to add
# 4: Prefix to strip
SQLITE_construct_target = $(2)/$(3)$(1:$(4)%=%)ite

# 1: Files
# 2: Parent directory
# 3: Prefix to add
# 4: Prefix to strip
define SQLITE_template
$(eval $(foreach file,$(1),$(call SQLITE_template_file,$(file),$(call SQLITE_construct_target,$(file),$(2),$(3),$(4)))))
endef

define SQLITE_ADD_template_file
$(2): $(1)

endef

# 1: Files
# 2: Parent directory
# 3: Prefix to add
# 4: Prefix to strip
define SQLITE_IF_EXISTS_template
$(eval $(foreach file,$(1),$(if $(wildcard $(file)),$(call SQLITE_ADD_template_file,$(file),$(call SQLITE_construct_target,$(file),$(2),$(3),$(4))))))
endef

define EMACS_template_file
source_elc := $(1:.el=).elc
target_elc := $(2:.el=).elc
install: $$(target_elc)

$$(source_elc): $(1)
	$$(EMACS) --batch -Q -L share/emacs/site-lisp -l $(emacsuserloaddefs) -f batch-byte-compile $$<

$$(target_elc): $$(source_elc)
	$$(INSTALL) -D --preserve-timestamps $$< $$(call shell_quote,$$@)

endef

# 1: Files
# 2: Parent directory
# 3: Prefix to add
define EMACS_template
$(eval $(foreach file,$(1),$(call EMACS_template_file,$(file),$(2)/$(3)$(file))))
endef

uname := $(shell uname -s)
ifeq ($(patsubst CYGWIN_%,,$(uname)),)
  uname := Cygwin
endif

DIFF = diff
INSTALL = install
SQLITE = sqlite3
TOUCH = touch
ZSHELL = /bin/zsh
EMACS = emacs

prefix = ~
bindir = $(prefix)/opt/bin
sharedir = $(prefix)/opt/share
userconfdir = $(prefix)
guiuserconfdir = $(prefix)
audacityuserconfdir = $(userconfdir)/.audacity
emacsuserloaddefs = $(sharedir)/emacs/site-lisp/userloaddefs.el
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))
vlcuserconfdir = $(prefix)/.config/vlc

-include Config/$(uname)
-include config.mk

DOTFILES = \
	   dircolors \
	   editrc \
	   fonts.conf \
	   gemrc \
	   gitconfig \
	   gtkrc-2.0 \
	   indent.pro \
	   inputrc \
	   mailcap \
	   vim/after/ftplugin/sh.vim \
	   vim/after/ftplugin/vim.vim \
	   vim/after/ftplugin/zsh.vim \
	   vim/after/syntax/vim.vim \
	   vim/colors/now.vim \
	   vim/ftplugin/man.vim \
	   vimrc \
	   zsh/functions/_unpack \
	   zsh/functions/_up \
	   zsh/functions/autoload/cd \
	   zsh/functions/autoload/freload \
	   zsh/functions/autoload/hc \
	   zsh/functions/autoload/up \
	   zsh/functions/autoload/urlify \
	   zsh/functions/define-digraphs \
	   zsh/functions/dynamic-directory-names \
	   zsh/functions/list-directory-on-chpwd \
	   zsh/functions/cache/invalid \
	   zsh/functions/cache/path \
	   zsh/functions/cache/retrieve \
	   zsh/functions/cache/store \
	   zsh/functions/zsh-mime-setup \
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
	   zsh/functions/zle/yank-clipboard \
	   zshenv

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	   emacs.el

emacs.el: $(emacsuserloaddefs)

$(call EMACS_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	   share/emacs/etc/schema/catalog.rnc \
	   share/emacs/etc/schema/gtk-builder.rnc \
	   share/emacs/etc/schema/schemas.xml

$(call GROUP_template,$(DOTFILES),$(sharedir),,share/)

DOTFILES = \
	   share/emacs/site-lisp/hide-mode-line.el \
	   share/emacs/site-lisp/ned/ned-info-on-file.el \
	   share/emacs/site-lisp/progmodes/rnc-mode.el \
	   share/emacs/site-lisp/themes/now-theme.el

install: $(emacsuserloaddefs)

$(emacsuserloaddefs): $(DOTFILES) Makefile
	mkdir -p "$(dir $@)"
	$(EMACS) --batch -Q --eval '(setq generated-autoload-file "$@")' -f batch-update-autoloads \
	  share/emacs/site-lisp \
	  share/emacs/site-lisp/ned \
	  share/emacs/site-lisp/progmodes && \
	  touch $@

$(call EMACS_template,$(DOTFILES),$(sharedir),,share/)

DOTFILES = \
	   zsh/zlogin \
	   zsh/zprofile \
	   zsh/zshrc

$(call GROUP_template,$(DOTFILES),$(userconfdir)/.zsh,.,zsh/)

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

DOTFILES = \
	   firefox/permissions.sql \
	   firefox/search.sql

$(call SQLITE_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)
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
	   bin/update-context \
	   bin/valgrind-ruby

bin_substitutables := $(BINFILES)

$(call GROUP_template,$(BINFILES),$(bindir),,bin/,755)

include os/os.mk
include host/host.mk

$(bin_substitutables): Makefile
	rm -f $@ $@.tmp
	$(edit) $@.in > $@.tmp
	chmod +x $@.tmp
	chmod a-w $@.tmp
	mv $@.tmp $@

define bin_substitutables_file
$(1): $(1).in

endef

$(eval $(foreach file,$(bin_substitutables),$(call bin_substitutables_file,$(file))))

DEPENDENCIES = \
	       vim-quit-if-only-quickfix-buffer-left \
	       vim-restore-position \
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

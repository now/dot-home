# contents: dot files Makefile.
#
# Copyright © 2006,2008 Nikolai Weibull <now@bitwi.se>

.PHONY: all diff install

all: diff

empty :=
space := $(empty) $(empty)
shell_quote = $(subst $(space),\ ,$(1))

# 1: File
# 2: Target
# 3: Mode
define GROUP_template_file
GROUP_diff_target := $(2).diff
.PHONY diff: $$(GROUP_diff_target)
$$(GROUP_diff_target):
	@$$(DIFF) -u $(2) $(1) || true

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

uname := $(shell uname -s)
ifeq ($(patsubst CYGWIN_%,,$(uname)),)
  uname := Cygwin
endif

DIFF = diff
INSTALL = install
SQLITE = sqlite3
TOUCH = touch
ZSHELL = /bin/zsh

timestamps = .timestamps

prefix = ~
userconfdir = $(prefix)
guiuserconfdir = $(prefix)
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))
vlcuserconfdir = $(prefix)/.config/vlc

-include Config/$(uname)
-include config.mk

DOTFILES = \
	   Xresources \
	   dircolors \
	   editrc \
	   emacs \
	   fonts.conf \
	   gemrc \
	   gitconfig \
	   gtkrc-2.0 \
	   indent.pro \
	   inputrc \
	   irbrc \
	   irssi/config \
	   irssi/default.theme \
	   mailcap \
	   mplayer/config \
	   sbclrc \
	   screenrc \
	   vim/after/ftplugin/c.vim \
	   vim/after/ftplugin/context.vim \
	   vim/after/ftplugin/css.vim \
	   vim/after/ftplugin/dtd.vim \
	   vim/after/ftplugin/gitcommit.vim \
	   vim/after/ftplugin/html.vim \
	   vim/after/ftplugin/java.vim \
	   vim/after/ftplugin/javascript.vim \
	   vim/after/ftplugin/mail.vim \
	   vim/after/ftplugin/nml.vim \
	   vim/after/ftplugin/racc.vim \
	   vim/after/ftplugin/rnc.vim \
	   vim/after/ftplugin/ruby.vim \
	   vim/after/ftplugin/sh.vim \
	   vim/after/ftplugin/treetop.vim \
	   vim/after/ftplugin/vb.vim \
	   vim/after/ftplugin/vim.vim \
	   vim/after/ftplugin/wsh.vim \
	   vim/after/ftplugin/xml.vim \
	   vim/after/ftplugin/xslt.vim \
	   vim/after/ftplugin/zsh.vim \
	   vim/after/indent/java.vim \
	   vim/after/syntax/c.vim \
	   vim/after/syntax/mail.vim \
	   vim/after/syntax/ruby.vim \
	   vim/after/syntax/vim.vim \
	   vim/colors/now.vim \
	   vim/compiler/jing.vim \
	   vim/compiler/rakelookout.vim \
	   vim/compiler/xmllint.vim \
	   vim/doc/matchit.txt \
	   vim/filetype.vim \
	   vim/ftplugin/man.vim \
	   vim/ftplugin/nmc.vim \
	   vim/ftplugin/nml.vim \
	   vim/ftplugin/treetop.vim \
	   vim/indent/nml.vim \
	   vim/indent/treetop.vim \
	   vim/macros/less.vim \
	   vim/macros/quickfix.vim \
	   vim/plugin/matchit.vim \
	   vim/syntax/ilprec.vim \
	   vim/syntax/javascript.vim \
	   vim/syntax/markdown.vim \
	   vim/syntax/nml.vim \
	   vim/syntax/prolog.vim \
	   vim/syntax/treetop.vim \
           vim/templates/cpp.template \
	   vim/templates/ruby.template \
           vim/templates/vim/default.template \
           vim/templates/vim/syntax.template \
           vim/templates/xslt.template \
           vim/templates/zsh.template \
	   vimperatorrc \
	   vimrc \
	   xmonad/xmonad.hs \
	   zsh/functions/_mem-map \
	   zsh/functions/_unpack \
	   zsh/functions/_up \
	   zsh/functions/autoload/cd \
	   zsh/functions/autoload/cygify \
	   zsh/functions/autoload/d \
	   zsh/functions/autoload/dp \
	   zsh/functions/autoload/dr \
	   zsh/functions/autoload/foldl \
	   zsh/functions/autoload/foldr \
	   zsh/functions/autoload/freload \
	   zsh/functions/autoload/hc \
	   zsh/functions/autoload/up \
	   zsh/functions/autoload/urlify \
	   zsh/functions/autoload/winify \
	   zsh/functions/define-digraphs \
	   zsh/functions/dynamic-directory-names \
	   zsh/functions/list-directory-on-chpwd \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/set-terminal-title-from-command \
	   zsh/functions/set-terminal-title-to-pwd \
	   zsh/functions/terminal-title \
	   zsh/functions/cache/invalid \
	   zsh/functions/cache/path \
	   zsh/functions/cache/retrieve \
	   zsh/functions/cache/store \
	   zsh/functions/zsh-mime-setup \
	   zsh/functions/zle/cd-from-list \
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
	   zsh/functions/zle/vim-increase-number \
	   zsh/functions/zle/yank-clipboard \
	   zshenv

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	   share/emacs/etc/schema/gtk-builder.rnc \
	   share/emacs/etc/schema/schemas.xml \
	   share/emacs/hide-mode-line.el \
	   share/emacs/ned/ned-info-on-file.el \
	   share/emacs/rc/bs.el \
	   share/emacs/rc/buffer.el \
	   share/emacs/rc/coding.el \
	   share/emacs/rc/custom.el \
	   share/emacs/rc/desktop.el \
	   share/emacs/rc/diff.el \
	   share/emacs/rc/evil.el \
	   share/emacs/rc/files.el \
	   share/emacs/rc/frame.el \
	   share/emacs/rc/hide-mode-line.el \
	   share/emacs/rc/icomplete.el \
	   share/emacs/rc/ido.el \
	   share/emacs/rc/indent.el \
	   share/emacs/rc/info.el \
	   share/emacs/rc/isearch.el \
	   share/emacs/rc/magit.el \
	   share/emacs/rc/minibuffer.el \
	   share/emacs/rc/nxml.el \
	   share/emacs/rc/org.el \
	   share/emacs/rc/os/nil.el \
	   share/emacs/rc/os/ns.el \
	   share/emacs/rc/os/w32.el \
	   share/emacs/rc/paren.el \
	   share/emacs/rc/progmodes/cc-mode.el \
	   share/emacs/rc/progmodes/compile.el \
	   share/emacs/rc/progmodes/grep.el \
	   share/emacs/rc/progmodes/make-mode.el \
	   share/emacs/rc/progmodes/ruby-mode.el \
	   share/emacs/rc/recentf.el \
	   share/emacs/rc/scroll-bar.el \
	   share/emacs/rc/simple.el \
	   share/emacs/rc/smex.el \
	   share/emacs/rc/startup.el \
	   share/emacs/rc/tool-bar.el \
	   share/emacs/rc/uniquify.el \
	   share/emacs/rc/woman.el \
	   share/emacs/rc/yasnippet.el \
	   share/emacs/rc/xdisp.el \
	   share/emacs/snippets/ruby-mode/au.yasnippet \
	   share/emacs/snippets/ruby-mode/d.yasnippet \
	   share/emacs/snippets/ruby-mode/tlc.yasnippet \
	   share/emacs/snippets/ruby-mode/tle.yasnippet \
	   share/emacs/snippets/ruby-mode/tlm.yasnippet \
	   share/emacs/smex.el \
	   share/emacs/term/screen-256color.el \
	   share/emacs/themes/now-theme.el

$(call GROUP_template,$(DOTFILES),$(userconfdir))

DOTFILES = \
	   zsh/zlogin \
	   zsh/zprofile \
	   zsh/zshrc

$(call GROUP_template,$(DOTFILES),$(userconfdir)/.zsh,.,zsh/)

DOTFILES = \
	   vimperator/plugin/_libly.js \
	   vimperator/plugin/edit-textarea-in-editor.js \
	   vimperator/plugin/feed-some-keys.js

$(call GROUP_template,$(DOTFILES),$(userconfdir),$(if $(subst Cygwin,,$(uname)),.))

DOTFILES = \
	   vlc/vlcrc

$(call GROUP_template,$(DOTFILES),$(vlcuserconfdir),,vlc/)

ifneq ($(firefoxuserconfdir),)
USERSCRIPT_field = s,^[ 	]*//[ 	]*@\($(1)\)[ 	][ 	]*\(.*\)$$,    $(2),p;

USERSCRIPT_fields = $(foreach field,$(1),$(call USERSCRIPT_field,$(field),\1="\2"))

GM_tag = $(call USERSCRIPT_field,$(1),<$(2)>\2</$(2)>)

GM_SCRIPTS = \
	     firefox/gm_scripts/divshare-auto-download.user.js \
	     firefox/gm_scripts/gmail-juno-player.user.js \
	     firefox/gm_scripts/google-reader-download-linked-content.user.js \
	     firefox/gm_scripts/google-reader-view-original-in-background.user.js \
	     firefox/gm_scripts/mediafire-auto-download.user.js \
	     firefox/gm_scripts/update-google-web-history.user.js \
	     firefox/gm_scripts/zshare-mp3-links.user.js

GM_CONFIG = $(firefoxuserconfdir)/gm_scripts/config.xml

$(GM_CONFIG): Makefile $(GM_SCRIPTS)
	{ \
	  echo '<UserScriptConfig>'; \
	  for f in $(wordlist 2,$(words $^),$^); do \
	    echo "  <Script filename=\"`basename $$f`\""; \
	    sed -n '$(call USERSCRIPT_fields,name namespace description)' < $$f; \
	    echo '    enabled="true" basedir=".">'; \
	    sed -n '$(call GM_tag,include,Include) $(call GM_tag,exclude,Exclude)' < $$f; \
	    echo '  </Script>'; \
	  done; \
	  echo '</UserScriptConfig>'; \
	} > $(call shell_quote,$@)

install: $(GM_CONFIG)

DOTFILES = \
	   $(GM_SCRIPTS) \
	   firefox/mimeTypes.rdf \
	   firefox/searchplugins/adlibris.xml \
	   firefox/searchplugins/codesearch.xml \
	   firefox/searchplugins/discogs.xml \
	   firefox/searchplugins/google-bookmarks.xml \
	   firefox/searchplugins/google-dictionary.xml \
	   firefox/searchplugins/hittase-where.xml \
	   firefox/searchplugins/hittase-who.xml \
	   firefox/searchplugins/imdb.xml \
	   firefox/searchplugins/juno-records.xml \
	   firefox/searchplugins/mancx.xml \
	   firefox/searchplugins/posix.xml \
	   firefox/searchplugins/thepiratebayorg.xml \
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
	   xsession

bin_substitutables := $(BINFILES)

$(call GROUP_template,$(BINFILES),~,.,,755)

BINFILES = \
	   bin/asciitable \
	   bin/backup-home \
	   bin/burn \
	   bin/clipboard \
	   bin/dfs \
	   bin/discogs-tags \
	   bin/e \
	   bin/emv \
	   bin/index-disc \
	   bin/m \
	   bin/mem-map \
	   bin/terminal-colors \
	   bin/pack \
	   bin/qf \
	   bin/sclipboard \
	   bin/scget \
	   bin/unpack \
	   bin/update-context \
	   bin/valgrind-ruby \
	   bin/vg \
	   bin/vgg \
	   bin/vimless

bin_substitutables += $(BINFILES)

$(call GROUP_template,$(BINFILES),~,,,755)

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
	       vim-now-base \
	       vim-diff-buffer-against-filesystem \
	       vim-info \
	       vim-man \
	       vim-modern-file \
	       vim-quit-if-only-quickfix-buffer-left \
	       vim-restore-position \
	       vim-templates \
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

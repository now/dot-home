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

timestamps = .timestamps

prefix = ~
userconfdir = $(prefix)
guiuserconfdir = $(prefix)
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))

-include Config/$(uname)
-include config.mk

DOTFILES = \
	   Xresources \
	   dircolors \
	   editrc \
	   emacs \
	   fonts.conf \
	   gitconfig \
	   gtkrc-2.0 \
	   indent.pro \
	   inputrc \
	   irbrc \
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
	   vim/after/ftplugin/racc.vim \
	   vim/after/ftplugin/rnc.vim \
	   vim/after/ftplugin/ruby.vim \
	   vim/after/ftplugin/sh.vim \
	   vim/after/ftplugin/vb.vim \
	   vim/after/ftplugin/vim.vim \
	   vim/after/ftplugin/wsh.vim \
	   vim/after/ftplugin/xml.vim \
	   vim/after/ftplugin/xslt.vim \
	   vim/after/ftplugin/zsh.vim \
	   vim/after/syntax/c.vim \
	   vim/after/syntax/mail.vim \
	   vim/after/syntax/ruby.vim \
	   vim/after/syntax/vim.vim \
	   vim/colors/now.vim \
	   vim/compiler/rakexpectations.vim \
	   vim/doc/ascii-tables.txt \
	   vim/ftplugin/docbk.vim \
	   vim/ftplugin/man.vim \
	   vim/ftplugin/urlextract.vim \
	   vim/macros/less.vim \
	   vim/syntax/ilprec.vim \
	   vim/syntax/javascript.vim \
	   vim/syntax/prolog.vim \
           vim/templates/CC.license \
           vim/templates/FDL.license \
           vim/templates/GPL.license \
           vim/templates/LGPL.license \
           vim/templates/automake.template \
           vim/templates/c.template \
           vim/templates/conf.template \
           vim/templates/cpp.template \
           vim/templates/default.license \
           vim/templates/mp.template \
	   vim/templates/ruby.template \
           vim/templates/scheme.template \
           vim/templates/screen.template \
           vim/templates/tex.template \
           vim/templates/vim/default.template \
           vim/templates/vim/syntax.template \
           vim/templates/xdefaults.template \
           vim/templates/xslt.template \
           vim/templates/yaml.template \
           vim/templates/zsh.template \
	   vimperatorrc \
	   vimrc \
	   xmonad/xmonad.hs \
	   zsh/functions/_j \
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
	   zsh/functions/autoload/j \
	   zsh/functions/autoload/up \
	   zsh/functions/autoload/urlify \
	   zsh/functions/autoload/winify \
	   zsh/functions/define-digraphs \
	   zsh/functions/history-beginning-search-menu \
	   zsh/functions/insert-digraph \
	   zsh/functions/list-directory-on-chpwd \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/set-terminal-title-from-command \
	   zsh/functions/set-terminal-title-to-pwd \
	   zsh/functions/terminal-title \
	   zsh/functions/update-pwd-history-on-chpwd \
	   zsh/functions/cache/invalid \
	   zsh/functions/cache/path \
	   zsh/functions/cache/retrieve \
	   zsh/functions/cache/store \
	   zsh/functions/zsh-mime-setup \
	   zsh/functions/zle/cd-from-list \
	   zsh/functions/zle/cd-to-alternate-directory \
	   zsh/functions/zle/self-insert-redir \
	   zsh/functions/zle/sudo-command-line \
	   zsh/functions/zle/up-directory \
	   zsh/functions/zle/urlify-current-argument \
	   zsh/functions/zle/vim-increase-number \
	   zsh/functions/zle/yank-clipboard \
	   zshenv

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	   share/emacs/color-theme.el \
	   share/emacs/color-theme-autoloads.el \
	   share/emacs/cygwin-mount.el \
	   share/emacs/digraph.el \
	   share/emacs/hide-mode-line.el \
	   share/emacs/ned/ned-info-on-file.el \
	   share/emacs/rc/os/ns.el \
	   share/emacs/rc/os/w32.el \
	   share/emacs/rc/progmodes/ruby.el \
	   share/emacs/rect-mark.el \
	   share/emacs/redo.el \
	   share/emacs/ruby-mode.el \
	   share/emacs/themes/color-theme-now.el \
	   share/emacs/vimpulse.el \

$(call GROUP_template,$(DOTFILES),$(userconfdir))

DOTFILES = \
	   zsh/zlogin \
	   zsh/zprofile \
	   zsh/zshrc

$(call GROUP_template,$(DOTFILES),$(userconfdir)/.zsh,.,zsh/)

DOTFILES = \
	   vimperator/plugin/edit-textarea-in-editor.js

$(call GROUP_template,$(DOTFILES),$(userconfdir),$(if $(subst Cygwin,,$(uname)),.))

DOTFILES = \
	   vlc/vlcrc

$(call GROUP_template,$(DOTFILES),$(guiuserconfdir))


USERSCRIPT_field = s,^[ 	]*//[ 	]*@\($(1)\)[ 	][ 	]*\(.*\)$$,    $(2),p;

USERSCRIPT_fields = $(foreach field,$(1),$(call USERSCRIPT_field,$(field),\1="\2"))

GM_tag = $(call USERSCRIPT_field,$(1),<$(2)>\2</$(2)>)

GM_SCRIPTS = \
	     firefox/gm_scripts/delicious-favicons.user.js \
	     firefox/gm_scripts/divshare-auto-download.user.js \
	     firefox/gm_scripts/google-favicons.user.js \
	     firefox/gm_scripts/google-reader-view-original-in-background.user.js \
	     firefox/gm_scripts/juno-download-all-link.user.js \
	     firefox/gm_scripts/secure-google-docs-connection.user.js \
	     firefox/gm_scripts/vbulletin-cleanup.user.js \
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

CF_SCRIPTS = \
	     firefox/chickenfoot/mediafire-automatic-download.js

CF_TRIGGERS = $(firefoxuserconfdir)/chickenfoot/triggers.xml

$(CF_TRIGGERS): Makefile $(CF_SCRIPTS)
	{ \
	  echo '<triggers version="0.5">'; \
	  for f in $(wordlist 2,$(words $^),$^); do \
	    echo "  <trigger path=\"`basename $$f`\""; \
	    sed -n '$(call USERSCRIPT_fields,name when description)' < $$f; \
	    echo '    enabled="true">'; \
	    sed -n '$(call USERSCRIPT_field,includes,<include urlPattern="\2"/>)' < $$f; \
	    echo '  </trigger>'; \
	  done; \
	  echo '</triggers>'; \
	} > $(call shell_quote,$@)

install: $(CF_TRIGGERS)

DOTFILES = \
	   $(GM_SCRIPTS) \
	   $(CF_SCRIPTS) \
	   firefox/searchplugins/adlibris.xml \
	   firefox/searchplugins/codesearch.xml \
	   firefox/searchplugins/discogs.xml \
	   firefox/searchplugins/google-bookmarks.xml \
	   firefox/searchplugins/google-dictionary.xml \
	   firefox/searchplugins/hittase-where.xml \
	   firefox/searchplugins/hittase-who.xml \
	   firefox/searchplugins/imdb.xml \
	   firefox/searchplugins/juno-records.xml \
	   firefox/searchplugins/posix.xml \
	   firefox/searchplugins/thepiratebayorg.xml \
	   firefox/searchplugins/tvragecom.xml \
	   firefox/searchplugins/youtube.xml \
	   firefox/user.js

$(call GROUP_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)

DOTFILES = \
	   firefox/permissions.sql \
	   firefox/search.sql \
	   firefox/stylish.sql

$(call SQLITE_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)

BINFILES = \
	   xsession

$(call GROUP_template,$(BINFILES),~,.,,755)

BINFILES = \
	   bin/asciitable \
	   bin/backup-home \
	   bin/burn \
	   bin/clipboard \
	   bin/dfs \
	   bin/e \
	   bin/emv \
	   bin/index-disc \
	   bin/m \
	   bin/mem-map \
	   bin/terminal-colors \
	   bin/pack \
	   bin/unpack \
	   bin/update-context \
	   bin/valgrind-ruby \
	   bin/vg \
	   bin/vimless

$(call GROUP_template,$(BINFILES),~,,,755)

include os/os.mk
include host/host.mk

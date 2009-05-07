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

DIFF = diff
INSTALL = install

on_cygwin := $(if $(subst Cygwin,,$(shell uname -o)),,1)
on_darwin := $(if $(subst Darwin,,$(shell uname)),,1)

prefix = ~
userconfdir = $(prefix)
ifdef on_darwin
  firefoxuserconfdir = $(call shell_quote,$(wildcard ~/Library/Application\ Support/Firefox/Profiles/*.default))
else
  firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))
endif

-include config.mk

DOTFILES = \
	   Xresources \
	   dircolors \
	   emacs \
	   fonts.conf \
	   gitconfig \
	   gtkrc-2.0 \
	   indent.pro \
	   inputrc \
	   irbrc \
	   mailcap \
	   sbclrc \
	   screenrc \
	   vim/after/ftplugin/c.vim \
	   vim/after/ftplugin/context.vim \
	   vim/after/ftplugin/css.vim \
	   vim/after/ftplugin/dtd.vim \
	   vim/after/ftplugin/html.vim \
	   vim/after/ftplugin/javascript.vim \
	   vim/after/ftplugin/mail.vim \
	   vim/after/ftplugin/racc.vim \
	   vim/after/ftplugin/rnc.vim \
	   vim/after/ftplugin/ruby.vim \
	   vim/after/ftplugin/vb.vim \
	   vim/after/ftplugin/vim.vim \
	   vim/after/ftplugin/xml.vim \
	   vim/after/ftplugin/zsh.vim \
	   vim/after/syntax/c.vim \
	   vim/after/syntax/mail.vim \
	   vim/after/syntax/ruby.vim \
	   vim/after/syntax/vim.vim \
	   vim/colors/now.vim \
	   vim/doc/ascii-tables.txt \
	   vim/ftplugin/docbk.vim \
	   vim/ftplugin/gitcommit.vim \
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
           vim/templates/make.template \
           vim/templates/mp.template \
           vim/templates/ruby.template \
           vim/templates/scheme.template \
           vim/templates/screen.template \
           vim/templates/tex.template \
           vim/templates/vim/default.template \
           vim/templates/vim/syntax.template \
           vim/templates/xdefaults.template \
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
	   zsh/functions/autoload/d \
	   zsh/functions/autoload/dp \
	   zsh/functions/autoload/dr \
	   zsh/functions/autoload/foldl \
	   zsh/functions/autoload/foldr \
	   zsh/functions/autoload/freload \
	   zsh/functions/autoload/hc \
	   zsh/functions/autoload/j \
	   zsh/functions/autoload/up \
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
	   zsh/functions/zle/vim-increase-number \
	   zsh/functions/zle/yank-x11-clipboard \
	   zsh/login/os/Cygwin \
	   zsh/profile/os/darwin \
	   zsh/rc/hosts/puritan \
	   zsh/rc/os/Cygwin \
	   zsh/rc/os/darwin \
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
	   vimperator/plugin/delicious.js

ifdef on_cygwin
  $(call GROUP_template,$(DOTFILES),$(userconfdir))
else
  $(call GROUP_template,$(DOTFILES),$(userconfdir),.)
endif

GM_SCRIPTS = \
	     firefox/gm_scripts/delicious-favicons.user.js \
	     firefox/gm_scripts/google-favicons.user.js \
	     firefox/gm_scripts/secure-google-docs-connection.user.js \
	     firefox/gm_scripts/vbulletin-cleanup.user.js \
	     firefox/gm_scripts/zshare-mp3-links.user.js

GM_CONFIG = $(firefoxuserconfdir)/gm_scripts/config.xml

$(GM_CONFIG): Makefile $(GM_SCRIPTS)
	{ \
	  echo '<UserScriptConfig>'; \
	  for f in $^; do \
	    test $$f = Makefile && continue; \
	    echo "  <Script filename=\"`basename $$f`\""; \
	    for field in name namespace description; do \
	      sed -n 's,^[ 	]*//[ 	]*@'$$field'[ 	]\+\(.*\),          '$$field'="\1",p' < $$f; \
	    done; \
	    echo '          enabled="true" basedir=".">'; \
	    for field in include exclude; do \
	      sed -n 's,^[ 	]*//[ 	]*@'$$field'[ 	]\+\(.*\)$$,    <\u'$$field'>\1</\u'$$field'>,p' < $$f; \
	    done; \
	    echo '  </Script>'; \
	  done; \
	  echo '</UserScriptConfig>'; \
	} > $(call shell_quote,$@)

install: $(GM_CONFIG)

DOTFILES = \
	   $(GM_SCRIPTS) \
	   firefox/stylish.rdf \
	   firefox/user.js

$(call GROUP_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)

FIREFOXPERMISSIONS = $(firefoxuserconfdir)/permissions.sqlite

$(FIREFOXPERMISSIONS): firefox/permissions.sql
	sqlite3 $(call shell_quote,$@) < $<

install: $(FIREFOXPERMISSIONS)

BINFILES = \
	   xsession

$(call GROUP_template,$(BINFILES),~,.,,755)

BINFILES = \
	   bin/backup-home \
	   bin/burn \
	   bin/dfs \
	   bin/e \
	   bin/emv \
	   bin/index-disc \
	   bin/mem-map \
	   bin/terminal-colors \
	   bin/pack \
	   bin/unpack \
	   bin/update-context \
	   bin/valgrind-ruby \
	   bin/vg \
	   bin/vimless

$(call GROUP_template,$(BINFILES),~,,,755)

ifdef on_cygwin
  firefoxprofilesdir=$(call shell_quote,$(shell cygpath -u "$(APPDATA)")/Mozilla/Firefox)
  DOTFILES = \
	     firefox/profiles.ini

  $(call GROUP_template,$(DOTFILES),$(firefoxprofilesdir),,firefox/)
else
  ifndef on_darwin
    LDLIBS = -lX11
    BINFILES = \
	       bin/xdigraph

    $(call GROUP_template,$(BINFILES),~,,,755)
  endif
endif

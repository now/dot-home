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

prefix = ~
userconfdir = $(prefix)
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))

on_cygwin := $(if $(subst Cygwin,,$(shell uname -o)),,1)

-include config.mk

DOTFILES = \
	   Xresources \
	   cmus/now.theme \
	   cmus/rc \
	   dircolors \
	   emacs \
	   fonts.conf \
	   gitconfig \
	   gtkrc-2.0 \
	   indent.pro \
	   inputrc \
	   irbrc \
	   lighttpd/lighttpd.conf \
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
	   zsh/functions/_mem-map \
	   zsh/functions/_unpack \
	   zsh/functions/_up \
	   zsh/functions/up \
	   zsh/functions/d \
	   zsh/functions/define-digraphs \
	   zsh/functions/history-beginning-search-menu \
	   zsh/functions/insert-digraph \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/set-terminal-title-from-command \
	   zsh/functions/terminal-title \
	   zsh/functions/cache/invalid \
	   zsh/functions/cache/path \
	   zsh/functions/cache/retrieve \
	   zsh/functions/cache/store \
	   zsh/functions/zsh-mime-setup \
	   zsh/functions/zle/vim-increase-number \
	   zsh/login/os/Cygwin \
	   zsh/rc/hosts/puritan \
	   zsh/rc/os/Cygwin \
	   zshenv

$(call GROUP_template,$(DOTFILES),$(userconfdir),.)

DOTFILES = \
	   share/emacs/color-theme.el \
	   share/emacs/color-theme-autoloads.el \
	   share/emacs/cygwin-mount.el \
	   share/emacs/digraph.el \
	   share/emacs/hide-mode-line.el \
	   share/emacs/ned/ned-info-on-file.el \
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
	   vimperator/plugin/bookmarks.js

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
	} > $@

install: $(GM_CONFIG)

DOTFILES = \
	   $(GM_SCRIPTS) \
	   firefox/stylish.rdf \
	   firefox/user.js

$(call GROUP_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)

BINFILES = \
	   xsession

$(call GROUP_template,$(BINFILES),~,.,,755)

BINFILES = \
	   bin/backup-home \
	   bin/burn \
	   bin/dfs \
	   bin/emv \
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
  LDLIBS = -lX11
  BINFILES = \
	     bin/xdigraph

  $(call GROUP_template,$(BINFILES),~,,,755)
endif

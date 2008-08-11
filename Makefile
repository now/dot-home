# contents: dot files Makefile.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

.PHONY: all diff install

all: diff

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
	$$(INSTALL) -D --mode=$(if $(3),$(3),644) --preserve-timestamps $$< $$@

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

prefix = ~/.local
userconfdir = $(prefix)/etc
usersharedir = $(prefix)/share
firefoxuserconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))

-include config.mk

DOTFILES = \
	   X11/Xresources \
	   cmus/now.theme \
	   cmus/rc \
	   dircolors \
	   gnupg/gpg.conf \
	   gtkrc \
	   indentrc \
	   inputrc \
	   irbrc \
	   lftp/rc \
	   mailcap \
	   mplayer/config \
	   sbclrc \
	   screenrc \
	   vim/vimrc \
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
	   zsh/functions/zcalc \
	   zsh/functions/cache/invalid \
	   zsh/functions/cache/path \
	   zsh/functions/cache/retrieve \
	   zsh/functions/cache/store \
	   zsh/functions/zsh-mime-setup \
	   zsh/functions/zle/vim-increase-number \
	   zsh/rc/hosts/puritan

$(call GROUP_template,$(DOTFILES),$(userconfdir))

DOTFILES = \
	   zsh/zlogin \
	   zsh/zprofile \
	   zsh/zshenv \
	   zsh/zshrc

$(call GROUP_template,$(DOTFILES),$(userconfdir)/zsh,.,zsh/)

DOTFILES = \
	   vimperator/vimperator/plugin/bookmarks.js \
	   vimperator/vimperatorrc

# TODO: Don’t add . prefix to vimperator directory on win32.
$(call GROUP_template,$(DOTFILES),$(userconfdir)/vimperator,.,vimperator/)

DOTFILES = \
	   firefox/gm_scripts/config.xml \
	   firefox/gm_scripts/gmail-fixed-font-toggle.user.js \
	   firefox/gm_scripts/gmail-macros.user.js \
	   firefox/gm_scripts/gmail-secure.user.js \
	   firefox/user.js

$(call GROUP_template,$(DOTFILES),$(firefoxuserconfdir),,firefox/)

DOTFILES = \
	   share/emacs/rect-mark.el \
	   share/emacs/redo.el \
	   share/emacs/ruby-mode.el \
	   share/emacs/vimpulse.el

$(call GROUP_template,$(DOTFILES),$(usersharedir),,share/)

DOTFILES = \
	   emacs \
	   fonts.conf \
	   gitconfig \
	   xmonad/xmonad.hs

ifdef ICANTMODIFYETC
DOTFILES += \
	    zshenv
endif

$(call GROUP_template,$(DOTFILES),~,.)

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
	   bin/vg \
	   bin/vimless

$(call GROUP_template,$(BINFILES),~,,,755)

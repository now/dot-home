# contents: dot files Makefile.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

.PHONY: all diff install

all: diff

# 1: prefix to strip
# 2: text
define GROUP_strip_prefix
$(if $(1),$(patsubst $(1)/%,%,$(2)),$(2))
endef

# 1: parent directory
# 2: prefix to strip
# 3: file
# 4: prefix to add
define GROUP_build_target
$(1)/$(4)$(call GROUP_strip_prefix,$(2),$(3))
endef

# 1: file
# 2: target
define GROUP_diff_template
GROUP_diff_target := $(2).diff
.PHONY: $$(GROUP_diff_target)
diff: $$(GROUP_diff_target)
$$(GROUP_diff_target):
	-@$$(DIFF) -u $(2) $(1)

endef

# 1: file
# 2: target
# 3: file mode
define GROUP_install_template
install: $(2)
$(2): $(1)
	$$(INSTALL) --mode=$(3) $$< $$@

endef

# 1: parent directory
# 2: prefix to strip
# 3: directories
define GROUP_template_directories
target_DIRS := $(addprefix $(1)/,$(call GROUP_strip_prefix,$(2),$(3)))
install: $$(target_DIRS)
$$(target_DIRS):
	$$(INSTALL) --directory --mode=755 $$@

endef

# 1: parent directory
# 2: prefix to strip
# 3: file
# 4: prefix to add
# 5: file mode
define GROUP_template_file
$(call GROUP_diff_template,$(3),$(call GROUP_build_target,$(1),$(2),$(3),$(4)))
$(call GROUP_install_template,$(3),$(call GROUP_build_target,$(1),$(2),$(3),$(4)),$(5))
endef

# 1: parent directory
# 2: prefix to strip
# 3: directories to install
# 4: files to install
# 5: file mode
# 6: prefix to add
define GROUP_template
$(call GROUP_template_directories,$(1),$(2),$(3))
$(foreach file,$(4),$(call GROUP_template_file,$(1),$(2),$(file),$(6),$(5)))
endef

DIFF = diff
INSTALL = install

prefix = ~/.local
userconfdir = $(prefix)/etc

-include config.mk

DOTDIRS = \
	  X11 \
	  cmus \
	  gnupg \
	  irssi \
	  lftp \
	  mplayer \
	  vim \
	  vim/after \
	  vim/after/ftplugin \
	  vim/after/syntax \
	  vim/colors \
	  vim/doc \
	  vim/ftplugin \
	  vim/macros \
	  vim/syntax \
	  vim/templates \
	  vim/templates/vim \
	  zsh \
	  zsh/functions \
	  zsh/functions/zle

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
	   irssi/clean.theme \
	   irssi/config \
	   lftp/rc \
	   mailcap \
	   mplayer/config \
	   sbclrc \
	   screenrc \
	   vim/vimrc \
	   vim/after/ftplugin/c.vim \
	   vim/after/ftplugin/context.vim \
	   vim/after/ftplugin/css.vim \
	   vim/after/ftplugin/html.vim \
	   vim/after/ftplugin/javascript.vim \
	   vim/after/ftplugin/mail.vim \
	   vim/after/ftplugin/racc.vim \
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
	   zsh/functions/_unpack \
	   zsh/functions/_up \
	   zsh/functions/up \
	   zsh/functions/d \
	   zsh/functions/define-digraphs \
	   zsh/functions/history-beginning-search-menu \
	   zsh/functions/insert-digraph \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/zcalc \
	   zsh/functions/zle/vim-increase-number

$(eval $(call GROUP_template,$(userconfdir),,$(DOTDIRS),$(DOTFILES),644))

DOTDIRS =

DOTFILES = \
	   zsh/zlogin \
	   zsh/zprofile \
	   zsh/zshenv \
	   zsh/zshrc

$(eval $(call GROUP_template,$(userconfdir)/zsh,zsh,$(DOTDIRS),$(DOTFILES),644,.))

DOTDIRS = \
	  vimperator \
	  vimperator/.vimperator \
	  vimperator/.vimperator/plugin

DOTFILES = \
	   vimperator/vimperator/plugin/bookmarks.js \
	   vimperator/vimperatorrc

$(eval $(call GROUP_template,$(userconfdir)/vimperator,vimperator,$(DOTDIRS),$(DOTFILES),644,.))

userconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))

DOTDIRS = \
	  firefox/gm_scripts

DOTFILES = \
	   firefox/gm_scripts/config.xml \
	   firefox/gm_scripts/gmail-fixed-font-toggle.user.js \
	   firefox/gm_scripts/gmail-macros.user.js \
	   firefox/gm_scripts/gmail-secure.user.js \
	   firefox/user.js

$(eval $(call GROUP_template,$(userconfdir),firefox,$(DOTDIRS),$(DOTFILES),644))

DOTDIRS = \
	  .xmonad

DOTFILES = \
	   fonts.conf \
	   gitconfig \
	   xmonad/xmonad.hs

ifdef ICANTMODIFYETC
DOTFILES += \
	    zshenv
endif

$(eval $(call GROUP_template,~,,$(DOTDIRS),$(DOTFILES),644,.))

DOTDIRS =

DOTFILES = \
	   xsession

$(eval $(call GROUP_template,~,,$(DOTDIRS),$(DOTFILES),755,.))

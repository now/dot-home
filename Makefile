# contents: dot files Makefile.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

.PHONY: all diff install

all: diff

define GROUP_strip_prefix
$(if $(1),$(patsubst $(1)/%,%,$(2)),$(2))
endef

define GROUP_diff_template
GROUP_diff_target := $(1)/$(call GROUP_strip_prefix,$(2),$(3)).diff
.PHONY: $$(GROUP_diff_target)
diff: $$(GROUP_diff_target)
$$(GROUP_diff_target):
	-@$$(DIFF) -u $$(patsubst %.diff,%,$$@) $(3)

endef

define GROUP_install_template
GROUP_install_target := $(1)/$(call GROUP_strip_prefix,$(2),$(3))
install: $$(GROUP_install_target)
$$(GROUP_install_target): $(3)
	$$(INSTALL) --mode=$(4) $$< $$@

endef

define GROUP_template
target_DIRS := $(addprefix $(1)/,$(call GROUP_strip_prefix,$(2),$(3)))
install: $$(target_DIRS)
$$(target_DIRS):
	$$(INSTALL) --directory --mode=755 $$@
$(foreach file,$(4),$(call GROUP_diff_template,$(1),$(2),$(file)))
$(foreach file,$(4),$(call GROUP_install_template,$(1),$(2),$(file),$(5)))
endef

DIFF = diff
INSTALL = install

prefix = ~/.local
userconfdir = $(prefix)/etc

DOTDIRS = \
	  X11 \
	  gnupg \
	  irssi \
	  ivman \
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
	  vim/templates \
	  zsh \
	  zsh/functions

DOTFILES = \
	   X11/Xresources \
	   dircolors \
	   gnupg/gpg.conf \
	   gtkrc \
	   indentrc \
	   inputrc \
	   irbrc \
	   irssi/clean.theme \
	   irssi/config \
	   ivman/IvmConfigActions.xml \
	   ivman/IvmConfigBase.xml \
	   ivman/IvmConfigConditions.xml \
	   ivman/IvmConfigProperties.xml \
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
           vim/templates/vim.template \
           vim/templates/xdefaults.template \
           vim/templates/yaml.template \
           vim/templates/zsh.template \
	   zsh/.zlogin \
	   zsh/.zprofile \
	   zsh/.zshenv \
	   zsh/.zshrc \
	   zsh/functions/_unpack \
	   zsh/functions/cdup \
	   zsh/functions/d \
	   zsh/functions/define-digraphs \
	   zsh/functions/insert-digraph \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/zcalc

$(eval $(call GROUP_template,$(userconfdir),,$(DOTDIRS),$(DOTFILES),644))

DOTDIRS =

DOTFILES = \
	   ratpoison/ratpoisonrc

$(eval $(call GROUP_template,$(userconfdir),ratpoison,$(DOTDIRS),$(DOTFILES),644))

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

DOTDIRS =

DOTFILES = \
	   .vimperatorrc

$(eval $(call GROUP_template,~,,$(DOTDIRS),$(DOTFILES),644))

userbindir = ~

BINDIRS = \
	  bin

BINFILES = \
	   ratpoison/bin/ratpoison-expose \
	   ratpoison/bin/ratpoison-select-by-class \
	   ratpoison/bin/ratpoison-workspaces

$(eval $(call GROUP_template,$(userbindir),ratpoison,$(BINDIRS),$(BINFILES),755))

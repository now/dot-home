# contents: dot files Makefile.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

# TODO: As we want to maintain the mozilla stuff as well, we better break this
# up a bit so that the ~/.local/etc part can register the directories and files
# it depends upon and then we can later add stuff with another prefix to the
# same list so that we can reuse the rules.

target_DOTDIRS :=
target_DOTFILES :=
target_DOTDIFFS :=

define GROUP_template
stripped_DOTDIRS := $(if firefox/,$(patsubst firefox/%,%,$(3)),$(3))
stripped_DOTFILES := $(if firefox/,$(patsubst firefox/%,%,$(4)),$(4))
target_DOTDIRS += $$(addprefix $(1)/,$$(stripped_DOTDIRS))
target_DOTFILES += $$(addprefix $(1)/,$$(stripped_DOTFILES))
target_DOTDIFFS += $$(addprefix $(1)/,$$(addsuffix .diff,$$(stripped_DOTFILES)))
$(1)/%.diff:
	-@$$(DIFF) -u $(1)/$$* $(if $(2),$(2)/)$$*
$(1)/%: $(if $(2),$(2)/)%
	$$(INSTALL) --mode=644 $$< $$@
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
	  mutt \
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
	   ratpoisonrc \
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
	   zsh/functions/prompt_now_setup \
	   zsh/functions/zcalc

$(eval $(call GROUP_template,$(userconfdir),,$(DOTDIRS),$(DOTFILES)))

userconfdir = $(firstword $(wildcard ~/.mozilla/firefox/*.default))

DOTDIRS = \
	  firefox/gm_scripts

DOTFILES = \
	   firefox/gm_scripts/config.xml \
	   firefox/gm_scripts/gmail-fixed-font-toggle.user.js \
	   firefox/gm_scripts/gmail-macros.user.js \
	   firefox/gm_scripts/gmail-secure.user.js \
	   firefox/user.js

$(eval $(call GROUP_template,$(userconfdir),firefox,$(DOTDIRS),$(DOTFILES)))

#target_DOTDIRS += $(addprefix $(userconfdir)/,$(DOTDIRS))

#target_DOTFILES += $(addprefix $(userconfdir)/,$(DOTFILES))

#target_DOTDIFFS += $(addprefix $(userconfdir)/,$(addsuffix .diff,$(DOTFILES)))
#DOTFILES_DIFF += $(addsuffix .diff,$(DOTFILES))

.PHONY: all diff install
#$(DOTFILES_DIFF) install

all: diff

diff: $(target_DOTDIFFS)

#$(DOTFILES_DIFF):
#	-@$(DIFF) -u $(addprefix $(userconfdir)/,$(basename $@)) $(basename $@)

#$(userconfdir)/%.diff:
#	-@$(DIFF) -u $(addprefix $(userconfdir)/,$*) $*

install: $(target_DOTDIRS) $(target_DOTFILES)

$(target_DOTDIRS):
	$(INSTALL) --directory --mode=755 $@

#$(userconfdir)/%: %
#	$(INSTALL) --mode=644 $< $@

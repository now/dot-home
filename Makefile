# contents: dot files Makefile.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

prefix = ~/.local

userconfdir = $(prefix)/etc

DIFF = diff
INSTALL = install

DOTDIRS = \
	  X11 \
	  gnupg \
	  irssi \
	  lftp \
	  mplayer \
	  mutt \
	  zsh

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
	   lftp/rc \
	   mplayer/config \
	   ratpoisonrc \
	   sbclrc \
	   screenrc \
	   zsh/.zlogin \
	   zsh/.zprofile \
	   zsh/.zshenv \
	   zsh/.zshrc \
	   zsh/functions/_unpack \
	   zsh/functions/cdup \
	   zsh/functions/d \
	   zsh/functions/prompt_now_setup \
	   zsh/functions/zcalc

target_DOTFILES = $(addprefix $(userconfdir)/,$(DOTFILES))

target_DOTDIRS = $(addprefix $(userconfdir)/,$(DOTDIRS))

DOTFILES_DIFF = $(addsuffix .diff,$(DOTFILES))

.PHONY: all diff $(DOTFILES_DIFF) install

all: diff

diff: $(DOTFILES_DIFF)

$(DOTFILES_DIFF):
	-$(DIFF) -u $(addprefix $(userconfdir)/,$(basename $@)) $(basename $@)

install: $(target_DOTDIRS) $(target_DOTFILES)

$(target_DOTDIRS):
	$(INSTALL) --directory --mode=755 $@

$(userconfdir)/%: %
	$(INSTALL) --mode=644 $< $@

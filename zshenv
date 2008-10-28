# contents: Zsh environment settings.
#
# Copyright © 2006 Nikolai Weibull <now@bitwi.se>

ZDOTDIR=~/.zsh

fpath=($ZDOTDIR/functions{,/{autoload,zap,zle}} $fpath)

# We use this as we want to use extended globs from, for example, Vim.
setopt extendedglob

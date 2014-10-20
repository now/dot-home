ZDOTDIR=${XDG_CONFIG_HOME:-~/.config}/zsh

fpath[1,0]=($ZDOTDIR/functions)

# We use this as we want to use extended globs from, for example, Emacs.
setopt extendedglob

source $ZDOTDIR/os/env

ZDOTDIR=~/.zsh

fpath[1,0]=($ZDOTDIR/functions{,/{autoload,zap}})

# We use this as we want to use extended globs from, for example, Vim.
setopt extendedglob

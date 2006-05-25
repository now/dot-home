" Vim filetype plugin file
" Language:         Zsh shell script
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

setlocal shiftwidth=2 softtabstop=2
setlocal formatoptions-=t formatoptions+=croql

let b:undo_ftplugin .= ' | setl sw< sts< fo< '

" Vim filetype plugin file 
" Language:	    Mail
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

silent! g/^> -- $/,/^$/-1d
silent! %s/^\(>\s\+\)*[|}]/\1>/
silent! %s/^\(>\s\+\)*>\(\S\)/\1> \2/

setlocal formatoptions+=n

let b:undo_ftplugin .= ' | setl fo<'

call cursor(1, 1)
silent! /^$/

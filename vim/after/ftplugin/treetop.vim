" Vim filetype plugin file
" Language:	    Treetop
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-03-14

setlocal shiftwidth=2 softtabstop=2 expandtab
let b:undo_ftplugin .= ' | setl sw< sts< et<'

compiler rakelookout

" Vim filetype plugin file
" Language:	    Java
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-07-14

setlocal shiftwidth=2 softtabstop=2
setlocal errorformat=%E%f:%l:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

let b:undo_ftplugin .= ' | setl sw< sts< efm<'

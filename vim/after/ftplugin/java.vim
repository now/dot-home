" Vim filetype plugin file
" Language:	    Java
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2009-09-03

setl errorformat=%E%f:%l:\ %m,%-Z%p^,%-C%.%#,%-G%.%#

let b:undo_ftplugin .= ' | setl efm<'

" Vim ftplugin file
" Language:         urlextract(1) selection screen
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

noremap <buffer> <silent> <Enter> <Esc>:if line('.') != 1<Bar>silent 1,.-1d<Bar>endif<Bar>if line('.') != line('$')<Bar>silent .+1,$d<Bar>endif<Bar>silent wq<CR>

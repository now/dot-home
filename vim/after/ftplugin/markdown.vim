" Vim filetype plugin file
" Language:	    Ruby
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-11-30

let s:saved_syntax = b:current_syntax
unlet b:current_syntax

syn include @markdownCodeBlockCluster syntax/ruby.vim

let b:current_syntax = s:saved_syntax
unlet s:saved_syntax

" Vim compiler file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-02-20

if exists('current_compiler')
  finish
endif
let current_compiler = 'jing'

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=%E%f:%l:%c:\ error:\ %m,
		    \%-Z%p^,
		    \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

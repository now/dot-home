" Vim compiler file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-07-20

if exists("current_compiler")
  finish
endif
let current_compiler = "xmllint"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=xmllint\ --noout

CompilerSet errorformat=%E%f:%l:\ parser\ error\ :\ %m,
		    \%W%f:%l:\ warning\ :\ %m,
		    \%E%f:%l:\ validity\ error\ :\ %m,
		    \%W%f:%l:\ validity\ warning\ :\ %m,
		    \%-Z%p^,
		    \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

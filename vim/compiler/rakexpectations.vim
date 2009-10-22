" Vim compiler file
" Compiler:         Rake with expectations test framework
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2009-10-19

if exists('current_compiler')
  finish
endif
let current_compiler = 'rakexpectations'

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=rake\ $*

CompilerSet errorformat=
      \%E%f:%l:%m,
      \%-Z%^%$,
      \%C%m,
      \%+CGot\ %\\+%m,
      \%E%f:%l:in\ %*[^:]:\ %m,
      \%-C%\tfrom\ %f:%l:in\ %.%#,
      \%-Z%\tfrom\ %f:%l,
      \%D(in\ %f),
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

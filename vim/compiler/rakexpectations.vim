" Vim compiler file
" Compiler:         Rake with expectations test framework
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2009-10-01

if exists('current_compiler')
  finish
endif
let current_compiler = 'rakexpectations'

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=rake\ $*

CompilerSet errorformat=
      \%E%f:%l:\ %m,
      \%-Z%.%#String\ details:%.%#,
      \%-Z%^%$,
      \%C%m,
      \%E%f:%l,
      \%+CExpected\ %m,
      \%+CGot\ %\\+%m,
      \%D(in\ %f)

let &cpo = s:cpo_save
unlet s:cpo_save

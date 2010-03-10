" Vim compiler file
" Compiler:         Rake with expectations test framework
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-03-10

if exists('current_compiler')
  finish
endif
let current_compiler = 'rakexpectations'

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=rake\ $*\ 2>&1\ \\\|\ sed\ 's///g'

CompilerSet errorformat=
      \%+E%f:%l:\ parse\ error,
      \%W%f:%l:\ warning:\ %m,
      \%E%f:%l:in\ %*[^:]:\ %m,
      \%E%f:%l:\ %m,
      \%-C%\tfrom\ %f:%l:in\ %.%#,
      \%-Z%\tfrom\ %f:%l,
      \%-Z%p^,
      \%D(in\ %f),
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

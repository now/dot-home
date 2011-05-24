" Vim compiler file
" Compiler:         Rake with lookout test framework
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-05-24

if exists('current_compiler')
  finish
endif
let current_compiler = 'rakelookout'

let s:cpo_save = &cpo
set cpo-=C

if $OS == 'Windows_NT'
  CompilerSet makeprg=rake\ -s\ $*\ 2>&1\ \\\|\ sed\ 's///g'
else
  CompilerSet makeprg=rake\ -s\ $*\ 2>&1
endif

CompilerSet errorformat=
      \%+E%f:%l:\ parse\ error,
      \%W%f:%l:\ warning:\ %m,
      \%E%f:%l:in\ %*[^:]:\ %m,
      \%E%f:%l:\ %m\ (SyntaxError),
      \%-C%%.%#,
      \%-Z%p^,
      \%E%f:%l:\ %m,
      \%-C%\tfrom\ %f:%l:in\ %.%#,
      \%-Z%\tfrom\ %f:%l,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save

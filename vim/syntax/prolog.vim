" Vim syntax file
" Language:         Prolog
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword prologTodo    contained TODO FIXME XXX NOTE

syn region  prologComment start='/\*' end='\*/' contains=prologTodo,@Spell
syn region  prologComment display oneline start='%' end='$'
                          \ contains=prologTodo,@Spell
syn match   prologCommentError  '\*/'

syn match   prologNumber  '\<\d\+\>'

syn region  prologString  start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match   prologChar    +0'\\\=.+
syn region  prologAtom    start=+'+ skip=+\\\\\|\\'+ end=+'+

syn match   prologOperator  '[|!<>=]\|=\=<\|>=\=\|\\\===\=\|=[\:]=\|\\+'
syn match   prologAsIs      '\\\====\|<=\|=>'

syn match   prologDelimiter '[].[]'

syn match   prologConditional ':-\|;'

syn match   prologClauseHead  '^\l[^(]*('me=e-1

exec 'syn sync ccomment prologComment minlines='
      \ . (exists('g:prolog_minlines') ? g:prolog_minlines : 25)

hi def link prologTodo         Todo
hi def link prologComment      Comment
hi def link prologCommentError Error
hi def link prologNumber       Number
hi def link prologString       String
hi def link prologChar         SpecialChar
hi def link prologAtom         String
hi def link prologOperator     Operator
hi def link prologAsIs         None
hi def link prologDelimiter    Delimiter
hi def link prologConditional  Conditional
hi def link prologClauseHead   Function

let b:current_syntax = "prolog"

let &cpo = s:cpo_save
unlet s:cpo_save

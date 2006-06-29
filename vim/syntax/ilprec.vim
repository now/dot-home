" Vim syntax file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-06-29

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn match   ilprecBegin         display '^' nextgroup=ilprecKey

syn match   ilprecKey           display contained '\w\+'
                                \ nextgroup=ilprecEqual

syn match   ilprecEqual         display contained '='
                                \ nextgroup=ilprecValue

syn match   ilprecValue         display contained '.*'
                                \ contains=ilprecSpaceError

syn match   ilprecSpaceError    display contained '\s\+$'

hi def link ilprecKey           Identifier
hi def link ilprecEqual         Delimiter
hi def link ilprecSpaceError    Error

let b:current_syntax = "ilprec"

let &cpo = s:cpo_save
unlet s:cpo_save

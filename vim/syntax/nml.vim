" Vim syntax file
" Language:         nml
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-02-20

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

runtime! syntax/xml.vim

let b:current_syntax = 'nml'

let &cpo = s:cpo_save
unlet s:cpo_save

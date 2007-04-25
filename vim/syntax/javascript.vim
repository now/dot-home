" Vim syntax file
" Language:         JavaScript
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2007-04-25

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword javascriptTodo          contained TODO FIXME XXX NOTE

syn region  javascriptString        start=+"+ skip=+\\\\\|\\"+ end=+"+
                                    \ contains=javascriptSpecialChar
syn region  javascriptString        start=+'+ skip=+\\\\\|\\'+ end=+'+
                                    \ contains=javascriptSpecialChar
syn match  javascriptRegex          '/[^/\\]*\%(\\.[^/\\]*\)*/[gim]*'
                                    \ contains=javascriptRegexSpecial

syn match   javascriptComment       display '//.*' contains=javascriptTodo
syn region  javascriptComment       start='/\*' end='\*/'
                                    \ contains=javascriptTodo

syn match   javascriptSpecialChar   contained display
                                    \ +\\\o\o\o\|\\[\\bfnrt'"]+
syn match   javascriptRegexSpecial  contained display '\\.'

syn match   javascriptNumber        display '[+-]\=\<\d\+\>'
syn match   javascriptNumber        display '[+-]\=\<0[xX]\x\+\>'
syn match   javascriptOctal         display '[+-]\=\<0\o\+\>'
                                    \ contains=javascriptOctalZero
syn match   javascriptOctalZero     display contained '\<0'
syn match   javascriptOctalError    display "[+-]\=\<0\o*[89]\d*\>"
syn match   javascriptFloat	    display
                                    \ "[+-]\=\<\d\+\.\d*\%([eE][+-]\=\d\+\>\)\="
syn match   javascriptFloat	    display
                                    \ "[+-]\=\.\d\+\%([eE][+-]\=\d\+\)\=\>"
syn match   javascriptFloat	    display "[+-]\=\<\d\+[eE][-+]\=\d\+\>"

syn keyword javascriptBoolean       true false

syn keyword javascriptConditional   if else switch

syn keyword javascriptRepeat        for do in while

syn keyword javascriptStatement     break continue function return with

syn keyword javascriptLabel         case default label

syn keyword javascriptOperator      new delete instanceof typeof

syn keyword javascriptException	    try catch finally throw

syn keyword javascriptStorageClass  var const

syn keyword javascriptConstant      null undefined

syn keyword javascriptIdentifier    arguments this document

syn keyword javascriptPreProc       export import

syn keyword javascriptType          Array Boolean Date Function Number
                                    \ Object String RegExp

syn keyword javascriptReserved      abstract boolean byte char class
                                    \ debugger double enum extends final
                                    \ float goto implements import int
                                    \ interface long native package private
                                    \ protected public short static super
                                    \ synchronized transient void volatile

exec 'syn sync ccomment javascriptComment minlines='
      \ . (exists('g:javascript_minlines') ? g:javascript_minlines : 25)

hi def link javascriptTodo          Todo
hi def link javascriptComment       Comment
hi def link javascriptString        String
hi def link javascriptRegex         String
hi def link javascriptSpecialChar   SpecialChar
hi def link javascriptRegexSpecial  SpecialChar
hi def link javascriptNumber        Number
hi def link javascriptOctal         Number
hi def link javascriptOctalZero     SpecialChar
hi def link javascriptOctalError    Error
hi def link javascriptFloat         Number
hi def link javascriptBoolean       Boolean
hi def link javascriptConditional   Conditional
hi def link javascriptRepeat        Repeat
hi def link javascriptStatement     Statement
hi def link javascriptLabel         Label
hi def link javascriptOperator      Operator
hi def link javascriptException     Exception
hi def link javascriptStorageClass  StorageClass
hi def link javascriptConstant      Constant
hi def link javascriptIdentifier    Identifier
hi def link javascriptPreProc       PreProc
hi def link javascriptType          Type
hi def link javascriptReserved      Error

let b:current_syntax = "javascript"

let &cpo = s:cpo_save
unlet s:cpo_save

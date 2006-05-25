" Vim syntax file
" Language:         Ruby
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

let s:cpo_save = &cpo
set cpo&vim

syn match   rubyScopeSpecifier          display
      \                                 containedin=rubyClassVariable,
      \                                             rubyInstanceVariable,
      \                                             rubyGlobalVariable,
      \                                             rubyPredefinedVariable
      \                                 contained '\%(@\+\|\$\)'

syn keyword rubyControl                 returning
syn keyword rubyDefine                  def_delegator def_delegators
syn keyword rubyDefine                  add_attr_accessor
syn keyword rubyDefine                  needs
syn keyword rubyKeyword                 alias_method

unlet! b:current_syntax
syn include @ConTeXt                    syntax/context.vim
syn region  rubyDocComment              start='^\s*\zs#\s*¶'
      \                                 end='^\%(\s*\)\@>\%([^#]\|$\)\@='
      \                                 contains=rubyTodo,@Spell,@ConTeXt
let b:current_syntax = "ruby"

hi link rubyDocComment                  rubyComment
hi link rubyStringDelimiter	        String
hi link rubyClassVariable               Normal
hi link rubyInstanceVariable            Normal
hi link rubyScopeSpecifier              Identifier

let &cpo = s:cpo_save
unlet s:cpo_save

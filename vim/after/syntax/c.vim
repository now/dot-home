" Vim syntax file
" Language:         C
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-06-19

let s:cpo_save = &cpo
set cpo&vim

syn keyword	cType	      gboolean gpointer gconstpointer gchar guchar
syn keyword	cType	      gint guint gshort gushort glong gulong
syn keyword	cType	      gint8 guint8 gint16 guint16 gint32 guint32
syn keyword	cType	      gint64 guint64 gfloat gdouble gsize gssize
syn keyword	cType	      gunichar gunichar2

syn keyword	cType	      unichar

syn keyword	cOperator     lengthof offsetof

syn keyword	cPreProc      assert

syn keyword	cTodo	      NOTE

syn keyword	cConstant     TRUE FALSE

syn clear	cErrInParen

syn match	cFunctionDefinition '^\h\w*\s*(\@='

unlet! b:current_syntax
syn include     @ConTeXt      syntax/context.vim
syn region      cDocComment   matchgroup=cCommentStart
      \                       start='/\*¶' end='\*/'
      \                       contains=@cCommentGroup,cCommentStartError,
      \                                cSpaceError,@Spell,@ConTeXt
let b:current_syntax = "c"

hi def link cFunctionDefinition Identifier
hi def link cDocComment         cComment

let &cpo = s:cpo_save
unlet s:cpo_save

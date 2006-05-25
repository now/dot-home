" Vim syntax file
" Language:         C
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

let s:cpo_save = &cpo
set cpo&vim

syn keyword	cType	      gboolean gpointer gconstpointer gchar guchar
syn keyword	cType	      gint guint gshort gushort glong gulong
syn keyword	cType	      gint8 guint8 gint16 guint16 gint32 guint32
syn keyword	cType	      gint64 guint64 gfloat gdouble gsize gssize
syn keyword	cType	      gunichar gunichar2

syn keyword	cType	      uchar uint ushort ulong pointer unichar
syn keyword	cType	      constpointer

syn keyword	cRepeat	      loop until

syn keyword	cConditional  unless when

syn keyword	cOperator     lengthof new_struct new_array str_new new_cleared
syn keyword	cOperator     new_uncleared_array offsetof

syn keyword	cOperator     throw throwf throwc throw_const rethrow

syn keyword	cPreProc      verify verify_not_reached
syn keyword	cPreProc      return_unless return_value_unless
syn keyword	cPreProc      raise_unless raise_value_unless

syn keyword	cPreProc      assert

syn keyword	cTodo	      NOTE

syn keyword	cConstant     null
syn keyword	cConstant     NUL
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

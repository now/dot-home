" Vim filetype file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2008-07-19

let s:cpo_save = &cpo
set cpo&vim

setlocal comments=s:<!--,m:\ \ \ \ \ ,e:--> formatoptions-=t formatoptions+=croql

setlocal sw=2 sts=2

" Doesn’t work as 'include' only gets one line at a time.
"setlocal include=<!ENTITY\\_s\\+%\\_s\\+\\(\\S\\+\\)\\_s\\+\\%(PUBLIC\\_s\\+\\%(\"[^\"]\\+\"\\\|'[^']\\+'\\)\\\|SYSTEM\\)\\_s\\+\\zs\\%(\"[^\"]\\+\"\\\|'[^']\\+'\\)\\ze\\_s*>\\_.*%\\1;
"setlocal includeexpr=substitute(v:fname,'^\\%(\"\\(.*\\)\"\\\|''\\(.*\\)''\\)$','\\1\\2','')
"setlocal include=^\\s*%\\zs.*\\ze;\\s*$
"setlocal includeexpr=DTDIncludeExpr()
" TODO: Add other things that can be defined.
setlocal define=<!\\%(ENTITY\\\|ELEMENT\\)\\_s\\+\\%(%\\_s\\+\\)\\=

function! DTDIncludeExpr()
  let saved_pos = getpos('.')
  let [lnum, col] = searchpos('<!ENTITY\_s\+%\_s\+' . v:fname . '\_s\+\%(PUBLIC\_s\+\%("[^"]\+"\|''[^'']\+''\)\|SYSTEM\)\_s\+\zs\%("[^"]\+"\|''[^'']\+''\)\ze\_s*>')
  if lnum == 0
    return ""
  endif
  call setpos('.', saved_pos)

  return strpart(getline(lnum), col, col + strlen(v:fname) + 1)
endfunction

let b:undo_ftplugin .= ' | setl com< fo< sw< sts< inc< inex<'

let &cpo = s:cpo_save

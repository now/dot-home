" Vim filetype plugin file
" Language:	    XML
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

setlocal shiftwidth=2 softtabstop=2
setlocal formatoptions-=t formatoptions+=croql

inoremap <buffer> <expr> / <SID>FindTag()

let b:undo_ftplugin .= ' | setl sw< sts< fo< | iunmap <buffer> /'

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:FindTag()
  let col = col('.')
  let line = getline('.')
  if line[col - 2] != '<' || strpart(line, col - 1)
	\ =~ '^[^? \t>/]\+\%(\_s*>\|\_s\+\_[^>]*[^/]>\)'
    return '/'
  endif
  if searchpair('<[^!? \t>/]\+\%(\_s*>\|\_s\+\_[^>]*[^/]>\)', '',
	      \ '</[^? \t>/]\+>', 'bW') > 0
    let line = strpart(getline('.'), col('.'))
    let tag = substitute(line, '\([^? \t>/]\+\).*$', '\1', '')
    return '/' . tag . '>'
  endif
  return '/'
endfunction

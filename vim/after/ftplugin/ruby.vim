" Vim filetype plugin file
" Language:	    Ruby
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-04-21

setlocal shiftwidth=2 softtabstop=2 expandtab
setlocal path+=.;

inoremap <buffer> <CR> <C-O>:call <SID>CompleteStatement()<CR><CR>

omap <buffer> <silent> ac <Esc>:call <SID>SelectAComment()<CR>
omap <buffer> <silent> ic <Esc>:call <SID>SelectInnerComment()<CR>

nnoremap <buffer> <silent> <Leader>t <Esc>:call <SID>GoToOtherFile()<CR>
nnoremap <buffer> <silent> <Leader>M <Esc>:execute 'make' 'TEST=' . shellescape(expand('%')) 'LINE=' . line('.')<CR>

"inoremap <buffer> ( (<C-O>:call <SID>InsertParentheses()<CR><C-O>l
"
"function! s:InsertParentheses()
"  if strpart(getline('.'), col('.')) == ""
"    call setline(line('.'), getline('.') . ')')
"  endif
"endfunction

compiler rakexpectations

set includeexpr='lib/'.substitute(substitute(v:fname,'::','/','g'),'$','.rb','')

let b:undo_ftplugin .= ' | setl sw< sts< et< | iunmap <buffer> <CR>'
let b:undo_ftplugin .= ' | ounmap <buffer> ac | ounmap <buffer> ic'
let b:undo_ftplugin .= ' | nunmap <buffer> <Leader>t | nunmap <buffer> <Leader>M'

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:CompleteStatement()
  let view = winsaveview()
  if !search('^\s*\zs\%(begin\|case\|class\|def\|for\|if\|module\|unless\|until\|while\)\>\|\%(do\|{\)\%(\s*|[^|]*|\s*\)\=$',
           \ 'bcnW', line('.') - 1)
    return
  endif
  let start_pos = getpos('.')
  let start_indent = indent('.')
  let word = expand('<cword>')
  if word == ""
    let word = '{'
  endif
  normal %
  let end_pos = getpos('.')
  let end_indent = indent('.')
  call winrestview(view)
  if end_pos != start_pos && end_indent == start_indent
    return
  endif
  call append(line('.'), repeat(' ', start_indent) . (word == '{' ? '}' : 'end'))
endfunction

" TODO: make this into a script instead and have each filetype that wants to
" use it define an object that we call into.  Comments, expressions, and so on
" would be the main targets, I suppose.
function s:FindNonMatchingLine(pattern, ...)
  let flags = a:0 > 0 ? a:1 : ''
  let backwards = flags =~ 'b'
  let finalline = backwards ? 0 : line('$') + 1
  let stopline = a:0 >  1 ? a:2 : finalline
  let delta = backwards ? -1 : 1
  let lnum = line('.')
  while lnum != stopline
    let line = getline(lnum)
    if line !~ a:pattern
      return lnum
    endif
    let lnum += delta
  endwhile
  if lnum != finalline
    if getline(lnum) !~ a:pattern
      return lnum
    else
      return 0
    endif
  else
    return 0
  else
endfunction

function s:FindDocumentationCommentRange()
  let save_cursor = getpos('.')
  let lnum = line('.')
  let line = getline(lnum)
  if line =~ '=begin'
    call cursor(0, col('$'))
  elseif line =~ '=end'
    call cursor(line('.') - 1, col('$'))
  endif

  let [begin, _] = searchpairpos('=begin', '', '=end', 'bnW')
  if begin == 0
    return [0, 0]
  endif

  let [end, _] = searchpairpos('=begin', '', '=end', 'cnW')
  if end == 0
    return [0, 0]
  endif

  call cursor(save_cursor)

  return [begin, end]
endfunction

function s:FindLineCommentSetRange()
  if getline('.') !~ '^\s*#'
    return [0, 0]
  endif

  let begin = s:FindNonMatchingLine('^\s*#', 'b') + 1
  let end = s:FindNonMatchingLine('^\s*#')
  let end = end == 0 ? line('$') : end - 1
  return [begin, end]
endfunction

function s:SelectAComment()
  let range = s:FindDocumentationCommentRange()
  if range[0] == 0
    let range = s:FindLineCommentSetRange()
    if range[0] == 0
      return
    endif
  endif

  call cursor(range[0], 1)
  normal! V
  call cursor(range[1], 1)
endfunction

function s:SelectInnerComment()
  let range = s:FindDocumentationCommentRange()
  if range[0] != 0
    let range[0] += 1
    let range[1] -= 1
    if range[0] > range[1]
      call append(range[1], "")
      call cursor(range[1] + 1, 1)
      normal! V
      return
    endif

    call cursor(range[0], 1)
    normal! V
    call cursor(range[1], 1)
  else
    let range = s:FindLineCommentSetRange()
    if range[0] == 0
      return
    endif

    call cursor(range[0], matchend(getline(range[0]), '#\s*') + 1)
    normal! v
    call cursor(range[1], 1)
    call cursor(0, col('$') - (&selection == 'inclusive' ? 1 : 0))
  endif
endfunction

function s:GoToOtherFile()
  if s:GoToFile('\%(^\|.*/\)test\(/.\+\)', 'lib') ||
   \ s:GoToFile('\%(^\|.*/\)lib\(/.\+\)', 'test')
    return
  endif
  echoerr "E447: Can't find alternate file"
endfunction

function s:GoToFile(pattern, new_head)
  let path = expand('%')
  let target = substitute(path, a:pattern, a:new_head . '\1', "")
  if target == path
    return 0
  endif
  execute 'edit ' . target
  return 1
endfunction

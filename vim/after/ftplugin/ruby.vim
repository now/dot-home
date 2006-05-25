" Vim filetype plugin file
" Language:	    Ruby
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

setlocal shiftwidth=2 softtabstop=2 expandtab
setlocal path+=.;

inoremap <buffer> <expr> <CR> <SID>CompleteStatement()

omap <buffer> <silent> ac :call <SID>SelectAComment()<CR>
omap <buffer> <silent> ic :call <SID>SelectInnerComment()<CR>

"inoremap <buffer> ( (<C-O>:call <SID>InsertParentheses()<CR><C-O>l
"
"function! s:InsertParentheses()
"  if strpart(getline('.'), col('.')) == ""
"    call setline(line('.'), getline('.') . ')')
"  endif
"endfunction

let b:undo_ftplugin .= ' | setl sw< sts< et< | iunmap <buffer> <CR>'
let b:undo_ftplugin .= ' | ounmap <buffer> ac | ounmap <buffer> ic'

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:CompleteStatement()
  " TODO: we can check contents of ". register to make sure that they were
  " inserted while typing here...
  if getline('.') =~ '^\s*\%(begin\|case\|class\|def\|for\|if\|module\|unless\|until\|while\)\>\|do\%(\s*|[^|]*|\s*\)\=$'
    let n = indent('.')
    let ind = ''
    let i = 0
    while i < n
      let ind .= ' '
      let i += 1
    endwhile
    call append(line('.'), ind . 'end')
  endif
  return "\n"
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

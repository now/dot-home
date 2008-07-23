" Vim macro file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2008-07-23

if argc() > 0
  let s:i = 0
  while 1
    if filereadable(argv(s:i))
      if s:i != 0
        sleep 3
      endif
      break
    endif
    if isdirectory(argv(s:i))
      echomsg "Skipping directory " . argv(s:i)
    elseif getftime(argv(s:i)) < 0
      echomsg "Skipping non-existing file " . argv(s:i)
    else
      echomsg "Skipping unreadable file " . argv(s:i)
    endif
    echo "\n"
    let s:i = s:i + 1
    if s:i == argc()
      quit
    endif
    next
  endwhile
  unlet s:i
endif

set viminfo=

au VimEnter * set nomodified

noremap <buffer> <silent> q <Esc>:q<CR>
noremap <buffer> v <Esc>:call <SID>start_edit_mode()<CR>

function s:start_edit_mode()
  setlocal modifiable
  unmap <buffer> q
  unmap <buffer> v
  setlocal viminfo<
  echomsg "Buffer is now editable"
endfunction

setlocal nomodifiable

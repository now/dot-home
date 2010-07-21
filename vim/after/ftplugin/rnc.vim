" Vim filetype plugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-07-21

setlocal softtabstop=2 shiftwidth=2

command! -nargs=1 RNCElement call s:rnc_element(<f-args>)

function! s:rnc_element(name)
  call append('.', [
  \   printf('%s = element %s { %s.attlist, %s.content }',
  \          a:name, a:name, a:name, a:name),
  \   printf('%s.attlist =', a:name),
  \   printf('%s.content =', a:name)
  \ ])
  call cursor(line('.') + 2, 0)
endfunction

if exists('b:undo_plugin') && b:undo_plugin != ""
  let b:undo_plugin .= ' | '
else
  let b:undo_plugin = ""
endif

let b:undo_plugin .= ' | setl sts< sw< | delcommand RNCElement'

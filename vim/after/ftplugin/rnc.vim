" Vim filetype plugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-09-01

setlocal softtabstop=2 shiftwidth=2

command! -nargs=+ RNCElement call s:rnc_element(<f-args>)

function! s:rnc_element(...)
  let i = 0
  let attributes = 1
  let content = 1
  while i < len(a:000)
    if a:000[i][0] != '-'
      break
    endif
    if a:000[i] == '-a'
      let attributes = 0
    elseif a:000[i] == '-c'
      let content = 0
    else
      break
    endif
    let i += 1
  endwhile
  let name = a:000[i]
  let lines = []
  let rncontent = []
  if attributes
    call add(rncontent, printf('%s.attlist', name))
    call add(lines, printf('%s.attlist =', name))
  endif
  if content
    call add(rncontent, printf('%s.content', name))
    call add(lines, printf('%s.content =', name))
  else
    call add(rncontent, 'text')
  endif
  call insert(lines, printf('%s = element %s { %s }',
        \                   name, name, join(rncontent, ', ')))
  call add(lines, "")
  call append('.', lines)
  call cursor(line('.') + 2, 0)
endfunction

if exists('b:undo_plugin') && b:undo_plugin != ""
  let b:undo_plugin .= ' | '
else
  let b:undo_plugin = ""
endif

let b:undo_plugin .= ' | setl sts< sw< | delcommand RNCElement'

" Vim filetype plugin file
" Language:	    Treetop
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-07-26

setlocal shiftwidth=2 softtabstop=2 expandtab
let b:undo_ftplugin .= ' | setl sw< sts< et<'

compiler rakelookout

nnoremap <buffer> <silent> gf <Esc>:call <SID>go_to_def_file()<CR>
let b:undo_ftplugin .= ' | nunmap <buffer> gf'

nnoremap <buffer> <silent> <Leader>t <Esc>:call <SID>go_to_other_file()<CR>
let b:undo_ftplugin .= ' | nunmap <buffer> <Leader>t'

nnoremap <buffer> <silent> <Leader>M <Esc>:call <SID>run_test()<CR>
let b:undo_ftplugin .= ' | nunmap <buffer> <Leader>M'

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:go_to_def_file()
  let path = s:find_path_around_cursor()
  if path == ""
    echoerr "E447: Can't find file under cursor in path"
    return
  endif
  execute 'edit' path
endfunction

function s:find_path_around_cursor()
  let path = expand('<cfile>')
  let const = s:find_constant_path_around_cursor()
  for test in [path, s:libify(path), s:libify(const), s:treeify(path), s:treeify(const)]
    if filereadable(test)
      return test
    endif
  endfor
  return ""
endfunction

function s:libify(path)
  return printf('lib/%s.rb', a:path)
endfunction

function s:treeify(path)
  return printf('lib/%s.treetop', a:path)
endfunction

function s:find_constant_path_around_cursor()
  let [lnum, col] = searchpos('^\|[^A-Za-z0-9:]', 'bcnW', line('.'))
  if lnum == 0
    return ""
  endif
  let [end_lnum, end_col] = searchpos('$\|[^A-Za-z0-9:]', 'cnW', line('.'))
  if end_lnum == 0
    return ""
  endif
  let part = strpart(getline('.'), col, end_col - col)
  return substitute(tolower(part), '::', '/', 'g')
endfunction

function s:go_to_other_file()
  if s:go_to_other_file1(substitute(expand('%'), '\.treetop$', '.rb', ""))
    return
  endif
  echoerr "E447: Can't find alternate file"
endfunction

function s:go_to_other_file1(path)
  for [what, with] in [['test/unit', 'lib'], ['lib', 'test/unit']]
    if s:go_to_file(a:path, '\%(^\|.*/\)' . what . '\(/.\+\)', with)
      return 1
    endif
  endfor
  return 0
endfunction

function s:go_to_file(path, pattern, new_head)
  let target = substitute(a:path, a:pattern, a:new_head . '\1', "")
  if target == a:path
    return 0
  endif
  execute 'edit ' . target
  return 1
endfunction

function! s:run_test()
  let test = substitute(expand('%'), '\.treetop$', '.rb', '')
  let line = 'LINE=' . line('.')
  if test =~ '^lib/'
    let test = substitute(test, '^lib/', 'test/', '')
    let line = ""
  endif
  execute 'make' 'TEST=' . shellescape(test) line
endfunction

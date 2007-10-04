" Vim filetype plugin file 
" Language:	    VimL
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2007-09-28

setlocal softtabstop=2 shiftwidth=2

command! -nargs=0 -buffer GenerateHighlightDefaultLinks
      \ call s:GenerateHighlightDefaultLinks()

let b:undo_ftplugin .= ' | setl sts< sw<'
      \ . ' | delc GenerateHighlightDefaultLinks'
      \ . ' | augroup filetype-plugin-vim | autocmd! | augroup end'

augroup filetype-plugin-vim
  autocmd BufWritePre,FileWritePre <buffer> call s:update_syntax_definitions()
augroup end

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function s:GenerateHighlightDefaultLinks()
  1
  let groups = []
  while search('^syn\s\+\%(keyword\|region\|match\)', 'W') != 0
    let group = substitute(getline('.'),
                         \ '^syn\s\+\%(keyword\|region\|match\)\s\+\(\w\+\).*$',
                         \ '\1', '')
    if index(groups, group) != -1
      continue
    endif
    let groups = add(groups, group)
  endwhile
  $
  let lnum = line('.')
  if search('^\s*let b:current_syntax\s*=\s*[''"][^''"]*[''"]', 'bc')
    let lnum = line('.') - 2
  endif
  let cursor_lnum = lnum
  let conversions = [ ['Path$', 'String'],
        \ ['Variable$', 'Identifier'],
        \ ['Regex$', 'String'],
        \ ['URL$', 'String'],
        \ ['Format$', 'String']
        \ ]
  let ignores = [ 'Begin$' ]
  let longest_length = 0
  for group in groups
    let length = strlen(group)
    if length > longest_length
      let longest_length = length
    endif
  endfor
  if longest_length % 2 == 0
    let longest_length += 1
  endif
  for group in groups
    let ignore = 0
    for pattern in ignores
      if group =~ pattern
        let ignore = 1
        break
      endif
    endfor
    if ignore
      continue
    endif

    let to = substitute(group, 
                      \ '^.*\(Comment\|Constant\|String\|Character\|Number\|' .
                      \ 'Boolean\|Float\|Identifier\|Function\|Statement\|' .
                      \ 'Conditional\|Repeat\|Label\|Operator\|Keyword\|' .
                      \ 'Exception\|PreProc\|Include\|Define\|Macro\|' .
                      \ 'PreCondit\|Type\|StorageClass\|Structure\|Typedef\|' .
                      \ 'Special\|SpecialChar\|Tag\|Delimiter\|' .
                      \ 'SpecialComment\|Debug\|Underlined\|Ignore\|Error\|' .
                      \ 'Todo\)$',
                      \ '\1', '')
    if to == group
      let new_to = ""
      for [pattern, substitution] in conversions
        if to =~ pattern
          let new_to = substitution
          break
        endif
      endfor
      let to = new_to
    endif
    call append(lnum, 'hi def link ' . group . repeat(' ', 1 + (longest_length - strlen(group))) . to)
    let lnum += 1
  endfor
  call cursor(cursor_lnum, 1)
endfunction

function s:update_syntax_definitions()
  if getline(1) !~ '^" Vim syntax file$'
    return
  endif

  let name = expand('%:t:r')

  let saved_cursor = getpos('.')
  silent! execute 'keepjumps /^let b:current_syntax = [''"].*\%('.name.'\)\@<![''"]/s/[''"].*[''"]/\="''".name."''"'
  execute 'silent! keepjumps v/^\%(\s*syn\%[tax]\s\+\%(keyword\|match\|region\|cluster\)\s\+\l*\)\@!\|^\s*syn\%[tax]\s\+\%(keyword\|match\|region\|cluster\)\s\+'.name.'/s/^\s*syn\%[tax]\s\+\(keyword\|match\|region\|cluster\)\s\+\l*/\="syn ".submatch(1).repeat(" ", 8 - len(submatch(1))).name'
  call setpos('.', saved_cursor)
endfunction

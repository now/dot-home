" Vim filetype plugin file 
" Language:	    VimL
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2007-06-24

let b:pcp_header_author_regex = '^\(.\{1,3}\<Maintainer\>\s*:\s*\).*$'
let b:pcp_header_file_description_format = '%s'

setlocal softtabstop=2 shiftwidth=2

command! -nargs=0 -buffer GenerateHighlightDefaultLinks
      \ call s:GenerateHighlightDefaultLinks()

let b:undo_ftplugin .= ' | setl sts< sw<'
      \ . ' | unlet! b:pcp_header_file_description_format'
      \ .          ' b:pcp_header_author_regex'
      \ . ' | delc GenerateHighlightDefaultLinks'

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
  if search('^\s*let b:current_syntax\s*=\s*[''"][^''"]\+[''"]', 'bc')
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

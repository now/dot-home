" Vim filetype plugin file 
" Language:	    VimL
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

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
  let conversions = [ ['Path$', 'String'], ['Variable$', 'Identifier'] ]
  for group in groups
    let to = substitute(group, 
                      \ '^.*\%(Comment\|Constant\|String\|Character\|Number\|' .
                      \ 'Boolean\|Float\|Identifier\|Function\|Statement\|' .
                      \ 'Conditional\|Repeat\|Label\|Operator\|Keyword\|' .
                      \ 'Exception\|PreProc\|Include\|Define\|Macro\|' .
                      \ 'PreCondit\|Type\|StorageClass\|Structure\|Typedef\|' .
                      \ 'Special\|SpecialChar\|Tag\|Delimiter\|' .
                      \ 'SpecialComment\|Debug\|Underlined\|Ignore\|Error\|' .
                      \ 'Todo\)$',
                      \ '\1', '')
    if to == group
      for [pattern, substitution] in conversions
        let to = (to =~ pattern ? substitution : "")
      endfor
    endif
    call append(lnum, 'hi def link ' . group . ' ' . to)
    let lnum += 1
  endfor
endfunction

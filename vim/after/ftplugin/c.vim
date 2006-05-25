" Vim filetype plugin file
" Langugage:	    C
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

setlocal foldtext=CFoldText()

setlocal comments=s0:/*¶,mb:\ ,e:*/,s1:/*,mb:*,ex:*/,://

command! -nargs=0 -range -buffer FixMacroLineEnds
      \ call s:CFixMacroLineEnds(<line1>, <line2>)

compiler gcc

let b:undo_ftplugin .= " | setl fdt< com< | delc FixMacroLineEnds"

if exists('s:did_load')
  finish
endif
let s:did_load = 1

function CFoldText()
  let end = line('$')
  let lnum = v:foldstart
  let line = getline(lnum)
  while line !~ '\*/' && lnum <= end
    let lnum = lnum + 1
    let line = getline(lnum)
  endwhile
  let lnum = lnum + 1
  let line = getline(lnum)
  if line =~ '^\(\s*\(static\|inline\|const\)\s\+\)*\(\s*\h\w*\(\s\+\*\)\=\(\s\+\h\w*\(\s\+\*\)\=\)*\)$'
    let line = getline(lnum + 1)
    let func = substitute(line, '^\s*\(\h\w*\).*$', '\1', '')
  elseif line =~ '^\s*struct'
    let func = substitute(line, '^\s*\(struct\s\+\h\w*\).*$', '\1', '')
  elseif line =~ '^\(\s*\(static\|const\)*\s\+\)*\(\s*\h\w*\(\s\+\*\)\=\(\s\+\h\w*\(\s\+\*\)\=\)*\)\s*=\s*.\+$'
    let func = substitute(line, '^\(\s*\(static\|const\)*\s\+\)*\(\s*\h\w*\(\s\+\*\)\=\(\s\+\h\w*\(\s\+\*\)\=\)*\)\s*=\s*.\+$', '\3', '') . ': '
  elseif line =~ '^#define\s\h\w*'
    let func = substitute(line, '^#define\s\+\(\h\w*\).*$', '\1', '')
  elseif line =~ '^typedef.*\h\w*;$'
    let func = substitute(line, '^typedef.\{-}\(\h\w*\);$', '\1', '')
  else
    let func = ''
  endif

  let mrks = escape(&foldmarker, '.*~^$\')
  let rmrks = substitute(mrks, '^\([^,]\+\),', '\1\\d*\\|,', '')
  let rmrks = rmrks . substitute(mrks, ',\(.\+\)$', '\1\\d*', '')
  let cms = substitute(escape(&commentstring, '.*~^$\'), '%s', '\\|', '')
  let remove = rmrks . '\|' . cms . '\|^\s*\*\|^\s\+\|\s\+$\|:$'

  let w = winwidth(winnr()) < &columns ? winwidth(winnr()) : &columns

  let lnum = nextnonblank(v:foldstart)
  let l = getline(lnum)
  let line = substitute(l, remove, '', 'g')
  if l !~ '\*/'
    while strlen(line) < w
      let lnum = nextnonblank(lnum + 1)
      let l = getline(lnum)
      let line = line . substitute(l, remove, '', 'g')
      if l =~ '\*/'
	break
      endif
    endwhile
  endif
  let line = substitute(line, '/$', '', '')

  let func = substitute(func, '^\s\+\|\s\+$', '', 'g')
  if func != ''
    let func = func . ': '
  endif
  let text = '§ ' . func . substitute(line, '^\s\+\|\s\+$', '', 'g')
  "let w = winwidth(winnr()) < &columns ? winwidth(winnr()) : &columns
  while strlen(text) >= w
    let old_text = text
    let text = substitute(text, '\s\+[^[:space:]]*$', '…', '')
    if text == old_text
      let text = substitute(text, '^\(.\{'.w - 1.'}\).*$', '\1…', '')
      if strlen(text) >= w
	let text = substitute(text, '^\(.\{'.w - 1.'}\).*$', '\1', '')
      endif
    endif
  endwhile

  return text
endfunction

function s:GetStringWidth(str)
  let width = 0
  let i = 0
  let n = strlen(a:str)
  while i < n
    if a:str[i] != "\t"
      let width = width + 1
    else
      let width = width + &ts - (width % &ts)
    endif
    let i = i + 1
  endwhile
  return width
endfunction

function s:CFixMacroLineEnds(first, last)
  let i = a:first
  let widest = -1
  while i < a:last
    let line = substitute(getline(i), '\\$', '', '')
    let line = substitute(line, '\s\+$', '', '')
    let width = s:GetStringWidth(line)
    if width > widest
      let widest = width
    endif
    let i = i + 1
  endwhile
  let width = widest + &ts - (width % &ts)
  if width > &tw
    let width = &tw
  endif
  let i = a:first
  while i < a:last
    let line = substitute(getline(i), '\s*\\$', '', '')
    while s:GetStringWidth(line) < width
      let line = line . "\t"
    endwhile
    let line = line . "\\"
    call setline(i, line)
    let i = i + 1
  endwhile
endfunction

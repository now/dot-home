" Vim syntax file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-10-21

let s:cpo_save = &cpo
set cpo&vim

let s:specials_defaults = {
      \ 'lc': "\e[",
      \ 'rc': 'm',
      \ 'ec': "",
      \ 'no': '0',
      \ 'fi': '0',
      \ 'di': '01;34',
      \ 'ln': '01;36',
      \ 'pi': '33',
      \ 'so': '01;35',
      \ 'cd': '01;33',
      \ 'bd': '01;33',
      \ 'or': "",
      \ 'ex': '01;32',
      \ 'do': '01;35',
      \ 'su': '37;41',
      \ 'sg': '30;43',
      \ 'wo': '37;44',
      \ 'wt': '37;42'
      \ }

let s:mappings = map(split($LS_COLORS, ':'), 'split(v:val, "=")')
let s:extensions = filter(copy(s:mappings), '!has_key(s:specials_defaults, v:val[0])')
let s:specials_list = filter(copy(s:mappings), 'has_key(s:specials_defaults, v:val[0])')
let s:specials = deepcopy(s:specials_defaults)
for [key, value] in s:specials_list
  let s:specials[key] = value
endfor

for [extension, colors] in s:extensions
  let cs = split(colors, ';')
  let fg = ""
  let bg = ""
  if len(cs) > 0
    let fg = cs[0]
  endif
  if len(cs) > 1
    let bg = cs[1]
  endif
  let name = 'netrw' . toupper(substitute(extension, '\W', "", 'g'))
  let cleaned_extension = substitute(escape(extension, '.'), '\*', '.\\{-}', "")
  let cleaned_extension = (cleaned_extension =~ '^\.\\\{-\}') ? cleaned_extension : ('.\{-}' . cleaned_extension)
  execute 'syntax match ' . name . ' /^' . cleaned_extension . '\s\s/me=e-2'
  execute 'highlight def ' . name . ' ctermfg=' . fg . ' ctermbg=' . bg
endfor

let &cpo = s:cpo_save
unlet s:cpo_save

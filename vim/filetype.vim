" Vim support file to detect file types
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-02-20

if exists('did_load_filetypes')
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

augroup filetypedetect
  au! BufRead,BufNewFile README call s:FTREADME()
  au! BufRead,BufNewFile Gemfile setf ruby
  au! BufRead,BufNewFile *.ru setf ruby
  au! BufRead,BufNewFile *.nml setf xml
augroup END

function! s:FTREADME()
  if getline(1) =~ '^#.*#$'
    setf markdown
  else
    set filetype=
  endif
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

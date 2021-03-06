set cpo&vim

let s:on_windows = has("win32") || has("win64")

" TODO: Is this really needed?
set encoding=utf-8
if s:on_windows
  let &runtimepath = substitute(&rtp,
        \                       '\V' . escape(expand('~') . '/vimfiles', '\'),
        \                       '\~/.vim', 'g')
endif
set nostartofline
set incsearch
set path+=./**
let &fillchars = 'diff: ,fold: ,vert:' . (&encoding == 'utf-8' ? '│' : '|')
let &listchars .= (&encoding == 'utf-8' ? ',tab:»·,trail:·' : ',tab:>.,trail:.')
syntax enable
colorscheme now
set statusline=%F%(\ [%1*%M%*%(,%2*%R%*%)]%)%(\ %w%)%(\ %y%)
set hidden
set confirm
set autowriteall
set visualbell t_vb=
set textwidth=79
set backspace=indent,eol,start
set expandtab softtabstop=8
set shiftround
set autoindent
set cinoptions=:0,l1,t0,(0,u0
set fileformat=unix
set fileformats=unix,dos
if s:on_windows
  set backupcopy=yes
endif
set history=512
set wildcharm=<C-Z>
set wildmode=list:longest
set wildignore=*.{o\\,l[oa]}
set errorformat=%-G%.%#
set grepprg=grep\ -n\ -P\ $*\ /dev/null
if exists("+shellslash")
  set shellslash
endif
set viminfo-=<50
set sessionoptions+=localoptions

set guicursor=n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,
      \i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor,
      \a:blinkon0
set notitle
set guioptions=c
if has('gui_win32')
  set guifont=DejaVu_Sans_Mono:h10
endif

filetype plugin indent on

let c_no_bracket_error = 1
let ruby_no_identifiers = 1
let ruby_no_expensive = 1
let vimsyntax_noerror = 1
let zsh_syntax_variables = 'short,long'

let compiler_gcc_ignore_unmatched_lines = 1

let mapleader = ","

noremap s l
noremap l s
noremap S L
noremap L S
noremap <C-W>s <C-W>l
noremap <C-W>l <C-W>s
noremap <C-W>S <C-W>L
noremap <C-W>L <C-W>S
noremap <C-W><C-S> <C-W><C-L>
noremap <C-W><C-L> <C-W><C-S>
noremap zs zl
noremap zl zs
noremap zS zL

noremap <Space> <C-F>
ounmap <Space>
noremap <Backspace> <C-B>
omap <Backspace> <Delete>

noremap <silent> <C-Y> <Esc>:read !clipboard<CR>

inoremap <silent> <C-Y> <C-R>=pumvisible() ? "\<lt>C-Y>" : system("clipboard")<CR>
" TODO: Why do I have this?
inoremap <silent> <Tab> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>Tab>"<CR>

for digit in [1, 2, 3, 4, 5, 6, 8, 9]
  execute 'inoremap <silent> ' . digit . ' <C-R>=pumvisible() ? "' . repeat('\<lt>C-N>', digit) . '" : "' . digit . '"<CR>'
endfor

noremap <C-D> <C-Z>
inoremap <C-D> <C-C>
vnoremap <C-D> <C-C>
onoremap <C-D> <C-C>
cnoremap <C-G> <C-C>

inoremap <expr> <Esc> <SID>ignore_esc('<lt>C-D>')
vnoremap <expr> <Esc> <SID>ignore_esc('<lt>C-D>')
cnoremap <expr> <Esc> <SID>ignore_esc('<lt>C-G>')

function s:ignore_esc(alternative)
  echohl ErrorMsg
  echomsg 'Please use ' . a:alternative . ' instead of <Esc>'
  echohl None
  return ""
endfunction

noremap <Leader>m <Esc>:make<CR>
noremap <silent> <Leader>p :<C-U>execute v:count . (empty(getloclist(0)) ? 'c' : 'l') . 'previous'<CR>
noremap <silent> <Leader>n :<C-U>execute v:count . (empty(getloclist(0)) ? 'c' : 'l') . 'next'<CR>
noremap <silent> <Leader>P :<C-U>execute v:count . (empty(getloclist(0)) ? 'c' : 'l') . 'pfile'<CR>
noremap <silent> <Leader>N :<C-U>execute v:count . (empty(getloclist(0)) ? 'c' : 'l') . 'nfile'<CR>

noremap <silent> <C-P> :<C-U>execute v:count . 'bprevious'<CR>
noremap <silent> <C-N> :<C-U>execute v:count . 'bnext'<CR>

noremap <silent> <Leader>h <Esc>:set invhlsearch<CR>

noremap <silent> <Leader>s <Esc>:setlocal invspell spelllang=en_us<CR>

nnoremap <Leader>b <Esc>:ls<CR>:b<Space>

nnoremap <Leader>c <Esc>:cd %:p:h<CR>:pwd<CR>
nnoremap <Leader>C <Esc>:lcd %:p:h<CR>:pwd<CR>

nnoremap <Leader>e <Esc>:e <C-R>=expand('%:h')=~'^\.\=$'?"":escape(substitute(expand('%:h'),'^\./',"",""),' ').'/'<CR><C-Z>
nnoremap <Leader>E <Esc>:e <C-Z>

nnoremap <silent> <Leader>k <Esc>:bp <Bar> :bd #<CR>

cnoremap <C-A>  <Home>
cnoremap <C-B>  <Left>
cnoremap <C-D>  <Delete>
cnoremap <C-F>  <Right>
cnoremap <C-N>  <Down>
cnoremap <C-P>  <Up>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cmap     <Esc>d <C-\>e<SID>command_line_delete_word_to_right()<CR>
cnoremap <expr> <C-S> (getcmdtype() == '/' \|\| getcmdtype() == '?') ? '<Return>' . getcmdtype() . '<C-R>/' : ""
cnoremap <expr> <C-O> (getcmdtype() == '/' \|\| getcmdtype() == '?') ? '<Return>' . (getcmdtype() == '/' ? '?' : '/') . '<C-R>/' : ""
cnoremap <C-Y> <C-R><C-O>*

function! s:command_line_delete_word_to_right()
  let cmd = getcmdline()
  let pos = getcmdpos()
  let before = strpart(cmd, 0, pos - 1)
  let after = substitute(strpart(cmd, pos), '^\s*\w\+\ze\%(\s\+\|$\)', "", "")
  return before . after
endfunction

noremap <silent> g: <Esc>:set operatorfunc=<SID>get_command_mode_range<CR>g@

function! s:get_command_mode_range(type)
  let b = line("'[")
  let e = line("']")

  if b < e
    let range = '.,+' . (e - b)
  elseif b == e
    let range = '.'
  else
    let range = '.,+' . (b - e)
  endif

  call inputsave()
  call feedkeys(':' . range . "\<C-R>=''[inputrestore()]\<CR>", 'n')
endfunction

noremap <silent> gt <Esc>:set operatorfunc=<SID>go_to_tag<CR>g@

function! s:go_to_tag(type)
  let saved_reg = @@
  silent execute 'normal! `[v`]y'
  let tag = @@
  let @@ = saved_reg
  let error = ""
  try
    execute 'tag' '/' . tag
  catch /^Vim\%((\a\+)\)\=:E426/
    let message = substitute(v:exception, '^Vim\%((\a\+)\)\=:\(E426.*\)', '\1', '')
    echohl ErrorMsg | echo message | echohl None
  endtry
endfunction

autocmd VimLeave * if v:this_session != "" | exe "mks! ".v:this_session | endif
autocmd GuiEnter * set t_vb=

digraphs cb 8226 tb 8227 .3 8230 ,3 8943 (/ 8713 <Y 8826 </ 10216 >/ 10217

unlet s:on_windows

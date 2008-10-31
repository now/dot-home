" contents: vim(1) text editor user RC file.
"
" Copyright © 2002,2003,2004,2005,2006,2007 Nikolai Weibull <now@bitwi.se>

" TODO: Should we modify the 'complete' option?  Scanning included files is
" perhaps a bit much, better to leave that to Ctrl-X Ctrl-I.

let s:on_windows = has("win32") || has("win64")

set cpoptions&vim
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
" TODO: seriously, everyone wants this.  Why is this not enabled by default?
syntax enable
colorscheme now
set statusline=%F%(\ [%1*%M%*%(,%2*%R%*%)]%)\ %w%=%(\ %y%)\ line:\ %l\ of\ %L
set hidden
" TODO: can we remove this?
set noesckeys
set confirm
set autowriteall
set visualbell t_vb=
set textwidth=79
set backspace=indent,eol,start
set formatlistpat=^\\s*\\%(\\d\\+[\\]:.)}\\t\ ]\\\|[•‣][\\t\ ]\\)\\s*
set completeopt=menu,menuone,preview
set expandtab softtabstop=8
set shiftround
set autoindent
set cinoptions=:0,l1,t0,c0,C1,(0,u0
set foldtext=substitute(substitute(foldtext(),'^+-\\+\\s*\\d\\+\ lines:','§',
                       \'g'),'\\s\\+$','\ ⇱','')
set fileformat=unix
set fileformats=unix,dos
" TODO: decide on this
" set backupcopy=auto,breakhardlink
if s:on_windows
  set backupcopy=yes
endif
set history=512
set wildcharm=<C-Z>
set wildmode=list:longest
set wildignore=*.{o\\,l[oa]}
set errorformat=%-G%.%#
set grepprg=grep\ -n\ -P\ $*\ /dev/null
if &viminfo == ""
  set viminfo='20
endif
set viminfo+=<512
set sessionoptions+=localoptions

" FIXME: Reiser4
set nofsync
set swapsync=

set guicursor=n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,
      \i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor,
      \a:blinkon0
set notitle
set guioptions=acM
if has('gui_win32')
  set guifont=DejaVu_Sans_Mono:h10
endif

" TODO: same as for syntax...why is this not the default?
filetype plugin indent on

let netrw_longlist = 1
let netrw_list_hide = '^\%(\./\|\.[^.]\S*\s\|\.\.[^/]\S*\s\)'
let netrw_maxfilenamelen = 64
let netrw_timefmt = '%b %d, %Y %H:%M'

let ruby_no_identifiers = 1
let ruby_no_expensive = 1
let vimsyntax_noerror = 1
let zsh_syntax_variables = 'short,long'

let compiler_gcc_ignore_unmatched_lines = 1

let mapleader = ","

function! s:map_swap(lhs, rhs)
  execute 'noremap' a:lhs a:rhs
  execute 'noremap' a:rhs a:lhs
endfunction

function! s:map_swap_list(ms)
  for [lhs, rhs] in a:ms
    call s:map_swap(lhs, rhs)
  endfor
endfunction

function! s:map_swap_both_cases(...)
  call s:map_swap_list(a:000)
  for [lhs, rhs] in a:000
    let upper = [toupper(lhs), toupper(rhs)]
    if upper != [lhs, rhs]
      call s:map_swap(upper[0], upper[1])
    endif
  endfor
endfunction

" TODO: This is bugged.  Braw will fix?
"set langmap=tj;jt,nk;kn,sl;ls

call s:map_swap_both_cases(['s', 'l'])
"call s:map_swap_both_cases(['t', 'j'], ['n', 'k'], ['s', 'l'])
"noremap <silent> k :<C-U>call feedkeys(v:count1 . 'n', 'nt')<CR>
"noremap <silent> K :<C-U>call feedkeys(v:count1 . 'N', 'nt')<CR>

"call s:map_swap_both_cases(['<C-W>t', '<C-W>j'], ['<C-W><C-T>', '<C-W><C-J>'])
"call s:map_swap_both_cases(['<C-W>n', '<C-W>k'], ['<C-W><C-N>', '<C-W><C-K>'])
call s:map_swap_both_cases(['<C-W>s', '<C-W>l'], ['<C-W><C-S>', '<C-W><C-L>'])
"call s:map_swap_list([['zt', 'zj'], ['zn', 'zk'], ['zs', 'zl']])
call s:map_swap_list([['zs', 'zl']])
noremap zS zL
"noremap zK zN
"noremap gt gj
"noremap gn gk

delfunction s:map_swap_both_cases
delfunction s:map_swap_list
delfunction s:map_swap

noremap <Space> <C-F>
ounmap <Space>
noremap <Backspace> <C-B>
omap <Backspace> <Delete>

inoremap <silent> <C-Y> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>C-R>\<lt>C-O>*"<CR>
inoremap <silent> <Tab> <C-R>=pumvisible() ? "\<lt>C-Y>" : "\<lt>Tab>"<CR>

for digit in [1, 2, 3, 4, 5, 6, 8, 9]
  execute 'inoremap <silent> ' . digit . ' <C-R>=pumvisible() ? "' . repeat('\<lt>C-N>', digit) . '" : "' . digit . '"<CR>'
endfor

inoremap <C-Z> <C-C>

noremap <Leader>p :cprevious<CR>
noremap <Leader>n :cnext<CR>
noremap <Leader>P :cpfile<CR>
noremap <Leader>N :cnfile<CR>

noremap <silent> <C-P> <Esc>:bprevious<CR>
noremap <silent> <C-N> <Esc>:bnext<CR>

noremap <silent> <Leader>h <Esc>:set invhlsearch<CR>

noremap <silent> <Leader>s <Esc>:setlocal invspell spelllang=en_us<CR>

nnoremap <Leader>c :cd %:p:h<CR>:pwd<CR>
nnoremap <Leader>C :lcd %:p:h<CR>:pwd<CR>
nnoremap <Leader>e :e <C-R>=expand('%:p:h')<CR>/<C-Z>
nnoremap <Leader>E :e <C-Z>

nnoremap <silent> ,k :bn <Bar> :bd #<CR>

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

autocmd VimLeave * if v:this_session != "" | exe "mks! ".v:this_session | endif
autocmd GuiEnter * set t_vb=

digraphs cb 8226 tb 8227 .3 8230 ,3 8943 (/ 8713 <Y 8826 </ 10216 >/ 10217

unlet s:on_windows

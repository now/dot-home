" Vim macro file
" NOTE:             Based on Bram Moolenar's less.vim script. 
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

let loaded_less = 1

" If not reading from stdin, skip files that can't be read.
" Exit if there is no file at all.
if argc() > 0
  let s:i = 0
  while 1
    if filereadable(argv(s:i))
      if s:i != 0
        sleep 3
      endif
      break
    endif
    if isdirectory(argv(s:i))
      echomsg "Skipping directory " . argv(s:i)
    elseif getftime(argv(s:i)) < 0
      echomsg "Skipping non-existing file " . argv(s:i)
    else
      echomsg "Skipping unreadable file " . argv(s:i)
    endif
    echo "\n"
    let s:i = s:i + 1
    if s:i == argc()
      quit
    endif
    next
  endwhile
endif

set nocp
syntax on
set so=0
set hlsearch
" set incsearch
nohlsearch
" Don't remember file names and positions
set viminfo=
set nows

" Used after each command: put cursor at end and display position
if &wrap
  noremap <SID>L L0:redraw<CR>:file<CR>
  au VimEnter * normal! L0
else
  noremap <SID>L Lg0:redraw<CR>:file<CR>
  au VimEnter * normal! Lg0
endif

" When reading from stdin don't consider the file modified.
au VimEnter * set nomod

" Can't modify the text
set noma

" Give help
noremap H :call <SID>Help()<CR>
map ? H
fun! s:Help()
  echo "<Space>   One page forward          b, <BS>   One page backward"
  echo "d         Half a page forward       u         Half a page backward"
  echo "<Enter>   One line forward          n         One line backward"
  echo "G         End of file               gg        Start of file"
  echo "N%        percentage in file"
  echo "\n"
  echo "/pattern  Search for pattern"
  echo "k         next pattern match        K         Previous pattern match"
  echo "\n"
  echo ":n<Enter> Next file                 :p<Enter> Previous file"
  echo "\n"
  echo "q         Quit                      v         Edit file"
  let i = input("Hit Enter to continue")
endfun

" Scroll one page forward
noremap <script> <Space> :call <SID>NextPage()<CR><SID>L
map <C-V> <Space>
map f <Space>
map <C-F> <Space>
map z <Space>
map <Esc><Space> <Space>
fun! s:NextPage()
  if line(".") == line("$")
    if argidx() + 1 >= argc()
      quit
    endif
    next
    1
  else
    exe "normal! \<C-F>"
  endif
endfun

" Re-read file and page forward "tail -f"
map F :e<CR>G<SID>L:sleep 1<CR>F

" Scroll half a page forward
noremap <script> d <C-D><SID>L
map <C-D> d

" Scroll one line forward
noremap <script> <CR> <C-E><SID>L
map <C-N> <CR>
map e <CR>
map <C-E> <CR>
map t <CR>
map <C-J> <CR>

" Scroll one page backward
noremap <script> b <C-B><SID>L
map <C-B> b
map <BS> b
map w b
map <Esc>v b

" Scroll half a page backward
noremap <script> u <C-U><SID>L
noremap <script> <C-U> <C-U><SID>L

" Scroll one line backward
noremap <script> n <C-Y><SID>L
map y n
map <C-Y> n
map <C-P> n
map <C-K> n

" Redraw
noremap <script> r <C-L><SID>L
noremap <script> <C-R> <C-L><SID>L
noremap <script> R <C-L><SID>L

" Start of file
noremap <script> gg gg<SID>L
map < gg
map <Esc>< gg

" End of file
noremap <script> G G<SID>L
map > G
map <Esc>> G

" Go to percentage
noremap <script> % %<SID>L
map p %

" Search
noremap <script> / H$:call <SID>Forward()<CR>/
if &wrap
  noremap <script> ? H0:call <SID>Backward()<CR>?
else
  noremap <script> ? Hg0:call <SID>Backward()<CR>?
endif 

fun! s:Forward()
  " Searching forward
  noremap <script> k H$nzt<SID>L
  if &wrap
    noremap <script> K H0Nzt<SID>L
  else
    noremap <script> K Hg0Nzt<SID>L
  endif
  cnoremap <script> <CR> <CR>:cunmap <lt>CR><CR>zt<SID>L
endfun
    
fun! s:Backward()
  " Searching backward
  if &wrap
    noremap <script> k H0nzt<SID>L
  else
    noremap <script> k Hg0nzt<SID>L
  endif
  noremap <script> K H$Nzt<SID>L
  cnoremap <script> <CR> <CR>:cunmap <lt>CR><CR>zt<SID>L
endfun

call s:Forward() 

" Quitting
noremap q :q<CR>

" Switch to editing (switch off less mode)
map v :call <SID>End()<CR>
fun! s:End()
  set ma
  unmap h
  unmap H
  unmap <Space>
  unmap <C-V>
  unmap f
  unmap <C-F>
  unmap z
  unmap <Esc><Space>
  unmap F
  unmap d
  unmap <C-D>
  unmap <CR>
  unmap <C-N>
  unmap e
  unmap <C-E>
  unmap t
  unmap <C-J>
  unmap b
  unmap <C-B>
  unmap w
  unmap <Esc>v
  unmap u
  unmap <C-U>
  unmap n
  unmap y
  unmap <C-Y>
  unmap <C-P>
  unmap <C-K>
  unmap r
  unmap <C-R>
  unmap R
  unmap g
  unmap <
  unmap <Esc><
  unmap G
  unmap >
  unmap <Esc>>
  unmap %
  unmap p
  unmap k
  unmap K
  unmap q
  unmap v
endfun

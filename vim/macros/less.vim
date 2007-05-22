" Vim macro file
" NOTE:             Based on Bram Moolenar's less.vim script. 
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2007-05-16

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

let s:saved_mappings = {}

function s:push_map(lhs, rhs, maptype, ...)
  " TODO: Need to figure out what modes a:lhs is actually defined in, so that
  " we can restore that later on.
  let saved_mapping = maparg(a:lhs)
  if saved_mapping != ""
    let s:saved_mappings[a:lhs] = [saved_mapping, a:maptype]
  else
    let s:saved_mappings[a:lhs] = [saved_mapping, 2]
  endif
  if a:maptype == 0
    let mapcmd = 'map'
  elseif a:maptype == 1
    let mapcmd = 'noremap'
  elseif a:maptype == 3
    let mapcmd = 'cnoremap'
  endif
  execute mapcmd join(a:000) a:lhs a:rhs
endfunction

function s:push_maps(lhss, rhs, noremap, ...)
  for lhs in a:lhss
    call call('s:push_map', [lhs, a:rhs, a:noremap] + a:000)
  endfor
endfunction

" Give help
call s:push_map('H', ':call <SID>Help()<CR>', 1)
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
  call input("Hit Enter to continue")
endfun

" Scroll one page forward
call s:push_map('<Space>', ':call <SID>NextPage()<CR><SID>L', 1, '<script>')
call s:push_maps(['<C-V>', 'f', '<C-F>', 'z', '<Esc><Space>'], '<Space>', 0)
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
call s:push_map('F', ':e<CR>G<SID>L:sleep 1<CR>F', 1)

" Scroll half a page forward
call s:push_map('d', '<C-D><SID>L', 1, '<script>')
call s:push_map('<C-D>', 'd', 0)

" Scroll one line forward
call s:push_map('t', '<C-E><SID>L', 1, '<script>')
call s:push_maps(['<C-N>', 'e', '<C-E>', '<CR>', '<C-J>'], '<CR>', 0)

" Scroll one page backward
call s:push_map('b', '<C-B><SID>L', 1, '<script>')
call s:push_maps(['<C-B>', '<BS>', 'w', '<Esc>v'], 'b', 0)

" Scroll half a page backward
call s:push_map('u', '<C-U><SID>L', 1, '<script>')
call s:push_map('<C-U>', '<C-U><SID>L', 1, '<script>')

" Scroll one line backward
call s:push_map('n', '<C-Y><SID>L', 1, '<script>')
call s:push_maps(['y', '<C-Y>', '<C-P>', '<C-K>'], 'n', 0)

" Redraw
call s:push_map('r', '<C-L><SID>L', 1, '<script>')
call s:push_map('<C-R>', '<C-L><SID>L', 1, '<script>')
call s:push_map('R', '<C-L><SID>L', 1, '<script>')

" Start of file
call s:push_map('gg', 'gg<SID>L', 1, '<script>')
call s:push_maps(['<', '<Esc><'], 'gg', 0)

" End of file
call s:push_map('G', 'G<SID>L', 1, '<script>')
call s:push_maps(['>', '<Esc>>'], 'G', 0)

" Go to percentage
call s:push_map('%', '%<SID>L', 1, '<script>')
call s:push_maps(['p'], '%', 0)

" Search
call s:push_map('/', 'H$:call <SID>Forward(0)<CR>/', 1, '<script>')
if &wrap
  call s:push_map('?', 'H0:call <SID>Backward(0)<CR>?', 1, '<script>')
else
  call s:push_map('?', 'Hg0:call <SID>Backward(0)<CR>?', 1, '<script>')
endif 

function s:pop_map(lhs, rhs, mode)
  if a:mode == 2
    execute 'unmap' a:lhs
  else
    execute 'noremap' a:lhs a:rhs
  endif
endfunction

fun! s:Forward(initialize)
  if !a:initialize
    for lhs in ['k', 'K', '<CR>']
      let mapping = s:saved_mappings[lhs]
      call s:pop_map(lhs, mapping[0], mapping[1])
    endfor
  endif
  " Searching forward
  call s:push_map('k', 'H$nzt<SID>L', 1, '<script>')
  if &wrap
    call s:push_map('K', 'H0Nzt<SID>L', 1, '<script>')
  else
    call s:push_map('K', 'Hg0Nzt<SID>L', 1, '<script>')
  endif
  call s:push_map('<CR>', '<CR>:cunmap <lt>CR><CR>zt<SID>L', 3, '<script')
endfun
    
fun! s:Backward(initialize)
  if !a:initialize
    for lhs in ['k', 'K', '<CR>']
      let mapping = s:saved_mappings[lhs]
      call s:pop_map(lhs, mapping[0], mapping[1])
    endfor
  endif
  " Searching backward
  if &wrap
    call s:push_map('k', 'H0nzt<SID>L', 1, '<script>')
  else
    call s:push_map('k', 'Hg0nzt<SID>L', 1, '<script>')
  endif
  call s:push_map('K', 'H$Nzt<SID>L', 1, '<script>')
  call s:push_map('<CR>', '<CR>:cunmap <lt>CR><CR>zt<SID>L', 3, '<script')
endfun

call s:Forward(1)

" Quitting
call s:push_map('q', ':q<CR>', 1)

" Switch to editing (switch off less mode)
call s:push_map('v', ':call <SID>End()<CR>', 1, '<silent>')
fun! s:End()
  set ma
  for lhs in keys(s:saved_mappings)
    let mapping = s:saved_mappings[lhs]
    call s:pop_map(lhs, mapping[0], mapping[1])
  endfor
  echo 'Buffer is now modifiable'
endfun

" Vim ftplugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2007-10-02

setlocal tw=74
setlocal nowarn nowritebackup

let s:file_pattern = '^#\t\%(new file\|modified\|deleted\):\s\+\(.*\)'

call cursor(1, 1)
let lnum = search('^# On branch ', 'n') + 4
if lnum > 4 && getline(lnum) =~ s:file_pattern && getline(lnum + 1) == "#"
  call setline(1, substitute(getline(lnum), s:file_pattern, '\1: ', ""))
endif

augroup filetype-plugin-gitcommit
  autocmd BufEnter <buffer> call cursor(1, len(getline(1)))
augroup end

noremap <buffer> ,gd :call <SID>show_diff(0, 0)<CR>
noremap <buffer> ,ghd :call <SID>show_diff(0, 0)<CR>
noremap <buffer> ,gvd :call <SID>show_diff(1, 0)<CR>

if !exists('g:git_diff_spawn_mode')
  let g:git_diff_spawn_mode = 0
endif

if g:git_diff_spawn_mode == 1
    call s:show_diff(0, 1)
elseif g:git_diff_spawn_mode == 2
    call s:show_diff(1, 1)
endif

if exists('*s:show_diff')
  finish
endif

function s:extract_path(string, pattern, group, list)
  if a:string !~ a:pattern
    return 0
  endif
  call add(a:list, substitute(a:string, a:pattern, '\' . a:group, ""))
  return 1
endfunction

let s:move_pattern = '^#[^:]*:\s*\(.*\)\s*->\s*\(.*\)\s*$'
let s:change_pattern = '^#[^:]*:\s*\(.*\)\s*$'

function s:show_diff(vertsplit, auto)
  let files = []

  call cursor(1, 1)
  let lnum = search('^#   (use "git reset HEAD <file>..." to unstage)$\|\%$') + 2

  call cursor(lnum, 1)
  let end = search('^#\s*$\|\%$')
  while lnum <= end
    let line = getline(lnum)
    for pair in [[s:move_pattern, 2], [s:move_pattern, 1], [s:change_pattern, 1]]
      if s:extract_path(line, pair[0], pair[1], files)
        break
      endif
    endfor
    let lnum += 1
  endwhile

  if len(files) == 0
    return
  endif

  let files_str = join(map(files, 'escape(v:val, " \\")'))

  execute 'rightbelow' (a:vertsplit ? 'vnew' : 'new') '[Git Diff]'

  silent! setlocal ft=diff previewwindow bufhidden=delete nobackup noswf nobuflisted nowrap buftype=nofile modifiable

  let git_dir = system('git rev-parse --git-dir 2>/dev/null')
  if v:shell_error
    return
  endif
  let git_dir = substitute(git_dir, '.git\n', "", "")

  let saved_cwd = getcwd()
  if git_dir != ""
    execute 'cd' git_dir
  endif

  " TODO: Deal with v:errmsg
  execute 'silent! normal :r!LANG=C git diff HEAD -- ' . files_str . "\n1Gdd"
  execute 'silent! normal :r!LANG=C git diff HEAD -- ' . files_str . " \| git apply --stat\no\<Esc>1GddO\<Esc>"

  if git_dir != ""
    execute 'cd' saved_cwd
  endif

  setlocal nomodifiable

  noremap <buffer> q :bw<cr>

  if a:auto
    redraw!
    wincmd p
    redraw!
  endif
endfunction

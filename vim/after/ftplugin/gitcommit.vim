" Vim ftplugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2009-10-02

setlocal nowritebackup tw=72

let b:undo_ftplugin = b:undo_ftplugin . " | setl wb< tw<"

augroup filetype-plugin-gitcommit
  autocmd FileType gitcommit call cursor(1, len(getline(1))) | DiffGitCached
augroup end

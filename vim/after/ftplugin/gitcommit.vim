" Vim ftplugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-03-09

setlocal nowritebackup tw=72

let b:undo_ftplugin = b:undo_ftplugin . " | setl wb< tw<"

call cursor(1, len(getline(1)))
DiffGitCached

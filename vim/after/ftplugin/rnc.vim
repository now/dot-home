" Vim filetype plugin file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2008-01-24

setlocal softtabstop=2 shiftwidth=2

if exists("b:undo_plugin") && b:undo_plugin != ""
  let b:undo_plugin .= ' | '
else
  let b:undo_plugin = ""
endif

let b:undo_plugin .= 'setl sts< sw<'

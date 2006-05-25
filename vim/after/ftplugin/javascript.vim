" Vim filetype plugin file
" Maintainer:       Nikolai Weibull <nikolai@bitwi.se>
" Latest Revision:  2006-01-04

setlocal softtabstop=2 shiftwidth=2
setlocal formatoptions-=t formatoptions+=croql

if exists("b:undo_plugin") && b:undo_plugin != ""
  let b:undo_plugin .= ' | '
else
  let b:undo_plugin = ""
endif

let b:undo_plugin .= 'setl sts< sw< fo<'

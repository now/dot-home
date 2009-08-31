let s:cpo_save = &cpo
set cpo&vim

setlocal shiftwidth=2 softtabstop=2 formatoptions-=t formatoptions+=croql

let b:undo_ftplugin = 'setl sw< sts< fo<'

let &cpo = s:cpo_save
unlet s:cpo_save

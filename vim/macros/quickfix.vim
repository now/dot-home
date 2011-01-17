let s:cpo_save = &cpo
set cpo&vim

let list = []
let i = 1
let n = line('$')
while i <= n
  let line = getline(i)
  let file = substitute(line, ':.*', '', '')
  let lnum = str2nr(substitute(line, '^[^:]\+:\(\d\+\):.*', '\1', ''))
  let text = substitute(line, '^[^:]\+:\d\+:', '', '')
  call add(list, { 'filename': file, 'lnum': lnum, 'text': text })
  let i += 1
endwhile

%delete _

echoerr string(list)
call setloclist(0, list)
bdelete!
lwindow
ll

let &cpo = s:cpo_save
unlet s:cpo_save

" Vim syntax file
" Language:         Mail file
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

syn match   mailHeaderKey   contained '^Date:.*$'

syn match   mailURL	    contained '\(ftp\|https\=\|news\)://[^ >)]\+'

hi mailHeaderKey  ctermfg=DarkGreen   guifg=DarkGreen
hi mailHeader	  ctermfg=3           guifg=Brown
hi mailQuoted1	  ctermfg=DarkGreen   guifg=DarkGreen
hi mailQuoted2	  ctermfg=3           guifg=Brown
hi mailQuoted3	  ctermfg=Red         guifg=Red
hi mailQuoted4	  ctermfg=DarkMagenta guifg=DarkMagenta
hi mailQuoted5	  ctermfg=Blue        guifg=Blue
hi mailQuoted6	  ctermfg=DarkCyan    guifg=DarkCyan
hi mailSignature  ctermfg=DarkMagenta guifg=DarkMagenta
hi mailEmail	  ctermfg=Blue        guifg=Blue
hi mailSubject	  ctermfg=DarkMagenta guifg=DarkMagenta
hi mailURL	  ctermfg=Blue        guifg=Blue

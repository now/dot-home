" Vim filetype plugin file
" Language:         Zsh shell script
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2010-12-07

setlocal shiftwidth=2 softtabstop=2
setlocal formatoptions-=t formatoptions+=croql
let b:undo_ftplugin .= ' | setl sw< sts< fo<'

let b:match_words =
      \   &matchpairs
      \ . ',\<if\>:\<elif\>:\<else\>:\<fi\>'
      \ . ',\<case\>:^\s*([^)]*):\<esac\>'
      \ . ',\<select\>:\<done\>'
      \ . ',\<while\>:\<done\>'
      \ . ',\<until\>:\<done\>'
      \ . ',\<repeat\>:\<done\>'
      \ . ',\<for\>:\<done\>'
      \ . ',\<foreach\>:\<done\>'
let b:undo_ftplugin .= ' | unlet b:match_words'

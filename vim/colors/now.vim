" Vim color file
" Maintainer:       Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2009-10-03

set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "now"

function! s:term_color_to_rgb(color_offset)
  let sequence = ""
  let xterm_sequence = '\e]4;' . a:color_offset . ';?\007'
  if $TERM =~ 'xterm'
    let sequence = xterm_sequence
  elseif $TERM =~ 'screen'
    let sequence = '\eP' . xterm_sequence . '\e\'
  endif

  if sequence == ""
    return ""
  endif

  return system('exec </dev/tty; old="$(stty -g)"; stty raw -echo min 0 time 1; ' .
              \ "echo -n '" .  sequence . "' > /dev/tty; " .
              \ 'IFS="" read -r response; stty $old; ' .
              \ 'echo -n $response | ' .
              \ 'sed ''s@^.*rgb:\(..\)../\(..\)../\(..\)...$@#\1\2\3@''')
endfunction

" TODO: perhaps the best way to do this is set a default, and then modify it if
" the terminal supports 256 or more colors.
"if &t_Co >= 256 || &term == 'builtin_gui'
hi! link          SpecialKey          NonText
hi  NonText                                                               guifg=#2080c0                     gui=None
let s:di_color = substitute($LS_COLORS, '^.*di=\(\d\+\).*$', '\1', '')
if s:di_color != ""
"  let s:rgb = s:term_color_to_rgb(s:di_color)
  execute 'hi Directory ctermfg=' . s:di_color
  ". (s:rgb != "" ? (' guifg=' . s:rgb) : "")
"  unlet s:rgb
else
  hi! link        Directory           NonText
endif
unlet s:di_color
hi  ErrorMsg                                                                                guibg=#951616
hi  IncSearch     ctermfg=White       ctermbg=Brown       cterm=None      guifg=White       guibg=#af5f00   gui=None
hi  Search                                                                                  guibg=#f0a500
hi  MoreMsg                                                               guifg=#257325                     gui=None
hi  ModeMsg                                               cterm=None                                        gui=None
hi  LineNr                                                                guifg=#af5f00
hi! link          Question MoreMsg
hi  StatusLine    ctermfg=None        ctermbg=249         cterm=None      guifg=NONE        guibg=#b2b2b2   gui=None
hi  StatusLineNC  ctermfg=236         ctermbg=253         cterm=None      guifg=#303030     guibg=#dadada   gui=None
hi  User1         ctermfg=DarkGreen   ctermbg=249                         guifg=#257325     guibg=#b2b2b2
hi  User2         ctermfg=DarkRed     ctermbg=249                         guifg=#951616     guibg=#b2b2b2
hi  User3         ctermfg=DarkGreen                                       guifg=#257325
hi  User4         ctermfg=DarkRed                                         guifg=#951616
hi  VertSplit     ctermfg=Black       ctermbg=254         cterm=None      guifg=Black       guibg=#e4e4e4   gui=None
hi  Title         ctermfg=Blue                                            guifg=#2080c0
hi  Visual                            ctermbg=None        cterm=Reverse                     guibg=#5598d7
hi  VisualNOS     ctermfg=DarkBlue                        cterm=Underline guifg=#2f5a9b                     gui=Underline
hi  WarningMsg                                                            guifg=#951616
hi  WildMenu                                                                                guibg=#50a500
hi  Folded        ctermfg=None        ctermbg=254                         guifg=NONE        guibg=#e4e4e4
hi! link          FoldColumn          Folded
hi  DiffAdd       ctermfg=White       ctermbg=DarkGreen                   guifg=White       guibg=#257325
hi  DiffChange    ctermfg=White       ctermbg=DarkBlue                    guifg=White       guibg=#2f5a9b
hi  DiffDelete    ctermfg=White       ctermbg=DarkRed                     guifg=White       guibg=#951616   gui=None
hi  DiffText      ctermfg=Red         ctermbg=None                        guifg=#f02626     guibg=NONE
hi  SignColumn                                                            guifg=#2f5a9b     guibg=#a8a8a8
hi  SpellBad                          ctermbg=224
hi  SpellCap                                                                                                                guisp=#5fd7ff
hi  SpellRare                         ctermbg=253                                                                           guisp=#dadada
hi  SpellLocal                                                                                                              guisp=#80b0b0
hi  Pmenu                             ctermbg=253                                           guibg=#dadada
hi  PmenuSel      ctermfg=White       ctermbg=32                          guifg=White       guibg=#5598d7
hi  PmenuSBar                         ctermbg=250                                           guibg=#d9d2c7
hi  PmenuThumb                        ctermbg=32          cterm=None                        guibg=#73acdc   gui=None
hi  CursorColumn                      ctermbg=None        cterm=Reverse                     guibg=#5598d7
hi  CursorLine                                                                              guibg=NONE      gui=Underline
hi  MatchParen                        ctermbg=215                                           guibg=#ffaf5f
hi  Comment       ctermfg=DarkGreen                                       guifg=#257325
hi  Constant      ctermfg=DarkRed                                         guifg=#951616
hi  Special       ctermfg=Red                                             guifg=#f02626
hi  Identifier    ctermfg=DarkBlue                                        guifg=#2f5a9b
hi  Statement     ctermfg=3                                               guifg=#766020                     gui=None
hi  PreProc       ctermfg=Magenta                                         guifg=#93376e
hi  Type          ctermfg=DarkMagenta                                     guifg=#602f80                     gui=None
hi  Underlined    ctermfg=None                                            guifg=NONE
" TODO: decide
hi! link          Number Normal
hi  Error                                                                                   guibg=#f02626
hi  Todo          ctermfg=None        ctermbg=172                         guifg=Black       guibg=#f0a500
hi  Normal                                                                guifg=#181818     guibg=#f6f6f6
hi! link          NOWModernFileMod    User3
hi! link          NOWModernFileRO     User4
hi  NOWModernFileCommonPrefix ctermfg=Gray                                guifg=Gray

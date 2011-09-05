" Vim filetype plugin file
" Language:	    NMC
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2011-09-05

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal formatoptions=tln

setlocal formatlistpat=^\s*[₁₂₃₄₅₆₇₈₉•]\s\s\s

setlocal shiftwidth=2 softtabstop=2 expandtab

let b:undo_ftplugin = "setl fo< flp< sw< sts< et<"

let &cpo = s:cpo_save
unlet s:cpo_save

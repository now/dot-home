" Vim filetype plugin file
" Language:	    DocBook
" Maintainer:	    Nikolai Weibull <now@bitwi.se>
" Latest Revision:  2006-05-25

if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/xml.vim ftplugin/xml_*.vim ftplugin/xml/*.vim

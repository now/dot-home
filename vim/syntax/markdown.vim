" Vim syntax file
" Language:         Markdown
" Maintainer:       Nikolai Weibull <NWeibull@nweibull-ws>
" Latest Revision:  2011-02-07

if exists('b:current_syntax')
  finish
endif

syn include @markdownHTML syntax/html.vim
unlet b:current_syntax

let s:cpo_save = &cpo
set cpo&vim

syn match   markdownStart                   '^'
                                          \ nextgroup=
                                          \   markdownComment,
                                          \   markdownHTMLBlock,
                                          \   markdownSelfCloseTag,
                                          \   markdownFootnote,
                                          \   @markdownBlock

syn region markdownLine                     start='.'
                                          \ end='^\s*$'
                                          \ transparent
                                          \ keepend
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownLine

syn match   markdownComment                 '<!--.\{-}-->'
                                          \ contained
                                          \ contains=markdownTodo

syn keyword markdownTodo                    contained
                                          \ TODO
                                          \ FIXME
                                          \ XXX
                                          \ NOTE

syn match   markdownFootnote                '\s\{0,3}\[[^[]*\]:'
                                          \ contained
                                          \  contains=markdownFootnoteTag
                                          \  nextgroup=
                                          \    markdownFootnoteURL
                                          \  skipwhite

syn match   markdownFootnoteTag             '\[[^[]*\]'hs=s+1,he=e-1
                                          \ contained

syn match   markdownFootnoteURL             '\S\+'
                                          \ contained
                                          \ nextgroup=
                                          \   markdownFootnoteDimensions,
                                          \   markdownFootnoteTitle
                                          \ skipwhite

syn match   markdownFootnoteDimensions      '=\d\+x\d\+'
                                          \ contained
                                          \ contains=markdownFootnoteDimension
                                          \ nextgroup=markdownFootnoteTitle
                                          \ skipwhite
                                          \ skipnl

syn match   markdownFootnoteDimension       '\d\+'
                                          \ contained

syn match   markdownFootnoteTitle           '..*.'hs=s+1,he=e-1
                                          \ contained

syn region  markdownCodeBlock               start='\s\{4,}'
                                          \ skip='^\s*$'
                                          \ end='^\ze\s\{0,3}\S'
                                          \ contained
                                          \ contains=
                                          \   @markdownCodeBlockCluster,
                                          \   @NoSpell
syn cluster markdownBlock add=markdownCodeBlock

syn region   markdownUnorderedListItem      start='\z(\s\{0,3}\)[*+-]\s'he=e-1
                                          \ skip='^\s*$'
                                          \ end='^\ze\%(\z1\s\)\@!'
                                          \ contained
                                          \ contains=
                                          \   markdownUnorderedListItemLabel,
                                          \   @markdownInline
syn cluster markdownBlock add=markdownUnorderedListItem

syn match   markdownUnorderedListItemLabel  '^\s\{0,3}\zs[*+-]\ze\s'
                                          \ contained

syn region  markdownOrderedListItem         start='\z(\s\{0,3}\)\d\+\.\s'he=e-1
                                          \ skip='^\s*$'
                                          \ end='^\ze\%(\z1\s\)\@!'
                                          \ contained
                                          \ contains=
                                          \   markdownOrderedListItemLabel,
                                          \   @markdownInline
syn cluster markdownBlock add=markdownOrderedListItem

syn match   markdownOrderedListItemLabel    '^\s\{0,3}\zs\d\+\.\ze\s'
                                          \ contained

syn match   markdownHR                      '\s\{0,3}[*_-]\s*[*_-]\s*[*_-].*'
                                          \ contained
syn cluster markdownBlock add=markdownHR

syn region  markdownBlockQuote              start='\s\{0,3}>'
                                          \ skip='^\s*$'
                                          \ end='^\ze\%(\s\{0,3}>\)\@!'
                                          \ contained
syn cluster markdownBlock add=markdownBlockQuote

syn region  markdownH1                      matchgroup=markdownHeadingDelimiter
                                          \ start='##\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH1

syn region  markdownH2                      matchgroup=markdownHeadingDelimiter
                                          \ start='###\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH2

syn region  markdownH3                      matchgroup=markdownHeadingDelimiter
                                          \ start='####\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH3

syn region  markdownH4                      matchgroup=markdownHeadingDelimiter
                                          \ start='#####\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH4

syn region  markdownH5                      matchgroup=markdownHeadingDelimiter
                                          \ start='######\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH5

syn region  markdownH6                      matchgroup=markdownHeadingDelimiter
                                          \ start='#######\@!'
                                          \ end='#*\s*$'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownH6

syn match   markdownH1                      '.\+\n=\+$'
                                          \ contained
                                          \ contains=
                                          \   @markdownInline,
                                          \   markdownHeadingRule

syn match   markdownH2                      '.\+\n-\+$'
                                          \ contained
                                          \ contains=
                                          \   @markdownInline,
                                          \   markdownHeadingRule

syn match   markdownHeadingRule             '^[=-]\+$'
                                          \ contained

syn match   markdownTable                   '^.*|.*\n\%([:[:space:]]*|[:[:space:]*-[:[:space:]|-]\|[:[:space:]]*-[:[:space:]*|[:[:space:]|-]\).*'
                                          \ transparent
                                          \ contained
                                          \ contains=markdownTableHeadRow
                                          \ nextgroup=markdownTableBodyRow
                                          \ skipwhite
                                          \ skipnl
syn cluster markdownBlock add=markdownTable

syn match   markdownTableHeadRow            '^.*$'
                                          \ transparent
                                          \ contained
                                          \ contains=
                                          \   @markdownInline,
                                          \   markdownTableColumnDivider
                                          \ nextgroup=markdownTableHeadEnd
                                          \ skipwhite
                                          \ skipnl

syn match   markdownTableColumnDivider      '|'
                                          \ contained

syn match   markdownTableHeadEnd            '^.*$'
                                          \ transparent
                                          \ contained
                                          \ contains=
                                          \   markdownTableColumnDivider,
                                          \   markdownTableColumnAlign

syn match   markdownTableColumnAlign        '\%(^\||\):\|:\%(|\|$\)'
                                          \ contained

syn match   markdownTableBodyRow            '^.*|.*$'
                                          \ transparent
                                          \ contained
                                          \ contains=
                                          \   markdownTableColumnDivider,
                                          \   @markdownInline
                                          \ nextgroup=markdownTableBodyRow
                                          \ skipwhite
                                          \ skipnl

syn cluster markdownInline add=@markdownHTML

" NOTE: These two following items are here to compensate for the inclusion of
" @markdownHTML above.
syn region  markdownHTMLBlock               start='<\z(style\|script\|address\|bdo\|blockquote\|center\|dfn\|div\|h1\|h2\|h3\|h4\|h5\|h6\|listing\|nobr\|ul\|p\|ol\|dl\|plaintext\|pre\|table\|wbr\|xmp\|iframe\|map\)\ze[>/[:space:]]'
                                          \ end='</\z1>'
                                          \ keepend
                                          \ transparent
                                          \ contained
                                          \ contains=@markdownHTML

syn match   markdownSelfCloseTag            '<[hb]r.*>'
                                          \ transparent
                                          \ contained
                                          \ contains=@markdownHTML

syn region  markdownCentered                start='\s\{0,3}->'
                                          \ end='<-'
                                          \ keepend
                                          \ oneline
                                          \ contained
                                          \ contains=@markdownInline
syn cluster markdownBlock add=markdownCentered

syn match   markdownImageLink               @!\[.\{-}\](.\{-}\%(\s\+['"].*['"]\)\=)@
                                          \ contained
                                          \ contains=markdownImageLinkAlt,
                                          \   markdownImageLinkURL,
                                          \   markdownImageLinkTitle
syn cluster markdownInline add=markdownImageLink

syn match   markdownImageLinkAlt            '!\[.\{-}\]'ms=s+1,me=e-1
                                          \ contained

syn match   markdownImageLinkURL            '\](.\{-}\%(\s\|)\)'ms=s+2,me=e-1
                                          \ contained

syn match   markdownImageLinkTitle          '\s\+\zs['"].*['"])'me=e-1
                                          \ contained

syn match   markdownImageRef                '!\[.\{-}\]\s\=\[[^[]*\]'
                                          \ contained
                                          \ contains=markdownImageRefAlt
syn cluster markdownInline add=markdownImageRef

syn match   markdownImageRefAlt             '!\[.\{-}\]'ms=s+1,me=e-1
                                          \ contained
                                          \ nextgroup=markdownImageRefName

syn match   markdownImageRefName            '\s\=\zs\[.\{-}\]'me=e-1
                                          \ contained

syn match   markdownLink                    @\[\_.\{-}\](.\{-}\%(\s\+['"].*['"]\)\=)@
                                          \ contained
                                          \ contains=markdownLinkAlt,
                                          \   markdownLinkURL,
                                          \   markdownLinkTitle
syn cluster markdownInline add=markdownLink

syn match   markdownLinkAlt                 '\[\_.\{-}\]'ms=s+1,me=e-1
                                          \ contained

syn match   markdownLinkURL                 '\](\_.\{-}\%(\s\|)\)'ms=s+2,me=e-1
                                          \ contained

syn match   markdownLinkTitle               '\s\+\zs['"].*['"])'me=e-1
                                          \ contained

syn match   markdownRefText                 '\[\_.\{-}\]'hs=s+1,he=e-1
                                          \ contained
                                          \ nextgroup=markdownRefNameContainer
syn cluster markdownInline add=markdownRefText

syn match   markdownRefNameContainer        '\s\=\[.\{-}\]'
                                          \ contained
                                          \ contains=markdownRefName

syn match   markdownRefName                 '\[.\{-}\]'hs=s+1,he=e-1
                                          \ contained

syn match   markdownSuperscript             '\S^\S\+'ms=s+1
                                          \ contained
syn cluster markdownInline add=markdownSuperscript

syn region  markdownEmphasis                start='\S\@<=\*\|\*\S\@='
                                          \ end='\S\@<=\*\|\*\S\@='
                                          \ contained
syn region  markdownEmphasis                start='\S\@<=_\|_\S\@='
                                          \ end='\S\@<=_\|_\S\@='
                                          \ contained
syn cluster markdownInline add=markdownEmphasis

syn region  markdownStrong                  start='\S\@<=\*\*\|\*\*\S\@='
                                          \ end='\S\@<=\*\*\|\*\*\S\@='
                                          \ contained
syn region  markdownStrong                  start='\S\@<=__\|__\S\@='
                                          \ end='\S\@<=__\|__\S\@='
                                          \ contained
syn cluster markdownInline add=markdownStrong

syn region  markdownWayTooStrong            start='\S\@<=\*\*\*\|\*\*\*\S\@='
                                          \ end='\S\@<=\*\*\*\|\*\*\*\S\@='
                                          \ contained
syn region  markdownWayTooStrong            start='\S\@<=___\|___\S\@='
                                          \ end='\S\@<=___\|___\S\@='
                                          \ contained
syn cluster markdownInline add=markdownWayTooStrong

syn match   markdownIgnored                 '[[:alnum:]]_\%([[:alnum:]]\|$\)'
                                          \ contained
syn cluster markdownInline add=markdownIgnored

syn region  markdownCode                    matchgroup=markdownCodeDelimiter
                                          \ start="`"
                                          \ end="`"
                                          \ contained
                                          \ contains=@NoSpell
syn region  markdownCode                    matchgroup=markdownCodeDelimiter
                                          \ start="`` \="
                                          \ end=" \=``"
                                          \ contained
                                          \ contains=@NoSpell
syn cluster markdownInline add=markdownCode

syn match   markdownEscape                '\\[]&<>#.+{}![*_\\()`-]'
                                          \ contained
syn cluster markdownInline add=markdownEscape

syn sync minlines=50

hi def link markdownTodo                    Todo
hi def link markdownComment                 Comment
hi def link markdownSelfCloseTag            Tag
hi def link markdownIgnored                 Normal
hi def link markdownFootnoteTag             Type
hi def link markdownFootnoteURL             String
hi def link markdownFootnoteDimension       Number
hi def link markdownFootnoteTitle           Title
"hi def link markdownCodeBlock               Comment
hi def link markdownUnorderedListItemLabel  Label
hi def link markdownOrderedListItemLabel    Label
hi def link markdownHR                      PreProc
hi def link markdownBlockQuote              Comment
hi def link markdownHeadingDelimiter        Delimiter
hi def link markdownH1                      htmlH1
hi def link markdownH2                      htmlH2
hi def link markdownH3                      htmlH3
hi def link markdownH4                      htmlH4
hi def link markdownH5                      htmlH5
hi def link markdownH6                      htmlH6
hi def link markdownHeadingRule             markdownHR
hi def link markdownTableColumnDivider      Special
hi def link markdownTableColumnAlign        Special
hi def link markdownCentered                PreProc
hi def link markdownImageLinkAlt            htmlLink
hi def link markdownImageLinkURL            String
hi def link markdownImageLinkTitle          Title
hi def link markdownImageRefAlt             htmlLink
hi def link markdownImageRefName            Type
hi def link markdownLinkAlt                 htmlLink
hi def link markdownLinkURL                 String
hi def link markdownLinkTitle               Title
hi def link markdownRefText                 htmlLink
hi def link markdownRefName                 Type
hi def link markdownEmphasis                htmlItalic
hi def link markdownStrong                  htmlBold
hi def link markdownWayTooStrong            htmlBoldItalic
hi def link markdownCodeDelimiter           Delimiter
hi def link markdownEscape                  Special

let b:current_syntax = 'markdown'

let &cpo = s:cpo_save
unlet s:cpo_save

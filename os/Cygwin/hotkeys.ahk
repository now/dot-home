#NoEnv
#SingleInstance force

SetTitleMatchMode Regex

CTRL_M := Chr(10)
CTRL_K := Chr(11)

WM_COMMAND := 0x111

#Include .autohotkey\digraphs.ahk

TagEditorVerify()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Document := TagEditor.ActiveDocument
  VisibleBefore := Document.MessageView.Visible
  Document.Verify()
  VisibleAfter := Document.MessageView.Visible
  if !VisibleBefore && VisibleAfter
    WinMenuSelectItem, , , View, Toolbars, Messages
}

TagEditorSaveAll()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Count := TagEditor.Documents.Count
  loop %Count% {
    TagEditor.Documents.Item(A_Index - 1).SaveBilingual()
  }
}

TagEditorSaveAllAsTarget()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Count := TagEditor.Documents.Count
  loop %Count% {
    Document := TagEditor.Documents.Item(A_Index - 1)
    Document.SaveTargetAs(Document.Path . "\" . Document.OriginalName)
  }
}

TagEditorCloseAll()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Count := TagEditor.Documents.Count
  loop %Count% {
    TagEditor.Documents.Item(Count - (A_Index - 1) - 1).Close()
  }
}

#IfWinActive ^Nirvana \d+\.\d+\.\d+ - \\\\Remote$ ahk_class Transparent Windows Client
!n::SendInput {F2}+{F10}a
!j::SendInput +{Tab}+{Tab}+{Tab}

#IfWinActive ^Job \d+ Overview\*? - \\\\Remote$ ahk_class Transparent Windows Client
!g::SendInput {Tab}{Tab}{Tab}{Tab}{Tab}{Tab}{Tab}{Tab}H{Tab}+{F10}a0,5+{Tab}+{Tab}+{Tab}+{Tab}+{Tab}+{Tab}{Space}Weibull{Enter}!o
!w::SendInput !sWeibull{Enter}!o
Escape::!c
Enter::!o

#IfWinActive ^Customer|Employee$ ahk_class Transparent Window Client
Escape::WinClose

#IfWinActive Microsoft Outlook$ ahk_class rctrl_renwnd32
!i::
ControlFocus NetUIHWND4
if (ErrorLevel = 0)
  return
SendInput !s
return

!c::ControlFocus _WwG1

!l::ControlFocus SUPERGRID1

#IfWinActive ^SDL Trados TagEditor
^+s::TagEditorSaveAll()

^+F12::TagEditorSaveAllAsTarget()

^w::SendInput ^{F4}

^+w::TagEditorCloseAll()

^n::PostMessage %WM_COMMAND%, 32879

^p::PostMessage %WM_COMMAND%, 32880

^l::PostMessage %WM_COMMAND%, 32875

F3::
SendInput ^f
WinWaitActive Find
SendInput {Enter}{Escape}
return

F8::TagEditorVerify()

#IfWinActive ^TranslatorTool
^u::
InputBox nRows, Rows to Copy, How many rows do you want to copy to memory?
if ErrorLevel
  return
Loop %nRows% {
  SendInput !4
  Sleep 100
  SendInput {Down}
}
return

#IfWinActive ^SDL Trados S-Tagger for FrameMaker
+^d::
SendInput !r
ControlSend Button1, {Enter}
WinWaitActive Select STF File(s) to
SendInput +{Tab}^a{Enter}
SavedTitleMatchMode = %A_TitleMatchMode%
SetTitleMatchMode Regex
WinWait ^Confirm (?:Verification of S-Tags|STF Conversion)$
ControlSend Button1, {Enter}
SetTitleMatchMode %SavedTitleMatchMode%
return

^p::
Saved_KeyDelay := A_KeyDelay
SetKeyDelay 100
SendEvent ^!v3{Tab}{Tab}^!v3{Tab}{Tab}^!v3
SetKeyDelay %Saved_KeyDelay%
return

#IfWinActive ^Find/Change$ ahk_class InDesign_Window:5660125
Esc::SendInput !d
!f::SendInput !x
!r::SendInput !g

#IfWinActive ahk_class Illustrator9
F3::
Saved_KeyDelay := A_KeyDelay
SetKeyDelay 200
SendEvent !frj
SetKeyDelay %Saved_KeyDelay%
return

#IfWinActive ahk_class illustrator
F3::
SendInput !ou!ou!ou
Saved_KeyDelay := A_KeyDelay
SetKeyDelay 200
SendEvent !frj
SetKeyDelay %Saved_KeyDelay%
return

!z::ControlFocus Edit1

#IfWinActive ahk_class TTOTAL_CMD
!PgUp::SendInput {Home}
!PgDn::SendInput {End}
+!PgUp::SendInput +{Home}
+!PgDn::SendInput +{End}

#IfWinActive ahk_class TDLGZIP
!h::ControlFocus TEdit1

#IfWinActive ahk_class TDLGUNZIPALL
!h::ControlFocus TEdit1

#IfWinActive ahk_class TDLGUNZIP
!h::ControlFocus TEdit1

#IfWinActive ahk_class TNewConnDlg
!c::ControlFocus Edit1

#IfWinActive ahk_class TInpComboDlg
!f::ControlFocus TEdit1
!r::ControlFocus Edit1

#IfWinActive ahk_class TCheckEditBox
!f::ControlFocus TEdit1

#IfWinActive ahk_class tdb_wndw_cls_edord
!l::ControlFocus SysListView321

#IfWinActive ahk_class IsoDraw5Class
/::
WinMenuSelectItem, , , Macros, Search
WinWait Macro Input:
WinWaitActive ahk_class IsoDraw5Class
WinMenuSelectItem, , , Window, Size, Full page
WinMenuSelectItem, , , Window, Size, 200 `%
return

^/::
WinMenuSelectItem, , , Macros, Search-Exact
WinWait Macro Input:
WinWaitActive ahk_class IsoDraw5Class
WinMenuSelectItem, , , Window, Size, Full page
WinMenuSelectItem, , , Window, Size, 200 `%
return

^r::
WinMenuSelectItem, , , Macros, Initialize-Window
Saved_KeyDelay := A_KeyDelay
SetKeyDelay 100
SendEvent !-x
SetKeyDelay %Saved_KeyDelay%
return

#IfWinActive

#w::PostMessage 0x112, 0xf060, , , A

^k::
if WinActive("ahk_class Emacs") or WinActive("ahk_class mintty") or WinActive("ahk_class Vim") or WinActive("ahk_class PuTTY")
  Dg := "kk"
else
  Input Dg, L2 C
if (Dg = "kk") {
  Hotkey ^k, Off
  SendInput ^k
  Hotkey ^k, On
  return
}
digraph := digraphs.item[Dg]
if (digraph = "") {
  Dg := SubStr(Dg, 2, 1) . SubStr(Dg, 1, 1)
  digraph := digraphs.item[Dg]
  if (digraph = "")
    return
}
SendInput %digraph%
return

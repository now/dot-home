#NoEnv
#SingleInstance force
#MaxHotkeysPerInterval 180

Process, Priority, , H
SendMode Input
SetTitleMatchMode Regex

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

TagEditorCloseAll()
{
  TagEditor := ComObjActive("TagEditor.Application")
  Count := TagEditor.Documents.Count
  loop %Count% {
    TagEditor.Documents.Item(Count - (A_Index - 1) - 1).Close()
  }
}

ScrollTimeout := 1500
ScrollBoost := 20
ScrollLimit := 60
scrollDistance := 0
scrollVMax := 1

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

#IfWinActive ^SDL Trados TagEditor
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

WheelUp::Goto Scroll
WheelDown::Goto Scroll
WheelLeft::Goto Scroll
WheelRight::Goto Scroll

Scroll:
  t := A_TimeSincePriorHotkey
  if (A_PriorHotkey = A_ThisHotkey && t < ScrollTimeout) {
    scrollDistance++
    v := (t < 80 && t > 1) ? (250.0 / t) - 1 : 1
    if (ScrollBoost > 1 && scrollDistance > ScrollBoost) {
      if (v > scrollVMax)
        scrollVMax := v
      else
        v := scrollVMax
      v *= scrollDistance / ScrollBoost
    }
    v := (v > 1) ? ((v > ScrollLimit) ? ScrollLimit : Floor(v)) : 1
    MouseClick, %A_ThisHotkey%, , , v
  } else {
    scrollDistance := 0
    scrollVMax := 1
    MouseClick %A_ThisHotkey%
  }
  return

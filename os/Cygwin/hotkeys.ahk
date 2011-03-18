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

#IfWinActive ^Job Overview\*?$ ahk_class WindowsForms10.Window.8.app3
!g::ControlFocus WindowsForms10.EDIT.app319

!u::ControlFocus WindowsForms10.COMBOBOX.app32

!w::
SendInput !sWeibull{Enter}
SendInput !o
return

Escape::!c

Enter::!o

#IfWinActive ^Customer|Employee$ ahk_class WindowsForms10.Window.8.app3
Escape::WinClose

#IfWinActive ^Nirvana ahk_class WindowsForms10.Window.8.app3
!n::
ControlFocus WindowsForms10.EDIT.app322
if (ErrorLevel != 0)
  return
ControlGet Edit, Hwnd, , WindowsForms10.EDIT.app322
if (ErrorLevel != 0)
  return
SendMessage 177, 0, -1, , ahk_id %Edit%
return

!j::ControlFocus WindowsForms10.SysListView32.app31

!F4::
WinClose
WinWait Exit Application?, Are you sure you want to exit?, 2
ControlSend Button1, {Enter}
return

^!c::
Control Check, , WindowsForms10.Button.app39
return

#IfWinActive ^Deliver ahk_class WindowsForms10.Window.8.app3
!d::Control Check, , WindowsForms10.BUTTON.app33
Enter::!o

#IfWinActive Microsoft Outlook$ ahk_class rctrl_renwnd32
!i::
ControlFocus NetUIHWND4
if (ErrorLevel = 0)
  return
SendInput !s
return

!c::ControlFocus _WwG1

!l::ControlFocus SUPERGRID1

!y::
SendInput !ts
WinWaitActive ^Trust Center$
SendInput {Up 4}!g
WinWaitActive ^COM Add-Ins$
SendInput {PageUp 4}{Up}
Sleep 50
SendInput {Space}{Enter}
WinWaitActive Microsoft Outlook$ ahk_class rctrl_renwnd32
SendInput !ts
WinWaitActive ^Trust Center$
SendInput {Up 4}!g
WinWaitActive ^COM Add-Ins$
SendInput {PageUp 4}{Up}
Sleep 50
SendInput {Space}{Enter}
return

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
^m::
Hotkey ^m, Off
Input Key, L1 C M
if Key = b
  DllCall("SetCursorPos", int, A_ScreenWidth, int, 0)
else if (Key = "k" or Key = CTRL_K)
  WinClose A
else if Key = m
  SendInput ^m
else if (Key = CTRL_M)
  SendInput {Alt down}{Tab}{Alt up}
else if Key = r
  Reload
else if Key = w
{
  SavedDetectHiddenWindows := A_DetectHiddenWindows
  DetectHiddenWindows on
  PostMessage %WM_COMMAND%, 1001, , , ahk_class BINDKEYS
  DetectHiddenWindows %SavedDetectHiddenWindows%
}
else if Key = c
{
  WinGetPos X, Y, Width, Height, A
  SysGet nMonitors, MonitorCount
  if (nMonitors > 1) {
    SysGet PrimaryMonitor, MonitorPrimary
    if (PrimaryMonitor + 1 > nMonitors)
      OtherMonitor := PrimaryMonitor - 1
    else
      OtherMonitor := PrimaryMonitor + 1
    SysGet Monitor, Monitor, %OtherMonitor%
  } else {
    SysGet Monitor, Monitor
  }
  X := MonitorLeft + (MonitorRight - MonitorLeft) / 2 - Width / 2
  Y := MonitorTop + (MonitorBottom - MonitorTop) / 2 - Height / 2
  WinMove A, , X, Y
}
else if Key = x
{
  WinMove A, , 0, 0, %A_ScreenWidth%, %A_ScreenHeight%
}
else if Key = y
{
  WinGetPos X, Y, Width, Height, A
  SysGet Monitor, Monitor
  X := MonitorLeft + (MonitorRight - MonitorLeft) / 2 - Width / 2
  Y := MonitorTop + (MonitorBottom - MonitorTop) / 2 - Height / 2
  WinMove A, , X, Y
}
Hotkey ^m, On
return

#w::PostMessage 0x112, 0xf060, , , A

^k::
if WinActive("ahk_class Emacs") or WinActive("ahk_class Vim") or WinActive("ahk_class PuTTY")
  Dg := "kk"
else
  Input Dg, L2 C
if (Dg = "kk") {
  Hotkey ^k, Off
  SendInput ^k
  Hotkey ^k, On
  return
}
digraph := digraphs[Dg]
if (digraph = "") {
  Dg := SubStr(Dg, 2, 1) . SubStr(Dg, 1, 1)
  digraph := digraphs[Dg]
  if (digraph = "")
    return
}
SendInput %digraph%
return

If WScript.Arguments.Count < 2 Then WScript.Quit 1

Set AutoIt = CreateObject("AutoItX3.Control")
AutoIt.Opt "WinTitleMatchMode", 4

IsoDrawTitle = "classname=IsoDraw5Class"

Timeout = 60

Sub WaitFor(Title)
  If AutoIt.WinWait(Title, "", Timeout) = 0 Then WScript.Quit 1
End Sub

Sub WaitForActive(Title)
  If AutoIt.WinWaitActive(Title, "", Timeout) = 0 Then WScript.Quit 1
End Sub

Sub Press(Button)
  AutoIt.ControlSend "", "", Button, "{Space}"
End Sub

AutoIt.WinActivate IsoDrawTitle
If AutoIt.WinActive("") = 0 Then WScript.Quit 1

Operation = WScript.Arguments(0)

For i = 1 To WScript.Arguments.Count - 1
  WaitForActive IsoDrawTitle
  If AutoIt.WinMenuSelectItem("", "", "&Macros", Operation) = 0 Then WScript.Quit 1
  WaitFor "Macro Input:"
  If AutoIt.ControlSetText("", "", "Edit1", WScript.Arguments(i)) = 0 Then WScript.Quit 1
  Press "Button1"
  WaitForActive IsoDrawTitle
  AutoIt.Sleep 2500
  WaitForActive IsoDrawTitle
  AutoIt.Sleep 3500
Next

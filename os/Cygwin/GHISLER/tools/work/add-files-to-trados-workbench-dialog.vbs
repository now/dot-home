Set AutoIt = CreateObject("AutoItX3.Control")
AutoIt.Opt "WinTitleMatchMode", 4

For Each Window In Array("Clean Up Files", "Analyse Files", "Translate Files")
  If AutoIt.WinExists(Window) Then
    For Each Argument In WScript.Arguments
      AutoIt.ControlCommand "", "", "ListBox1", "AddString", Argument
    Next
    AutoIt.WinActivate ""
    AutoIt.ControlFocus "", "", "ListBox1"
    WScript.Quit AutoIt.error = 0
  End If
Next

WScript.Quit 1

WorkbenchApplication = "TW4Win.Application"
On Error Resume Next
Set Workbench = GetObject(, WorkbenchApplication)
If Err.Number <> 0 Then
  Set Workbench = CreateObject(WorkbenchApplication)
  If Workbench Is Nothing Then WScript.Quit 1
End If

Workbench.TranslationMemory.Open WScript.Arguments(0), "SUPER", , "x"
MsgBox WScript.Arguments() & ": " & Err.Description & " (" & Err.Number & ")"

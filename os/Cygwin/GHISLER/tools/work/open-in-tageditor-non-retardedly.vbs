TagEditorApplication = "TagEditor.Application"
On Error Resume Next
Set TagEditor = GetObject(, TagEditorApplication)
If Err.Number <> 0 Then
  Set TagEditor = CreateObject(TagEditorApplication)
  If TagEditor Is Nothing Then WScript.Quit 1
End If
On Error GoTo 0

ErrorMessage = ""
For Each Argument In WScript.Arguments
  On Error Resume Next
  TagEditor.Documents.Open Argument, Nothing
  If Err.Number <> 0 Then ErrorMessage = ErrorMessage & Argument & ": " & Err.Description & " (" & Err.Number & ")" & vbNewLine
Next

TagEditor.Visible = True
TagEditor.UserControl = True
TagEditor.SetFocus

If Len(ErrorMessage) > 0 Then MsgBox ErrorMessage, vbExclamation, "TagEditor"

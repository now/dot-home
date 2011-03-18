If WScript.Arguments.Count = 0 Then WScript.Quit 1

Set AutoIt = CreateObject("AutoItX3.Control")
AutoIt.AutoItSetOption "WinTitleMatchMode", 4

IdealWindowTitle = "classname=tdb_wndw_cls_main"
OpenDialogTitle = "Select starter files"

Timeout = 30

Function ArrayPush(Ary, Value)
  If IsEmpty(Ary) Or IsNull(Ary) Then
    ReDim Ary(0)
  Else
    ReDim Preserve Ary(UBound(Ary) + 1)
  End If
  Ary(UBound(Ary)) = Value

  ArrayPush = Ary
End Function

Function GetIdealHandle(HadToStartIdeal)
  If AutoIt.WinExists(IdealWindowTitle) = 0 Then
    Set Fs = CreateObject("Scripting.FileSystemObject")
    Set Shell = CreateObject("WScript.Shell")
    Set ShellApp = CreateObject("Shell.Application")
    ShellApp.ShellExecute Fs.BuildPath(Shell.Environment("Process")("ProgramFiles"), "IDEA INSTITUTE INC\0.2 !DEAL Translation for Adobe InDesign CS\tradobeindecs00.exe")
    If AutoIt.WinWait(IdealWindowTitle, "", Timeout) = 0 Then WScript.Quit 1
    HadToStartIdeal = True
  Else
    HadToStartIdeal = False
  End If

  GetIdealHandle = AutoIt.WinGetHandle("", "")
  If AutoIt.Error = 1 Then WScript.Quit 1
End Function

Function DivideIntoPerFolder(Paths)
  Set Fs = CreateObject("Scripting.FileSystemObject")
  Set Folders = CreateObject("Scripting.Dictionary")

  For Each Path In Paths
    Folder = Fs.GetParentFolderName(Path)
    If Not Folders.Exists(Folder) Then Folders.Add Folder, Null
    Folders.Item(Folder) = ArrayPush(Folders.Item(Folder), Fs.GetFileName(Path))
  Next

  Set DivideIntoPerFolder = Folders
End Function

Function DivideIntoPerExtension(FileNames)
  Set Fs = CreateObject("Scripting.FileSystemObject")
  Set Extensions = CreateObject("Scripting.Dictionary")

  For i = 0 To UBound(FileNames)
    FileName = FileNames(i)
    Extension = Fs.GetExtensionName(FileName)
    If Not Extensions.Exists(Extension) Then Extensions.Add Extension, Null
    Extensions.Item(Extension) = ArrayPush(Extensions.Item(Extension), FileName)
  Next

  Set DivideIntoPerExtension = Extensions
End Function

Function DivideIntoBatchesAndProccess(IdealHandle, Folder, Extension, FileNames)
  Batch = Null

  For i = 0 To UBound(FileNames)
    Batch = ArrayPush(Batch, FileNames(i))
    If UBound(Batch) = 5 Then
      Process IdealHandle, Folder, Extension, Batch
      Batch = Null
    End If
  Next

  If Not IsNull(Batch) Then Process IdealHandle, Folder, Extension, Batch
End Function

Sub Process(IdealHandle, Folder, Extension, FileNames)
  ' FIXME: It seems handles retrieved from AutoIt can’t be passed to the AutoIt
  ' COM object.
  If AutoIt.WinMenuSelectItem(IdealWindowTitle, "", "&File", "&New" & Chr(9) & "Ctrl+N") = 0 Then WScript.Quit 1
  If AutoIt.WinWait(OpenDialogTitle, "", Timeout) = 0 Then WScript.Quit 1
  AutoIt.Sleep 250
  If AutoIt.ControlSetText("", "", "Edit1", Folder) = 0 Then WScript.Quit 1
  AutoIt.Sleep 250
  AutoIt.ControlSend "", "", "Button2", "{Enter}"
  Do
    If AutoIt.Error <> 0 Then WScript.Quit 1
    AutoIt.Sleep 500
  Loop While AutoIt.ControlCommand("", "", "Button2", "IsEnabled", "") = 0
  FileList = ""
  For i = 0 To UBound(FileNames)
    FileList = FileList & """" & FileNames(i) & """ "
  Next
  If AutoIt.ControlSetText("", "", "Edit1", FileList) = 0 Then WScript.Quit 1
  Select Case Extension
  Case "joy"
    FileType = 2
  Case "ord"
    FileType = 1
  Case Else
    FileType = 0
  End Select
  AutoIt.ControlCommand "", "", "ComboBox2", "SetCurrentSelection", FileType
  If AutoIt.Error <> 0 Then WScript.Quit 1
  AutoIt.Sleep 500
  AutoIt.ControlSend "", "", "Button2", "{Enter}"

  Set Pattern = CreateObject("VBScript.RegExp")
  Pattern.Pattern = "Failure on dropped files"
  If AutoIt.WinWait("TRADOBEINDECS", "", TimeOut * 10) = 0 Then WScript.Quit 1
  WasFailure = Pattern.Test(AutoIt.WinGetText(""))
  AutoIt.WinClose ""
  If Not WasFailure Then AutoIt.WinClose "classname=IEFrame"
End Sub

StartedIdeal = False
IdealHandle = GetIdealHandle(StartedIdeal)

Set Folders = DivideIntoPerFolder(WScript.Arguments)
For Each Folder In Folders.Keys
  Set Extensions = DivideIntoPerExtension(Folders.Item(Folder))

  For Each Extension In Extensions.Keys
    DivideIntoBatchesAndProccess IdealHandle, Folder, Extension, Extensions.Item(Extension)
  Next
Next

If StartedIdeal Then AutoIt.WinClose IdealWindowTitle
If WasFailure Then WScript.Quit 1

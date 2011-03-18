Const ForReading = 1
Const SystemDefaultFormat = -2

Sub Die(Explanation)
  MsgBox Explanation, vbOKOnly, "Copy to Multiple Folders"
  WScript.Quit 1
End Sub

Function GetFilePaths(FileListPath)
  Set Fs = CreateObject("Scripting.FileSystemObject")

  Set FileList = Fs.OpenTextFile(FileListPath, ForReading, False, SystemDefaultFormat)

  FilePaths = Split(FileList.ReadAll, Chr(13) & Chr(10))
  ReDim Preserve FilePaths(UBound(FilePaths) - 1)

  FileList.Close

  GetFilePaths = FilePaths
End Function

Sub CheckArguments()
  If WScript.Arguments.Count = 0 Then Die("Missing arguments.")
  If WScript.Arguments.Count = 1 Then Die("Missing target-parent-folder argument.")
End Sub

Sub CopyFiles(FilePaths, TargetFolderPath)
  Set Fs = CreateObject("Scripting.FileSystemObject")

  If Not Fs.FolderExists(TargetFolderPath) Then Fs.CreateFolder TargetFolderPath

  ' If the target path doesn’t end with a ‘\’, CopyFile()/CopyFolder() get
  ' confused about what to actually copy to.
  TargetFolderPath = TargetFolderPath & "\"

  For i = 0 To UBound(FilePaths)
    FilePath = FilePaths(i)
    If Fs.FolderExists(FilePath) Then
      If Mid(FilePath, Len(FilePath)) = "\" Then FilePath = Mid(FilePath, 1, Len(FilePath) -1)
      Fs.CopyFolder FilePath, TargetFolderPath
    Else
      Fs.CopyFile FilePath, TargetFolderPath
    End If
  Next
End Sub

Sub CopyToAllSubFolders(FilePaths, TargetParentFolderPath)
  Set Fs = CreateObject("Scripting.FileSystemObject")

  For Each SubFolder In Fs.GetFolder(TargetParentFolderPath).SubFolders
    CopyFiles FilePaths, SubFolder.Path
  Next
End Sub

Sub CopyToFoldersPathsFromArguments(FilePaths, TargetParentFolderPath)
  Set Fs = CreateObject("Scripting.FileSystemObject")

  For i = 2 To WScript.Arguments.Count - 1
    CopyFiles FilePaths, Fs.BuildPath(TargetParentFolderPath, WScript.Arguments(i))
  Next
End Sub

CheckArguments

FilePaths = GetFilePaths(WScript.Arguments(0))
TargetParentFolderPath = WScript.Arguments(1)

If WScript.Arguments.Count = 2 Then
  CopyToAllSubFolders FilePaths, TargetParentFolderPath
Else
  CopyToFoldersPathsFromArguments FilePaths, TargetParentFolderPath
End If

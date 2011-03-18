Set Shell = CreateObject("Shell.Application")

For i = 0 To WScript.Arguments.Count - 1
  Shell.ShellExecute WScript.Arguments(i)
Next

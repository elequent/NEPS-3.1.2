Imports System.Globalization
Imports System.IO
Module Functions
    'divide string to substrings
    Public Function Divide(ByVal inputstr As String, ByVal length As Integer, ByVal separator As String) As String()
        Dim Arr() As String
        ReDim Arr(length)
        Arr = inputstr.Split(separator)
        Return Arr
    End Function
    'run bat file
    Public Sub RunBat(ByVal name As String)
        Dim pStart As New System.Diagnostics.Process
        pStart.StartInfo.FileName = pathTemp + name
        pStart.StartInfo.WorkingDirectory = pathTemp  'Set to where ever the files you want to convert are
        pStart.StartInfo.WindowStyle = ProcessWindowStyle.Hidden
        pStart.StartInfo.UseShellExecute = False
        pStart.StartInfo.RedirectStandardOutput = True
        pStart.Start()
        Dim strOutput As String
        strOutput = pStart.StandardOutput.ReadToEnd()
        pStart.WaitForExit()
        pStart.Dispose()
    End Sub
    'merge datetime
    Public Function MergeDateTime(IntDate As String, time As String) As DateTime
        Dim OrigDate As DateTime = New DateTime(1900, 1, 1, 0, 0, 0)
        OrigDate = OrigDate.AddDays(ConvertToInt(IntDate) - 2)
        Dim CheckDate As DateTime = DateTime.ParseExact(time, FarmInfo.TimeFormat, Nothing)
        CheckDate = New DateTime(OrigDate.Year, OrigDate.Month, OrigDate.Day, CheckDate.Hour, CheckDate.Minute, CheckDate.Second)
        Return CheckDate
    End Function
    'convert strting to datetime
    Public Function ConvertToDateTime(str As String, format As String) As DateTime
        Dim CheckDate As DateTime
        Try
            CheckDate = DateTime.ParseExact(str, format, Nothing)
        Catch
            CheckDate = "01/01/2017 00:00:00"
        End Try
        Return CheckDate
    End Function
    'Remove special characters in a string
    Public Function RemoveSpecialCharacter(str As String) As String
        For Each i As Char In str
            Select Case i
                Case " ", "'", "*"
                    str = str.Replace(i, String.Empty)
            End Select
        Next
        Return str
    End Function
    'Delete temporate files
    Public Sub DeleteFilesFromFolder(Folder As String, filetype As String)
        If Directory.Exists(Folder) Then
            For Each _file As String In Directory.GetFiles(Folder, filetype)
                File.Delete(_file)
            Next
        End If
    End Sub
    'conversion to double
    Public Function ConvertToDouble(input As String) As Double
        Dim temp As Double
        Try
            temp = CDbl(input)
        Catch ex As Exception
            temp = 1
        End Try
        Return temp
    End Function
    'conversion to integer
    Public Function ConvertToInt(input As String) As Integer
        Dim temp As Double
        Try
            temp = CInt(input)
        Catch ex As Exception
            temp = 1
        End Try
        Return temp
    End Function
End Module

Imports System.IO
Module SharedFunctions
    'divide string to substrings
    Public Function Divide(ByVal str As String, ByVal length As Integer, ByVal sep As String) As String()
        Dim Arr() As String
        ReDim Arr(length)
        Arr = str.Split(sep)
        Return Arr
    End Function
    'run bat file
    Public Sub RunBat(ByVal name As String)
        Dim pStart As New System.Diagnostics.Process
        pStart.StartInfo.FileName = pathTemp + name
        pStart.StartInfo.WorkingDirectory = pathTemp  'Set to where ever the files you want to convert are
        pStart.StartInfo.WindowStyle = ProcessWindowStyle.Hidden
        pStart.Start()
        pStart.WaitForExit()
    End Sub
    'run exe file
    Public Sub RunExE(ByVal path As String, ByVal name As String, ByVal Arg As String)
        Dim pStart As New System.Diagnostics.Process
        pStart.StartInfo.FileName = path + name
        pStart.StartInfo.Arguments = Arg
        pStart.StartInfo.WorkingDirectory = path  'Set to where ever the files you want to convert are
        pStart.StartInfo.WindowStyle = ProcessWindowStyle.Hidden
        pStart.Start()
    End Sub
    'convert strting to datetime
    Public Function ConvertToDateTime(str As String, df As String) As DateTime
        Dim CheckDate As DateTime
        Try
            CheckDate = DateTime.ParseExact(str, df, Nothing)
        Catch
            CheckDate = New DateTime(Now.Year, Now.Month, Now.Day, 0, 0, 0)
        End Try
        Return CheckDate
    End Function
    'check all zeros
    Public Function CheckAllZeros(array() As Integer) As Integer
        Dim AllZero As Integer = 0
        For i As Integer = 0 To array.Length - 1
            If array(i) = 0 Then
                AllZero += 1
            End If
        Next
        If AllZero <> array.Length Then
            Return 1
        End If
        Return 0
    End Function
    'convert strting to date
    Public Function ConvertToDate(str As String) As Date
        Dim CheckDate As Date
        Try
            CheckDate = Date.ParseExact(str, "yyyy-MM-dd", Nothing)
        Catch
            CheckDate = "01/01/2017"
        End Try
        Return CheckDate
    End Function
    'convert strting to date
    Public Function ConvertToTime(str As String) As DateTime
        Dim CheckDate As DateTime
        Try
            CheckDate = DateTime.ParseExact(str, "h:mm tt", Nothing)
        Catch
            CheckDate = "0:00 AM"
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
    ''Round the schedule to the closest hour
    Public Function Round(ByVal Val As DateTime, ByVal StartTime As Integer, ByVal EndTime As Integer) As DateTime
        Dim Temp As DateTime
        If Math.Abs(Val.Minute - StartTime) < Math.Abs(Val.Minute - EndTime) Then
            Temp = Val.AddMinutes(StartTime - Val.Minute)
        Else
            Temp = Val.AddMinutes(EndTime - Val.Minute)
        End If
        Return Temp
    End Function
    Public Sub SortPriority(Array() As Integer, Index() As Integer)
        Dim tmp As Integer = 0
        For i As Integer = 0 To Index.Length - 1
            For j As Integer = i To Index.Length - 1
                If Array(i) > Array(j) Then
                    tmp = Index(j)
                    Index(j) = Index(i)
                    Index(i) = tmp
                End If
            Next
        Next
    End Sub
    'Sort the array and keep the index
    Public Sub SortArray(Array() As Double, Index() As Integer)
        Dim tmp As Integer = 0
        For i As Integer = 0 To Index.Length - 1
            For j As Integer = 0 To Index.Length - 2
                If Array(Index(j)) > Array(Index(j + 1)) Then
                    tmp = Index(j + 1)
                    Index(j + 1) = Index(j)
                    Index(j) = tmp
                End If
            Next
        Next
    End Sub
    Public Sub Writemsg(msg As String)
        outputstring.Add(msg)
        Console.WriteLine(msg)
    End Sub
    Public Sub Writeshortmsg(msg As String)
        outputstring.Add(msg)
        shortstring.Add(msg)
        Console.WriteLine(msg)
    End Sub
    Public Sub outputmsg()
        Using writer As New StreamWriter(pathSch + CStr(UserID) + Now.Second.ToString + Now.Minute.ToString + Now.Hour.ToString + Now.Day.ToString + Now.Month.ToString + "~msg.txt", False)
            For i As Integer = 0 To outputstring.Count - 1
                writer.WriteLine(outputstring(i))
            Next
        End Using
        Using writer As New StreamWriter(pathSch + CStr(UserID) + "~shortmsg.txt", False)
            For i As Integer = 0 To shortstring.Count - 1
                writer.WriteLine(shortstring(i))
            Next
        End Using
    End Sub
    'check fallow block
    Public Function CheckFallow(name As String) As Boolean
        For i As Integer = 0 To FallowBlock.Length - 1
            If name = FallowBlock(i) Then
                Return True
            End If
        Next
        Return False
    End Function
    'check priority
    Public Function CheckPriority(name As String) As Integer
        For i As Integer = 0 To Priority.Length - 1
            If name = Priority(i) Then
                Return i
            End If
        Next
    End Function
End Module

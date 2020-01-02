Imports System.IO
Imports System.DateTime
Imports System.Globalization
Module DateTimeTest
    Dim TestID() As String
    Dim Time(11) As String

    Dim AM As String = " tt"
    Dim HourFormat As String
    Dim MinuteFormat As String
    Dim SecondFormat As String
    Public TimeFormat As String

    'Program path
    Dim path As String = Directory.GetCurrentDirectory() + "\Data\Temp\"        'Current program path
    'divide string to substrings
    Public Function Divide(ByVal str As String, ByVal length As Integer, ByVal sep As String) As String()
        Dim Arr() As String
        ReDim Arr(length)
        Arr = str.Split(sep)
        Return Arr
    End Function
    Private Sub RunBat(ByVal name As String)
        Dim pStart As New System.Diagnostics.Process
        pStart.StartInfo.FileName = path + name
        pStart.StartInfo.WorkingDirectory = path  'Set to where ever the files you want to convert are
        pStart.StartInfo.WindowStyle = ProcessWindowStyle.Hidden
        pStart.Start()
        pStart.WaitForExit()
    End Sub
    Private Sub BatteryInfo()
        System.IO.File.WriteAllText(path + "ProfileSQL.txt", "SELECT UID FROM Devices WHERE (DeviceType = 31)")
        If File.Exists(path + "ProfileSQL.txt") Then
            System.IO.File.WriteAllText(path + "ProfileCMD.bat", "nxSQLExec /Alias:ALConf /SQLFile:ProfileSQL.txt /ResultFile:Profile.txt")
        End If

        If File.Exists(path + "ProfileCMD.bat") Then
            RunBat("ProfileCMD.bat")
            While File.Exists(path + "Profile.txt") = False
                RunBat("ProfileCMD.bat")
            End While
        End If

        If File.Exists(path + "Profile.txt") Then
            Dim lines As String() = File.ReadAllLines(path + "Profile.txt")
            ReDim TestID(lines.Length - 2)
            For i As Integer = 1 To lines.Length - 1
                TestID(i - 1) = lines(i)
            Next
        End If
    End Sub
    Private Sub WriteSQL()
        Dim EndDT As DateTime = New DateTime(Now.Year, Now.Month, Now.Day, 11, 0, 0)
        Dim StartDT As DateTime = New DateTime(Now.Year, Now.Month, Now.Day, 1, 0, 0)

        Dim Flag As Integer = 0
        Dim DayIndex As Integer = 0
        While (Flag = 0)
            Dim StartDateString As String = StartDT.ToString("yyyy-MM-dd HH:mm:ss")
            Dim EndDateString As String = EndDT.ToString("yyyy-MM-dd HH:mm:ss")

            Dim SQLBattery As String = "SELECT CAST(LogDT As Time) FROM SensorLogs WHERE ("
            For i As Integer = 0 To TestID.Length - 1
                SQLBattery = SQLBattery + "UID=" + TestID(i) + " or  "
            Next
            SQLBattery = SQLBattery.Substring(0, Len(SQLBattery) - 4)
            SQLBattery = SQLBattery + ") And (CAST(LogDT As TimeStamp) >= CAST('" + StartDateString + "' AS TimeStamp)) AND (CAST(LogDT AS TimeStamp)< CAST('" + EndDateString + "' AS TimeStamp))"
            Using outputFile As New StreamWriter(path + "BatterySQL.txt")
                outputFile.WriteLine(SQLBattery)
            End Using

            If File.Exists(path + "BatterySQL.txt") Then
                System.IO.File.WriteAllText(path + "BatteryCMD.bat", "nxSQLExec /Alias:ALData /SQLFile:BatterySQL.txt /ResultFile:Battery.txt")
            End If

            If File.Exists(path + "BatteryCMD.bat") Then
                RunBat("BatteryCMD.bat")
                While File.Exists(path + "Battery.txt") = False
                    RunBat("BatteryCMD.bat")
                End While
            End If

            If File.Exists(path + "Battery.txt") Then
                Dim lines As String() = File.ReadAllLines(path + "Battery.txt")
                If lines.Length > 3 Then
                    Flag = 1
                End If
            End If
            StartDT = StartDT.AddDays(-30)
            EndDT = EndDT.AddDays(-30)
        End While

        If File.Exists(path + "Battery.txt") Then
            Dim lines As String() = File.ReadAllLines(path + "Battery.txt")
            For i As Integer = 1 To 12
                Time(i - 1) = lines(i)
            Next
        End If
    End Sub
    Public Sub CheckTimeFormat()
        Dim Array() As String
        Dim TimeArray() As String
        Array = Divide(Time(1), 2, " ")
        TimeArray = Divide(Array(0), 3, ":")

        If TimeArray(1).Length = 2 Then
            MinuteFormat = "mm"
        Else
            MinuteFormat = "m"
        End If
        If TimeArray(2).Length = 2 Then
            SecondFormat = "ss"
        Else
            SecondFormat = "s"
        End If

        If Array.Length = 1 Then
            HourFormat = "H"
            TimeFormat = HourFormat + ":" + MinuteFormat + ":" + SecondFormat
        Else
            If TimeArray(0).Length = 2 Then
                HourFormat = "hh"
            Else
                HourFormat = "h"
            End If
            TimeFormat = HourFormat + ":" + MinuteFormat + ":" + SecondFormat + AM
        End If
        Console.WriteLine(TimeFormat)
    End Sub
    'Run data extraction, assignment, calculation and storage
    Public Sub Run()
        BatteryInfo()
        WriteSQL()
        CheckTimeFormat()
    End Sub
End Module

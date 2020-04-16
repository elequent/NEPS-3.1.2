Imports System.Drawing
Imports System.IO
Imports System.Timers
Imports System.Windows.Forms

Module Main
    Private Declare Auto Function GetConsoleWindow Lib "kernel32.dll" () As IntPtr
    Private Declare Auto Function ShowWindow Lib "user32.dll" (ByVal hWnd As IntPtr, ByVal nCmdShow As Integer) As Boolean

    Private Const SW_HIDE As Integer = 0
    Private Const SW_SHOW As Integer = 5

    Private aTimer As System.Timers.Timer
    'copy shift and cycle files to the shift folder
    Private Sub Irrigate()
        'download IrrigWeb irrigation schedule
        Download()
        'Console.WriteLine("IrrigWeb schedule downloaded!")
        'Assign IrrigWeb schedule to data arrays
        AssignIrrigWebSchData()
        'ReadHis()
        ReadDrip()
        ReadFurrow()
        'days to run irrigation in advanceD:\OneDrive - James Cook University\Irrigation\Program\Combined Test\Downlink V1.4\Downlink V1.1\Main.vb


        For j As Integer = 0 To HydraulicGroup.Length - 1
            If HydraulicGroup(j).Type = "Drip" Then
                CalculateOperationTime(0)
                RunDrip(0, j)
                CalShift(0, j)
            Else
                RunFurrow(0, j)
            End If

            WriteShift(j)
            WriteCycle(j)
        Next
        outputmsg()
        If KillAqualink() Then
            Console.WriteLine("Aqualink closed!")
            CopyFile()
            Console.WriteLine("Shift anc Cycle files copied")
            RunAqualink()
            Console.WriteLine("Aqualink restart!")
        End If
    End Sub
    Public Sub Screenshot(address As String)
        Dim bounds As Rectangle
        Dim screenshot As System.Drawing.Bitmap
        Dim graph As Graphics
        System.Threading.Thread.Sleep(200)
        bounds = Screen.PrimaryScreen.Bounds
        screenshot = New System.Drawing.Bitmap(bounds.Width, bounds.Height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        graph = Graphics.FromImage(screenshot)
        graph.CopyFromScreen(bounds.X, bounds.Y, 0, 0, bounds.Size, CopyPixelOperation.SourceCopy)
        screenshot.Save(pathSch + address)
    End Sub
    'Set timers
    Private Sub SetTimer()
        aTimer = New System.Timers.Timer(600 * 1000)
        AddHandler aTimer.Elapsed, AddressOf OnTimedEvent
        aTimer.AutoReset = False
        aTimer.Enabled = True
        aTimer.Start()
        Console.ReadKey()
    End Sub
    'Timer event
    Private Sub OnTimedEvent(source As Object, e As ElapsedEventArgs)
        Console.WriteLine("Wake up:" + Now.ToString)
        If Now.Hour = SchedulingHour And Now.Minute >= 10 And Now.Minute < 20 Then
            If CheckRunning() = True Then
                ReadConfig()
                ExtractProfile()
                ReadProfile()
                DeleteFilesFromFolder(pathTemp, "*.txt")
                DeleteFilesFromFolder(pathTemp, "*.bat")
                'DeleteFilesFromFolder(pathSch, "*.txt")
                outputstring.Clear()
                shortstring.Clear()
                'ShiftCount = 0
                'CycleCount = 0
                Irrigate()
                Threading.Thread.Sleep(5000)

                SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "eric.wang@jcu.edu.au", "eric wang")
                SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "aalinton@bigpond.com", "aaron linton")
                'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "steve@agritechsolutions.com.au", "steve attard")
                'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "yvette.everingham@jcu.edu.au", "Yvette Everingham")
                SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "steven.w@agritechsolutions.com.au", "Steven Wei")
                SchedulingHour = 8
            Else
                SchedulingHour = SchedulingHour + 1
            End If
            If SchedulingHour = 24 Then
                SchedulingHour = 0
            End If
        End If
        aTimer.Start()
        Console.WriteLine("Sleep:" + Now.ToString)
    End Sub
    'Main function
    Sub Main()

        'Dim hwnd As Integer
        'hwnd = GetConsoleWindow()
        'ShowWindow(hwnd, SW_HIDE)
        ReadConfig()
        ExtractProfile()
        ReadProfile()
        DeleteFilesFromFolder(pathTemp, "*.txt")
        DeleteFilesFromFolder(pathTemp, "*.bat")
        DeleteFilesFromFolder(pathSch, "*.txt")
        outputstring.Clear()
        shortstring.Clear()
        'ShiftCount = 0
        'CycleCount = 0
        Irrigate()
        Threading.Thread.Sleep(2000)
        'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "eric.wang@jcu.edu.au", "Eric wang")
        'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", "aalinton@bigpond.com", "Aaron linton")
        'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", " steve@agritechsolutions.com.au", "Steve attard")
        'SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", " yvette.everingham@jcu.edu.au", "Yvette Everingham")
        ''SendEmail(pathSch + CStr(UserID) + "~shortmsg.txt", " alejandra@agritechsolutions.com.au", "alejandra santos", "")
        'SetTimer()
        Console.Read()
    End Sub
End Module

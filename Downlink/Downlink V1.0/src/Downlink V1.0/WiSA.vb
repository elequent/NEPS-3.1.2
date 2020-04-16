Imports System.IO
Module WiSA
    Public Sub CopyFile()
        For i As Integer = 1 To 9
            File.Delete(ShftFolder + "Shift08" + CStr(i) + ".sft")
            File.Delete(ShftFolder + "Cycle1" + CStr(i) + ".sft")
        Next
        For i As Integer = 1 To Shift.Count
            File.Copy(pathTemp + "Shift08" + CStr(i) + ".sft", ShftFolder + "Shift08" + CStr(i) + ".sft", True)
        Next
        For i As Integer = 1 To Shift.Count
            File.Copy(pathTemp + "Cycle1" + CStr(i) + ".sft", ShftFolder + "Cycle1" + CStr(i) + ".sft", True)
        Next
    End Sub
    Public Sub WriteShift(group As Integer)
        For i As Integer = 0 To Shift.Count - 1
            If Shift(i).Sta.Length = HydraulicGroup(group).Valve.Length Then
                Dim filename As String = pathTemp + "Shift08" + CStr(i + 1) + ".sft"
                Using outputFile As New StreamWriter(filename)
                    outputFile.WriteLine("[Version Info]")
                    outputFile.WriteLine("Version Number=1.3.0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Shift Details]")
                    Dim name As String = ""
                    For j As Integer = 0 To Shift(i).Sta.Length - 1
                        If Shift(i).Sta(j) = 1 Then
                            name = name + HydraulicGroup(group).Valve(j).Name
                        End If
                    Next
                    outputFile.WriteLine("Shift Name=“ + name)
                    outputFile.WriteLine("Shift Enabled=1")
                    outputFile.WriteLine("Hydraulic Group=" + CStr(HydraulicGroup(group).Group))
                    outputFile.WriteLine("Repeatable=1")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Probe Details]")
                    outputFile.WriteLine("Start on Moisture=0")
                    outputFile.WriteLine("No Start Probes=0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Probe Stop Details]")
                    If HydraulicGroup(group).Type = "Furrow" Then
                        outputFile.WriteLine("Stop on Moisture=1")
                        outputFile.WriteLine("Stop on Moisture Difference=1")
                        outputFile.WriteLine("Stop Moisture Difference Value=50")
                        outputFile.WriteLine("No Stop Probes=" + CStr(HydraulicGroup(group).MoistureProbe.Length))
                        For j As Integer = 0 To HydraulicGroup(group).MoistureProbe.Length - 1
                            outputFile.WriteLine("Probe " + CStr(j + 1) + " UID=" + CStr(HydraulicGroup(group).MoistureProbe(j).UID))
                            outputFile.WriteLine("Probe " + CStr(j + 1) + " Shutdown Value=50")
                        Next
                    Else
                        outputFile.WriteLine("Stop on Moisture=0")
                        outputFile.WriteLine("Stop on Moisture Difference=0")
                        outputFile.WriteLine("Stop Moisture Difference Value=50")
                        outputFile.WriteLine("No Stop Probes=0")
                    End If
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Valve Details]")
                    outputFile.WriteLine("Inter Valve Delay=10")
                    outputFile.WriteLine("Reverse order Shutdown=1")
                    outputFile.WriteLine("use Valve Runtimes=0")
                    outputFile.WriteLine("Enforce Min Flow=0")
                    outputFile.WriteLine("Enforce Max Flow=0")
                    'For i As Integer = 0 To Shift.Count - 1
                    If HydraulicGroup(group).Type = "Furrow" And Shift(i).Sta.Length = HydraulicGroup(group).Valve.Length Then
                        Dim Runtime(Shift(i).Sta.Length - 1) As Integer
                        Dim largest As Integer = Integer.MinValue
                        Dim MaxiRun As Integer
                        For j As Integer = 0 To Shift(i).Sta.Length - 1
                            If Shift(i).Sta(j) = 1 Then
                                Runtime(j) = Shift(i).Sta(j) * HydraulicGroup(group).Valve(j).MaxRun
                                MaxiRun = Math.Max(largest, Runtime(j))
                            End If
                        Next
                        outputFile.WriteLine("Shift Runtime=" + CStr(MaxiRun * 3600))
                    Else
                        outputFile.WriteLine("Shift Runtime=" + CStr(Shift(i).Duration))
                    End If
                    'Next
                    'If HydraulicGroup(group).Type = "Furrow" Then
                    '    Dim Runtime(Shift(i).Sta.Length - 1) As Integer
                    '    Dim largest As Integer = Integer.MinValue
                    '    Dim MaxiRun As Integer
                    '    For j As Integer = 0 To Shift(i).Sta.Length - 1

                    '    Next
                    '    outputFile.WriteLine("Shift Runtime=" + CStr(MaxiRun * 3600))
                    'Else
                    '    outputFile.WriteLine("Shift Runtime=" + CStr(Shift(i).Duration))
                    'End If
                    Dim count As Integer = 0
                    For j As Integer = 0 To Shift(i).Sta.Length - 1
                        If Shift(i).Sta(j) = 1 Then
                            count = count + 1
                        End If
                    Next
                    outputFile.WriteLine("Number of Shift Valves=" + CStr(count))
                    count = 0
                    'pump rate required to power the valves
                    Dim PumpRate As Integer = 0
                    For j As Integer = 0 To Shift(i).Sta.Length - 1
                        If Shift(i).Sta(j) = 1 Then
                            outputFile.WriteLine("Valve " + CStr(count + 1) + " UID=" + CStr(HydraulicGroup(group).Valve(j).UID))
                            outputFile.WriteLine("Valve " + CStr(count + 1) + " RunTime=" + CStr(Shift(i).Duration))
                            outputFile.WriteLine("Valve " + CStr(count + 1) + " HasRun=0")
                            outputFile.WriteLine("Valve " + CStr(count + 1) + " HasOpn=1")
                            outputFile.WriteLine("Valve " + CStr(count + 1) + " Secs2Run=0")
                            PumpRate += HydraulicGroup(group).Valve(j).Flow
                            count = count + 1
                        End If
                    Next
                    outputFile.WriteLine("Scale Runtime on Previous=0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Volume Stop Details]")
                    outputFile.WriteLine("Max Volume=0")
                    outputFile.WriteLine("Flow Meter UID=0")
                    outputFile.WriteLine("")
                    'to find the pump count
                    Dim pumpcount As Integer = 1
                    'flow rate
                    Dim flowrate As Integer = HydraulicGroup(group).Pump(0).Flow
                    If HydraulicGroup(group).Pump.Length > 1 Then
                        For j As Integer = 1 To HydraulicGroup(group).Pump.Length - 1
                            If flowrate + HydraulicGroup(group).Pump(j).Flow < PumpRate Then
                                pumpcount += 1
                                flowrate += HydraulicGroup(group).Pump(j).Flow
                            End If
                        Next
                    End If

                    outputFile.WriteLine("[Pump Details]")
                    outputFile.WriteLine("No Shift Pumps=" + CStr(pumpcount))
                    For j As Integer = 0 To pumpcount - 1
                        outputFile.WriteLine("Pump " + CStr(j + 1) + " UID=" + CStr(HydraulicGroup(group).Pump(j).UID))
                    Next

                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Booster Pump Details]")
                    outputFile.WriteLine("No Booster Pumps=0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Injector Details]")
                    outputFile.WriteLine("No Shift Injectors=0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Date Information]")
                    outputFile.WriteLine("Last Day Run=0")
                    outputFile.WriteLine("Last Hour Run=0")
                    outputFile.WriteLine("Exit Status=")
                    Dim d4 As DateTime
                    Dim dRef As Date = New Date(1899, 12, 30, 0, 0, 0)
                    d4 = New Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day, DateTime.Now.Hour, DateTime.Now.Minute, 0)
                    Dim span1 As Double = d4.Subtract(dRef).TotalSeconds / 86400
                    outputFile.WriteLine("Modified=" + CStr(span1))
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Setpoint Sensor Details]")
                    outputFile.WriteLine("No of Setpoints=1")
                    outputFile.WriteLine("Sensor 0 UID=69")
                    outputFile.WriteLine("Value 0=32")
                    outputFile.WriteLine("Delay Time 0=0")
                End Using
            End If
        Next
    End Sub
    Public Sub WriteCycle(group As Integer)
        For i As Integer = 0 To Shift.Count - 1
            If Shift(i).Sta.Length = HydraulicGroup(group).Valve.Length Then
                Dim filename As String = pathTemp + "Cycle1" + CStr(i + 1) + ".sft"
                Using outputFile As New StreamWriter(filename)
                    outputFile.WriteLine("[Version Info]")
                    outputFile.WriteLine("Version Number=1.3.0")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Cycle Details]")
                    Dim name As String = ""
                    For j As Integer = 0 To Shift(i).Sta.Length - 1
                        If Shift(i).Sta(j) = 1 Then
                            name = name + HydraulicGroup(group).Valve(j).Name
                        End If
                    Next
                    outputFile.WriteLine("Cycle Name=" + name)
                    outputFile.WriteLine("Cycle Enabled=1")
                    outputFile.WriteLine("Cycle Paused=0")
                    outputFile.WriteLine("Cycle Ident=")
                    outputFile.WriteLine("Cycle Type=1")
                    outputFile.WriteLine("Idle Time=0")
                    outputFile.WriteLine("Merge Valves=1")
                    outputFile.WriteLine("Merge Open Valve First=1")
                    outputFile.WriteLine("Shift Factor=100")
                    outputFile.WriteLine("Hydraulic Group=1")
                    outputFile.WriteLine("Protection Shifts=none")
                    outputFile.WriteLine("Ignore Old Schedules=1")
                    outputFile.WriteLine("Delete Old Schedules=0")
                    outputFile.WriteLine("Delete Old Schedules Time=14")
                    outputFile.WriteLine("Ignore Old Schedules=1")
                    outputFile.WriteLine("Schedule Old Repeats=0")
                    outputFile.WriteLine("Schedule Count=1")
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Date Information]")
                    outputFile.WriteLine("Last Run Time=0")
                    outputFile.WriteLine("Exit Status=")
                    Dim dd As DateTime
                    Dim dRef As Date = New Date(1899, 12, 30, 0, 0, 0)
                    dd = New Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day, DateTime.Now.Hour, DateTime.Now.Minute, 0)
                    Dim spandd As Double = dd.Subtract(dRef).TotalSeconds / 86400
                    outputFile.WriteLine("Modified=" + CStr(spandd))
                    outputFile.WriteLine("")
                    outputFile.WriteLine("[Schedule 0]")
                    outputFile.WriteLine("Schedule Name=")
                    Dim span1 As Double = Shift(i).StartDT.Subtract(dRef).TotalSeconds / 86400
                    outputFile.WriteLine("Start Time=" + CStr(span1))
                    outputFile.WriteLine("Stop Time=" + CStr(span1))
                    outputFile.WriteLine("Loop Shifts=0")
                    outputFile.WriteLine("Repeat Time=0")
                    outputFile.WriteLine("Last Repeat Time=0")
                    outputFile.WriteLine("Shifts=8" + CStr(i + 1))
                    outputFile.WriteLine("Has Ran=0")
                End Using
            End If
        Next

    End Sub
    'Run Aqualink exe
    Public Sub RunAqualink()
        Process.Start("C:\WiSA7\AquaLink\Aqualink.exe")
        Threading.Thread.Sleep(10000)
        If Process.GetProcessesByName("Aqualink").Length >= 1 Then
            For Each ObjProcess As Process In Process.GetProcessesByName("Aqualink")
                AppActivate(ObjProcess.Id)
                Threading.Thread.Sleep(40000)
                'My.Computer.Keyboard.SendKeys("{F5}", True) 'sends the Enter key
                'Threading.Thread.Sleep(5000)
                'Screenshot("Downlink.bmp")
                TakeScreeShot()
                Console.WriteLine("Irrigation timer start!")
            Next
        End If
    End Sub
    Public Sub TakeScreeShot()
        My.Computer.Keyboard.SendKeys("%(W)", True) 'sends the Enter key
        Threading.Thread.Sleep(500)
        My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
        Threading.Thread.Sleep(1000)
        My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
        Threading.Thread.Sleep(1000)
        My.Computer.Keyboard.SendKeys("{ENTER}", True) 'sends the Enter key
        Threading.Thread.Sleep(1000)
        My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
        Threading.Thread.Sleep(1000)
        My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
        Threading.Thread.Sleep(1000)
        My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
        For i As Integer = 0 To Shift.Count - 1
            My.Computer.Keyboard.SendKeys("{DOWN}", True) 'sends the Enter key
            Screenshot("Downlink_" + (i + 1).ToString + ".bmp")
            Threading.Thread.Sleep(1000)
        Next
    End Sub
    'check if there is a cycle runing at this time
    Public Function CheckRunning() As Boolean
        Dim Folder As New IO.DirectoryInfo(ShftFolder)
        For Each File As IO.FileInfo In Folder.GetFiles("*.sft", IO.SearchOption.AllDirectories)
            If File.Name.Substring(0, 1) = "C" Then
                Dim lines As List(Of String) = System.IO.File.ReadAllLines(File.FullName).ToList
                For Each line As String In lines
                    If line.Length > 13 Then
                        If line.Substring(0, 11) = "Cycle Ident" Then
                            Return False
                        End If
                    End If
                Next
            End If
        Next
        Return True
    End Function
    'Kill Aqualink exe
    Public Function KillAqualink() As Boolean
        If CheckRunning() = True Then
            If Process.GetProcessesByName("Aqualink").Length >= 1 Then
                For Each ObjProcess As Process In Process.GetProcessesByName("Aqualink")
                    ObjProcess.Kill()
                    ObjProcess.WaitForExit()
                Next
            End If
            Return True
        Else
            Return False
        End If
    End Function
End Module

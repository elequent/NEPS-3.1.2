Imports System.IO
Imports System.Globalization
Module Database
    'Read configuration file
    Public Sub ReadConfig()
        If File.Exists(configfile) Then
            Dim lines() As String = IO.File.ReadAllLines(configfile)
            'go through each line for confirguration items
            For i As Integer = 0 To lines.Length - 1
                Dim Array() As String
                'split to name and value
                Array = lines(i).Split("=")
                If Array.Length = 2 Then
                    FarmInfo.UserID = IIf(Array(0) = "Site Key", Array(1), FarmInfo.UserID)
                    FarmInfo.ComputerName = IIf(Array(0) = "Computer Name", Array(1), FarmInfo.ComputerName)
                    FarmInfo.TimeFormat = IIf(Array(0) = "DateTime Format", Array(1), FarmInfo.TimeFormat)
                    FarmInfo.HasRainGauge = IIf(Array(0) = "Has Rain Gauge", Array(1), FarmInfo.HasRainGauge)
                    FarmInfo.UploadInterval = IIf(Array(0) = "Update Interval", Array(1), FarmInfo.UploadInterval)
                    FarmInfo.NoHydrauGrp = IIf(Array(0) = "No. Hydro Group", Array(1), FarmInfo.NoHydrauGrp)
                    HistoryData.HisImport = IIf(Array(0) = "Enable", ConvertToInt(Array(1)), HistoryData.HisImport)
                    Dim date_info As DateTimeFormatInfo = CultureInfo.CurrentCulture.DateTimeFormat()
                    Dim dateformat As String = date_info.ShortDatePattern + " " + date_info.LongTimePattern
                    HistoryData.HisStart = IIf(Array(0) = "Start Date", ConvertToDateTime(Array(1), dateformat), HistoryData.HisStart)
                    HistoryData.HisEnd = IIf(Array(0) = "End Date", ConvertToDateTime(Array(1), dateformat), HistoryData.HisEnd)

                    ReDim HydraulicGroup(FarmInfo.NoHydrauGrp - 1)

                    RainGauge.Name = IIf(Array(0) = "Rain Gauge Name", Array(1), RainGauge.Name)
                    RainGauge.UID = IIf(Array(0) = "Rain Gauge ID", Array(1), RainGauge.UID)
                End If

            Next
            Dim Index As Integer = 0
            For i As Integer = 0 To lines.Length - 1
                ' initialise each hydraulic group
                If Divide(lines(i), 2, "=")(0) = "Hydr Grp ID" Then
                    HydraulicGroup(Index).GroupID = Asc(Divide(lines(i), 2, "=")(1)) - Asc("A") + 1
                    HydraulicGroup(Index).Type = Divide(lines(i + 2), 2, "=")(1)
                    HydraulicGroup(Index).FarmName = Divide(lines(i + 1), 2, "=")(1)

                    ' initialise each pump
                    Dim NoPump As Integer = ConvertToInt(Divide(lines(i + 3), 2, "=")(1))
                    ReDim HydraulicGroup(Index).Pump(NoPump - 1)
                    For j As Integer = 0 To NoPump - 1
                        HydraulicGroup(Index).Pump(j).UID = ConvertToInt(Divide(lines(i + j + 4), 2, "=")(1))
                    Next

                    ' initialise each flowmeter
                    Dim NoFlowMeter As Integer = ConvertToInt(Divide(lines(i + 4 + NoPump), 2, "=")(1))
                    ReDim HydraulicGroup(Index).FlowMeter(NoFlowMeter - 1)
                    For j As Integer = 0 To NoFlowMeter - 1
                        HydraulicGroup(Index).FlowMeter(j).UID = ConvertToInt(Divide(lines(i + j + 5 + NoPump), 2, "=")(1))
                    Next

                    ' initialise each moisture probe
                    Dim NoMoistureProbe As Integer = ConvertToInt(Divide(lines(i + 5 + NoPump + NoFlowMeter), 2, "=")(1))
                    ReDim HydraulicGroup(Index).MoistureProbe(NoMoistureProbe - 1)
                    For j As Integer = 0 To NoMoistureProbe - 1
                        HydraulicGroup(Index).MoistureProbe(j).UID = ConvertToInt(Divide(lines(i + j + 6 + NoPump + NoFlowMeter), 2, "=")(1))
                    Next

                    ' initialise each valve
                    Dim NoValve As Integer = ConvertToInt(Divide(lines(i + 6 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                    ReDim HydraulicGroup(Index).Valve(NoValve - 1)
                    For j As Integer = 0 To NoValve - 1
                        HydraulicGroup(Index).Valve(j).UID = ConvertToInt(Divide(lines(i + 6 * j + 7 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                        HydraulicGroup(Index).Valve(j).MinThreshold = ConvertToInt(Divide(lines(i + 6 * j + 9 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                        HydraulicGroup(Index).Valve(j).MaxThreshold = ConvertToInt(Divide(lines(i + 6 * j + 10 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                        HydraulicGroup(Index).Valve(j).DefaultFlow = Divide(lines(i + 6 * j + 11 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1)
                        HydraulicGroup(Index).Valve(j).CustomCal = ConvertToInt(Divide(lines(i + 6 * j + 12 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                    Next
                    Index = Index + 1
                End If
            Next
        End If
    End Sub
    'Read profile, i.e., UID, landarea and flowrate
    Public Sub ExtractProfile()
        Dim SQL As String = "SELECT DeviceN,UID,DeviceType,HydGrp,LandArea,MinFlowRate,MaxFlowRate FROM Devices WHERE ("

        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                SQL = SQL + "UID=" + CStr(HydraulicGroup(i).Pump(j).UID) + " or  "
            Next
            For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                SQL = SQL + "UID=" + CStr(HydraulicGroup(i).FlowMeter(j).UID) + " or  "
            Next
            For j As Integer = 0 To HydraulicGroup(i).MoistureProbe.Length - 1
                SQL = SQL + "UID=" + CStr(HydraulicGroup(i).MoistureProbe(j).UID) + " or  "
            Next
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                SQL = SQL + "UID=" + CStr(HydraulicGroup(i).Valve(j).UID) + " or  "
            Next
        Next

        SQL = SQL.Substring(0, Len(SQL) - 4) + ")"
        System.IO.File.WriteAllText(pathTemp + "ProfileSQL.txt", SQL)
        Threading.Thread.Sleep(100)

        If File.Exists(pathTemp + "ProfileSQL.txt") Then
            Dim CMD As String = "nxSQLExec /Alias:ALConf /SQLFile:ProfileSQL.txt /ResultFile:Profile.txt"
            System.IO.File.WriteAllText(pathTemp + "ProfileCMD.bat", CMD)
        End If
        Threading.Thread.Sleep(100)

        If File.Exists(pathTemp + "ProfileCMD.bat") Then
            RunBat("ProfileCMD.bat")
        End If
    End Sub
    'Extract rain gauge data
    Public Function ReadRainData(ByVal str As DateTime, ByVal flag As String) As Double
        Dim CMD As String = "SELECT LogDT,UID,SValue FROM SensorLogs WHERE UID=" + CStr(RainGauge.UID)
        CMD = CMD + " And (SValue <> 0)  And (CAST(LogDT AS TimeStamp)>= CAST('" +
                str.ToString("yyyy-MM-dd HH:mm:ss") + "' AS TimeStamp)) AND (CAST(LogDT AS TimeStamp)< CAST('" +
                str.AddDays(1).ToString("yyyy-MM-dd HH:mm:ss") + "' AS TimeStamp))"

        System.IO.File.WriteAllText(pathTemp + "Rain" + flag + ".txt", CMD)
        Threading.Thread.Sleep(100)

        If File.Exists(pathTemp + "Rain" + flag + ".txt") Then
            CMD = "nxSQLExec /Alias:ALData /SQLFile:Rain" + flag + ".txt /ResultFile:RainData" + flag + ".txt"
            System.IO.File.WriteAllText(pathTemp + "RainCMD" + flag + ".bat", CMD)
        End If
        Threading.Thread.Sleep(100)

        If File.Exists(pathTemp + "RainCMD" + flag + ".bat") Then
            RunBat("RainCMD" + flag + ".bat")
        End If


        If File.Exists(pathTemp + "RainData" + flag + ".txt") Then
            Dim lines() As String = IO.File.ReadAllLines(pathTemp + "RainData" + flag + ".txt")
            Dim Array() As String
            If lines.Length > 2 Then
                Array = Divide(lines(lines.Length - 1), 2, ",")
                Return ConvertToDouble(Array(2))
            End If
        End If
        Return -1
    End Function
    'Calculate rainfall data based on the cumulative rainfall
    Public Sub GenRain()
        Using outputFile As New StreamWriter(pathRainfall + FarmInfo.UserID + "~" + FarmInfo.ComputerName + ".dat")
            outputFile.WriteLine("Farm Name,Rain Gauge Name, Rainfall Date,RainFall (mm)")
            For i As Integer = -1 To 5
                Dim OldRain, NewRain As Double
                OldRain = ReadRainData(DateRange.StartDate.AddDays(i), "1")
                Threading.Thread.Sleep(200)
                NewRain = ReadRainData(DateRange.StartDate.AddDays(i + 1), "2")

                If NewRain = -1 Or OldRain = -1 Then
                    RainGauge.DailyRain = -1
                Else
                    RainGauge.DailyRain = NewRain - OldRain
                End If
                outputFile.WriteLine(FarmInfo.ComputerName + "," + RainGauge.Name + "," + DateRange.StartDate.AddDays(i + 1).ToString("dd/MM/yyyy") + "," + (RainGauge.DailyRain).ToString("F2"))
            Next
        End Using
    End Sub
    'Read profile into dataar   ray
    Public Sub ReadProfile()
        'Console.WriteLine("ready")
        If File.Exists(pathTemp + "Profile.txt") Then
            Dim lines As String() = System.IO.File.ReadAllLines(pathTemp + "Profile.txt")

            Dim count As Integer = 0
            For k As Integer = 1 To lines.Length - 1
                Dim Array() As String
                Array = Divide(lines(k), 7, ",")

                For i As Integer = 0 To HydraulicGroup.Length - 1
                    For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                        If ConvertToInt(Array(1)) = HydraulicGroup(i).Pump(j).UID Then
                            HydraulicGroup(i).Pump(j).Name = Array(0).Substring(1, Len(Array(0)) - 2)
                            HydraulicGroup(i).Pump(j).WorkFlow = ConvertToDouble(Array(5)) / 2 + ConvertToDouble(Array(6)) / 2
                        End If
                    Next
                    For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                        If ConvertToInt(Array(1)) = HydraulicGroup(i).FlowMeter(j).UID Then
                            HydraulicGroup(i).FlowMeter(j).Name = Array(0).Substring(1, Len(Array(0)) - 2)
                        End If
                    Next
                    For j As Integer = 0 To HydraulicGroup(i).MoistureProbe.Length - 1
                        If ConvertToInt(Array(1)) = HydraulicGroup(i).MoistureProbe(j).UID Then
                            HydraulicGroup(i).MoistureProbe(j).Name = Array(0).Substring(1, Len(Array(0)) - 2).Replace("-", "")
                        End If
                    Next
                    For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                        If ConvertToInt(Array(1)) = HydraulicGroup(i).Valve(j).UID Then
                            HydraulicGroup(i).Valve(j).Name = Array(0).Substring(1, Len(Array(0)) - 2)
                            HydraulicGroup(i).Valve(j).Area = ConvertToDouble(Array(4))
                            'modified to remove minmum and maximum flow rate. 08/10/2019
                            HydraulicGroup(i).Valve(j).Flow = ConvertToDouble(Array(5))
                            'If HydraulicGroup(i).Type = "Drip" Then
                            '    HydraulicGroup(i).Valve(j).Flow = ConvertToDouble(Array(5))
                            'Else
                            '    HydraulicGroup(i).Valve(j).Flow = ConvertToDouble(Array(5)) / 2 + ConvertToDouble(Array(6)) / 2
                            'End If
                        End If
                    Next
                Next
            Next
        End If
    End Sub
    'Write SQL for data extraction
    Public Sub WriteSQL()
        Dim StartDateString As String = DateRange.StartDate.ToString("yyyy-MM-dd HH:mm:ss")
        Dim EndDateString As String = DateRange.EndDate.ToString("yyyy-MM-dd HH:mm:ss")

        Dim SQLSensor As String = "SELECT CAST(LogDT AS Int), CAST(LogDT As Time),UID,SValue FROM SensorLogs WHERE ("
        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                SQLSensor = SQLSensor + "UID=" + CStr(HydraulicGroup(i).FlowMeter(j).UID) + " or  "
            Next
        Next
        If SQLSensor.Substring(SQLSensor.Length - 1) <> "(" Then
            SQLSensor = SQLSensor.Substring(0, Len(SQLSensor) - 4)
            SQLSensor = SQLSensor + ") And (SValue <> 0)  And (CAST(LogDT AS TimeStamp)>= CAST('" + DateRange.StartDate.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "' AS TimeStamp)) AND (CAST(LogDT AS TimeStamp)< CAST('" + EndDateString + "' AS TimeStamp))"
            Using outputFile As New StreamWriter(pathTemp + "Sensors.txt")
                outputFile.WriteLine(SQLSensor)
            End Using
        End If

        Dim SQLDevice As String = "SELECT CAST(StartDT AS Int), CAST(StartDT As Time),UID,CAST(FinishDT AS Int), CAST(FinishDT As Time) FROM OutputLogs WHERE ("
        Dim SQLDeviceTest As String = "SELECT StartDT,UID,FinishDT FROM OutputLogs WHERE ("

        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                SQLDevice = SQLDevice + "UID=" + CStr(HydraulicGroup(i).Pump(j).UID) + " or "
                SQLDeviceTest = SQLDeviceTest + "UID=" + CStr(HydraulicGroup(i).Pump(j).UID) + " or "
            Next
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                SQLDevice = SQLDevice + "UID=" + CStr(HydraulicGroup(i).Valve(j).UID) + " or "
                SQLDeviceTest = SQLDeviceTest + "UID=" + CStr(HydraulicGroup(i).Valve(j).UID) + " or "
            Next
        Next

        'End If
        SQLDevice = SQLDevice.Substring(0, Len(SQLDevice) - 4)
        SQLDevice = SQLDevice + ") AND   (CAST(FinishDT AS TimeStamp)> CAST('" + StartDateString + "' AS TimeStamp) AND CAST(FinishDT AS TimeStamp) < CAST('" + EndDateString + "' AS TimeStamp))"

        SQLDeviceTest = SQLDeviceTest.Substring(0, Len(SQLDeviceTest) - 4)
        SQLDeviceTest = SQLDeviceTest + ") AND   (CAST(FinishDT AS TimeStamp)> CAST('" + StartDateString + "' AS TimeStamp) AND CAST(FinishDT AS TimeStamp) < CAST('" + EndDateString + "' AS TimeStamp))"


        Using outputFile As New StreamWriter(pathTemp + "Devices.txt")
            outputFile.WriteLine(SQLDevice)
        End Using
        Using outputFile As New StreamWriter(pathTemp + "DevicesTest.txt")
            outputFile.WriteLine(SQLDeviceTest)
        End Using
    End Sub
    'Write batch commands for data extraction
    Public Sub WriteCMD()
        If File.Exists(pathTemp + "Sensors.txt") Then
            Dim CMD As String = "nxSQLExec /Alias:ALData /SQLFile:Sensors.txt /ResultFile:SensorsData.txt"
            Using outputFile As New StreamWriter(pathTemp + "SensorsCMD.bat")
                outputFile.WriteLine(CMD)
            End Using
        End If
        Threading.Thread.Sleep(100)

        If File.Exists(pathTemp + "Devices.txt") Then
            Dim CMD As String = "nxSQLExec /Alias:ALData /SQLFile:Devices.txt /ResultFile:DevicesData.txt"
            Using outputFile As New StreamWriter(pathTemp + "DevicesCMD.bat")
                outputFile.WriteLine(CMD)
            End Using
        End If
        If File.Exists(pathTemp + "DevicesTest.txt") Then
            Dim CMD As String = "nxSQLExec /Alias:ALData /SQLFile:DevicesTest.txt /ResultFile:DevicesDataTest.txt"
            Using outputFile As New StreamWriter(pathTemp + "DevicesTestCMD.bat")
                outputFile.WriteLine(CMD)
            End Using
        End If
        Threading.Thread.Sleep(100)
    End Sub
    'Run batch commands for data extraction
    Public Sub RunBatch()
        If File.Exists(pathTemp + "SensorsCMD.bat") Then
            RunBat("SensorsCMD.bat")
            If File.Exists(pathTemp + "SensorsData.txt") Then
                Dim lines As List(Of String) = System.IO.File.ReadAllLines(pathTemp + "SensorsData.txt").ToList
                If lines.Count > 1 Then
                    lines.RemoveAt(0) ' index starts at 0 
                End If
                System.IO.File.WriteAllLines(pathTemp + "SensorsData.txt", lines)
            End If
            Threading.Thread.Sleep(1000)
        End If
        If File.Exists(pathTemp + "DevicesCMD.bat") Then
            RunBat("DevicesCMD.bat")
            If File.Exists(pathTemp + "DevicesData.txt") Then
                Dim lines As List(Of String) = System.IO.File.ReadAllLines(pathTemp + "DevicesData.txt").ToList
                If lines.Count > 1 Then
                    lines.RemoveAt(0) ' index starts at 0 
                End If
                System.IO.File.WriteAllLines(pathTemp + "DevicesData.txt", lines)
            End If
            Threading.Thread.Sleep(1000)
        End If
        If File.Exists(pathTemp + "DevicesTestCMD.bat") Then
            RunBat("DevicesTestCMD.bat")
            If File.Exists(pathTemp + "DevicesDataTest.txt") Then
                Dim lines As List(Of String) = System.IO.File.ReadAllLines(pathTemp + "DevicesDataTest.txt").ToList
                If lines.Count > 1 Then
                    lines.RemoveAt(0) ' index starts at 0 
                End If
                System.IO.File.WriteAllLines(pathTemp + "DevicesDataTest.txt", lines)
            End If
        End If
    End Sub
    'Read number of dataarray counts
    Public Function ReadCount(filepath As String, UID As Integer) As Integer
        Dim count As Integer = 0
        Using sr As New IO.StreamReader(filepath)
            Do Until sr.EndOfStream
                Dim line As String = sr.ReadLine
                If line IsNot Nothing Then
                    Dim Array() As String
                    Array = Divide(line, 5, ",")
                    If UID = ConvertToInt(Array(2)) Then
                        count = count + 1
                    End If
                End If
            Loop
        End Using
        Return count - 1
    End Function
    'Assign data
    Public Sub AssignData()
        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                Dim Count As Integer = 0
                Using sr As New IO.StreamReader(pathTemp + "DevicesData.txt")
                    Do Until sr.EndOfStream
                        Dim line As String = sr.ReadLine
                        If line IsNot Nothing Then
                            Dim Array() As String
                            Array = Divide(line, 5, ",")
                            If HydraulicGroup(i).Pump(j).UID = ConvertToInt(Array(2)) Then
                                Try
                                    If Array(0).Length > 4 And Array(1).Length > 4 And Array(3).Length > 4 And Array(4).Length > 4 Then
                                        HydraulicGroup(i).Pump(j).Data(Count).StartDT = MergeDateTime(Array(0), Array(1))
                                        HydraulicGroup(i).Pump(j).Data(Count).FinishDT = MergeDateTime(Array(3), Array(4))
                                        Count = Count + 1
                                    End If
                                Catch ex As Exception
                                End Try
                            End If
                        End If
                    Loop
                End Using
            Next
            For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                Dim Count As Integer = 0
                Using sr As New IO.StreamReader(pathTemp + "SensorsData.txt")
                    Do Until sr.EndOfStream
                        Dim line As String = sr.ReadLine
                        If line IsNot Nothing Then
                            Dim Array() As String
                            Array = Divide(line, 4, ",")
                            If HydraulicGroup(i).FlowMeter(j).UID = ConvertToInt(Array(2)) Then
                                Try
                                    If Array(0).Length > 4 And Array(1).Length > 4 Then
                                        HydraulicGroup(i).FlowMeter(j).Data(Count).LogDT = MergeDateTime(Array(0), Array(1))
                                        HydraulicGroup(i).FlowMeter(j).Data(Count).SValue = ConvertToDouble(Array(3))
                                        Count = Count + 1
                                    End If
                                Catch ex As Exception
                                End Try
                            End If
                        End If
                    Loop
                End Using
            Next
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                Dim Count As Integer = 0
                Using sr As New IO.StreamReader(pathTemp + "DevicesData.txt")
                    Do Until sr.EndOfStream
                        Dim line As String = sr.ReadLine
                        If line IsNot Nothing Then
                            Dim Array() As String
                            Array = Divide(line, 5, ",")
                            If HydraulicGroup(i).Valve(j).UID = ConvertToInt(Array(2)) Then
                                Try
                                    If Array(0).Length > 4 And Array(1).Length > 4 And Array(3).Length > 4 And Array(4).Length > 4 Then
                                        HydraulicGroup(i).Valve(j).Data(Count).StartDT = MergeDateTime(Array(0), Array(1))
                                        HydraulicGroup(i).Valve(j).Data(Count).FinishDT = MergeDateTime(Array(3), Array(4))
                                        Count = Count + 1
                                    End If
                                Catch ex As Exception
                                End Try
                            End If
                        End If
                    Loop
                End Using
            Next
        Next
    End Sub
    'Read data
    Public Sub ReadData()
        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                ReDim HydraulicGroup(i).Pump(j).Data(ReadCount(pathTemp + "DevicesData.txt", HydraulicGroup(i).Pump(j).UID))
            Next
            For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                ReDim HydraulicGroup(i).FlowMeter(j).Data(ReadCount(pathTemp + "SensorsData.txt", HydraulicGroup(i).FlowMeter(j).UID))
            Next
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                ReDim HydraulicGroup(i).Valve(j).Data(ReadCount(pathTemp + "DevicesData.txt", HydraulicGroup(i).Valve(j).UID))
                ReDim HydraulicGroup(i).Valve(j).Irrigation(Span)
            Next
        Next
        AssignData()
    End Sub
End Module

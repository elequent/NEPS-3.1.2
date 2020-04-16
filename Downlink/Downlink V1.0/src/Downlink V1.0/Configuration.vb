Imports System.IO
Module Configuration
    'Read configuration file
    Public Sub ReadConfig()
        If File.Exists(pathConfig + "Config.txt") Then
            Dim lines() As String = IO.File.ReadAllLines(pathConfig + "Config.txt")
            'go through each line for confirguration items
            For i As Integer = 0 To lines.Length - 1
                If lines(i) = "[Preferred Time]" Then
                    For j As Integer = 0 To 1
                        Dim TmpArray() As String
                        TmpArray = lines(i + j + 1).Split("=")
                        Dim Temp() As String
                        Temp = TmpArray(1).Split("-")
                        PreferredTime(j).PrefStartDT = ConvertToTime(Temp(0))
                        PreferredTime(j).PrefEndDT = ConvertToTime(Temp(1))
                    Next
                End If
                Dim Array() As String
                'split to name and value
                Array = lines(i).Split("=")
                Select Case Array(0)
                    'User ID, e.g., 21, 81...
                    Case "Site Key"
                        UserID = Array(1)
                        Exit Select
                    'Computer Name
                    Case "Computer Name"
                        ComputerName = Array(1)
                        TrimedFarmName = RemoveSpecialCharacter(ComputerName)
                        Exit Select
                    Case "DateTime Format"
                        DateFormat = Array(1)
                        Exit Select
                        'No. of hydro group,
                    Case "No. Hydro Group"
                        NoHydroGroup = ConvertToInt(Array(1))
                        ReDim HydraulicGroup(NoHydroGroup - 1)
                        Exit Select
                End Select
            Next
            'initialise hydraulic group data 
            Dim Index As Integer = 0
            For i As Integer = 0 To lines.Length - 1
                If Divide(lines(i), 2, "=")(0) = "Hydr Grp ID" Then
                    HydraulicGroup(Index).Group = Asc(Divide(lines(i), 2, "=")(1)) - Asc("A") + 1
                    If HydraulicGroup(Index).Group = 2 Then
                        HydraulicGroup(Index).GroupName = "Drip"
                    End If
                    If HydraulicGroup(Index).Group = 3 Then
                        HydraulicGroup(Index).GroupName = "Upriver"
                    End If
                    If HydraulicGroup(Index).Group = 4 Then
                        HydraulicGroup(Index).GroupName = "Downriver"
                    End If
                    HydraulicGroup(Index).Type = Divide(lines(i + 2), 2, "=")(1)
                    HydraulicGroup(Index).FarmName = Divide(lines(i + 1), 2, "=")(1)
                    Dim NoPump As Integer = ConvertToInt(Divide(lines(i + 3), 2, "=")(1))
                    ReDim HydraulicGroup(Index).Pump(NoPump - 1)
                    For j As Integer = 0 To NoPump - 1
                        HydraulicGroup(Index).Pump(j).UID = ConvertToInt(Divide(lines(i + j + 4), 2, "=")(1))
                    Next
                    Dim NoFlowMeter As Integer = ConvertToInt(Divide(lines(i + 4 + NoPump), 2, "=")(1))
                    Dim NoMoistureProbe As Integer = ConvertToInt(Divide(lines(i + 5 + NoPump + NoFlowMeter), 2, "=")(1))
                    ReDim HydraulicGroup(Index).MoistureProbe(NoMoistureProbe - 1)
                    For j As Integer = 0 To NoMoistureProbe - 1
                        HydraulicGroup(Index).MoistureProbe(j).UID = ConvertToInt(Divide(lines(i + j + 6 + NoPump + NoFlowMeter), 2, "=")(1))
                    Next
                    Dim NoValve As Integer = ConvertToInt(Divide(lines(i + 6 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                    ReDim HydraulicGroup(Index).Valve(NoValve - 1)
                    For j As Integer = 0 To NoValve - 1
                        HydraulicGroup(Index).Valve(j).UID = ConvertToInt(Divide(lines(i + 3 * j + 7 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                        HydraulicGroup(Index).Valve(j).MaxRun = ConvertToInt(Divide(lines(i + 3 * j + 8 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1))
                        HydraulicGroup(Index).Valve(j).Ratoon = Divide(lines(i + 3 * j + 9 + NoPump + NoFlowMeter + NoMoistureProbe), 2, "=")(1)
                    Next
                    Index = Index + 1
                End If
            Next
        End If
    End Sub
    Public Sub ReadDrip()
        Dim filename As String = pathConfig + "Drip.txt"
        If File.Exists(filename) Then
            Dim lines() As String = IO.File.ReadAllLines(filename)
            Dim Array() As String
            For k As Integer = 0 To lines.Length - 1
                Array = Divide(lines(k), 2, "=")
                For i As Integer = 0 To HydraulicGroup.Length - 1
                    For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                        If Array.Length > 1 And Array(0) = "Set ID" And Array(1) = HydraulicGroup(i).Valve(j).UID.ToString Then
                            HydraulicGroup(i).Valve(j).Threshold = ConvertToInt(Divide(lines(k + 1), 2, "=")(1))
                        End If
                    Next
                Next
            Next
        End If
    End Sub
    Public Sub ReadFurrow()
        Dim filename As String = pathConfig + "Furrow.txt"
        If File.Exists(filename) Then
            Dim lines() As String = IO.File.ReadAllLines(filename)
            Dim Array() As String
            For k As Integer = 0 To lines.Length - 1
                Array = Divide(lines(k), 2, "=")
                For i As Integer = 0 To HydraulicGroup.Length - 1
                    For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                        If Array.Length > 1 And Array(0) = "Set ID" And Array(1) = HydraulicGroup(i).Valve(j).UID.ToString Then
                            HydraulicGroup(i).Valve(j).Threshold = ConvertToInt(Divide(lines(k + 2), 2, "=")(1))
                            HydraulicGroup(i).Valve(j).DefaultWater = ConvertToInt(Divide(lines(k + 3), 2, "=")(1))
                            HydraulicGroup(i).Valve(j).HourlyRate = ConvertToDouble(Divide(lines(k + 4), 2, "=")(1))
                            HydraulicGroup(i).Valve(j).MaxRunOrg = HydraulicGroup(i).Valve(j).DefaultWater / HydraulicGroup(i).Valve(j).HourlyRate
                            If HydraulicGroup(i).Valve(j).MaxRunOrg > 24 Then
                                HydraulicGroup(i).Valve(j).MaxRun = HydraulicGroup(i).Valve(j).MaxRunOrg / 2
                            End If
                        End If
                    Next
                Next
            Next
        End If
    End Sub
End Module

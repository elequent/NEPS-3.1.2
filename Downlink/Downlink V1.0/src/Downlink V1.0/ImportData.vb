Imports System.IO
Imports System.Net
Module ImportData
    'Read profile, i.e., UID, landarea and flowrate
    Public Sub ExtractProfile()
        Dim SQL As String = "SELECT DeviceN,UID,DeviceType,HydGrp,LandArea,MinFlowRate FROM Devices WHERE ("

        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                SQL = SQL + "UID=" + CStr(HydraulicGroup(i).Pump(j).UID) + " or  "
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
            While File.Exists(pathTemp + "Profile.txt") = False
                RunBat("ProfileCMD.bat")
                Threading.Thread.Sleep(200)
            End While
        End If

        'Remove first line
        Dim lines As List(Of String) = System.IO.File.ReadAllLines(pathTemp + "Profile.txt").ToList
        lines.RemoveAt(0) ' index starts at 0 
        System.IO.File.WriteAllLines(pathTemp + "Profile.txt", lines)
    End Sub
    'Read profile into dataar   ray
    Public Sub ReadProfile()
        If File.Exists(pathTemp + "Profile.txt") Then
            Dim lines As String() = System.IO.File.ReadAllLines(pathTemp + "Profile.txt")

            Dim count As Integer = 0
            Using sr As New IO.StreamReader(pathTemp + "Profile.txt")
                Do Until sr.EndOfStream
                    Dim line As String = sr.ReadLine
                    If line IsNot Nothing Then
                        Dim Array() As String
                        Array = Divide(line, 6, ",")

                        For i As Integer = 0 To HydraulicGroup.Length - 1
                            For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                                If ConvertToInt(Array(1)) = HydraulicGroup(i).Pump(j).UID Then
                                    HydraulicGroup(i).Pump(j).Name = Array(0).Substring(1, Len(Array(0)) - 2).Replace("-", "")
                                    HydraulicGroup(i).Pump(j).Flow = ConvertToDouble(Array(5)) * 2
                                    HydraulicGroup(i).Capacity += HydraulicGroup(i).Pump(j).Flow 'calculate the maximum pump capacity of this hydraulic group
                                End If
                            Next
                            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                                If ConvertToInt(Array(1)) = HydraulicGroup(i).Valve(j).UID Then
                                    HydraulicGroup(i).Valve(j).Name = Array(0).Substring(1, Len(Array(0)) - 2).Replace("-", "")
                                    HydraulicGroup(i).Valve(j).Area = ConvertToDouble(Array(4))
                                    HydraulicGroup(i).Valve(j).Flow = ConvertToDouble(Array(5))
                                    If HydraulicGroup(i).Type = "Drip" Then
                                        HydraulicGroup(i).Valve(j).HourlyWater = HydraulicGroup(i).Valve(j).Flow * 3600 / 10000 / HydraulicGroup(i).Valve(j).Area
                                    Else
                                        HydraulicGroup(i).Valve(j).HourlyWater = 30 / HydraulicGroup(i).Valve(j).MaxRun
                                    End If
                                    'Console.WriteLine(HydraulicGroup(i).Valve(j).HourlyWater.ToString)
                                    'ReDim HydraulicGroup(i).Valve(j).SchData(6)
                                End If
                            Next
                        Next
                    End If
                Loop
            End Using
        End If
    End Sub
    'Assign IrrigWeb SchData
    Public Sub AssignIrrigWebSchData()
        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                ReDim HydraulicGroup(i).Valve(j).SchData(7)
            Next
        Next

        Dim FileName As String
        FileName = pathTemp + UserID + ".dat"
        If File.Exists(FileName) Then
            Dim count = 0
            Using sr As New IO.StreamReader(FileName)
                Do Until sr.EndOfStream
                    Dim line As String = sr.ReadLine
                    If line IsNot Nothing Then
                        Dim Array() As String
                        Array = Divide(line, 6, ",")
                        For i As Integer = 0 To HydraulicGroup.Length - 1
                            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                                If Array(0) = HydraulicGroup(i).FarmName And Array(1) = HydraulicGroup(i).Valve(j).Name And CheckPriority(HydraulicGroup(i).Valve(j).Ratoon) <> 5 Then
                                    HydraulicGroup(i).Valve(j).SchData(count).SchDate = ConvertToDate(Array(2))
                                    HydraulicGroup(i).Valve(j).SchData(count).IrrigWater = ConvertToDouble(Array(3))
                                    HydraulicGroup(i).Valve(j).SchData(count).SWD = ConvertToDouble(Array(4))
                                    HydraulicGroup(i).Valve(j).SchData(count).CWU = ConvertToDouble(Array(5))
                                    'If count = 0 Then
                                    '    Console.WriteLine(HydraulicGroup(i).FarmName.ToString + "," + HydraulicGroup(i).Group.ToString + "," + HydraulicGroup(i).Valve(j).Name + "," +
                                    '                      HydraulicGroup(i).Valve(j).SchData(count).SchDate.ToString + "," + HydraulicGroup(i).Valve(j).SchData(count).IrrigWater.ToString + "," +
                                    '                      HydraulicGroup(i).Valve(j).SchData(count).SWD.ToString)
                                    'End If
                                    count = count + 1
                                End If
                                If count > 6 Then
                                    count = 0
                                End If
                            Next
                        Next
                    End If
                Loop
            End Using
        End If
    End Sub

End Module

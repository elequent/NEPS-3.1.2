Imports System.IO
Module DripIrrigation
    Public Sub RunDrip(day As Integer, group As Integer)


        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Farm,Hydraulic Group,Valve,Date,Water required,Start Soil water deficit")
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            HydraulicGroup(group).Valve(i).WaterRequired = HydraulicGroup(group).Valve(i).Threshold - HydraulicGroup(group).Valve(i).SchData(day).SWD
            If HydraulicGroup(group).Valve(i).WaterRequired / HydraulicGroup(group).Valve(i).HourlyWater >
                OperationTime.NoInterval * IrrigInterval / 60 Then
                HydraulicGroup(group).Valve(i).WaterRequired = (OperationTime.NoInterval - 2) * IrrigInterval *
                    HydraulicGroup(group).Valve(i).HourlyWater / 60
            End If
            If HydraulicGroup(group).Valve(i).WaterRequired < 1 Then
                HydraulicGroup(group).Valve(i).WaterRequired = 0
            End If
            Writemsg(HydraulicGroup(group).GroupName.ToString + "," + HydraulicGroup(group).Valve(i).Name + "," +
                                  HydraulicGroup(group).Valve(i).SchData(day).SchDate.ToShortDateString + "," + HydraulicGroup(group).Valve(i).WaterRequired.ToString("F1") + "," +
                                  HydraulicGroup(group).Valve(i).SchData(day).SWD.ToString("F1"))
        Next
        Dim Rank() As Integer = CalRank(day, group)

        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Irrigation priority:")
        Dim Msg As String
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            If DebugFlag Then
                Msg = HydraulicGroup(group).Valve(Rank(i)).Name + ":" + HydraulicGroup(group).Valve(Rank(i)).SchData(day).SWD.ToString("F1") + ";"
                Writemsg(Msg)
            End If
        Next

        Dim IrrigSta(HydraulicGroup(group).Valve.Length - 1) As Integer
        'store the current flow with multiple v
        Dim CurrentFlow As Double
        Dim StartIndex As Integer = 0

        For i As Integer = 0 To Rank.Length - 1
            'initialise irrigation status for each irrigation set the time duration of the operation time
            For j As Integer = StartIndex To OperationTime.NoInterval - 1
                For z As Integer = 0 To Rank.Length - 1
                    IrrigSta(z) = 0
                Next
                'run irrigation until reach to the water requested from IrrigWeb schedule
                If HydraulicGroup(group).Valve(Rank(i)).WaterRequired > 0 Then
                    'Set irrigation status to active
                    IrrigSta(Rank(i)) = 1
                    'calculate the water flow for runing this set
                    CurrentFlow = HydraulicGroup(group).Valve(Rank(i)).Flow
                    'deduct the irrigation water requested by the water amount applied in the interval
                    AssignWater(day, group, Rank(i))
                    'find out the irrigation set that can run with the above irrigation set based on the overall water flow requirement
                    For k As Integer = 0 To Rank.Length - 1
                        If k <> i And CurrentFlow + HydraulicGroup(group).Valve(Rank(k)).Flow <= HydraulicGroup(group).Capacity And
                            HydraulicGroup(group).Valve(Rank(k)).WaterRequired > 0 Then
                            IrrigSta(Rank(k)) = 1
                            CurrentFlow += HydraulicGroup(group).Valve(Rank(k)).Flow
                            AssignWater(day, group, Rank(k))
                        End If
                    Next
                Else
                    'start with the new time interval
                    StartIndex = j
                    Exit For
                End If
                Msg = OperationTime.StartDT.AddMinutes(IrrigInterval * j) + ";"
                For z As Integer = 0 To Rank.Length - 1
                    HydraulicGroup(group).Valve(z).SchData(day).IrrigStatus(j) = IrrigSta(z)
                    Msg += IrrigSta(z).ToString + ";"
                Next
                'Console.WriteLine(Msg)
            Next
        Next
    End Sub
    'summrise irrigation schedule for each sets 
    Public Sub CalShift(day As Integer, group As Integer)
        Writeshortmsg("Group|Start|End|Set|Irrig (mm)")

        Shift.Clear()
        Dim Status(HydraulicGroup(group).Valve.Length - 1) As Integer
        Dim TempShft As Shft
        Dim StoreSch As New List(Of String)


        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            Status(i) = HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(0)
            'TempShft.Sta(i) = 0
        Next
        If CheckAllZeros(Status) Then
            TempShft.StartDT = OperationTime.StartDT
        End If

        For j As Integer = 0 To OperationTime.NoInterval - 1
            ReDim TempShft.Sta(HydraulicGroup(group).Valve.Length - 1)
            Dim Index As Integer = 0
            For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
                If Status(i) = HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(j) Then
                    Index += 1
                End If
            Next

            If Index <> HydraulicGroup(group).Valve.Length Or j = OperationTime.NoInterval - 1 Then
                If CheckAllZeros(Status) Then
                    TempShft.FinishDT = OperationTime.StartDT.AddMinutes(IrrigInterval * j)
                    Dim Msg, Msg1 As String
                    Msg = HydraulicGroup(group).GroupName + "|" + TempShft.StartDT.ToString("dd/MM HHmm") + "|" + TempShft.FinishDT.ToString("dd/MM/yy HHmm") + "|"
                    Dim TempSpan As TimeSpan = TempShft.FinishDT - TempShft.StartDT
                    TempShft.Duration = TempSpan.TotalSeconds
                    For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
                        TempShft.Sta(i) = Status(i)
                        If Status(i) = 1 Then
                            Msg += HydraulicGroup(group).Valve(i).Name + "|" + (HydraulicGroup(group).Valve(i).HourlyWater * TempSpan.TotalHours).ToString("F0")
                            Msg1 = "Aaron's Farm," + Now.AddDays(day).ToShortDateString() +
                                             "," + HydraulicGroup(group).Valve(i).Name + "," + HydraulicGroup(group).Valve(i).SchData(day).WaterApplied.ToString("F0")
                        End If
                    Next
                    Shift.Add(TempShft)

                    Writeshortmsg(Msg)
                    System.IO.File.AppendAllText(pathSch + "81~AaronFarm.txt", Environment.NewLine + Msg1)

                    For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
                        Status(i) = HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(j)
                        TempShft.StartDT = OperationTime.StartDT.AddMinutes(IrrigInterval * j)
                    Next
                    'Upload("NESP_Test/" + CStr(UserID) + "~" + Now.ToString("ddMMyyyy") + ".txt", pathSch + CStr(UserID) + "~" + Now.ToString("ddMMyyyy") + ".txt")
                End If
            End If
        Next
    End Sub
End Module

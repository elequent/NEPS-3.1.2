Imports System.IO
Module DripIrrigation
    Public Sub RunDrip(day As Integer, group As Integer)


        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Farm,Hydraulic Group,Valve,Date,Water required,Start Soil water deficit")
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            Dim CurrentDay As DateTime = New DateTime(Now.Year, Now.Month, Now.Day, 0, 0, 0)
            CurrentDay = CurrentDay.AddDays(day)
            If CurrentDay.DayOfWeek = 5 Then
                HydraulicGroup(group).Valve(i).WaterRequired = HydraulicGroup(group).Valve(i).Threshold - HydraulicGroup(group).Valve(i).SchData(day).SWD + HydraulicGroup(group).Valve(i).SchData(day + 1).CWU + HydraulicGroup(group).Valve(i).SchData(day + 2).CWU
            Else
                HydraulicGroup(group).Valve(i).WaterRequired = HydraulicGroup(group).Valve(i).Threshold - HydraulicGroup(group).Valve(i).SchData(day).SWD
            End If
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
                Console.WriteLine(Msg)
            Next
        Next
    End Sub
    Public Sub CalShift(day As Integer, group As Integer)
        Writeshortmsg("Group|Start|End|Set|Irrig (mm)")

        'clear shifts
        Shift.Clear()
        Dim Status(HydraulicGroup(group).Valve.Length - 1) As Integer

        'copy the running status for each valve with 5 mintue interval for the whole schedule
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            Dim TempShft As Shft
            Dim startflag, finishflag As Integer
            startflag = 0
            finishflag = 0
            For j As Integer = 0 To OperationTime.NoInterval - 1
                If HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(j) <> 0 And startflag = 0 Then
                    TempShft.StartDT = OperationTime.StartDT.AddMinutes(IrrigInterval * j)
                    startflag = 1
                End If
                If HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(j) = 0 And finishflag = 0 And startflag = 1 Then
                    TempShft.FinishDT = OperationTime.StartDT.AddMinutes(IrrigInterval * j)
                    finishflag = 1
                End If
                If j = OperationTime.NoInterval - 1 And finishflag = 0 And startflag = 1 Then
                    TempShft.FinishDT = OperationTime.StartDT.AddMinutes(IrrigInterval * j)
                    finishflag = 1
                End If
            Next
            If startflag = 1 And finishflag = 1 Then
                ReDim TempShft.Sta(HydraulicGroup(group).Valve.Length - 1)
                TempShft.Sta(i) = 1
                Dim Msg As String
                Msg = HydraulicGroup(group).GroupName + "|" + TempShft.StartDT.ToString("dd/MM HHmm") + "|" + TempShft.FinishDT.ToString("dd/MM/yy HHmm")
                Dim TempSpan As TimeSpan = TempShft.FinishDT - TempShft.StartDT
                TempShft.Duration = TempSpan.TotalSeconds
                Msg += " |" + HydraulicGroup(group).Valve(i).Name + "|" + (HydraulicGroup(group).Valve(i).HourlyWater * TempSpan.TotalHours).ToString("F0")
                Shift.Add(TempShft)
                Writeshortmsg(Msg)
            End If
        Next
    End Sub
End Module
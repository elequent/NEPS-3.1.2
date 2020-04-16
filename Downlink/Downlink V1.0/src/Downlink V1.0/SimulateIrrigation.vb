Imports System.IO
Module SimulateIrrigation
    'calculate operation time based on the user preferred time
    Public Sub CalculateOperationTime(day As Integer)
        'calcuate the preferred irrigation time based on the scheduling date

        Dim CurrentDay As DateTime = New DateTime(Now.Year, Now.Month, Now.Day, 0, 0, 0)
        CurrentDay = CurrentDay.AddDays(day)
        Select Case CurrentDay.DayOfWeek
            Case 1, 2, 3, 4
                OperationTime.StartDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(0).PrefStartDT.Hour, PreferredTime(0).PrefStartDT.Minute, PreferredTime(0).PrefStartDT.Second)
                OperationTime.EndDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(0).PrefEndDT.Hour, PreferredTime(0).PrefEndDT.Minute, PreferredTime(0).PrefEndDT.Second)
                OperationTime.EndDT = OperationTime.EndDT.AddDays(1)
                Exit Select
            Case 5
                OperationTime.StartDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(0).PrefStartDT.Hour, PreferredTime(0).PrefStartDT.Minute, PreferredTime(0).PrefStartDT.Second)
                OperationTime.EndDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(1).PrefEndDT.Hour, PreferredTime(1).PrefEndDT.Minute, PreferredTime(1).PrefEndDT.Second)
                OperationTime.EndDT = OperationTime.EndDT.AddDays(3)
                Exit Select
            Case 6
                OperationTime.StartDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(1).PrefStartDT.Hour, PreferredTime(1).PrefStartDT.Minute, PreferredTime(1).PrefStartDT.Second)
                OperationTime.EndDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(1).PrefEndDT.Hour, PreferredTime(1).PrefEndDT.Minute, PreferredTime(1).PrefEndDT.Second)
                OperationTime.EndDT = OperationTime.EndDT.AddDays(2)
                Exit Select
            Case 0
                OperationTime.StartDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(1).PrefStartDT.Hour, PreferredTime(1).PrefStartDT.Minute, PreferredTime(1).PrefStartDT.Second)
                OperationTime.EndDT = New DateTime(CurrentDay.Year, CurrentDay.Month, CurrentDay.Day,
                                                     PreferredTime(1).PrefEndDT.Hour, PreferredTime(1).PrefEndDT.Minute, PreferredTime(1).PrefEndDT.Second)
                OperationTime.EndDT = OperationTime.EndDT.AddDays(1)
                Exit Select
        End Select

        If OperationTime.StartDT < Now Then
            OperationTime.StartDT = New DateTime(Now.Year, Now.Month, Now.Day, Now.Hour + 1, 0, 0)
        End If
        OperationTime.StartDT = OperationTime.StartDT.AddMinutes(5)
        OperationTime.EndDT = OperationTime.EndDT.AddMinutes(-5)
        'divide the duration into seleted time interval
        Dim IrrigDuration As TimeSpan = OperationTime.EndDT.Subtract(OperationTime.StartDT)
        OperationTime.NoInterval = IrrigDuration.TotalMinutes / IrrigInterval

        Dim Msg As String
        Msg = OperationTime.StartDT.ToString + "," + OperationTime.EndDT.ToString + "," + OperationTime.NoInterval.ToString
        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Operation Time:")
        Writemsg(Msg)
    End Sub
    Public Function CalRank(day As Integer, group As Integer) As Integer()
        Dim Rank(HydraulicGroup(group).Valve.Length - 1) As Integer
        Dim SWD(HydraulicGroup(group).Valve.Length - 1) As Integer
        'find the rank of irrigation water required
        Dim Msg As String


        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            SWD(i) = CheckPriority(HydraulicGroup(group).Valve(i).Ratoon)
            Rank(i) = i
            ReDim HydraulicGroup(group).Valve(i).SchData(day).IrrigStatus(OperationTime.NoInterval - 1)
        Next
        'ranking irrigation priority based on the water deficit of the irrigation set
        SortPriority(SWD, Rank)

        Return Rank
    End Function
    Public Sub AssignWater(day As Integer, group As Integer, valve As Integer)
        HydraulicGroup(group).Valve(valve).WaterRequired -= HydraulicGroup(group).Valve(valve).HourlyWater / NoIntervalHour
        HydraulicGroup(group).Valve(valve).SchData(day).WaterApplied += HydraulicGroup(group).Valve(valve).HourlyWater / NoIntervalHour
    End Sub
End Module

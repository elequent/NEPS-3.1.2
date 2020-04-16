Imports System.IO
Module FurrowIrrigation
    Public Sub RunFurrow(day As Integer, group As Integer)
        'caculate irrigation ranking based on the SWD of today

        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Farm,Hydraulic Group,Valve,Date,Water required,Start Soil water deficit")
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            If HydraulicGroup(group).Valve(i).SchData(day).SWD < HydraulicGroup(group).Valve(i).Threshold Then
                HydraulicGroup(group).Valve(i).SchData(day).IrrigWater = HydraulicGroup(group).Valve(i).DefaultWater
            End If
            If HydraulicGroup(group).Valve(i).MaxRun > OperationTime.NoInterval * IrrigInterval / 60 Then
                Dim eum As Double = OperationTime.NoInterval * IrrigInterval / 60
                Dim num As Double = HydraulicGroup(group).Valve(i).MaxRun
                HydraulicGroup(group).Valve(i).SchData(day).IrrigWater *= eum / num
                HydraulicGroup(group).Valve(i).MaxRun = OperationTime.NoInterval * IrrigInterval / 60
            End If
            Writemsg(HydraulicGroup(group).GroupName.ToString + "," + HydraulicGroup(group).Valve(i).Name + "," +
                                  HydraulicGroup(group).Valve(i).SchData(day).SchDate.ToShortDateString + "," + HydraulicGroup(group).Valve(i).WaterRequired.ToString("F1") + "," +
                                  HydraulicGroup(group).Valve(i).SchData(day).SWD.ToString("F1"))
        Next


        Dim Rank() As Integer = CalRank(day, group)


        Writemsg("-----------------------------------------------------------------------------------------------------------")
        Writemsg("Irrigation priority:")
        Dim Msg, Msg1 As String
        For i As Integer = 0 To HydraulicGroup(group).Valve.Length - 1
            If DebugFlag Then
                Msg = HydraulicGroup(group).Valve(Rank(i)).Name + "|" + HydraulicGroup(group).Valve(Rank(i)).SchData(day).SWD.ToString("F1") + ";"
                Console.WriteLine(Msg)
            End If
        Next

        Dim IrrigSta(HydraulicGroup(group).Valve.Length - 1) As Integer
        'store the current flow with multiple v

        'Writeshortmsg("Group          Start Time                               Irrigation Set")
        'Shift.Clear()
        Dim TempShft As Shft
        ReDim TempShft.Sta(HydraulicGroup(group).Valve.Length - 1)

        Dim TempTS As Integer = OperationTime.EndDT.Subtract(OperationTime.StartDT).TotalHours
        Dim StartTS As Integer = 0
        For i As Integer = 0 To Rank.Length - 1
            If HydraulicGroup(group).Valve(Rank(i)).MaxRun <= TempTS And HydraulicGroup(group).Valve(Rank(i)).SchData(day).IrrigWater <> 0 Then
                TempShft.Sta(Rank(i)) = 1
                TempShft.StartDT = OperationTime.StartDT.AddHours(StartTS)
                TempShft.FinishDT = TempShft.StartDT.AddHours(HydraulicGroup(group).Valve(Rank(i)).MaxRun)
                StartTS += HydraulicGroup(group).Valve(Rank(i)).MaxRun
                TempTS -= HydraulicGroup(group).Valve(Rank(i)).MaxRun
                Shift.Add(TempShft)
                HydraulicGroup(group).Valve(Rank(i)).SchData(day).WaterApplied = HydraulicGroup(group).Valve(Rank(i)).SchData(day).IrrigWater

                Msg = HydraulicGroup(group).GroupName + "|" + TempShft.StartDT.ToString("dd/MM HHmm") + "|" + TempShft.FinishDT.ToString("dd/MM/yy HHmm") + "|" +
                    HydraulicGroup(group).Valve(Rank(i)).Name + "|" + Convert.ToInt16(HydraulicGroup(group).Valve(Rank(i)).SchData(day).IrrigWater).ToString
                'Msg = HydraulicGroup(group).GroupName + "|" + TempShft.StartDT.ToString("dd/MM HHmm") + "|" +
                '    HydraulicGroup(group).Valve(Rank(i)).Name + "|" + Convert.ToInt16(HydraulicGroup(group).Valve(Rank(i)).SchData(day).IrrigWater).ToString
                Msg1 = "Aaron's Farm," + Now.AddDays(day).ToShortDateString() +
                                             "," + HydraulicGroup(group).Valve(i).Name + "," + HydraulicGroup(group).Valve(i).SchData(day).WaterApplied.ToString("F0")
                Writeshortmsg(Msg)
                System.IO.File.AppendAllText(pathSch + "81~AaronFarm.txt", Environment.NewLine + Msg1)
                Using writer As New StreamWriter(pathSch + CStr(UserID) + ".txt", True)
                    writer.WriteLine(Msg)
                End Using
            End If
        Next
    End Sub
End Module

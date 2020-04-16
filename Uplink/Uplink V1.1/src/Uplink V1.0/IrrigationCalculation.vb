Module IrrigationCalculation
    'calculation with flow meter
    Private Sub AssignWorkFlow(ByVal i As Integer)
        For z As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
            For x As Integer = 0 To HydraulicGroup(i).Pump(z).Data.Length - 1
                Dim TimeDiff As TimeSpan = HydraulicGroup(i).Pump(z).Data(x).FinishDT.Subtract(HydraulicGroup(i).Pump(z).Data(x).StartDT)
                Dim Sc As Integer = TimeDiff.TotalMinutes / 5
                ReDim HydraulicGroup(i).Pump(z).Data(x).Flow(Sc)
                For y As Integer = 0 To Sc - 1
                    HydraulicGroup(i).Pump(z).Data(x).Flow(y).LogDT = HydraulicGroup(i).Pump(z).Data(x).StartDT.AddMinutes(y * 5)
                    Dim totalarea As Double = 0
                    For k As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                        If HydraulicGroup(i).Valve(k).Data IsNot Nothing Then
                            For l As Integer = 0 To HydraulicGroup(i).Valve(k).Data.Length - 1
                                If HydraulicGroup(i).Pump(z).Data(x).Flow(y).LogDT >= HydraulicGroup(i).Valve(k).Data(l).StartDT And HydraulicGroup(i).Pump(z).Data(x).Flow(y).LogDT <= HydraulicGroup(i).Valve(k).Data(l).FinishDT Then
                                    totalarea = totalarea + HydraulicGroup(i).Valve(z).Area
                                End If
                            Next
                        End If
                    Next
                    If totalarea <> 0 Then
                        HydraulicGroup(i).Pump(z).Data(x).Flow(y).SValue = HydraulicGroup(i).Pump(z).WorkFlow / totalarea
                    End If
                Next
            Next
        Next
    End Sub
    Private Sub AssignWater(ByVal i As Integer, ByVal z As Integer)
        For n As Integer = 0 To HydraulicGroup(i).Valve(z).Irrigation.Length - 1
            If HydraulicGroup(i).Valve(z).Data IsNot Nothing Then
                Dim TempDate As DateTime = DateRange.StartDate.AddDays(n)
                For l As Integer = 0 To HydraulicGroup(i).Valve(z).Data.Length - 1
                    Dim count As Double = 0
                    Dim index As Double = 0
                    If HydraulicGroup(i).Valve(z).Data(l).StartDT > TempDate And HydraulicGroup(i).Valve(z).Data(l).StartDT < TempDate.AddDays(1) Then
                        For j As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                            If HydraulicGroup(i).Pump(j).Data IsNot Nothing Then
                                For k As Integer = 0 To HydraulicGroup(i).Pump(j).Data.Length - 1
                                    For m As Integer = 0 To HydraulicGroup(i).Pump(j).Data(k).Flow.Length - 1
                                        If HydraulicGroup(i).Pump(j).Data(k).Flow(m).LogDT >= HydraulicGroup(i).Valve(z).Data(l).StartDT And HydraulicGroup(i).Pump(j).Data(k).Flow(m).LogDT <= HydraulicGroup(i).Valve(z).Data(l).FinishDT Then
                                            count = count + HydraulicGroup(i).Pump(j).Data(k).Flow(m).SValue * 300
                                            index = index + 300
                                        End If
                                    Next
                                Next
                            End If
                        Next
                    End If
                    HydraulicGroup(i).Valve(z).Irrigation(n).Water = HydraulicGroup(i).Valve(z).Irrigation(n).Water + count / 10000
                    HydraulicGroup(i).Valve(z).Irrigation(n).Duration = HydraulicGroup(i).Valve(z).Irrigation(n).Duration + index / 3600
                Next
            End If
        Next
    End Sub
    Private Sub Withoutflowmeter(ByVal i As Integer)
        For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
            If HydraulicGroup(i).Valve(j).DefaultFlow = "Valve" Then
                For n As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                    Dim TempDate As DateTime = DateRange.StartDate.AddDays(n)
                    For k As Integer = 0 To HydraulicGroup(i).Valve(j).Data.Length - 1
                        If HydraulicGroup(i).Valve(j).Data(k).FinishDT > TempDate And HydraulicGroup(i).Valve(j).Data(k).FinishDT < TempDate.AddDays(1) Then
                            'If HydraulicGroup(i).Valve(j).DefaultFlow = "Valve" Then
                            Dim TimeDiff As TimeSpan = HydraulicGroup(i).Valve(j).Data(k).FinishDT.Subtract(HydraulicGroup(i).Valve(j).Data(k).StartDT)
                            Dim Sc As Integer = TimeDiff.TotalSeconds
                            HydraulicGroup(i).Valve(j).Irrigation(n).Water = HydraulicGroup(i).Valve(j).Irrigation(n).Water + (Sc * HydraulicGroup(i).Valve(j).Flow / 10000) / HydraulicGroup(i).Valve(j).Area
                            'Else
                            'For z As Integer = 0 To HydraulicGroup(i).Pump.Length - 1
                            '    For x As Integer = 0 To HydraulicGroup(i).Pump(z).Data.Length - 1
                            '        Dim A As DateTime = HydraulicGroup(i).Pump(z).Data(x).StartDT
                            '        Dim B As DateTime = HydraulicGroup(i).Pump(z).Data(x).FinishDT
                            '        Dim C As DateTime = HydraulicGroup(i).Valve(j).Data(k).StartDT
                            '        Dim D As DateTime = HydraulicGroup(i).Valve(j).Data(k).FinishDT

                            '        If A < C Then
                            '            If B < D And B > C Then
                            '                Dim TimeDiff As TimeSpan = B.Subtract(C)
                            '                Dim Sc As Integer = TimeDiff.TotalSeconds
                            '                HydraulicGroup(i).Valve(j).Irrigation(n).Water += HydraulicGroup(i).Valve(j).Irrigation(n).Water + (Sc * HydraulicGroup(i).Pump(z).WorkFlow / 10000) / HydraulicGroup(i).Valve(j).Area
                            '            Else
                            '                Dim TimeDiff As TimeSpan = D.Subtract(C)
                            '                Dim Sc As Integer = TimeDiff.TotalSeconds
                            '                HydraulicGroup(i).Valve(j).Irrigation(n).Water += HydraulicGroup(i).Valve(j).Irrigation(n).Water + (Sc * HydraulicGroup(i).Pump(z).WorkFlow / 10000) / HydraulicGroup(i).Valve(j).Area
                            '            End If
                            '        Else
                            '            If B < D And B > C Then
                            '                Dim TimeDiff As TimeSpan = B.Subtract(A)
                            '                Dim Sc As Integer = TimeDiff.TotalSeconds
                            '                HydraulicGroup(i).Valve(j).Irrigation(n).Water += HydraulicGroup(i).Valve(j).Irrigation(n).Water + (Sc * HydraulicGroup(i).Pump(z).WorkFlow / 10000) / HydraulicGroup(i).Valve(j).Area
                            '            Else
                            '                Dim TimeDiff As TimeSpan = D.Subtract(A)
                            '                Dim Sc As Integer = TimeDiff.TotalSeconds
                            '                HydraulicGroup(i).Valve(j).Irrigation(n).Water += HydraulicGroup(i).Valve(j).Irrigation(n).Water + (Sc * HydraulicGroup(i).Pump(z).WorkFlow / 10000) / HydraulicGroup(i).Valve(j).Area
                            '            End If
                            '        End If
                            '    Next
                            'Next
                        End If
                    Next
                Next
            Else
                AssignWorkFlow(i)
                AssignWater(i, j)
            End If
        Next
    End Sub
    'calculation with flow meter
    Private Sub AssignWaterflow(ByVal i As Integer, ByVal j As Integer, ByVal k As Integer)
        Dim totalarea As Double = 0
        For z As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
            If HydraulicGroup(i).Valve(z).Data IsNot Nothing Then
                For l As Integer = 0 To HydraulicGroup(i).Valve(z).Data.Length - 1
                    If HydraulicGroup(i).FlowMeter(j).Data(k).LogDT >= HydraulicGroup(i).Valve(z).Data(l).StartDT And HydraulicGroup(i).FlowMeter(j).Data(k).LogDT <= HydraulicGroup(i).Valve(z).Data(l).FinishDT Then
                        'add the new irrigation set area to the total area
                        totalarea = totalarea + HydraulicGroup(i).Valve(z).Area
                    End If
                Next
            End If
        Next
        'calculate the water applied (mm) for this irrigation record
        'divide the current water flow by the area
        'which is the water applied (mm) in three minutes (flow meter logging interval)
        If totalarea <> 0 Then
            HydraulicGroup(i).FlowMeter(j).Data(k).SValue = HydraulicGroup(i).FlowMeter(j).Data(k).SValue / totalarea
        End If
    End Sub
    'calcuate water applied for each valve
    Private Sub CalculateWaterApplied(ByVal i As Integer, ByVal z As Integer, ByVal n As Integer)
        If HydraulicGroup(i).Valve(z).Data IsNot Nothing Then
            Dim TempDate As DateTime = DateRange.StartDate.AddDays(n)
            For l As Integer = 0 To HydraulicGroup(i).Valve(z).Data.Length - 1
                Dim count As Double = 0
                Dim index As Double = 0
                If HydraulicGroup(i).Valve(z).Data(l).StartDT > TempDate And HydraulicGroup(i).Valve(z).Data(l).StartDT < TempDate.AddDays(1) Then
                    For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
                        If HydraulicGroup(i).FlowMeter(j).Data IsNot Nothing Then
                            For k As Integer = 0 To HydraulicGroup(i).FlowMeter(j).Data.Length - 1
                                If HydraulicGroup(i).FlowMeter(j).Data(k).LogDT >= HydraulicGroup(i).Valve(z).Data(l).StartDT And HydraulicGroup(i).FlowMeter(j).Data(k).LogDT <= HydraulicGroup(i).Valve(z).Data(l).FinishDT Then
                                    'add the new irrigation set area to the total area
                                    Dim flowmeterinterval As Integer
                                    flowmeterinterval = 300
                                    If HydraulicGroup(i).FlowMeter(j).Data.Length > 1 Then
                                        If k < 1 Then
                                            If HydraulicGroup(i).FlowMeter(j).Data(k + 1).LogDT.Subtract(HydraulicGroup(i).FlowMeter(j).Data(k).LogDT).TotalSeconds < 500 Then
                                                flowmeterinterval = HydraulicGroup(i).FlowMeter(j).Data(k + 1).LogDT.Subtract(HydraulicGroup(i).FlowMeter(j).Data(k).LogDT).TotalSeconds
                                            End If
                                        Else
                                            If HydraulicGroup(i).FlowMeter(j).Data(k).LogDT.Subtract(HydraulicGroup(i).FlowMeter(j).Data(k - 1).LogDT).TotalSeconds < 500 Then
                                                flowmeterinterval = HydraulicGroup(i).FlowMeter(j).Data(k).LogDT.Subtract(HydraulicGroup(i).FlowMeter(j).Data(k - 1).LogDT).TotalSeconds
                                            End If
                                        End If
                                    End If

                                    If HydraulicGroup(i).FlowMeter(j).Data(k).SValue < HydraulicGroup(i).Valve(z).MaxThreshold And HydraulicGroup(i).FlowMeter(j).Data(k).SValue > HydraulicGroup(i).Valve(z).MinThreshold Then
                                        count = count + HydraulicGroup(i).FlowMeter(j).Data(k).SValue * flowmeterinterval
                                        'Console.WriteLine(HydraulicGroup(i).FlowMeter(j).UID.ToString + "," + HydraulicGroup(i).FlowMeter(j).Data(k).SValue.ToString + "," + count.ToString)
                                        index = index + flowmeterinterval
                                        unnormalcount = 0
                                    Else
                                        unnormalcount = unnormalcount + 1
                                        If unnormalcount > 24 Then
                                            Dim Message As String = ""
                                            Message += HydraulicGroup(i).FlowMeter(j).Data(k).LogDT.ToString + "," + HydraulicGroup(i).FarmName + "," + HydraulicGroup(i).FlowMeter(j).Data(k).SValue.ToString + " L/s"
                                            Console.WriteLine(Message)
                                            outputstring.Add(Message)
                                            unnormalcount = 0
                                        End If
                                        count = count + HydraulicGroup(i).Valve(z).Flow * flowmeterinterval
                                        index = index + flowmeterinterval
                                    End If
                                End If
                            Next
                        End If
                    Next
                End If
                HydraulicGroup(i).Valve(z).Irrigation(n).Water = HydraulicGroup(i).Valve(z).Irrigation(n).Water + count / 10000
                HydraulicGroup(i).Valve(z).Irrigation(n).Duration = HydraulicGroup(i).Valve(z).Irrigation(n).Duration + index / 3600
            Next
        End If
    End Sub
    'calculation with flow meter
    Private Sub Withflowmeter(ByVal i As Integer)
        For j As Integer = 0 To HydraulicGroup(i).FlowMeter.Length - 1
            If HydraulicGroup(i).FlowMeter(j).Data IsNot Nothing Then
                For k As Integer = 0 To HydraulicGroup(i).FlowMeter(j).Data.Length - 1
                    AssignWaterflow(i, j, k)
                Next
            End If
        Next

        For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
            For k As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                CalculateWaterApplied(i, j, k)
            Next
        Next
    End Sub
    'Calculate 
    Public Sub Calculate()
        For i As Integer = 0 To HydraulicGroup.Length - 1
            If HydraulicGroup(i).FlowMeter.Length = 0 Then
                'without flower meter using the default flow rate for both drip and furrow
                Withoutflowmeter(i)
            Else
                'has flowmeter
                Withflowmeter(i)
            End If
        Next
    End Sub
End Module

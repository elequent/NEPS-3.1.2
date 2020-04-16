Imports System.IO
Module Variables
    Public Const FL = 5             'Flow meter device ID
    Public Const PP = 7             'Pump deivce ID
    Public Const VL = 30            'Valve device ID

    'To record the start and end time for database interogation
    Public StartDate As DateTime        'Irrigation event start time
    Public EndDate As DateTime          'Irrigation event end time
    Public SchedulingHour As Integer = 8 'Downlink starting hour
    'Farm profiles from the configuration file
    Public UserID As String 'User ID
    Public DateFormat As String 'Datetime format
    Public ComputerName As String 'computer name
    Public TrimedFarmName As String 'trimed farm name with special characters removed
    Public NoHydroGroup As Integer 'number of hydraulic group

    Public DayWeek() As String = {"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"}
    Public FallowBlock() As String = {"Set 2", "D4", "Set 7", "Set 8"}
    Public Priority() As String = {"P", "R1", "R2", "R3", "R4", "FAL"}
    'Irrigation interval in minutes
    Public IrrigInterval As Double = 5
    Public NoIntervalHour As Double = 60 / IrrigInterval
    'Program path
    Public path As String = Directory.GetCurrentDirectory()         'Current program path
    Public pathConfig As String = path + "\Data\"
    Public pathTemp As String = path + "\Data\Temp\"
    Public pathIrrigApp As String = path + "\Data\IrrigApp\"
    Public pathSch As String = path + "\Data\IrrigSch\"
    'user preferred daily operation time
    Public Structure PreferredIrrigTime
        Public PrefStartDT As DateTime
        Public PrefEndDT As DateTime
    End Structure
    Public Structure OpTime
        Public StartDT As DateTime
        Public EndDT As DateTime
        Public NoInterval As Double
    End Structure
    Public OperationTime As OpTime
    'hydraulic group data structure
    Public Structure HydraulicGroupData
        Public Group As Integer 'group number 
        Public GroupName As String 'group number 
        Public Type As String
        Public FarmName As String
        Public Pump() As PumpData
        Public Valve() As ValveData
        Public MoistureProbe() As ValveData
        Public Capacity As Double
    End Structure
    'pump data structure
    Public Structure PumpData
        Public Name As String
        Public UID As Integer
        Public Flow As Double
    End Structure
    'Moisture Probe structure
    Public Structure MoistureProbeData
        Public Name As String
        Public UID As Integer
    End Structure
    'valve data structure
    Public Structure ValveData
        Public Name As String
        Public UID As Integer
        Public Flow As Double
        Public Area As Double
        Public WaterRequired As Double
        Public SchData() As ScheduleData
        Public IrrigData() As ScheduleData
        Public HourlyWater As Double
        Public MaxRun As Double
        Public Threshold As Integer
        Public DefaultWater As Integer
        Public HourlyRate As Integer
        Public dryoffflag As Integer
        Public Irrigflag As Integer
        Public MaxRunOrg As Integer
        Public Ratoon As String
    End Structure
    'schedule data structure
    Public Structure ScheduleData
        Public SchDate As Date 'Schedule date 
        Public StartDT As Date  'Schedule start time
        Public FinishDT As Date 'Schedule finish time
        Public IrrigWater As Double 'schedule water required
        Public WaterApplied As Double 'schedule water required
        Public CWU As Double 'daily crop water use
        Public SWD As Double 'daily soil water deficit
        Public DDRO As Double 'Deep drain + runoff
        Public Rainfall As Double
        Public IrrigStatus() As Integer 'irrigation status for each time interval
    End Structure
    'shift structure
    Structure Shft
        Public StartDT As DateTime
        Public FinishDT As DateTime
        Public Duration As Integer
        Public Sta() As Integer
    End Structure
    'shift data array
    Public Shift As New List(Of Shft)()
    'cycle data array
    Public Cycle As New List(Of DateTime)()
    'shift file folder
    Public ShftFolder As String = "C:\WiSA7\nxdb\Data\shifts\"

    Public HydraulicGroup() As HydraulicGroupData
    Public PreferredTime(2) As PreferredIrrigTime
    'Public ShiftCount As Integer = 1
    'Public CycleCount As Integer = 1

    Public flag As Integer
    Public dryoffindex As Integer = 0
    Public outputstring As New List(Of String)
    Public shortstring As New List(Of String)
    Public DebugFlag As Boolean = True

End Module

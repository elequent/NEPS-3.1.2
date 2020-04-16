Imports System.IO
Module Variables
    'Program path
    Public path As String = Directory.GetCurrentDirectory()         'Current program path
    Public pathConfig As String = path + "\Data\"   'Data program path
    Public pathTemp As String = path + "\Data\Temp\"    'Temporary file path
    Public pathRainfall As String = path + "\Data\Rainfall\" 'To store rainfall
    Public pathIrrigApp As String = path + "\Data\IrrigApp\" 'To store irrigation app
    Public configfile As String = pathConfig + "Config.txt" 'Uplink configuration
    Dim unnormalcount As Integer = 0
    Public outputstring As New List(Of String)
    'To record the start and end time for database interogation
    Structure SearchData
        Dim StartDate As DateTime        'Irrigation event start time
        Dim EndDate As DateTime          'Irrigation event end time
    End Structure
    'Import historical irigation data
    Structure HisData
        Dim HisImport As Integer    'History data import ticked
        Dim HisStart As DateTime   'History import start date
        Dim HisEnd As DateTime          'History import end date
        Dim Span As Integer                 'Number of days span
    End Structure
    'Farm profiles from the configuration file
    Structure FarmData
        Dim UserID As String 'IrrigWeb user ID, e.g., 21
        Dim TimeFormat As String    'Time format, e.g., HH:mm:ss, h:mm:ss tt
        Dim ComputerName As String  'PC name of the computer running this program
        Dim HasRainGauge As Integer     'Is there a raingauge for this farm
        Dim UploadInterval As Integer       'unpload interval for the uplink
        Dim NoHydrauGrp As Integer         'number of hydraulic group
    End Structure
    'Data structure for rain guages
    Structure RainData
        Dim Name As String  'name of rain gauge
        Dim UID As String   'rain gauge ID
        Dim DailyRain As Double 'Daily rainfall
    End Structure
    'data structure for a hydraulic group
    Structure HydraulicGroupData
        Dim GroupID As Integer 'group ID, A, B, C, D, etc.
        Dim Type As String  'Hydraulic group irrigation type: Drip or Furrow
        Dim FarmName As String  'farmname of this hydraulic group, there might be many hydraulic groups for multiple farms on a single Aqualink
        Dim Pump() As PumpData  'Pump data
        Dim FlowMeter() As FlowMeterData  'flowmeter data
        Dim Valve() As ValveData    'valve data
        Dim MoistureProbe() As MoistureProbeData 'moisture probe data (for downlink program)
    End Structure
    'data structure for a pump
    Structure PumpData
        Dim Name As String 'pump name
        Dim UID As Integer  'pump UID
        Dim WorkFlow As Double 'Average working flow for the pump: use this value when there is no flow meters
        Dim Data() As OutputData 'output data structure for a pump

    End Structure
    'data structure for a valve
    Structure ValveData
        Dim Name As String 'valve name
        Dim UID As Integer
        Dim Flow As Double 'default flow for this valve: only use for pressurised irrigation sets
        Dim MinThreshold As Double
        Dim MaxThreshold As Double
        Dim DefaultFlow As String
        Dim CustomCal As Integer
        Dim Area As Double  'size in ha
        Dim Data() As OutputData 'output data structure
        Dim Irrigation() As IrrigData 'irrigation data for upload
    End Structure
    'data structure for a flow meter
    Structure FlowMeterData
        Dim Name As String
        Dim UID As Integer
        Dim Data() As FlowData
    End Structure
    'data structure for a moisture probe: only used for downlink program
    Structure MoistureProbeData
        Dim Name As String
        Dim UID As Integer
    End Structure
    'data structure for sensors, e.g., flow meter
    Structure FlowData
        Dim LogDT As Date   'data log datetime
        Dim SValue As Double 'flow meter valve
    End Structure
    'data structure for output devices, e.g., pump, valve
    Structure OutputData
        Dim StartDT As DateTime
        Dim FinishDT As DateTime
        Dim Flow() As FlowData
    End Structure
    'data structure irrigation data for valves
    Structure IrrigData
        Dim IrrigDate As DateTime 'irrigation date
        Dim Duration As Double 'irrigation duration
        Dim Water As Double 'water applied
    End Structure
    Public HydraulicGroup() As HydraulicGroupData
    Public RainGauge As RainData
    Public FarmInfo As FarmData
    Public HistoryData As HisData
    Public DateRange As SearchData
    Public Span As Integer
End Module

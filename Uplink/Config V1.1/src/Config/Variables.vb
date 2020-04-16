Imports System.Globalization
Imports System.IO
Imports System.Net
Module Variables
    Public path As String = Apppath + "Data\"        'Current program path
    Public fileconfig As String = Apppath + "Data\Config.txt"        'Current program path
    Public UplinkName As String      'Uplink program name
    Public LatestUplinkName As String      'Latest Uplink program name
    Public DownlinkName As String     'Downlink program name
    Public Apppath As String = Directory.GetCurrentDirectory() + "\"        'Current program path

    Public NoHydrauGroup As Integer  'Number of hydraulic group
    Public Result As New List(Of String)
    Public His As Integer
    Public SelectedRow As Integer
    Public SaveFlag As Integer = 0
    Structure DeviceData
        Dim ID As String
        Dim Group As String
        Dim Type As String
        Dim IrrigType As String
        Dim FarmName As String
        Dim MaxFlow As Integer
        Dim MaxRun As Integer
        Dim MinThreshold As Integer
        Dim MaxThreshold As Integer
        Dim DefaultFlow As String
        Dim CustomCal As Integer
    End Structure
    Structure HytraulicGroupData
        Dim NoPump As Integer
        Dim NoSet As Integer
        Dim NoFlowMeter As Integer
        Dim NoMoistureProbe As Integer
        Dim IrrigationType As String
        Dim MaxFlow As Integer
        Dim FarmName As String
    End Structure
    Public DataArray() As DeviceData
    Public HydrauGroup() As HytraulicGroupData
End Module

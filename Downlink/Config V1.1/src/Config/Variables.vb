Imports System.Globalization
Imports System.IO
Imports System.Net
Module Variables
    Public path As String = Apppath + "Data\"        'Current program path
    Public fileconfig As String = Apppath + "Data\Config.txt"        'Current program path
    Public DownlinkName As String      'Uplink program name
    Public LatestDownlinkName As String      'Latest Uplink program name
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
        Dim MaxRun As Integer
        Dim DefaultFlow As String
    End Structure
    Structure HytraulicGroupData
        Dim NoPump As Integer
        Dim NoSet As Integer
        Dim NoMoistureProbe As Integer
        Dim IrrigationType As String
        Dim FarmName As String
    End Structure
    Public DataArray() As DeviceData
    Public HydrauGroup() As HytraulicGroupData
End Module

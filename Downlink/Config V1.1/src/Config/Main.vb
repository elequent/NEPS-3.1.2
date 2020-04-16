Imports System.Globalization
Imports System.IO
Imports System.Net
Imports IWshRuntimeLibrary
Imports Shell32
Public Class Main
    'divide string to substrings
    'str: input string
    'length: string length
    'sep: separator
    'find uplink program
    Private Sub FindDownlink()
        Dim files() As String
        files = Directory.GetFiles(Directory.GetCurrentDirectory(), "*.exe", SearchOption.TopDirectoryOnly)
        For Each FileName As String In files
            Dim str As String = FileName.Substring(FileName.Length - 15, 6)
            If str = "Downlink" Then
                DownlinkName = FileName.Substring(FileName.Length - 15, 11)
            End If
        Next
    End Sub
    Private Function Divide(ByVal str As String, ByVal length As Integer, ByVal sep As String) As String()
        Dim Arr() As String
        ReDim Arr(length)
        Arr = str.Split(sep)
        Return Arr
    End Function
    'convert strting to datetime
    Private Function ConvertToDateTime(str As String) As DateTime
        Dim CheckDate As DateTime
        Dim date_info As DateTimeFormatInfo = CultureInfo.CurrentCulture.DateTimeFormat()
        Dim dateformat As String = date_info.ShortDatePattern + " " + date_info.LongTimePattern
        Try
            CheckDate = DateTime.ParseExact(str, dateformat, Nothing)
        Catch
            CheckDate = "01/01/2017 00:00:00"
        End Try
        Return CheckDate
    End Function
    'conversion to integer
    Private Function ConvertToInt(input As String) As Integer
        Dim temp As Double
        Try
            temp = CInt(input)
        Catch ex As Exception
            temp = 0
        End Try
        Return temp
    End Function
    Private Sub StopUplink(sender As Object, e As EventArgs) Handles UplinkStop.Click
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
    End Sub
    Private Sub ImportHistory(sender As Object, e As EventArgs)
        His = 1
        AddList()
        Count()
        WriteFile()
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
        Process.Start(Apppath + DownlinkName + ".exe")
        Dim check As Integer
        check = 1
        While (check)
            check = 0
            For Each prog As Process In Process.GetProcesses
                If prog.ProcessName = DownlinkName Then
                    check = 1
                End If
            Next
        End While
        Dim FILE_NAME As String = path + "IrrigApp\" + UserID.Text + "~" + PCName.Text + "his.dat"

        If System.IO.File.Exists(FILE_NAME) = True Then
            Process.Start(FILE_NAME)
        End If
    End Sub
    Private Sub RunUplink(sender As Object, e As EventArgs) Handles UplinkRun.Click
        His = 0
        AddList()
        Count()
        WriteFile()
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
        Process.Start(Apppath + DownlinkName + ".exe")
    End Sub
    Private Sub CreateFolder()
        If (Not System.IO.Directory.Exists(path + "\IrrigSch\")) Then
            System.IO.Directory.CreateDirectory(path + "\IrrigSch\")
        End If
    End Sub
    Private Sub UpdateList()
        DeviceList.Columns.Add("ID", 30)
        DeviceList.Columns.Add("Grp", 40)
        DeviceList.Columns.Add("Type", 80)
        DeviceList.Columns.Add("Irrig Type", 60)
        DeviceList.Columns.Add("Farm", 60)
        DeviceList.Columns.Add("Df FR", 60)
        DeviceList.Columns.Add("Max Runtime (Hour)", 60)
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        FindDownlink()
        Try
            CheckDownlink()
        Catch
            MsgBox("Unable to connect to server.")
        End Try
        ToolStripStatusLabel1.Text = "Current Version: " + DownlinkName + ".     Latest Version: " + LatestDownlinkName
        CreateFolder()
        UpdateList()

        ReadConfig()
        AddList()
        SetStartUp()
    End Sub
    Private Sub AddDevice(sender As Object, e As EventArgs) Handles Add.Click
        Dim arr As String() = New String(5) {}
        Dim itm As ListViewItem
        'add items to ListView
        arr(0) = DeviceID.Text
        arr(1) = HydraulicGroup.Text
        Select Case DeviceTypeComboBox.SelectedIndex
            Case "0"
                arr(2) = "Pump"
                Exit Select
            Case "1"
                arr(2) = "Moisture Probe"
                Exit Select
            Case "2"
                arr(2) = "Valve"
                Exit Select
        End Select
        Select Case DeviceType.SelectedIndex
            Case "0"
                arr(3) = "Drip"
                Exit Select
            Case "1"
                arr(3) = "Furrow"
                Exit Select
        End Select
        arr(4) = FarmName.Text
        arr(5) = MaxRun.Text
        arr(6) = DefaultFlow.Text
        itm = New ListViewItem(arr)
        DeviceList.Items.Add(itm)
    End Sub
    Private Sub DeleteDevice(sender As Object, e As EventArgs) Handles Delete.Click
        DeviceList.SelectedItems(0).Remove()
    End Sub
    Private Sub AddList()
        ReDim DataArray(DeviceList.Items.Count - 1)
        For i As Integer = 0 To DeviceList.Items.Count - 1
            DataArray(i).ID = DeviceList.Items(i).Text
            DataArray(i).Group = DeviceList.Items(i).SubItems(1).Text
            DataArray(i).Type = DeviceList.Items(i).SubItems(2).Text
            DataArray(i).IrrigType = DeviceList.Items(i).SubItems(3).Text
            DataArray(i).FarmName = DeviceList.Items(i).SubItems(4).Text
            DataArray(i).MaxRun = ConvertToInt(DeviceList.Items(i).SubItems(5).Text)
            DataArray(i).DefaultFlow = DeviceList.Items(i).SubItems(6).Text
        Next
    End Sub
    Private Sub ReadConfig()
        Dim GroupID As String
        Dim Type As String
        Dim FarmName As String
        If File.Exists(fileconfig) Then
            Dim lines() As String = IO.File.ReadAllLines(fileconfig)
            For i As Integer = 0 To lines.Length - 1
                Dim Array() As String
                Array = lines(i).Split("=")
                Select Case Array(0)
                    Case "Site Key"
                        UserID.Text = Array(1)
                        Exit Select
                    Case "Computer Name"
                        PCName.Text = Array(1)
                        Exit Select
                    Case "DateTime Format"
                        DateFormatBox.Text = Array(1)
                        Exit Select
                End Select
                If Array(0) = "Hydr Grp ID" Then
                    GroupID = Array(1)
                End If
                If Array(0) = "Irrig Type" Then
                    Type = Array(1)
                End If
                If Array(0) = "Farm Name" Then
                    FarmName = Array(1)
                End If
                Dim arr As String() = New String(9) {}
                Dim itm As ListViewItem
                If Array(0) = "Pump ID" Then
                    arr(0) = Array(1)
                    arr(2) = "Pump"
                    arr(1) = GroupID
                    arr(3) = Type
                    arr(4) = FarmName
                    arr(5) = ConvertToInt(Divide(lines(i + 1), 2, "=")(1))
                    arr(6) = "N/A"
                    itm = New ListViewItem(arr)
                    DeviceList.Items.Add(itm)
                End If
                If Array(0) = "Moisture Probe ID" Then
                    arr(0) = Array(1)
                    arr(2) = "Moisture Probe"
                    arr(1) = GroupID
                    arr(3) = Type
                    arr(4) = FarmName
                    arr(5) = "N/A"
                    arr(6) = "N/A"
                    itm = New ListViewItem(arr)
                    DeviceList.Items.Add(itm)
                End If
                If Array(0) = "Set ID" Then
                    arr(0) = Array(1)
                    arr(2) = "Valve"
                    arr(1) = GroupID
                    arr(3) = Type
                    arr(4) = FarmName
                    arr(5) = ConvertToInt(Divide(lines(i + 1), 2, "=")(1))
                    arr(6) = ConvertToInt(Divide(lines(i + 2), 2, "=")(1))
                    itm = New ListViewItem(arr)
                    DeviceList.Items.Add(itm)
                End If
            Next
        End If
    End Sub
    Private Sub Count()
        Dim Lister As New List(Of String)()
        For i As Integer = 0 To DataArray.Length - 1
            Lister.Add(DataArray(i).Group)
        Next

        Result = Lister.Distinct().ToList
        NoHydrauGroup = Result.Count

        ReDim HydrauGroup(NoHydrauGroup - 1)
        For i As Integer = 0 To Result.Count - 1
            For j As Integer = 0 To DataArray.Length - 1
                If DataArray(j).Group = Result(i) And DataArray(j).Type = "Pump" Then
                    HydrauGroup(i).NoPump = HydrauGroup(i).NoPump + 1
                    HydrauGroup(i).IrrigationType = DataArray(j).IrrigType
                    HydrauGroup(i).FarmName = DataArray(j).FarmName
                End If
                If DataArray(j).Group = Result(i) And DataArray(j).Type = "Valve" Then
                    HydrauGroup(i).NoSet = HydrauGroup(i).NoSet + 1
                End If
                If DataArray(j).Group = Result(i) And DataArray(j).Type = "Moisture Probe" Then
                    HydrauGroup(i).NoMoistureProbe = HydrauGroup(i).NoMoistureProbe + 1
                End If
            Next
        Next
    End Sub
    Private Sub WriteFile()
        Using outputFile As New StreamWriter(fileconfig)
            outputFile.WriteLine("[Site Key]")
            outputFile.WriteLine("Site Key=" + UserID.Text)
            outputFile.WriteLine("")

            outputFile.WriteLine("[Farm Information]")
            outputFile.WriteLine("Computer Name=" + PCName.Text)
            outputFile.WriteLine("No. Hydro Group=" + CStr(NoHydrauGroup))
            outputFile.WriteLine("DateTime Format=" + DateFormatBox.Text)
            outputFile.WriteLine("")

            outputFile.WriteLine("[Preferred Time]")
            outputFile.WriteLine("Weekdays=" + (WeekDayStart.Value.ToShortTimeString) + "-" + (WeekDayEnd.Value.ToShortTimeString))
            outputFile.WriteLine("Weekends=" + (WeekEndsStart.Value.ToShortTimeString) + "-" + (WeekEndsEnd.Value.ToShortTimeString))
            outputFile.WriteLine("")

            For i As Integer = 0 To HydrauGroup.Length - 1
                outputFile.WriteLine("[Hydr Group " + Result(i) + "]")
                outputFile.WriteLine("Hydr Grp ID=" + Result(i))
                outputFile.WriteLine("Farm Name=" + HydrauGroup(i).FarmName)
                outputFile.WriteLine("Irrig Type=" + HydrauGroup(i).IrrigationType)
                outputFile.WriteLine("No. Pump=" + CStr(HydrauGroup(i).NoPump))
                For j As Integer = 0 To DataArray.Length - 1
                    If DataArray(j).Group = Result(i) And DataArray(j).Type = "Pump" Then
                        outputFile.WriteLine("Pump ID=" + DataArray(j).ID)
                    End If
                Next
                outputFile.WriteLine("No. Moisture Probe=" + CStr(HydrauGroup(i).NoMoistureProbe))
                For j As Integer = 0 To DataArray.Length - 1
                    If DataArray(j).Group = Result(i) And DataArray(j).Type = "Moisture Probe" Then
                        outputFile.WriteLine("Moisture Probe ID=" + DataArray(j).ID)
                    End If
                Next
                outputFile.WriteLine("No. Set=" + CStr(HydrauGroup(i).NoSet))
                For j As Integer = 0 To DataArray.Length - 1
                    If DataArray(j).Group = Result(i) And DataArray(j).Type = "Valve" Then
                        outputFile.WriteLine("Set ID=" + DataArray(j).ID)
                        outputFile.WriteLine("Max Runtime=" + CStr(DataArray(j).MaxRun))
                        outputFile.WriteLine("Default Flow=" + DataArray(j).DefaultFlow)
                    End If
                Next
                outputFile.WriteLine("")
            Next
        End Using
        SaveFlag = 1
    End Sub
    Private Sub StopDownlink(sender As Object, e As EventArgs)
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
    End Sub
    Private Sub RunDownlink(sender As Object, e As EventArgs)
        AddList()
        Count()
        WriteFile()
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
        Process.Start(Apppath + DownlinkName + ".exe")
    End Sub
    Private Sub SaveConfig(sender As Object, e As EventArgs) Handles Save.Click
        AddList()
        Count()
        WriteFile()
    End Sub
    Private Sub EditList(sender As Object, e As EventArgs) Handles DeviceList.DoubleClick
        SelectedRow = DeviceList.SelectedItems(0).Index
        DeviceID.Text = DeviceList.SelectedItems(0).SubItems(0).Text
        HydraulicGroup.Text = DeviceList.SelectedItems(0).SubItems(1).Text
        DeviceTypeComboBox.Text = DeviceList.SelectedItems(0).SubItems(2).Text
        DeviceType.Text = DeviceList.SelectedItems(0).SubItems(3).Text
        FarmName.Text = DeviceList.SelectedItems(0).SubItems(4).Text

        If DeviceTypeComboBox.Text = "Valve" Then
            MaxRun.Enabled = True
            DefaultFlow.Enabled = True
            DefaultFlow.Text = DeviceList.SelectedItems(0).SubItems(5).Text
            MaxRun.Text = DeviceList.SelectedItems(0).SubItems(6).Text
        End If
        If DeviceTypeComboBox.Text = "Pump" Then
            MaxRun.Enabled = False
            DefaultFlow.Enabled = True
            DefaultFlow.Text = DeviceList.SelectedItems(0).SubItems(5).Text
        End If
        If DeviceTypeComboBox.Text = "Moisture Probe" Then
            MaxRun.Enabled = False
            DefaultFlow.Enabled = False
        End If

    End Sub
    Private Sub EidtDevice(sender As Object, e As EventArgs) Handles Edit.Click
        DeviceList.Items(SelectedRow).SubItems(0).Text = DeviceID.Text
        DeviceList.Items(SelectedRow).SubItems(1).Text = HydraulicGroup.Text
        DeviceList.Items(SelectedRow).SubItems(2).Text = DeviceTypeComboBox.Text
        DeviceList.Items(SelectedRow).SubItems(3).Text = DeviceType.Text
        DeviceList.Items(SelectedRow).SubItems(4).Text = FarmName.Text
        DeviceList.Items(SelectedRow).SubItems(5).Text = MaxRun.Text
        DeviceList.Items(SelectedRow).SubItems(8).Text = DefaultFlow.Text
    End Sub
    Private Sub InputNumberCheck(sender As Object, e As KeyPressEventArgs) Handles UserID.KeyPress, DeviceID.KeyPress, IrrigInterval.KeyPress
        If Asc(e.KeyChar) <> 13 AndAlso Asc(e.KeyChar) <> 8 AndAlso Not IsNumeric(e.KeyChar) Then
            MessageBox.Show("Please enter numbers only")
            e.Handled = True
        End If
    End Sub
    Private Sub InputLetterCheck(sender As Object, e As KeyPressEventArgs) Handles HydraulicGroup.KeyPress
        If Asc(e.KeyChar) < 65 Or Asc(e.KeyChar) > 90 Then
            MessageBox.Show("Please enter letters A-Z only")
            e.Handled = True
        End If
    End Sub
    Private Sub SpecialCharacterCheck(sender As Object, e As KeyPressEventArgs) Handles PCName.KeyPress
        If Char.IsLetterOrDigit(e.KeyChar) Or Char.IsControl(e.KeyChar) Then
            e.Handled = False
        Else
            MessageBox.Show("Please enter letters only")
            e.Handled = True
        End If
    End Sub
    'check and update uplink program
    Private Sub Update_Click(sender As Object, e As EventArgs) Handles Update.Click
        UpdateUplink()
        ToolStripStatusLabel1.Text = "Current Version: " + DownlinkName + ".     Latest Version: " + LatestDownlinkName
    End Sub
    'set Uplink as a startup program
    Private Sub SetStartUp()
        Create_ShortCut(Apppath + DownlinkName + ".exe", Apppath, DownlinkName, "", 6, 0)
        Dim dir As String = Environment.GetFolderPath(Environment.SpecialFolder.Startup)
        File.Copy(Apppath + DownlinkName + ".lnk", dir + "\" + DownlinkName + ".lnk", True)
        File.Delete(Apppath + DownlinkName + ".lnk")
        'MsgBox(DownlinkName + " will automatically start with windows.")
    End Sub
    Private Sub Create_ShortCut(ByVal TargetPath As String, ByVal ShortCutPath As String, ByVal ShortCutname As String, ByVal WorkPath As String, ByVal Window_Style As Integer, ByVal IconNum As Integer)
        Dim VbsObj As Object
        VbsObj = CreateObject("WScript.Shell")
        Dim MyShortcut As Object
        'ShortCutPath = VbsObj.SpecialFolders(ShortCutPath)
        MyShortcut = VbsObj.CreateShortcut(ShortCutPath & "\" & ShortCutname & ".lnk")
        MyShortcut.TargetPath = TargetPath
        MyShortcut.WorkingDirectory = WorkPath
        MyShortcut.WindowStyle = Window_Style
        MyShortcut.IconLocation = TargetPath & "," & IconNum
        MyShortcut.Save()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Run()
        DateFormatBox.Text = TimeFormat
    End Sub
End Class

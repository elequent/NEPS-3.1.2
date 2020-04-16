Imports System.Globalization
Imports System
Imports System.IO
Imports System.Net.Mail
Imports System.Net
Imports System.Timers
Imports System.Text
Imports System.Web
Imports Newtonsoft.Json.Linq

'changes: 03/04/2019
'fix the error when the flow meter interval is 300 seconds, remove the constraint to 500 seconds
Module Main
    Dim request As HttpWebRequest
    Dim response As HttpWebResponse = Nothing
    Dim reader As StreamReader
    Dim address As Uri
    Dim token As String
    Dim data As StringBuilder
    Dim byteData() As Byte
    Dim postStream As Stream = Nothing
    'For hiding console window
    Private Declare Auto Function GetConsoleWindow Lib "kernel32.dll" () As IntPtr
    Private Declare Auto Function ShowWindow Lib "user32.dll" (ByVal hWnd As IntPtr, ByVal nCmdShow As Integer) As Boolean

    'Timer to trigger uplink
    Private aTimer As System.Timers.Timer
    'save irrigation data to IrrigApp folder
    Private Sub SaveIrrigation()
        Using outputFile As New StreamWriter(pathIrrigApp + FarmInfo.UserID + "~" + FarmInfo.ComputerName + ".dat")
            For i As Integer = 0 To HydraulicGroup.Length - 1
                For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                    For k As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                        outputFile.WriteLine(HydraulicGroup(i).FarmName + "," + DateRange.StartDate.AddDays(k).ToString("dd/MM/yyyy") + "," + HydraulicGroup(i).Valve(j).Name + "," + HydraulicGroup(i).Valve(j).Irrigation(k).Water.ToString("F2"))
                    Next
                Next
            Next
        End Using
    End Sub
    Private Sub SaveHis()
        Using outputFile As New StreamWriter(pathIrrigApp + FarmInfo.UserID + "~" + FarmInfo.ComputerName + "his.dat")
            For i As Integer = 0 To HydraulicGroup.Length - 1
                For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                    For k As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                        outputFile.WriteLine(HydraulicGroup(i).FarmName + "," + DateRange.StartDate.AddDays(k).ToString("dd/MM/yyyy") + "," + HydraulicGroup(i).Valve(j).Name + "," + HydraulicGroup(i).Valve(j).Irrigation(k).Water.ToString("F2"))
                    Next
                Next
            Next
        End Using
    End Sub
    'for test
    Private Sub SaveTest()
        Using outputFile As New StreamWriter(pathIrrigApp + FarmInfo.UserID + "~Test.dat")
            For i As Integer = 0 To HydraulicGroup.Length - 1
                For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                    For k As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                        outputFile.WriteLine(HydraulicGroup(i).FarmName + "," + DateRange.StartDate.AddDays(k).ToString("dd/MM/yyyy") + "," + HydraulicGroup(i).Valve(j).Name +
                            "," + HydraulicGroup(i).Valve(j).Irrigation(k).Water.ToString("F2") + "," + HydraulicGroup(i).Valve(j).Irrigation(k).Duration.ToString("F2") + "," +
                            (HydraulicGroup(i).Valve(j).Irrigation(k).Water / HydraulicGroup(i).Valve(j).Irrigation(k).Duration).ToString("F2"))
                    Next
                Next
            Next
        End Using
    End Sub
    'upload files to FTP server
    Private Sub Upload(ByVal str As String)
        Try
            Dim filename As String = "ftp://40.119.207.243:21/" + str
            Dim request As System.Net.FtpWebRequest = DirectCast(System.Net.WebRequest.Create(filename), System.Net.FtpWebRequest)
            request.Credentials = New System.Net.NetworkCredential("Aqualink", "bBCzaaZ4L}g(")
            request.Method = System.Net.WebRequestMethods.Ftp.UploadFile
            Threading.Thread.Sleep(1000)

            Dim file As Byte() = System.IO.File.ReadAllBytes(pathConfig + str)

            Dim strz As System.IO.Stream = request.GetRequestStream()
            strz.Write(file, 0, file.Length)
            strz.Close()
            strz.Dispose()
        Catch
            Using outputFile As New StreamWriter(pathConfig + "Logs.txt")
                outputFile.WriteLine("Upload Failed" + DateTime.Now.ToString("yyyy/MM/dd HH:mm:ss"))
            End Using
        End Try
    End Sub
    'Run data extraction, assignment, calculation and storage
    Private Sub Run()
        'Console.WriteLine("Start")
        If FarmInfo.HasRainGauge = 1 Then
            GenRain()
        End If
        ExtractProfile() 'extract pump, valve, flow meter profiles from the database to a "profile.txt"
        ReadProfile() 'read profiles for all the devices, including, UID, default flow rate, name, etc.

        'interrogate the database for all the data entries within the range of the search
        WriteSQL()
        WriteCMD()
        RunBatch()
        If File.Exists(pathTemp + "DevicesData.txt") Then
            'read the output data from the database
            ReadData()
            'caculate the irrigation data for each valve
            Calculate()
            'save irrigation data
            SaveIrrigation()
            'save test data
            SaveTest()
        Else
            SendEmail("eric.wang@jcu.edu.au", "Uplink Failed")
            System.Diagnostics.Process.Start(System.AppDomain.CurrentDomain.FriendlyName)
            Environment.Exit(0)
        End If
        If outputstring.Count > 0 Then
            SendEmail("eric.wang@jcu.edu.au", "Abnormal flow meter readings")
            outputstring.Clear()
        End If
    End Sub
    Public Sub SendEmail(address As String, title As String)
        Dim msg As MailMessage = New MailMessage()
        msg.[To].Add(New MailAddress(address))
        msg.From = New MailAddress("eric.wgk@hotmail.com", "Eric Wang")
        msg.Subject = title
        msg.IsBodyHtml = True
        Dim sbContent As New StringBuilder()
        For j As Integer = 0 To outputstring.Count - 1
            msg.Body += outputstring(j) + "<br>"
        Next

        Dim client As SmtpClient = New SmtpClient()
        client.Credentials = New System.Net.NetworkCredential("eric.wgk@hotmail.com", "Eric137456")
        client.Port = 587
        client.Host = "smtp.live.com"
        client.DeliveryMethod = SmtpDeliveryMethod.Network
        client.EnableSsl = True
        Try
            client.Send(msg)
        Catch ex As Exception
        End Try
    End Sub
    'import history data
    Private Sub ImportHis()
        DateRange.StartDate = New DateTime(HistoryData.HisStart.Year, HistoryData.HisStart.Month, HistoryData.HisStart.Day, 0, 0, 0, 0)
        DateRange.EndDate = New DateTime(HistoryData.HisEnd.Year, HistoryData.HisEnd.Month, HistoryData.HisEnd.Day, 23, 59, 59, 0)
        'HistoryData.Span = HistoryData.HisEnd.Subtract(HistoryData.HisStart).Days
        Span = HistoryData.HisEnd.Subtract(HistoryData.HisStart).Days

        Console.WriteLine("Start")
        If FarmInfo.HasRainGauge = 1 Then
            GenRain()
        End If
        ExtractProfile() 'extract pump, valve, flow meter profiles from the database to a "profile.txt"
        ReadProfile() 'read profiles for all the devices, including, UID, default flow rate, name, etc.

        'interrogate the database for all the data entries within the range of the search
        WriteSQL()
        WriteCMD()
        RunBatch()
        If File.Exists(pathTemp + "DevicesData.txt") Then
            'read the output data from the database
            ReadData()
            'caculate the irrigation data for each valve
            Calculate()
            'save irrigation data
            SaveIrrigation()
            ''save test data
            SaveTest()
            SaveHis()

            Dim UploadPath As String
            UploadPath = "IrrigApp/" + FarmInfo.UserID + "~" + FarmInfo.ComputerName + "his.dat"
            Upload(UploadPath)
            'Gettoken()
            'UploadIrrigation()
        End If
    End Sub
    'Main function
    Private Sub Gettoken()
        address = New Uri("http://40.119.207.243:81/api/token")
        ' Create the web request  
        request = DirectCast(WebRequest.Create(address), HttpWebRequest)

        ' Set type to POST  
        request.Method = "POST"
        request.ContentType = "application/x-www-form-urlencoded"

        data = New StringBuilder()

        data.Append("grant_type=" + HttpUtility.UrlEncode("password"))
        data.Append("&username=" + HttpUtility.UrlEncode("JCU"))
        data.Append("&password=" + HttpUtility.UrlEncode("7[A9p.K_wgu+?E^C"))

        ' Create a byte array of the data we want to send  
        byteData = UTF8Encoding.UTF8.GetBytes(data.ToString())

        ' Set the content length in the request headers  
        request.ContentLength = byteData.Length

        ' Write data  
        Try
            postStream = request.GetRequestStream()
            postStream.Write(byteData, 0, byteData.Length)
        Finally
            If Not postStream Is Nothing Then postStream.Close()
        End Try

        Try
            ' Get response  
            response = DirectCast(request.GetResponse(), HttpWebResponse)

            ' Get the response stream into a reader  
            reader = New StreamReader(response.GetResponseStream())
            Dim rawresp As String
            rawresp = reader.ReadToEnd()
            Dim json As JObject = JObject.Parse(rawresp)
            token = json.SelectToken("access_token")
        Finally
            If Not response Is Nothing Then response.Close()
        End Try
    End Sub
    Private Sub IrrigWebUpload(farmname As String, blockname As String, dt As String, amount As String)
        address = New Uri("http://40.119.207.243:81/api/Block/Irrigation?" _
                                     + "Username=eric.wang@jcu.edu.au" + "&FarmName=" + farmname _
                                     + "&BlockName=" + blockname + "&IrrigDate=" + dt + "&IrrigAmount=" + amount)
        request = DirectCast(WebRequest.Create(address), HttpWebRequest)

        request.Method = "PUT"
        request.Headers.Add("Authorization", "Bearer " & token)
        request.ContentLength = 0

        Try
            ' Get response  
            response = DirectCast(request.GetResponse(), HttpWebResponse)

            reader = New StreamReader(response.GetResponseStream())
            Dim rawresp As String
            rawresp = reader.ReadToEnd()
            Console.WriteLine(rawresp)
        Finally
            If Not response Is Nothing Then response.Close()
        End Try
    End Sub
    Sub Main()

        'delete all the temporary files

        '03/04/2019
        ''''''''''''''''''''''''''''''''''''''''''''''''
        DeleteFilesFromFolder(pathTemp, "*.txt")
        DeleteFilesFromFolder(pathTemp, "*.bat")
        ''''''''''''''''''''''''''''''''''''''''''''''''

        'hide the console window
        Dim hwnd As Integer
        hwnd = GetConsoleWindow()
        ShowWindow(hwnd, 0)
        'read the configuration file for the basic farm information
        ReadConfig()

        If HistoryData.HisImport = 1 Then
            ImportHis()
        Else
            Span = 6
            SetTimer()
        End If
        'Console.Read()

    End Sub
    'Set timers
    Private Sub UploadIrrigation()
        For i As Integer = 0 To HydraulicGroup.Length - 1
            For j As Integer = 0 To HydraulicGroup(i).Valve.Length - 1
                For k As Integer = 0 To HydraulicGroup(i).Valve(j).Irrigation.Length - 1
                    Console.WriteLine(HydraulicGroup(i).FarmName + ";" + HydraulicGroup(i).Valve(j).Name + ";" + DateRange.StartDate.AddDays(k).ToString("yyyy-MM-dd") + ";" + HydraulicGroup(i).Valve(j).Irrigation(k).Water.ToString("F2"))
                    IrrigWebUpload(HydraulicGroup(i).FarmName, HydraulicGroup(i).Valve(j).Name, DateRange.StartDate.AddDays(k).ToString("yyyy-MM-dd"), HydraulicGroup(i).Valve(j).Irrigation(k).Water.ToString("F2"))
                Next
            Next
        Next
    End Sub
    Private Sub SetTimer()
        aTimer = New System.Timers.Timer(FarmInfo.UploadInterval * 1000)
        AddHandler aTimer.Elapsed, AddressOf OnTimedEvent
        aTimer.AutoReset = False
        aTimer.Enabled = True
        aTimer.Start()
        Console.ReadKey()
    End Sub
    'Timer event
    Private Sub OnTimedEvent(source As Object, e As ElapsedEventArgs)
        'If Now.Hour = 20 Then
        '    System.Diagnostics.Process.Start(System.AppDomain.CurrentDomain.FriendlyName)
        '    Environment.Exit(0)
        'End If
        DeleteFilesFromFolder(pathTemp, "*.txt")
        DeleteFilesFromFolder(pathTemp, "*.bat")
        DateRange.StartDate = New DateTime(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day, 0, 0, 0, 0)
        DateRange.StartDate = DateRange.StartDate.AddDays(-6)
        DateRange.EndDate = DateTime.Now
        ReadConfig()
        Run()

        Dim UploadPath As String
        UploadPath = "IrrigApp/" + FarmInfo.UserID + "~" + FarmInfo.ComputerName + ".dat"
        Upload(UploadPath)
        If FarmInfo.HasRainGauge = 1 Then
            UploadPath = "Rainfall/" + FarmInfo.UserID + "~" + FarmInfo.ComputerName + ".dat"
            Upload(UploadPath)
        End If
        aTimer.Start()
    End Sub
End Module

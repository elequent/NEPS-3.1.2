Imports System.Net
Imports System.IO
Imports System.Net.Mail
Imports System.Text
Imports System.Timers
Public Class Main
    Private Declare Auto Function GetConsoleWindow Lib "kernel32.dll" () As IntPtr
    Private Declare Auto Function ShowWindow Lib "user32.dll" (ByVal hWnd As IntPtr, ByVal nCmdShow As Integer) As Boolean

    Private Const SW_HIDE As Integer = 0
    Private Const SW_SHOW As Integer = 5

    Private aTimer As System.Timers.Timer
    Structure FarmData
        Dim ID As String
        Dim Name As String
        Dim LastUpdated As String
    End Structure
    Dim FarmName() As FarmData
    Dim path As String = Directory.GetCurrentDirectory() + "\"         'Current program path
    Public SelectedRow As Integer
    Dim Emails() As String
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        InitiateList()
        LoadNames()
        LoadEmails()
        CheckEmails()
    End Sub

    Private Sub SaveFarms()
        ReDim FarmName(List.Items.Count - 1)
        For i As Integer = 0 To List.Items.Count - 1
            FarmName(i).ID = List.Items(i).Text
            FarmName(i).Name = List.Items(i).SubItems(1).Text
            FarmName(i).LastUpdated = List.Items(i).SubItems(2).Text
        Next
    End Sub
    Private Sub SaveEmails()
        ReDim Emails(EmailList.Items.Count - 1)
        For i As Integer = 0 To EmailList.Items.Count - 1
            Emails(i) = EmailList.Items(i).Text
        Next
    End Sub
    Private Sub SaveFiles()
        Using outputFile As New StreamWriter(path + "Names.dat")
            For i As Integer = 0 To FarmName.Length - 1
                outputFile.WriteLine(FarmName(i).ID + "~" + FarmName(i).Name)
            Next
        End Using
        Using outputFile As New StreamWriter(path + "Emails.dat")
            For i As Integer = 0 To Emails.Length - 1
                outputFile.WriteLine(Emails(i))
            Next
        End Using
    End Sub
    Private Sub Btn_Run_Click(sender As Object, e As EventArgs) Handles Btn_Run.Click
        SaveFarms()
        SaveEmails()

        Download()
        CheckTime()
        SetTimer()
    End Sub
    Private Sub Btn_Stop_Click(sender As Object, e As EventArgs) Handles Btn_Stop.Click
        aTimer.Stop()
    End Sub
    Public Sub SendEmail(Message As String, address As String)
        Dim msg As MailMessage = New MailMessage()
        msg.[To].Add(New MailAddress(address, Name))
        msg.From = New MailAddress("eric.wgk@hotmail.com", "")
        msg.Subject = Message

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
    Sub CheckTime()
        For i As Integer = 0 To FarmName.Length - 1
            If Now.Subtract(FarmName(i).LastUpdated).TotalHours > 24 Then
                Dim msg = FarmName(i).Name + " has not been updated for " + Now.Subtract(FarmName(i).LastUpdated).TotalHours.ToString("F0") + " hours"
                If Chk_Notification.Checked = True Then
                    MsgBox(msg)
                End If
                If Chk_Email.Checked = True Then
                    For j As Integer = 0 To Emails.Length - 1
                        SendEmail(msg, Emails(j))
                    Next
                End If
            End If
        Next
    End Sub
    Sub SetTimer()
        aTimer = New System.Timers.Timer(CInt(Edt_Interval.Text) * 1000)
        AddHandler aTimer.Elapsed, AddressOf OnTimedEvent
        aTimer.AutoReset = False
        aTimer.Enabled = True
        aTimer.Start()
    End Sub
    'Timer event
    Sub OnTimedEvent(source As Object, e As ElapsedEventArgs)
        Download()
        CheckTime()
        aTimer.Start()
    End Sub
    Private Sub InitiateList()
        List.Columns.Add("ID", 40)
        List.Columns.Add("Computer Name", 160)
        List.Columns.Add("Last Updated", 160)
        EmailList.Columns.Add("Email", 360)
    End Sub
    Private Sub CheckEmails()
        If Chk_Email.Checked = True Then
            EmailList.Enabled = True
            Btn_AddEmail.Enabled = True
            Btn_DelEmail.Enabled = True
            Btn_EdtEmail.Enabled = True
            Txt_Email.Enabled = True
            Edt_Email.Enabled = True
        Else
            EmailList.Enabled = False
            Btn_AddEmail.Enabled = False
            Btn_DelEmail.Enabled = False
            Btn_EdtEmail.Enabled = False
            Txt_Email.Enabled = False
            Edt_Email.Enabled = False
        End If
    End Sub
    'load farm names
    Private Sub LoadNames()
        If File.Exists(path + "Names.dat") Then
            Dim lines As String() = System.IO.File.ReadAllLines(path + "Names.dat")
            For i As Integer = 0 To lines.Length - 1
                Dim arr As String() = New String(4) {}
                Dim itm As ListViewItem
                Dim Array() As String
                Array = lines(i).Split("~")
                arr(0) = Array(0)
                arr(1) = Array(1)
                arr(2) = "N/A"
                itm = New ListViewItem(arr)
                List.Items.Add(itm)
            Next
        End If
    End Sub
    'load email list
    Private Sub LoadEmails()
        If File.Exists(path + "Emails.dat") Then
            Dim lines As String() = System.IO.File.ReadAllLines(path + "Emails.dat")
            For i As Integer = 0 To lines.Length - 1
                Dim itm As ListViewItem
                itm = New ListViewItem(lines(i))
                EmailList.Items.Add(itm)
            Next
        End If
    End Sub
    'Download schedule
    Sub Download()
        For i As Integer = 0 To FarmName.Length - 1
            Dim temp As String = FarmName(i).ID + "~" + FarmName(i).Name + ".dat"
            Dim ftp As Net.FtpWebRequest = Net.FtpWebRequest.Create("ftp://Aqualink:bBCzaaZ4L}g(@40.119.207.243:21/IrrigApp/" + temp)
            ftp.Method = Net.WebRequestMethods.Ftp.GetDateTimestamp
            Try
                Using response = CType(ftp.GetResponse(), Net.FtpWebResponse)
                    FarmName(i).LastUpdated = response.LastModified.ToString
                    List.Items(i).SubItems(2).Text = FarmName(i).LastUpdated
                End Using
            Catch
            End Try
        Next
    End Sub
    Private Sub Chk_Email_CheckedChanged(sender As Object, e As EventArgs) Handles Chk_Email.CheckedChanged
        CheckEmails()
    End Sub
    'minimise to notification icon
    Private Sub Main_Resize(sender As Object, e As EventArgs) Handles MyBase.Resize
        If Me.WindowState = FormWindowState.Minimized Then
            Me.Visible = False
            NotifyIcon1.Visible = True
        End If
    End Sub
    'double click the notify icon to restore the window
    Private Sub NotifyIcon1_DoubleClick(sender As Object, e As EventArgs) Handles NotifyIcon1.DoubleClick
        Me.Visible = True
        Me.WindowState = FormWindowState.Normal
    End Sub
    'Listview actions
    'Add farm
    Private Sub Btn_Add_Click(sender As Object, e As EventArgs) Handles Btn_Add.Click
        Dim arr As String() = New String(2) {}
        Dim itm As ListViewItem
        arr(0) = Edt_ID.Text
        arr(1) = Edt_Name.Text
        itm = New ListViewItem(arr)
        List.Items.Add(itm)
    End Sub
    'Add Email
    Private Sub Btn_AddEmail_Click(sender As Object, e As EventArgs) Handles Btn_AddEmail.Click
        Dim itm As ListViewItem
        itm = New ListViewItem(Edt_Email.Text)
        EmailList.Items.Add(itm)
    End Sub
    'Delete farm
    Private Sub DeleteDevice(sender As Object, e As EventArgs) Handles Btn_Delete.Click
        List.SelectedItems(0).Remove()
    End Sub
    'delete email
    Private Sub DeleteEmail(sender As Object, e As EventArgs) Handles Btn_DelEmail.Click
        EmailList.SelectedItems(0).Remove()
    End Sub
    'Select list item
    Private Sub EditList(sender As Object, e As EventArgs) Handles List.DoubleClick
        SelectedRow = List.SelectedItems(0).Index
        Edt_ID.Text = List.SelectedItems(0).SubItems(0).Text
        Edt_Name.Text = List.SelectedItems(0).SubItems(1).Text
    End Sub
    'select email list item
    Private Sub EditEmailList(sender As Object, e As EventArgs) Handles EmailList.DoubleClick
        SelectedRow = EmailList.SelectedItems(0).Index
        Edt_Email.Text = EmailList.SelectedItems(0).SubItems(0).Text
    End Sub
    'edit farm
    Private Sub EidtDevice(sender As Object, e As EventArgs) Handles Btn_Edt.Click
        List.Items(SelectedRow).SubItems(0).Text = Edt_ID.Text
        List.Items(SelectedRow).SubItems(1).Text = Edt_Name.Text
    End Sub
    'edit email
    Private Sub EditEmail(sender As Object, e As EventArgs) Handles Btn_EdtEmail.Click
        EmailList.Items(SelectedRow).SubItems(0).Text = Edt_ID.Text
        EmailList.Items(SelectedRow).SubItems(1).Text = Edt_Name.Text
    End Sub

    Private Sub Main_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        SaveFarms()
        SaveEmails()
        SaveFiles()
    End Sub
End Class

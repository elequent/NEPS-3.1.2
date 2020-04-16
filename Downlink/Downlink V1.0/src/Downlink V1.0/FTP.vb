Imports System.Net
Imports System.IO
Imports System.Net.Mail
Imports System.Text

Module FTP
    'Upload files to FTP
    Public Sub Upload(ByVal des As String, ByVal src As String)
        Try
            Dim filename As String = "ftp://138.91.41.137:62050/" + des
            Dim request As System.Net.FtpWebRequest = DirectCast(System.Net.WebRequest.Create(filename), System.Net.FtpWebRequest)
            request.Credentials = New System.Net.NetworkCredential("Aqualink", "bBCzaaZ4L}g(")
            request.Method = System.Net.WebRequestMethods.Ftp.UploadFile

            Threading.Thread.Sleep(1000) ' Sleep 1 minute and check again
            Dim file() As Byte = System.IO.File.ReadAllBytes(src)

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
    'Download schedule
    Public Sub Download()
        Dim request As New WebClient()
        ' Confirm the Network credentials based on the user name and password passed in.
        request.Credentials = New NetworkCredential("Aqualink", "bBCzaaZ4L}g(")
        'Read the file data into a Byte array
        Dim bytes() As Byte = request.DownloadData("ftp://40.119.207.243:21/IrrigSched/" + UserID + ".dat")
        Try
            '  Create a FileStream to read the file into
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            Dim FileName As String = pathTemp + UserID + ".dat"
            Dim DownloadStream As FileStream = IO.File.Create(FileName)
            '  Stream this data into the file
            DownloadStream.Write(bytes, 0, bytes.Length)
            '  Close the FileStream
            DownloadStream.Close()
        Catch ex As Exception
            Exit Sub
        End Try
    End Sub
    Public Sub SendEmail(FilePath As String, address As String)
        Try
            Dim smtp_server As New SmtpClient
            Dim e_mail As New MailMessage()
            smtp_server.UseDefaultCredentials = False
            smtp_server.Credentials = New Net.NetworkCredential("eric.wgk@hotmail.com", "Eric137456")
            smtp_server.Port = 587
            smtp_server.EnableSsl = True
            smtp_server.Host = "smtp.outlook.com"

            e_mail = New MailMessage()
            e_mail.From = New MailAddress("eric.wang@jcu.edu.au")
            e_mail.To.Add(address)
            e_mail.Subject = "Aaron Linton Farm Irrigation Schedule:" + Now.Date.ToString("dd/MM/yyyy")
            e_mail.IsBodyHtml = True

            'Dim sb As New StringBuilder
            Dim Text As String = "<table><tr><td>EmpId</td><td>Emp name</td><td>age</td></tr><tr><td>value</td><td>value</td><td>value</td></tr></table>"

            'If File.Exists(FilePath) Then
            '    Dim lines As String() = System.IO.File.ReadAllLines(FilePath)
            '    For i As Integer = 0 To lines.Length - 1
            '        sb.AppendLine(lines(i))
            '    Next
            'End If
            e_mail.Body = Text
            smtp_server.Send(e_mail)

            e_mail.Dispose()
            smtp_server = Nothing
            e_mail = Nothing

        Catch error_t As Exception
        End Try
    End Sub
    Public Sub SendEmail(FilePath As String, address As String, name As String)
        Dim msg As MailMessage = New MailMessage()
        msg.[To].Add(New MailAddress(address, name))
        msg.From = New MailAddress("eric.wgk@hotmail.com", "Eric Wang")
        msg.Subject = "Aaron Linton Farm Irrigation Schedule: " + Now.Date.ToString("dd/MM/yyyy")
        msg.IsBodyHtml = True

        Dim strHeader As String = "<table ><tbody>"
        Dim strFooter As String = "</tbody></table>"

        Dim sbContent As New StringBuilder()
        If File.Exists(FilePath) Then
            Dim lines As String() = System.IO.File.ReadAllLines(FilePath)
            For i As Integer = 0 To lines.Length - 1
                sbContent.Append("<tr>")
                Dim Arr() As String
                ReDim Arr(7)
                Arr = lines(i).Split("|")
                For j As Integer = 0 To Arr.Length - 1
                    sbContent.Append(String.Format("<td>{0}</td>", Arr(j)))
                Next
                sbContent.Append("</tr>")
            Next
        End If
        msg.Body = strHeader & sbContent.ToString & strFooter

        For i As Integer = 0 To Shift.Count - 1
            If File.Exists(pathSch + "Downlink_" + (i + 1).ToString + ".bmp") Then
                Dim attachment As System.Net.Mail.Attachment
                attachment = New System.Net.Mail.Attachment(pathSch + "Downlink_" + (i + 1).ToString + ".bmp")
                msg.Attachments.Add(attachment)
            End If
        Next
        Dim client As SmtpClient = New SmtpClient()
        'client.UseDefaultCredentials = False
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
End Module

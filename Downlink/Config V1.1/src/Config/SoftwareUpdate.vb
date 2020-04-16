Imports System.Globalization
Imports System.IO
Imports System.Net

Module SoftwareUpdate
    'downlink file from FTP
    Private Sub Download(sr As String)
        Dim request As New WebClient()
        ' Confirm the Network credentials based on the user name and password passed in.
        request.Credentials = New NetworkCredential("Aqualink", "bBCzaaZ4L}g(")
        'Read the file data into a Byte array
        Dim bytes() As Byte = request.DownloadData("ftp://40.119.207.243:21/Uplink/" + sr + ".exe")
        Try
            '  Create a FileStream to read the file into
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            Dim FileName As String = Apppath + sr + ".exe"
            Dim DownloadStream As FileStream = IO.File.Create(FileName)
            '  Stream this data into the file
            DownloadStream.Write(bytes, 0, bytes.Length)
            '  Close the FileStream
            DownloadStream.Close()
        Catch ex As Exception
            Exit Sub
        End Try
    End Sub
    Public Sub UpdateUplink()
        For Each prog As Process In Process.GetProcesses
            If prog.ProcessName = DownlinkName Then
                prog.Kill()
            End If
        Next
        File.Delete(Apppath + DownlinkName + ".exe")
        Download(LatestDownlinkName)
        MsgBox("Update completed!")
        DownlinkName = LatestDownlinkName
    End Sub
    'check uplink
    Public Sub CheckDownlink()
        Dim Dirlist As New List(Of String) 'I prefer List() instead of an array
        Dim request As FtpWebRequest = DirectCast(WebRequest.Create("ftp://40.119.207.243:21/Uplink"), FtpWebRequest)

        request.Method = WebRequestMethods.Ftp.ListDirectory
        request.Credentials = New NetworkCredential("Aqualink", "bBCzaaZ4L}g(")

        Dim response As FtpWebResponse = DirectCast(request.GetResponse(), FtpWebResponse)
        Dim responseStream As Stream = response.GetResponseStream

        Using reader As New StreamReader(responseStream)
            Do While reader.Peek <> -1
                Dirlist.Add(reader.ReadLine)
            Loop
        End Using
        response.Close()
        For i As Integer = 0 To Dirlist.Count - 1
            If Dirlist(i).Length > 7 Then
                If Dirlist(i).Substring(0, 6) = "Uplink" Then
                    LatestDownlinkName = Dirlist(i).Substring(0, 11)
                End If
            End If
        Next
    End Sub
End Module

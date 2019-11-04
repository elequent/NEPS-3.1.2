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
        Dim bytes() As Byte = request.DownloadData("ftp://138.91.41.137:62050/Uplink/" + sr + ".exe")
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
    'check uplink
    Public Sub CheckUplink()
        Dim Dirlist As New List(Of String) 'I prefer List() instead of an array
        Dim request As FtpWebRequest = DirectCast(WebRequest.Create("ftp://138.91.41.137:62050/Uplink"), FtpWebRequest)

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
            If Dirlist(i).Substring(0, 6) = "Uplink" Then
                If Dirlist(i).Substring(0, 11) <> UplinkName Then
                    MsgBox("There is a new version: " + Dirlist(i).Substring(0, 11) + ". Start update.")
                    Cursor.Current = Cursors.WaitCursor

                    For Each prog As Process In Process.GetProcesses
                        If prog.ProcessName = UplinkName Then
                            prog.Kill()
                        End If
                    Next
                    File.Delete(Apppath + UplinkName + ".exe")
                    Download(Dirlist(i).Substring(0, 11))
                    MsgBox("Update completed!")
                    UplinkName = Dirlist(i).Substring(0, 11)
                    Cursor.Current = Cursors.Default

                Else
                    MsgBox("This is the latest version.")
                End If
            End If
        Next
    End Sub
End Module

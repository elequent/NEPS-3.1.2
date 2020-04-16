<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Main
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Main))
        Me.Edt_ID = New System.Windows.Forms.TextBox()
        Me.Edt_Name = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Btn_Edt = New System.Windows.Forms.Button()
        Me.List = New System.Windows.Forms.ListView()
        Me.Btn_Add = New System.Windows.Forms.Button()
        Me.Btn_Delete = New System.Windows.Forms.Button()
        Me.Btn_Stop = New System.Windows.Forms.Button()
        Me.Btn_Run = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Edt_Interval = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Chk_Notification = New System.Windows.Forms.CheckBox()
        Me.Chk_Email = New System.Windows.Forms.CheckBox()
        Me.Edt_Email = New System.Windows.Forms.TextBox()
        Me.Txt_Email = New System.Windows.Forms.Label()
        Me.Btn_EdtEmail = New System.Windows.Forms.Button()
        Me.EmailList = New System.Windows.Forms.ListView()
        Me.Btn_AddEmail = New System.Windows.Forms.Button()
        Me.Btn_DelEmail = New System.Windows.Forms.Button()
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.SuspendLayout()
        '
        'Edt_ID
        '
        Me.Edt_ID.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Edt_ID.Location = New System.Drawing.Point(56, 16)
        Me.Edt_ID.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Edt_ID.Name = "Edt_ID"
        Me.Edt_ID.Size = New System.Drawing.Size(97, 20)
        Me.Edt_ID.TabIndex = 22
        '
        'Edt_Name
        '
        Me.Edt_Name.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Edt_Name.Location = New System.Drawing.Point(282, 16)
        Me.Edt_Name.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Edt_Name.Name = "Edt_Name"
        Me.Edt_Name.Size = New System.Drawing.Size(126, 20)
        Me.Edt_Name.TabIndex = 23
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.Location = New System.Drawing.Point(25, 19)
        Me.Label6.Margin = New System.Windows.Forms.Padding(5, 0, 5, 0)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(19, 14)
        Me.Label6.TabIndex = 20
        Me.Label6.Text = "ID"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.Location = New System.Drawing.Point(176, 19)
        Me.Label7.Margin = New System.Windows.Forms.Padding(5, 0, 5, 0)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(102, 14)
        Me.Label7.TabIndex = 21
        Me.Label7.Text = "Computer Name"
        '
        'Btn_Edt
        '
        Me.Btn_Edt.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_Edt.Location = New System.Drawing.Point(226, 54)
        Me.Btn_Edt.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_Edt.Name = "Btn_Edt"
        Me.Btn_Edt.Size = New System.Drawing.Size(90, 26)
        Me.Btn_Edt.TabIndex = 25
        Me.Btn_Edt.Text = "Edit"
        Me.Btn_Edt.UseVisualStyleBackColor = True
        '
        'List
        '
        Me.List.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.List.FullRowSelect = True
        Me.List.GridLines = True
        Me.List.HideSelection = False
        Me.List.LabelEdit = True
        Me.List.Location = New System.Drawing.Point(28, 88)
        Me.List.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.List.Name = "List"
        Me.List.Size = New System.Drawing.Size(379, 144)
        Me.List.TabIndex = 24
        Me.List.UseCompatibleStateImageBehavior = False
        Me.List.View = System.Windows.Forms.View.Details
        '
        'Btn_Add
        '
        Me.Btn_Add.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_Add.Location = New System.Drawing.Point(30, 54)
        Me.Btn_Add.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_Add.Name = "Btn_Add"
        Me.Btn_Add.Size = New System.Drawing.Size(90, 26)
        Me.Btn_Add.TabIndex = 18
        Me.Btn_Add.Text = "Add"
        Me.Btn_Add.UseVisualStyleBackColor = True
        '
        'Btn_Delete
        '
        Me.Btn_Delete.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_Delete.Location = New System.Drawing.Point(128, 54)
        Me.Btn_Delete.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_Delete.Name = "Btn_Delete"
        Me.Btn_Delete.Size = New System.Drawing.Size(90, 26)
        Me.Btn_Delete.TabIndex = 19
        Me.Btn_Delete.Text = "Delete"
        Me.Btn_Delete.UseVisualStyleBackColor = True
        '
        'Btn_Stop
        '
        Me.Btn_Stop.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_Stop.Location = New System.Drawing.Point(317, 240)
        Me.Btn_Stop.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_Stop.Name = "Btn_Stop"
        Me.Btn_Stop.Size = New System.Drawing.Size(90, 26)
        Me.Btn_Stop.TabIndex = 19
        Me.Btn_Stop.Text = "Stop"
        Me.Btn_Stop.UseVisualStyleBackColor = True
        '
        'Btn_Run
        '
        Me.Btn_Run.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_Run.Location = New System.Drawing.Point(219, 240)
        Me.Btn_Run.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_Run.Name = "Btn_Run"
        Me.Btn_Run.Size = New System.Drawing.Size(90, 26)
        Me.Btn_Run.TabIndex = 18
        Me.Btn_Run.Text = "Run"
        Me.Btn_Run.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(25, 246)
        Me.Label1.Margin = New System.Windows.Forms.Padding(5, 0, 5, 0)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(55, 14)
        Me.Label1.TabIndex = 20
        Me.Label1.Text = "Interval"
        '
        'Edt_Interval
        '
        Me.Edt_Interval.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Edt_Interval.Location = New System.Drawing.Point(90, 243)
        Me.Edt_Interval.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Edt_Interval.Name = "Edt_Interval"
        Me.Edt_Interval.Size = New System.Drawing.Size(97, 20)
        Me.Edt_Interval.TabIndex = 22
        Me.Edt_Interval.Text = "3600"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(197, 246)
        Me.Label2.Margin = New System.Windows.Forms.Padding(5, 0, 5, 0)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(12, 14)
        Me.Label2.TabIndex = 20
        Me.Label2.Text = "s"
        '
        'Chk_Notification
        '
        Me.Chk_Notification.AutoSize = True
        Me.Chk_Notification.Checked = True
        Me.Chk_Notification.CheckState = System.Windows.Forms.CheckState.Checked
        Me.Chk_Notification.Location = New System.Drawing.Point(27, 271)
        Me.Chk_Notification.Margin = New System.Windows.Forms.Padding(4)
        Me.Chk_Notification.Name = "Chk_Notification"
        Me.Chk_Notification.Size = New System.Drawing.Size(94, 18)
        Me.Chk_Notification.TabIndex = 26
        Me.Chk_Notification.Text = "Notification"
        Me.Chk_Notification.UseVisualStyleBackColor = True
        '
        'Chk_Email
        '
        Me.Chk_Email.AutoSize = True
        Me.Chk_Email.Location = New System.Drawing.Point(141, 271)
        Me.Chk_Email.Margin = New System.Windows.Forms.Padding(4)
        Me.Chk_Email.Name = "Chk_Email"
        Me.Chk_Email.Size = New System.Drawing.Size(60, 18)
        Me.Chk_Email.TabIndex = 27
        Me.Chk_Email.Text = "Email"
        Me.Chk_Email.UseVisualStyleBackColor = True
        '
        'Edt_Email
        '
        Me.Edt_Email.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Edt_Email.Location = New System.Drawing.Point(76, 297)
        Me.Edt_Email.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Edt_Email.Name = "Edt_Email"
        Me.Edt_Email.Size = New System.Drawing.Size(97, 20)
        Me.Edt_Email.TabIndex = 31
        '
        'Txt_Email
        '
        Me.Txt_Email.AutoSize = True
        Me.Txt_Email.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Txt_Email.Location = New System.Drawing.Point(25, 300)
        Me.Txt_Email.Margin = New System.Windows.Forms.Padding(5, 0, 5, 0)
        Me.Txt_Email.Name = "Txt_Email"
        Me.Txt_Email.Size = New System.Drawing.Size(41, 14)
        Me.Txt_Email.TabIndex = 30
        Me.Txt_Email.Text = "Email"
        '
        'Btn_EdtEmail
        '
        Me.Btn_EdtEmail.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_EdtEmail.Location = New System.Drawing.Point(224, 325)
        Me.Btn_EdtEmail.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_EdtEmail.Name = "Btn_EdtEmail"
        Me.Btn_EdtEmail.Size = New System.Drawing.Size(90, 26)
        Me.Btn_EdtEmail.TabIndex = 33
        Me.Btn_EdtEmail.Text = "Edit"
        Me.Btn_EdtEmail.UseVisualStyleBackColor = True
        '
        'EmailList
        '
        Me.EmailList.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.EmailList.FullRowSelect = True
        Me.EmailList.GridLines = True
        Me.EmailList.HideSelection = False
        Me.EmailList.LabelEdit = True
        Me.EmailList.Location = New System.Drawing.Point(26, 359)
        Me.EmailList.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.EmailList.Name = "EmailList"
        Me.EmailList.Size = New System.Drawing.Size(379, 149)
        Me.EmailList.TabIndex = 32
        Me.EmailList.UseCompatibleStateImageBehavior = False
        Me.EmailList.View = System.Windows.Forms.View.Details
        '
        'Btn_AddEmail
        '
        Me.Btn_AddEmail.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_AddEmail.Location = New System.Drawing.Point(28, 325)
        Me.Btn_AddEmail.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_AddEmail.Name = "Btn_AddEmail"
        Me.Btn_AddEmail.Size = New System.Drawing.Size(90, 26)
        Me.Btn_AddEmail.TabIndex = 28
        Me.Btn_AddEmail.Text = "Add"
        Me.Btn_AddEmail.UseVisualStyleBackColor = True
        '
        'Btn_DelEmail
        '
        Me.Btn_DelEmail.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Btn_DelEmail.Location = New System.Drawing.Point(126, 325)
        Me.Btn_DelEmail.Margin = New System.Windows.Forms.Padding(5, 4, 5, 4)
        Me.Btn_DelEmail.Name = "Btn_DelEmail"
        Me.Btn_DelEmail.Size = New System.Drawing.Size(90, 26)
        Me.Btn_DelEmail.TabIndex = 29
        Me.Btn_DelEmail.Text = "Delete"
        Me.Btn_DelEmail.UseVisualStyleBackColor = True
        '
        'NotifyIcon1
        '
        Me.NotifyIcon1.Icon = CType(resources.GetObject("NotifyIcon1.Icon"), System.Drawing.Icon)
        Me.NotifyIcon1.Text = "NotifyIcon1"
        Me.NotifyIcon1.Visible = True
        '
        'Main
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 14.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(429, 521)
        Me.Controls.Add(Me.Edt_Email)
        Me.Controls.Add(Me.Txt_Email)
        Me.Controls.Add(Me.Btn_EdtEmail)
        Me.Controls.Add(Me.EmailList)
        Me.Controls.Add(Me.Btn_AddEmail)
        Me.Controls.Add(Me.Btn_DelEmail)
        Me.Controls.Add(Me.Chk_Email)
        Me.Controls.Add(Me.Chk_Notification)
        Me.Controls.Add(Me.Edt_Interval)
        Me.Controls.Add(Me.Edt_ID)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Edt_Name)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Btn_Edt)
        Me.Controls.Add(Me.List)
        Me.Controls.Add(Me.Btn_Run)
        Me.Controls.Add(Me.Btn_Add)
        Me.Controls.Add(Me.Btn_Stop)
        Me.Controls.Add(Me.Btn_Delete)
        Me.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "Main"
        Me.Text = "Uplink Monitor"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Edt_ID As TextBox
    Friend WithEvents Edt_Name As TextBox
    Friend WithEvents Label6 As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents Btn_Edt As Button
    Friend WithEvents List As ListView
    Friend WithEvents Btn_Add As Button
    Friend WithEvents Btn_Delete As Button
    Friend WithEvents Btn_Stop As Button
    Friend WithEvents Btn_Run As Button
    Friend WithEvents Label1 As Label
    Friend WithEvents Edt_Interval As TextBox
    Friend WithEvents Label2 As Label
    Friend WithEvents Chk_Notification As CheckBox
    Friend WithEvents Chk_Email As CheckBox
    Friend WithEvents Edt_Email As TextBox
    Friend WithEvents Txt_Email As Label
    Friend WithEvents Btn_EdtEmail As Button
    Friend WithEvents EmailList As ListView
    Friend WithEvents Btn_AddEmail As Button
    Friend WithEvents Btn_DelEmail As Button
    Friend WithEvents NotifyIcon1 As NotifyIcon
End Class

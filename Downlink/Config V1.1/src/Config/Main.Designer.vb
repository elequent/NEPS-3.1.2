<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Main
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Main))
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.UserID = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.DateFormatBox = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.PCName = New System.Windows.Forms.TextBox()
        Me.Panel3 = New System.Windows.Forms.Panel()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.FarmName = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.DeviceID = New System.Windows.Forms.TextBox()
        Me.HydraulicGroup = New System.Windows.Forms.TextBox()
        Me.DeviceType = New System.Windows.Forms.ComboBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.DeviceTypeComboBox = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Edit = New System.Windows.Forms.Button()
        Me.Delete = New System.Windows.Forms.Button()
        Me.Add = New System.Windows.Forms.Button()
        Me.DeviceList = New System.Windows.Forms.ListView()
        Me.Panel6 = New System.Windows.Forms.Panel()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Save = New System.Windows.Forms.Button()
        Me.UplinkStop = New System.Windows.Forms.Button()
        Me.UplinkRun = New System.Windows.Forms.Button()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.IrrigInterval = New System.Windows.Forms.TextBox()
        Me.WeekDayStart = New System.Windows.Forms.DateTimePicker()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.WeekEndsStart = New System.Windows.Forms.DateTimePicker()
        Me.WeekDayEnd = New System.Windows.Forms.DateTimePicker()
        Me.WeekEndsEnd = New System.Windows.Forms.DateTimePicker()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.Update = New System.Windows.Forms.Button()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.DefaultFlow = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.MaxRun = New System.Windows.Forms.TextBox()
        Me.Panel1.SuspendLayout()
        Me.Panel3.SuspendLayout()
        Me.Panel6.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.Panel1.Controls.Add(Me.Button1)
        Me.Panel1.Controls.Add(Me.Label13)
        Me.Panel1.Controls.Add(Me.UserID)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.DateFormatBox)
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Controls.Add(Me.PCName)
        Me.Panel1.Location = New System.Drawing.Point(16, 13)
        Me.Panel1.Margin = New System.Windows.Forms.Padding(4)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(553, 47)
        Me.Panel1.TabIndex = 15
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(484, 12)
        Me.Button1.Margin = New System.Windows.Forms.Padding(4)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(30, 23)
        Me.Button1.TabIndex = 33
        Me.Button1.Text = "T"
        Me.Button1.UseVisualStyleBackColor = True
        Me.Button1.Visible = False
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(341, 15)
        Me.Label13.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(37, 14)
        Me.Label13.TabIndex = 13
        Me.Label13.Text = "Time"
        Me.Label13.Visible = False
        '
        'UserID
        '
        Me.UserID.Location = New System.Drawing.Point(55, 11)
        Me.UserID.Margin = New System.Windows.Forms.Padding(4)
        Me.UserID.Name = "UserID"
        Me.UserID.Size = New System.Drawing.Size(41, 20)
        Me.UserID.TabIndex = 11
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(4, 14)
        Me.Label1.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(48, 14)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "User ID"
        '
        'DateFormatBox
        '
        Me.DateFormatBox.Location = New System.Drawing.Point(386, 12)
        Me.DateFormatBox.Margin = New System.Windows.Forms.Padding(4)
        Me.DateFormatBox.Name = "DateFormatBox"
        Me.DateFormatBox.Size = New System.Drawing.Size(90, 20)
        Me.DateFormatBox.TabIndex = 14
        Me.DateFormatBox.Text = "h:mm:ss tt"
        Me.DateFormatBox.Visible = False
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(106, 14)
        Me.Label2.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(102, 14)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "Computer Name"
        '
        'PCName
        '
        Me.PCName.Location = New System.Drawing.Point(210, 12)
        Me.PCName.Margin = New System.Windows.Forms.Padding(4)
        Me.PCName.Name = "PCName"
        Me.PCName.Size = New System.Drawing.Size(121, 20)
        Me.PCName.TabIndex = 11
        '
        'Panel3
        '
        Me.Panel3.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.Panel3.Controls.Add(Me.MaxRun)
        Me.Panel3.Controls.Add(Me.Label3)
        Me.Panel3.Controls.Add(Me.DefaultFlow)
        Me.Panel3.Controls.Add(Me.Label5)
        Me.Panel3.Controls.Add(Me.FarmName)
        Me.Panel3.Controls.Add(Me.Label10)
        Me.Panel3.Controls.Add(Me.DeviceID)
        Me.Panel3.Controls.Add(Me.HydraulicGroup)
        Me.Panel3.Controls.Add(Me.DeviceType)
        Me.Panel3.Controls.Add(Me.Label12)
        Me.Panel3.Controls.Add(Me.DeviceTypeComboBox)
        Me.Panel3.Controls.Add(Me.Label9)
        Me.Panel3.Controls.Add(Me.Label6)
        Me.Panel3.Controls.Add(Me.Label7)
        Me.Panel3.Location = New System.Drawing.Point(7, 25)
        Me.Panel3.Margin = New System.Windows.Forms.Padding(2, 3, 2, 3)
        Me.Panel3.Name = "Panel3"
        Me.Panel3.Size = New System.Drawing.Size(533, 93)
        Me.Panel3.TabIndex = 17
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(4, 64)
        Me.Label5.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(67, 14)
        Me.Label5.TabIndex = 21
        Me.Label5.Text = "Default FR"
        '
        'FarmName
        '
        Me.FarmName.Location = New System.Drawing.Point(277, 34)
        Me.FarmName.Margin = New System.Windows.Forms.Padding(4)
        Me.FarmName.Name = "FarmName"
        Me.FarmName.Size = New System.Drawing.Size(84, 20)
        Me.FarmName.TabIndex = 16
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(194, 36)
        Me.Label10.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(76, 14)
        Me.Label10.TabIndex = 15
        Me.Label10.Text = "Farm Name"
        '
        'DeviceID
        '
        Me.DeviceID.Location = New System.Drawing.Point(80, 6)
        Me.DeviceID.Margin = New System.Windows.Forms.Padding(4)
        Me.DeviceID.Name = "DeviceID"
        Me.DeviceID.Size = New System.Drawing.Size(84, 20)
        Me.DeviceID.TabIndex = 11
        '
        'HydraulicGroup
        '
        Me.HydraulicGroup.Location = New System.Drawing.Point(277, 6)
        Me.HydraulicGroup.Margin = New System.Windows.Forms.Padding(4)
        Me.HydraulicGroup.Name = "HydraulicGroup"
        Me.HydraulicGroup.Size = New System.Drawing.Size(84, 20)
        Me.HydraulicGroup.TabIndex = 11
        '
        'DeviceType
        '
        Me.DeviceType.FormattingEnabled = True
        Me.DeviceType.Items.AddRange(New Object() {"Drip", "Furrow"})
        Me.DeviceType.Location = New System.Drawing.Point(80, 33)
        Me.DeviceType.Margin = New System.Windows.Forms.Padding(4)
        Me.DeviceType.Name = "DeviceType"
        Me.DeviceType.Size = New System.Drawing.Size(84, 22)
        Me.DeviceType.TabIndex = 14
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(4, 36)
        Me.Label12.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(70, 14)
        Me.Label12.TabIndex = 13
        Me.Label12.Text = "Irrig. Type"
        '
        'DeviceTypeComboBox
        '
        Me.DeviceTypeComboBox.FormattingEnabled = True
        Me.DeviceTypeComboBox.Items.AddRange(New Object() {"Pump", "Moisture Probe", "Valve"})
        Me.DeviceTypeComboBox.Location = New System.Drawing.Point(277, 61)
        Me.DeviceTypeComboBox.Margin = New System.Windows.Forms.Padding(4)
        Me.DeviceTypeComboBox.Name = "DeviceTypeComboBox"
        Me.DeviceTypeComboBox.Size = New System.Drawing.Size(84, 22)
        Me.DeviceTypeComboBox.TabIndex = 14
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(195, 64)
        Me.Label9.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(77, 14)
        Me.Label9.TabIndex = 13
        Me.Label9.Text = "Device Type"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(5, 8)
        Me.Label6.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(19, 14)
        Me.Label6.TabIndex = 8
        Me.Label6.Text = "ID"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(194, 8)
        Me.Label7.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(43, 14)
        Me.Label7.TabIndex = 8
        Me.Label7.Text = "Group"
        '
        'Edit
        '
        Me.Edit.Font = New System.Drawing.Font("Georgia", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Edit.Location = New System.Drawing.Point(177, 125)
        Me.Edit.Margin = New System.Windows.Forms.Padding(4)
        Me.Edit.Name = "Edit"
        Me.Edit.Size = New System.Drawing.Size(77, 24)
        Me.Edit.TabIndex = 17
        Me.Edit.Text = "Edit"
        Me.Edit.UseVisualStyleBackColor = True
        '
        'Delete
        '
        Me.Delete.Font = New System.Drawing.Font("Georgia", 10.2!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Delete.Location = New System.Drawing.Point(93, 125)
        Me.Delete.Margin = New System.Windows.Forms.Padding(4)
        Me.Delete.Name = "Delete"
        Me.Delete.Size = New System.Drawing.Size(77, 24)
        Me.Delete.TabIndex = 1
        Me.Delete.Text = "Delete"
        Me.Delete.UseVisualStyleBackColor = True
        '
        'Add
        '
        Me.Add.Location = New System.Drawing.Point(9, 125)
        Me.Add.Margin = New System.Windows.Forms.Padding(4)
        Me.Add.Name = "Add"
        Me.Add.Size = New System.Drawing.Size(77, 24)
        Me.Add.TabIndex = 1
        Me.Add.Text = "Add"
        Me.Add.UseVisualStyleBackColor = True
        '
        'DeviceList
        '
        Me.DeviceList.FullRowSelect = True
        Me.DeviceList.GridLines = True
        Me.DeviceList.HideSelection = False
        Me.DeviceList.LabelEdit = True
        Me.DeviceList.Location = New System.Drawing.Point(7, 157)
        Me.DeviceList.Margin = New System.Windows.Forms.Padding(4)
        Me.DeviceList.Name = "DeviceList"
        Me.DeviceList.Size = New System.Drawing.Size(533, 134)
        Me.DeviceList.TabIndex = 12
        Me.DeviceList.UseCompatibleStateImageBehavior = False
        Me.DeviceList.View = System.Windows.Forms.View.Details
        '
        'Panel6
        '
        Me.Panel6.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.Panel6.Controls.Add(Me.Edit)
        Me.Panel6.Controls.Add(Me.Label16)
        Me.Panel6.Controls.Add(Me.Panel3)
        Me.Panel6.Controls.Add(Me.DeviceList)
        Me.Panel6.Controls.Add(Me.Add)
        Me.Panel6.Controls.Add(Me.Delete)
        Me.Panel6.Location = New System.Drawing.Point(16, 77)
        Me.Panel6.Margin = New System.Windows.Forms.Padding(2, 3, 2, 3)
        Me.Panel6.Name = "Panel6"
        Me.Panel6.Size = New System.Drawing.Size(553, 301)
        Me.Panel6.TabIndex = 20
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.Location = New System.Drawing.Point(5, 7)
        Me.Label16.Margin = New System.Windows.Forms.Padding(2, 0, 2, 0)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(112, 14)
        Me.Label16.TabIndex = 18
        Me.Label16.Text = "Farm Information"
        '
        'Save
        '
        Me.Save.Location = New System.Drawing.Point(16, 385)
        Me.Save.Margin = New System.Windows.Forms.Padding(4)
        Me.Save.Name = "Save"
        Me.Save.Size = New System.Drawing.Size(98, 33)
        Me.Save.TabIndex = 21
        Me.Save.Text = "Save"
        Me.Save.UseVisualStyleBackColor = True
        '
        'UplinkStop
        '
        Me.UplinkStop.Location = New System.Drawing.Point(241, 385)
        Me.UplinkStop.Margin = New System.Windows.Forms.Padding(4)
        Me.UplinkStop.Name = "UplinkStop"
        Me.UplinkStop.Size = New System.Drawing.Size(98, 33)
        Me.UplinkStop.TabIndex = 30
        Me.UplinkStop.Text = "Stop"
        Me.UplinkStop.UseVisualStyleBackColor = True
        '
        'UplinkRun
        '
        Me.UplinkRun.Location = New System.Drawing.Point(125, 385)
        Me.UplinkRun.Margin = New System.Windows.Forms.Padding(4)
        Me.UplinkRun.Name = "UplinkRun"
        Me.UplinkRun.Size = New System.Drawing.Size(98, 33)
        Me.UplinkRun.TabIndex = 31
        Me.UplinkRun.Text = "Run"
        Me.UplinkRun.UseVisualStyleBackColor = True
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(4, 31)
        Me.Label11.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(73, 17)
        Me.Label11.TabIndex = 15
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(4, 57)
        Me.Label14.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(80, 17)
        Me.Label14.TabIndex = 15
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(74, 12)
        Me.Label8.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(74, 17)
        Me.Label8.TabIndex = 15
        '
        'IrrigInterval
        '
        Me.IrrigInterval.Location = New System.Drawing.Point(373, 29)
        Me.IrrigInterval.Margin = New System.Windows.Forms.Padding(4)
        Me.IrrigInterval.Name = "IrrigInterval"
        Me.IrrigInterval.Size = New System.Drawing.Size(71, 20)
        Me.IrrigInterval.TabIndex = 11
        '
        'WeekDayStart
        '
        Me.WeekDayStart.CustomFormat = "H:mm"
        Me.WeekDayStart.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.WeekDayStart.Location = New System.Drawing.Point(74, 29)
        Me.WeekDayStart.Margin = New System.Windows.Forms.Padding(3, 2, 3, 2)
        Me.WeekDayStart.Name = "WeekDayStart"
        Me.WeekDayStart.ShowUpDown = True
        Me.WeekDayStart.Size = New System.Drawing.Size(78, 20)
        Me.WeekDayStart.TabIndex = 16
        Me.WeekDayStart.Value = New Date(2018, 9, 12, 21, 0, 0, 0)
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.Location = New System.Drawing.Point(258, 31)
        Me.Label17.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(113, 17)
        Me.Label17.TabIndex = 2
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.Location = New System.Drawing.Point(158, 12)
        Me.Label15.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(75, 17)
        Me.Label15.TabIndex = 15
        '
        'WeekEndsStart
        '
        Me.WeekEndsStart.CustomFormat = "H:mm"
        Me.WeekEndsStart.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.WeekEndsStart.Location = New System.Drawing.Point(157, 29)
        Me.WeekEndsStart.Margin = New System.Windows.Forms.Padding(3, 2, 3, 2)
        Me.WeekEndsStart.Name = "WeekEndsStart"
        Me.WeekEndsStart.ShowUpDown = True
        Me.WeekEndsStart.Size = New System.Drawing.Size(78, 20)
        Me.WeekEndsStart.TabIndex = 16
        Me.WeekEndsStart.Value = New Date(2018, 9, 12, 0, 0, 0, 0)
        '
        'WeekDayEnd
        '
        Me.WeekDayEnd.CustomFormat = "H:mm"
        Me.WeekDayEnd.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.WeekDayEnd.Location = New System.Drawing.Point(74, 54)
        Me.WeekDayEnd.Margin = New System.Windows.Forms.Padding(3, 2, 3, 2)
        Me.WeekDayEnd.Name = "WeekDayEnd"
        Me.WeekDayEnd.ShowUpDown = True
        Me.WeekDayEnd.Size = New System.Drawing.Size(78, 20)
        Me.WeekDayEnd.TabIndex = 16
        Me.WeekDayEnd.Value = New Date(2018, 9, 12, 7, 0, 0, 0)
        '
        'WeekEndsEnd
        '
        Me.WeekEndsEnd.CustomFormat = "H:mm"
        Me.WeekEndsEnd.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.WeekEndsEnd.Location = New System.Drawing.Point(157, 54)
        Me.WeekEndsEnd.Margin = New System.Windows.Forms.Padding(3, 2, 3, 2)
        Me.WeekEndsEnd.Name = "WeekEndsEnd"
        Me.WeekEndsEnd.ShowUpDown = True
        Me.WeekEndsEnd.Size = New System.Drawing.Size(78, 20)
        Me.WeekEndsEnd.TabIndex = 16
        Me.WeekEndsEnd.Value = New Date(2018, 9, 12, 23, 59, 0, 0)
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(451, 31)
        Me.Label18.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(15, 17)
        Me.Label18.TabIndex = 2
        '
        'Update
        '
        Me.Update.Location = New System.Drawing.Point(471, 386)
        Me.Update.Margin = New System.Windows.Forms.Padding(4)
        Me.Update.Name = "Update"
        Me.Update.Size = New System.Drawing.Size(98, 33)
        Me.Update.TabIndex = 24
        Me.Update.Text = "Update"
        Me.Update.UseVisualStyleBackColor = True
        '
        'StatusStrip1
        '
        Me.StatusStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 675)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(581, 22)
        Me.StatusStrip1.TabIndex = 25
        Me.StatusStrip1.Text = "dddddddddddddddddd"
        Me.StatusStrip1.Visible = False
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.BackColor = System.Drawing.SystemColors.Control
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        Me.ToolStripStatusLabel1.Size = New System.Drawing.Size(119, 17)
        Me.ToolStripStatusLabel1.Text = "ToolStripStatusLabel1"
        '
        'DefaultFlow
        '
        Me.DefaultFlow.Location = New System.Drawing.Point(80, 61)
        Me.DefaultFlow.Margin = New System.Windows.Forms.Padding(4)
        Me.DefaultFlow.Name = "DefaultFlow"
        Me.DefaultFlow.Size = New System.Drawing.Size(84, 20)
        Me.DefaultFlow.TabIndex = 22
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(367, 9)
        Me.Label3.Margin = New System.Windows.Forms.Padding(4, 0, 4, 0)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(58, 14)
        Me.Label3.TabIndex = 21
        Me.Label3.Text = "Max Run"
        '
        'MaxRun
        '
        Me.MaxRun.Location = New System.Drawing.Point(433, 6)
        Me.MaxRun.Margin = New System.Windows.Forms.Padding(4)
        Me.MaxRun.Name = "MaxRun"
        Me.MaxRun.Size = New System.Drawing.Size(84, 20)
        Me.MaxRun.TabIndex = 22
        '
        'Main
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 14.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.ControlLightLight
        Me.ClientSize = New System.Drawing.Size(581, 432)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.UplinkStop)
        Me.Controls.Add(Me.UplinkRun)
        Me.Controls.Add(Me.Update)
        Me.Controls.Add(Me.Save)
        Me.Controls.Add(Me.Panel6)
        Me.Controls.Add(Me.Panel1)
        Me.Font = New System.Drawing.Font("Georgia", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(2, 3, 2, 3)
        Me.MaximizeBox = False
        Me.Name = "Main"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Uplink Configuration"
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.Panel3.ResumeLayout(False)
        Me.Panel3.PerformLayout()
        Me.Panel6.ResumeLayout(False)
        Me.Panel6.PerformLayout()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout
        Me.ResumeLayout(false)
        Me.PerformLayout

End Sub
    Friend WithEvents Panel1 As Panel
    Friend WithEvents Label13 As Label
    Friend WithEvents DateFormatBox As TextBox
    Friend WithEvents UserID As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents PCName As TextBox
    Friend WithEvents Panel3 As Panel
    Friend WithEvents DeviceID As TextBox
    Friend WithEvents HydraulicGroup As TextBox
    Friend WithEvents DeviceType As ComboBox
    Friend WithEvents Label12 As Label
    Friend WithEvents DeviceTypeComboBox As ComboBox
    Friend WithEvents Label9 As Label
    Friend WithEvents Label6 As Label
    Friend WithEvents Delete As Button
    Friend WithEvents Label7 As Label
    Friend WithEvents Add As Button
    Friend WithEvents DeviceList As ListView
    Friend WithEvents Panel6 As Panel
    Friend WithEvents Label16 As Label
    Friend WithEvents Save As Button
    Friend WithEvents FarmName As TextBox
    Friend WithEvents Label10 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Edit As Button
    Friend WithEvents UplinkStop As Button
    Friend WithEvents UplinkRun As Button
    Friend WithEvents Label11 As Label
    Friend WithEvents Label14 As Label
    Friend WithEvents Label8 As Label
    Friend WithEvents IrrigInterval As TextBox
    Friend WithEvents WeekDayStart As DateTimePicker
    Friend WithEvents Label17 As Label
    Friend WithEvents Label15 As Label
    Friend WithEvents WeekEndsStart As DateTimePicker
    Friend WithEvents WeekDayEnd As DateTimePicker
    Friend WithEvents WeekEndsEnd As DateTimePicker
    Friend WithEvents Label18 As Label
    Friend WithEvents Update As Button
    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As ToolStripStatusLabel
    Friend WithEvents Button1 As Button
    Friend WithEvents Label5 As Label
    Friend WithEvents DefaultFlow As TextBox
    Friend WithEvents MaxRun As TextBox
    Friend WithEvents Label3 As Label
End Class

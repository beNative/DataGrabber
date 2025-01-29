object frmSettingsDialog: TfrmSettingsDialog
  Left = 0
  Top = 0
  ActiveControl = pgcMain
  Caption = 'Settings'
  ClientHeight = 437
  ClientWidth = 709
  Color = clBtnFace
  Constraints.MinHeight = 317
  Constraints.MinWidth = 493
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Enabled = True
  KeyPreview = True
  Position = poMainFormCenter
  ShowHint = True
  DesignSize = (
    709
    437)
  TextHeight = 13
  object pgcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 703
    Height = 392
    ActivePage = tsConnectionProfiles
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = imlMain
    TabOrder = 0
    ExplicitWidth = 697
    ExplicitHeight = 375
    object tsConnectionProfiles: TTabSheet
      Caption = 'Connection &profiles'
      ImageIndex = -1
      object splVertical: TSplitter
        Left = 161
        Top = 0
        Width = 7
        Height = 363
        Color = clBtnHighlight
        ParentColor = False
        ExplicitHeight = 367
      end
      object pnlConnectionProfileDetail: TPanel
        Left = 168
        Top = 0
        Width = 527
        Height = 363
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 521
        ExplicitHeight = 346
        object pgcConnectionProfile: TPageControl
          Left = 0
          Top = 0
          Width = 527
          Height = 363
          ActivePage = tsBasic
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 521
          ExplicitHeight = 346
          object tsBasic: TTabSheet
            Caption = '&Basic'
            DesignSize = (
              519
              335)
            object grpClientSettings: TGroupBox
              Left = 3
              Top = 199
              Width = 507
              Height = 135
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Client settings'
              TabOrder = 2
              TabStop = True
              ExplicitWidth = 501
              DesignSize = (
                507
                135)
              object lblPacketrecords: TLabel
                Left = 139
                Top = 21
                Width = 77
                Height = 13
                Caption = 'Packet &records:'
                FocusControl = edtPacketRecords
              end
              object edtPacketRecords: TEdit
                Left = 220
                Top = 17
                Width = 58
                Height = 23
                Alignment = taCenter
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                Text = '100'
                StyleElements = [seFont]
                OnChange = edtPacketRecordsChange
              end
              object chkFetchOnDemand: TCheckBox
                Left = 10
                Top = 20
                Width = 111
                Height = 17
                Caption = '&Fetch on demand'
                Checked = True
                DoubleBuffered = False
                ParentDoubleBuffered = False
                State = cbChecked
                TabOrder = 1
                OnClick = chkFetchOnDemandClick
              end
              object chkAutoReconnect: TCheckBox
                Left = 10
                Top = 40
                Width = 391
                Height = 17
                Hint = 
                  'When enabled, the automatic connection recovery will detect when' +
                  ' a '#13#10'connection has been lost and will try to recover from this ' +
                  'situation.'
                Caption = 'Automatically restore database connection after connection loss.'
                TabOrder = 2
                OnClick = chkAutoReconnectClick
              end
              object chkMultipleResultSets: TCheckBox
                Left = 10
                Top = 61
                Width = 425
                Height = 17
                Caption = 
                  'Enable support for multiple resultsets (all fetched data will be' +
                  ' readonly) '
                TabOrder = 3
                OnClick = chkMultipleResultSetsClick
              end
              object chkReadOnlyResultSets: TCheckBox
                Left = 10
                Top = 82
                Width = 393
                Height = 17
                Caption = 'Readonly resultsets'
                TabOrder = 4
              end
              object chkDisconnectedMode: TCheckBox
                Left = 10
                Top = 103
                Width = 137
                Height = 19
                Caption = 'Disconnected mode'
                TabOrder = 5
                OnClick = chkDisconnectedModeClick
              end
              object btnTestConnection: TButton
                Left = 393
                Top = 14
                Width = 103
                Height = 44
                Action = actTestConnection
                Anchors = [akTop, akRight]
                TabOrder = 6
                WordWrap = True
                ExplicitLeft = 387
              end
            end
            object grpConnectionSettings: TGroupBox
              Left = 3
              Top = 39
              Width = 508
              Height = 154
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Database connection &settings'
              TabOrder = 1
              TabStop = True
              ExplicitWidth = 502
              DesignSize = (
                508
                154)
              object lblDriverID: TLabel
                Left = 12
                Top = 21
                Width = 47
                Height = 13
                Caption = '&Driver ID:'
                FocusControl = cbxDrivers
              end
              object lblDatabase: TLabel
                Left = 12
                Top = 47
                Width = 51
                Height = 13
                Caption = 'Data&base:'
                FocusControl = edtDatabase
              end
              object lblCatalog: TLabel
                Left = 12
                Top = 75
                Width = 43
                Height = 13
                Caption = '&Catalog:'
                FocusControl = edtCatalog
              end
              object lblConnectionDefinitionName: TLabel
                Left = 175
                Top = 21
                Width = 117
                Height = 13
                Caption = 'Connection definition:'
                FocusControl = cbxConnectionDefs
                OnDblClick = lblConnectionDefinitionNameDblClick
              end
              object cbxDrivers: TComboBox
                Left = 76
                Top = 17
                Width = 93
                Height = 23
                DropDownCount = 30
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 0
                StyleElements = [seFont]
                OnChange = cbxDriversChange
              end
              object edtDatabase: TButtonedEdit
                Left = 76
                Top = 44
                Width = 424
                Height = 23
                Hint = 'Database (server or path).'
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = []
                ParentFont = False
                RightButton.ImageIndex = 10
                RightButton.Visible = True
                TabOrder = 1
                StyleElements = [seFont]
                OnChange = edtDatabaseChange
                OnRightButtonClick = edtDatabaseRightButtonClick
                ExplicitWidth = 418
              end
              object edtCatalog: TButtonedEdit
                Left = 76
                Top = 72
                Width = 424
                Height = 23
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = []
                ParentFont = False
                RightButton.Enabled = False
                RightButton.ImageIndex = 10
                TabOrder = 2
                StyleElements = [seFont]
                OnChange = edtCatalogChange
                ExplicitWidth = 418
              end
              object grpDBMSUserLogin: TGroupBox
                Left = 8
                Top = 99
                Width = 492
                Height = 47
                Anchors = [akLeft, akTop, akRight]
                Caption = 'DBMS user &login'
                TabOrder = 3
                ExplicitWidth = 486
                DesignSize = (
                  492
                  47)
                object chkOSAuthent: TCheckBox
                  Left = 13
                  Top = 21
                  Width = 130
                  Height = 17
                  Hint = 
                    'Enable this use Windows authentication. If not checked, DBMS aut' +
                    'hentication is used.'
                  Caption = 'Use &OS authentication'
                  TabOrder = 0
                end
                object pnlLogin: TGridPanel
                  Left = 155
                  Top = 17
                  Width = 333
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  BevelOuter = bvNone
                  Caption = 'pnlLogin'
                  Color = clWindow
                  ColumnCollection = <
                    item
                      SizeStyle = ssAbsolute
                      Value = 65.000000000000000000
                    end
                    item
                      Value = 54.248366013071880000
                    end
                    item
                      SizeStyle = ssAbsolute
                      Value = 60.000000000000000000
                    end
                    item
                      Value = 45.751633986928120000
                    end>
                  ControlCollection = <
                    item
                      Column = 1
                      Control = edtUserName
                      Row = 0
                    end
                    item
                      Column = 2
                      Control = lblPassword
                      Row = 0
                    end
                    item
                      Column = 3
                      Control = edtPassword
                      Row = 0
                    end
                    item
                      Column = 0
                      Control = lblUserName
                      Row = 0
                    end>
                  RowCollection = <
                    item
                      Value = 100.000000000000000000
                    end
                    item
                      SizeStyle = ssAuto
                    end>
                  ShowCaption = False
                  TabOrder = 1
                  ExplicitWidth = 327
                  object edtUserName: TEdit
                    AlignWithMargins = True
                    Left = 66
                    Top = 1
                    Width = 111
                    Height = 21
                    Hint = 'The DBMS server login name.'
                    Margins.Left = 1
                    Margins.Top = 1
                    Margins.Right = 1
                    Margins.Bottom = 1
                    Align = alClient
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -12
                    Font.Name = 'Segoe UI'
                    Font.Style = []
                    ParentFont = False
                    TabOrder = 0
                    StyleElements = [seFont]
                    OnChange = edtUserNameChange
                    ExplicitHeight = 23
                  end
                  object lblPassword: TLabel
                    AlignWithMargins = True
                    Left = 181
                    Top = 3
                    Width = 54
                    Height = 18
                    Margins.Bottom = 2
                    Align = alClient
                    AutoSize = False
                    Caption = '&Password:'
                    Layout = tlCenter
                    ExplicitLeft = 183
                    ExplicitTop = 2
                    ExplicitWidth = 56
                    ExplicitHeight = 19
                  end
                  object edtPassword: TEdit
                    AlignWithMargins = True
                    Left = 239
                    Top = 1
                    Width = 93
                    Height = 21
                    Hint = 'The DBMS server login password.'
                    Margins.Left = 1
                    Margins.Top = 1
                    Margins.Right = 1
                    Margins.Bottom = 1
                    Align = alClient
                    Font.Charset = DEFAULT_CHARSET
                    Font.Color = clWindowText
                    Font.Height = -12
                    Font.Name = 'Segoe UI'
                    Font.Style = []
                    ParentFont = False
                    TabOrder = 1
                    StyleElements = [seFont]
                    OnChange = edtPasswordChange
                    ExplicitLeft = 236
                    ExplicitWidth = 90
                    ExplicitHeight = 23
                  end
                  object lblUserName: TLabel
                    Left = 0
                    Top = 0
                    Width = 65
                    Height = 23
                    Align = alClient
                    Caption = '&Username:'
                    Layout = tlCenter
                    ExplicitWidth = 54
                    ExplicitHeight = 13
                  end
                end
              end
              object cbxConnectionDefs: TComboBox
                Left = 296
                Top = 17
                Width = 204
                Height = 23
                Hint = 'Connection definitions are defined in FDConnectionDefs.ini.'
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 30
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = []
                ParentFont = False
                TabOrder = 4
                StyleElements = [seFont]
                OnChange = cbxConnectionDefsChange
                OnDropDown = cbxConnectionDefsDropDown
                ExplicitWidth = 198
              end
            end
            object grpProfileSettings: TGroupBox
              Left = 3
              Top = 3
              Width = 507
              Height = 36
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              TabStop = True
              ExplicitWidth = 501
              DesignSize = (
                507
                36)
              object lblProfileColor: TLabel
                Left = 388
                Top = 11
                Width = 65
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Profile c&olor:'
                FocusControl = btnProfileColor
                ExplicitLeft = 393
              end
              object btnProfileColor: TKColorButton
                Left = 460
                Top = 5
                Width = 40
                Height = 25
                Anchors = [akTop, akRight]
                FocusRect = False
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBtnText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = btnProfileColorClick
                ColorDlgOptions = [cdAnyColor]
                ExplicitLeft = 454
              end
              object edtProfileName: TLabeledEdit
                Left = 76
                Top = 7
                Width = 206
                Height = 23
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 67
                EditLabel.Height = 23
                EditLabel.Caption = 'Profile &name:'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Segoe UI'
                Font.Style = [fsBold]
                LabelPosition = lpLeft
                ParentFont = False
                TabOrder = 0
                Text = ''
                StyleElements = [seFont]
                OnChange = edtProfileNameChange
                ExplicitWidth = 200
              end
              object chkSetAsDefault: TCheckBox
                Left = 288
                Top = 10
                Width = 94
                Height = 17
                Hint = 
                  'Make this connection profile the default one when the applicatio' +
                  'n is started.'
                Anchors = [akTop, akRight]
                Caption = 'Set as d&efault'
                TabOrder = 2
                OnClick = chkSetAsDefaultClick
                ExplicitLeft = 282
              end
            end
          end
          object tsAdvanced: TTabSheet
            Caption = '&Advanced'
            ImageIndex = 1
            OnExit = tsAdvancedExit
            OnShow = tsAdvancedShow
          end
        end
      end
      object pnlConnectionProfilesList: TPanel
        Left = 0
        Top = 0
        Width = 161
        Height = 363
        Align = alLeft
        BevelOuter = bvNone
        Constraints.MinWidth = 137
        TabOrder = 1
        ExplicitHeight = 346
        object tlbConnectionProfiles: TToolBar
          Left = 0
          Top = 0
          Width = 161
          Height = 25
          Caption = 'tlbConnectionProfiles'
          Images = imlMain
          TabOrder = 0
          object btnAdd: TToolButton
            Left = 0
            Top = 0
            Action = actAdd
          end
          object btnDelete: TToolButton
            Left = 23
            Top = 0
            Action = actDelete
          end
          object btnSpacer1: TToolButton
            Left = 46
            Top = 0
            Width = 10
            ImageIndex = 8
            ImageName = 'border-vertical'
            Style = tbsSeparator
          end
          object btnDuplicate: TToolButton
            Left = 56
            Top = 0
            Action = actDuplicate
          end
          object btnSpacer2: TToolButton
            Left = 79
            Top = 0
            Width = 10
            ImageIndex = 8
            ImageName = 'border-vertical'
            Style = tbsSeparator
          end
          object btnMoveUp: TToolButton
            Left = 89
            Top = 0
            Action = actMoveUp
          end
          object btnMoveDown: TToolButton
            Left = 112
            Top = 0
            Action = actMoveDown
          end
        end
      end
    end
    object tsDisplay: TTabSheet
      Caption = '&Display settings'
      ImageIndex = -1
      DesignSize = (
        695
        363)
      object grpCellBackgroundColoring: TGroupBox
        Left = 2
        Top = 3
        Width = 127
        Height = 479
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Grid cell colors'
        TabOrder = 1
        object chkGridCellColoringEnabled: TCheckBox
          Left = 12
          Top = 20
          Width = 103
          Height = 31
          Caption = 'Enable grid cell coloring'
          Checked = True
          State = cbChecked
          TabOrder = 0
          WordWrap = True
          OnClick = chkGridCellColoringEnabledClick
        end
        object pnlGridTypeColoring: TGridPanel
          Left = 12
          Top = 57
          Width = 109
          Height = 257
          BevelOuter = bvNone
          ColumnCollection = <
            item
              SizeStyle = ssAbsolute
              Value = 60.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 46.000000000000000000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = lblIntegers
              Row = 0
            end
            item
              Column = 1
              Control = btnIntegerColor
              Row = 0
            end
            item
              Column = 0
              Control = lblFloats
              Row = 1
            end
            item
              Column = 1
              Control = btnFloatColor
              Row = 1
            end
            item
              Column = 0
              Control = lblString
              Row = 2
            end
            item
              Column = 1
              Control = btnStringColor
              Row = 2
            end
            item
              Column = 0
              Control = lblMemo
              Row = 3
            end
            item
              Column = 1
              Control = btnMemoColor
              Row = 3
            end
            item
              Column = 0
              Control = lblDates
              Row = 4
            end
            item
              Column = 1
              Control = btnDateColor
              Row = 4
            end
            item
              Column = 0
              Control = lblTimes
              Row = 5
            end
            item
              Column = 1
              Control = btnTimeColor
              Row = 5
            end
            item
              Column = 0
              Control = lblDateTimes
              Row = 6
            end
            item
              Column = 1
              Control = btnDateTimeColor
              Row = 6
            end
            item
              Column = 0
              Control = lblNULL
              Row = 7
            end
            item
              Column = 1
              Control = btnNullColor
              Row = 7
            end
            item
              Column = 0
              Control = lblBoolean
              Row = 8
            end
            item
              Column = 1
              Control = btnBooleanColor
              Row = 8
            end>
          RowCollection = <
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 28.000000000000000000
            end>
          ShowCaption = False
          TabOrder = 1
          DesignSize = (
            109
            257)
          object lblIntegers: TLabel
            Left = 0
            Top = 0
            Width = 60
            Height = 28
            Align = alClient
            Caption = 'Integer'
            FocusControl = btnIntegerColor
            Layout = tlCenter
            ExplicitWidth = 37
            ExplicitHeight = 13
          end
          object btnIntegerColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 3
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 0
            ColorDlgOptions = []
          end
          object lblFloats: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 31
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'Float'
            FocusControl = btnFloatColor
            Layout = tlCenter
            ExplicitWidth = 26
            ExplicitHeight = 13
          end
          object btnFloatColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 31
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 1
            ColorDlgOptions = []
          end
          object lblString: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 59
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'String'
            FocusControl = btnStringColor
            Layout = tlCenter
            ExplicitWidth = 31
            ExplicitHeight = 13
          end
          object btnStringColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 59
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 2
            ColorDlgOptions = []
          end
          object lblMemo: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 87
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'Memo'
            FocusControl = btnMemoColor
            Layout = tlCenter
            ExplicitWidth = 32
            ExplicitHeight = 13
          end
          object btnMemoColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 87
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 3
            ColorDlgOptions = []
          end
          object lblDates: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 115
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'Date'
            FocusControl = btnDateColor
            Layout = tlCenter
            ExplicitWidth = 24
            ExplicitHeight = 13
          end
          object btnDateColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 115
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 4
            ColorDlgOptions = []
          end
          object lblTimes: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 143
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'Time'
            FocusControl = btnTimeColor
            Layout = tlCenter
            ExplicitWidth = 23
            ExplicitHeight = 13
          end
          object btnTimeColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 143
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 5
            ColorDlgOptions = []
          end
          object lblDateTimes: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 171
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'DateTime'
            FocusControl = btnDateTimeColor
            Layout = tlCenter
            ExplicitWidth = 47
            ExplicitHeight = 13
          end
          object btnDateTimeColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 171
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 6
            ColorDlgOptions = []
          end
          object lblNULL: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 199
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'NULL'
            FocusControl = btnNullColor
            Layout = tlCenter
            ExplicitWidth = 26
            ExplicitHeight = 13
          end
          object btnNullColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 199
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 7
            ColorDlgOptions = []
          end
          object lblBoolean: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 227
            Width = 54
            Height = 22
            Align = alClient
            Caption = 'Boolean'
            FocusControl = btnBooleanColor
            Layout = tlCenter
            ExplicitWidth = 43
            ExplicitHeight = 13
          end
          object btnBooleanColor: TKColorButton
            AlignWithMargins = True
            Left = 63
            Top = 227
            Width = 40
            Height = 22
            Anchors = []
            FocusRect = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            IsCheck = True
            ParentFont = False
            TabOrder = 8
            ColorDlgOptions = []
          end
        end
      end
      object rgpGridTypes: TRadioGroup
        Left = 387
        Top = 3
        Width = 296
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Grid type'
        Columns = 3
        TabOrder = 0
      end
      object grpGridLines: TGroupBox
        Left = 135
        Top = 3
        Width = 90
        Height = 114
        Caption = 'Gridlines'
        TabOrder = 2
        object tlbGridlines: TToolBar
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 80
          Height = 91
          Align = alClient
          ButtonWidth = 81
          Images = imlMain
          List = True
          ShowCaptions = True
          TabOrder = 0
          object btnGridlinesNone: TToolButton
            Left = 0
            Top = 0
            Action = actGridlinesNone
            Wrap = True
          end
          object btnGridlinesVertical: TToolButton
            Left = 0
            Top = 22
            Action = actGridlinesVertical
            Wrap = True
          end
          object btnGridlinesHorizontal: TToolButton
            Left = 0
            Top = 44
            Action = actGridlinesHorizontal
            Wrap = True
          end
          object btnGridlinesAll: TToolButton
            Left = 0
            Top = 66
            Action = actGridlinesBoth
          end
        end
      end
      object grpResultSetDisplay: TGroupBox
        Left = 231
        Top = 3
        Width = 147
        Height = 114
        Caption = 'Display multiple resultsets'
        TabOrder = 3
        object tlbDisplayMultipleResultSets: TToolBar
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 137
          Height = 91
          Align = alClient
          ButtonWidth = 130
          Images = imlMain
          List = True
          ShowCaptions = True
          TabOrder = 0
          object btnMRSAsMultipleTabs: TToolButton
            Left = 0
            Top = 0
            Action = actMRSAsMultipleTabs
            Wrap = True
          end
          object btnMRSHorizontally: TToolButton
            Left = 0
            Top = 22
            Action = actMRSVertically
            Wrap = True
          end
          object btnMRSVertically: TToolButton
            Left = 0
            Top = 44
            Action = actMRSHorizontally
            Wrap = True
          end
        end
      end
      object grpEditorSettings: TGroupBox
        Left = 387
        Top = 57
        Width = 296
        Height = 258
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Editor settings'
        TabOrder = 4
        object lblEditorFont: TLabel
          Left = 19
          Top = 22
          Width = 81
          Height = 13
          Caption = 'SQL editor font:'
          FocusControl = edtEditorFont
        end
        object edtEditorFont: TButtonedEdit
          Left = 112
          Top = 19
          Width = 153
          Height = 21
          ReadOnly = True
          RightButton.ImageIndex = 23
          RightButton.Visible = True
          TabOrder = 0
          Text = 'Consolas'
          OnRightButtonClick = edtEditorFontRightButtonClick
        end
      end
      object grpGridFont: TGroupBox
        Left = 136
        Top = 122
        Width = 243
        Height = 239
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Grid font settings'
        TabOrder = 5
        object lblGridFont: TLabel
          Left = 12
          Top = 27
          Width = 50
          Height = 13
          Caption = 'Grid font:'
          FocusControl = edtGridFont
        end
        object edtGridFont: TButtonedEdit
          Left = 68
          Top = 24
          Width = 162
          Height = 21
          ReadOnly = True
          RightButton.ImageIndex = 23
          RightButton.Visible = True
          TabOrder = 0
          Text = 'Calibri'
          OnRightButtonClick = edtGridFontRightButtonClick
        end
      end
      object grpLogging: TGroupBox
        Left = 387
        Top = 319
        Width = 296
        Height = 40
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Logging'
        TabOrder = 6
        object chkEmitLogMessages: TCheckBox
          Left = 16
          Top = 17
          Width = 121
          Height = 17
          Caption = 'Emit log messages'
          TabOrder = 0
        end
      end
    end
    object tsSettings: TTabSheet
      AlignWithMargins = True
      Caption = 'Settings files'
      ImageIndex = -1
      OnEnter = tsSettingsEnter
      DesignSize = (
        689
        357)
      object pgcSettingsFiles: TPageControl
        Left = 0
        Top = 0
        Width = 689
        Height = 357
        ActivePage = tsDataGrabberSettings
        Align = alClient
        TabOrder = 0
        object tsDataGrabberSettings: TTabSheet
          Caption = 'Settings.json'
          object seSettings: TSynEdit
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 675
            Height = 323
            Align = alClient
            ActiveLineColor = clYellow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            TabOrder = 0
            CodeFolding.GutterShapeSize = 11
            CodeFolding.CollapsedLineColor = clGrayText
            CodeFolding.FolderBarLinesColor = clGrayText
            CodeFolding.IndentGuidesColor = clGray
            CodeFolding.IndentGuides = True
            CodeFolding.ShowCollapsedLine = False
            CodeFolding.ShowHintMark = True
            UseCodeFolding = False
            BookMarkOptions.LeftMargin = 0
            BookMarkOptions.XOffset = 0
            BorderStyle = bsNone
            Gutter.AutoSize = True
            Gutter.Color = cl3DLight
            Gutter.Font.Charset = ANSI_CHARSET
            Gutter.Font.Color = clSilver
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Consolas'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 0
            Gutter.RightOffset = 17
            Gutter.ShowLineNumbers = True
            Gutter.Width = 15
            Gutter.Gradient = True
            Gutter.GradientStartColor = clWhite
            Gutter.GradientEndColor = clWhite
            Highlighter = synJScript
            Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
            ReadOnly = True
            RightEdgeColor = cl3DLight
            TabWidth = 2
            OnChange = seSettingsChange
            FontSmoothing = fsmClearType
          end
        end
        object tsFDConnectionDefs: TTabSheet
          Caption = 'FDConnectionDefs.ini'
          ImageIndex = 1
          object seFDConnectionDefs: TSynEdit
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 675
            Height = 323
            Align = alClient
            ActiveLineColor = clYellow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            TabOrder = 0
            OnExit = seFDConnectionDefsExit
            CodeFolding.GutterShapeSize = 11
            CodeFolding.CollapsedLineColor = clGrayText
            CodeFolding.FolderBarLinesColor = clGrayText
            CodeFolding.IndentGuidesColor = clGray
            CodeFolding.IndentGuides = True
            CodeFolding.ShowCollapsedLine = False
            CodeFolding.ShowHintMark = True
            UseCodeFolding = False
            BookMarkOptions.LeftMargin = 0
            BookMarkOptions.XOffset = 0
            BorderStyle = bsNone
            Gutter.AutoSize = True
            Gutter.Color = cl3DLight
            Gutter.Font.Charset = ANSI_CHARSET
            Gutter.Font.Color = clSilver
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Consolas'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 0
            Gutter.RightOffset = 0
            Gutter.ShowLineNumbers = True
            Gutter.Width = 15
            Gutter.Gradient = True
            Gutter.GradientStartColor = clWhite
            Gutter.GradientEndColor = clWhite
            Highlighter = synIni
            Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
            RightEdgeColor = cl3DLight
            TabWidth = 2
            OnChange = seFDConnectionDefsChange
            FontSmoothing = fsmClearType
          end
        end
      end
      object tlbSettingsFiles: TToolBar
        Left = 621
        Top = -1
        Width = 72
        Height = 29
        Align = alNone
        Anchors = [akTop, akRight]
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = imlMain
        List = True
        TabOrder = 1
        object btnSaveFile: TToolButton
          Left = 0
          Top = 0
          Action = actSaveFile
        end
        object btnRefreshFile: TToolButton
          Left = 23
          Top = 0
          Action = actRefreshFile
        end
        object btnEditConnectionDef: TToolButton
          Left = 46
          Top = 0
          Action = actEditConnectionDef
        end
      end
    end
    object tsConnectionDefinitions: TTabSheet
      Caption = 'Connection definitions'
      ImageIndex = -1
      TabVisible = False
    end
  end
  object btnApply: TButton
    Left = 242
    Top = 401
    Width = 150
    Height = 26
    Action = actApply
    Anchors = [akRight, akBottom]
    ImageIndex = 11
    ImageName = 'check'
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    TabOrder = 1
    ExplicitLeft = 236
    ExplicitTop = 384
  end
  object btnClose: TButton
    Left = 554
    Top = 401
    Width = 150
    Height = 26
    Action = actClose
    Anchors = [akRight, akBottom]
    Default = True
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    ModalResult = 11
    TabOrder = 3
    ExplicitLeft = 548
    ExplicitTop = 384
  end
  object btnCancel: TButton
    Left = 398
    Top = 401
    Width = 150
    Height = 26
    Action = actCancel
    Anchors = [akRight, akBottom]
    DisabledImageIndex = 1
    DisabledImageName = 'arrow-big-down'
    HotImageIndex = 1
    HotImageName = 'arrow-big-down'
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 392
    ExplicitTop = 384
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 664
    Top = 320
    object actApply: TAction
      Caption = '&Apply'
      Hint = 'Apply changes'
      ImageIndex = 10
      ImageName = 'diskette-save-svgrepo-com'
      OnExecute = actApplyExecute
    end
    object actClose: TAction
      Caption = 'Clos&e'
      Hint = 'Save changes and close'
      ImageIndex = 12
      ImageName = 'square-x'
      OnExecute = actCloseExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      Hint = 'Cancel all changes and close'
      ImageIndex = 13
      ImageName = 'x'
      OnExecute = actCancelExecute
    end
    object actAdd: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 4
      ImageName = 'plus'
      ShortCut = 16429
      OnExecute = actAddExecute
    end
    object actDelete: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 3
      ImageName = 'minus'
      ShortCut = 16430
      OnExecute = actDeleteExecute
    end
    object actMoveUp: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Up'
      Hint = 'Move up'
      ImageIndex = 2
      ImageName = 'arrow-big-up'
      ShortCut = 16422
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Down'
      Hint = 'Move down'
      ImageIndex = 1
      ImageName = 'arrow-big-down'
      ShortCut = 16424
      OnExecute = actMoveDownExecute
    end
    object actDuplicate: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Duplicate'
      Hint = 'Duplicate'
      ImageIndex = 0
      ImageName = 'copy'
      OnExecute = actDuplicateExecute
    end
    object actEditConnectionDef: TAction
      Category = 'ConnectionDef'
      Caption = 'Open the FireDAC connection editor.'
      ImageIndex = 11
      ImageName = 'check'
      OnExecute = actEditConnectionDefExecute
    end
    object actGridlinesBoth: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Both'
      Checked = True
      GroupIndex = 1
      ImageIndex = 9
      ImageName = 'border-all'
      OnExecute = actGridlinesBothExecute
    end
    object actGridlinesHorizontal: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Horizontal'
      GroupIndex = 1
      ImageIndex = 6
      ImageName = 'border-horizontal'
      OnExecute = actGridlinesHorizontalExecute
    end
    object actGridlinesVertical: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Vertical'
      GroupIndex = 1
      ImageIndex = 8
      ImageName = 'border-vertical'
      OnExecute = actGridlinesVerticalExecute
    end
    object actGridlinesNone: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'None'
      GroupIndex = 1
      ImageIndex = 7
      ImageName = 'border-none'
      OnExecute = actGridlinesNoneExecute
    end
    object actTestConnection: TAction
      Caption = 'Test database connection'
      ImageIndex = 24
      OnExecute = actTestConnectionExecute
    end
    object actOpenSettingsFileLocation: TAction
      Caption = 'Open settings file location'
      OnExecute = actOpenSettingsFileLocationExecute
    end
    object actMRSAsMultipleTabs: TAction
      Category = 'DisplayMultipleResultsets'
      AutoCheck = True
      Caption = 'as multiple tabs'
      GroupIndex = 2
      ImageIndex = 21
      OnExecute = actMRSAsMultipleTabsExecute
    end
    object actMRSVertically: TAction
      Category = 'DisplayMultipleResultsets'
      AutoCheck = True
      Caption = 'stacked vertically'
      GroupIndex = 2
      ImageIndex = 20
      OnExecute = actMRSVerticallyExecute
    end
    object actMRSHorizontally: TAction
      Category = 'DisplayMultipleResultsets'
      AutoCheck = True
      Caption = 'stacked horizontally'
      GroupIndex = 2
      ImageIndex = 19
      OnExecute = actMRSHorizontallyExecute
    end
    object actSaveFile: TAction
      Caption = 'Save'
      Hint = 'Save file to disk'
      ImageIndex = 10
      ImageName = 'diskette-save-svgrepo-com'
      OnExecute = actSaveFileExecute
    end
    object actRefreshFile: TAction
      Caption = 'Refresh'
      Hint = 'Reload file from disk'
      ImageIndex = 5
      ImageName = 'refresh'
      OnExecute = actRefreshFileExecute
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 412
    Top = 320
  end
  object synJScript: TSynJScriptSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    CommentAttri.Style = []
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    Left = 851
    Top = 196
  end
  object synIni: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    CommentAttri.Style = []
    SectionAttri.Foreground = clMaroon
    SectionAttri.Style = [fsBold, fsUnderline]
    KeyAttri.Foreground = clBlue
    KeyAttri.Style = [fsBold]
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    Left = 854
    Top = 315
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 986
    Top = 190
  end
  object conTest: TFDConnection
    LoginPrompt = False
    OnError = conTestError
    Left = 994
    Top = 311
  end
  object imlMain: TVirtualImageList
    DisabledGrayscale = True
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'copy'
        Name = 'copy'
      end
      item
        CollectionIndex = 6
        CollectionName = 'arrow-big-down'
        Name = 'arrow-big-down'
      end
      item
        CollectionIndex = 7
        CollectionName = 'arrow-big-up'
        Name = 'arrow-big-up'
      end
      item
        CollectionIndex = 8
        CollectionName = 'minus'
        Name = 'minus'
      end
      item
        CollectionIndex = 9
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 10
        CollectionName = 'refresh'
        Name = 'refresh'
      end
      item
        CollectionIndex = 12
        CollectionName = 'border-horizontal'
        Name = 'border-horizontal'
      end
      item
        CollectionIndex = 13
        CollectionName = 'border-none'
        Name = 'border-none'
      end
      item
        CollectionIndex = 14
        CollectionName = 'border-vertical'
        Name = 'border-vertical'
      end
      item
        CollectionIndex = 11
        CollectionName = 'border-all'
        Name = 'border-all'
      end
      item
        CollectionIndex = 15
        CollectionName = 'diskette-save-svgrepo-com'
        Name = 'diskette-save-svgrepo-com'
      end
      item
        CollectionIndex = 16
        CollectionName = 'check'
        Name = 'check'
      end
      item
        CollectionIndex = 17
        CollectionName = 'square-x'
        Name = 'square-x'
      end
      item
        CollectionIndex = 18
        CollectionName = 'x'
        Name = 'x'
      end>
    ImageCollection = dmAssets.imcMain
    Left = 513
    Top = 327
  end
end

object frmSettingsDialog: TfrmSettingsDialog
  Left = 0
  Top = 0
  ActiveControl = pgcMain
  Caption = 'Settings'
  ClientHeight = 437
  ClientWidth = 684
  Color = clBtnFace
  Constraints.MinHeight = 476
  Constraints.MinWidth = 700
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  DesignSize = (
    684
    437)
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 678
    Height = 394
    ActivePage = tsConnectionProfiles
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = imlMain
    TabOrder = 0
    object tsConnectionProfiles: TTabSheet
      Caption = 'Connection &profiles'
      ImageIndex = 9
      object splVertical: TSplitter
        Left = 161
        Top = 0
        Width = 7
        Height = 365
        Color = clBtnHighlight
        ParentColor = False
        ExplicitLeft = 185
        ExplicitHeight = 377
      end
      object pnlConnectionProfileDetail: TPanel
        Left = 168
        Top = 0
        Width = 502
        Height = 365
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pgcConnectionProfile: TPageControl
          Left = 0
          Top = 0
          Width = 502
          Height = 365
          ActivePage = tsBasic
          Align = alClient
          TabOrder = 0
          object tsBasic: TTabSheet
            Caption = '&Basic'
            DesignSize = (
              494
              337)
            object grpClientSettings: TGroupBox
              Left = 3
              Top = 199
              Width = 488
              Height = 135
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Client settings'
              TabOrder = 2
              TabStop = True
              object lblPacketrecords: TLabel
                Left = 139
                Top = 23
                Width = 75
                Height = 13
                Caption = 'Packet &records:'
                FocusControl = edtPacketRecords
              end
              object edtPacketRecords: TEdit
                Left = 220
                Top = 17
                Width = 58
                Height = 21
                Alignment = taCenter
                TabOrder = 0
                Text = '100'
                OnChange = edtPacketRecordsChange
              end
              object chkFetchOnDemand: TCheckBox
                Left = 10
                Top = 21
                Width = 100
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
                Top = 39
                Width = 337
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
                Top = 58
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
                Top = 77
                Width = 393
                Height = 17
                Caption = 'Readonly resultsets'
                TabOrder = 4
              end
              object chkDisconnectedMode: TCheckBox
                Left = 10
                Top = 95
                Width = 137
                Height = 19
                Caption = 'Disconnected mode'
                TabOrder = 5
                OnClick = chkDisconnectedModeClick
              end
              object btnTestConnection: TButton
                Left = 335
                Top = 15
                Width = 150
                Height = 25
                Action = actTestConnection
                Images = imlMain
                TabOrder = 6
              end
            end
            object grpConnectionSettings: TGroupBox
              Left = 3
              Top = 39
              Width = 489
              Height = 154
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Database connection &settings'
              TabOrder = 1
              TabStop = True
              DesignSize = (
                489
                154)
              object lblDriverID: TLabel
                Left = 12
                Top = 20
                Width = 47
                Height = 13
                Caption = '&Driver ID:'
                FocusControl = cbxDrivers
              end
              object lblDatabase: TLabel
                Left = 12
                Top = 47
                Width = 50
                Height = 13
                Caption = 'Data&base:'
                FocusControl = edtDatabase
              end
              object lblCatalog: TLabel
                Left = 12
                Top = 75
                Width = 41
                Height = 13
                Caption = '&Catalog:'
                FocusControl = edtCatalog
              end
              object lblConnectionDefinitionName: TLabel
                Left = 175
                Top = 20
                Width = 105
                Height = 13
                Caption = 'Connection definition:'
                FocusControl = cbxConnectionDefs
                OnDblClick = lblConnectionDefinitionNameDblClick
              end
              object cbxDrivers: TComboBox
                Left = 76
                Top = 17
                Width = 93
                Height = 21
                DropDownCount = 30
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 0
                OnChange = cbxDriversChange
              end
              object edtDatabase: TButtonedEdit
                Left = 76
                Top = 44
                Width = 405
                Height = 21
                Hint = 'Database (server or path).'
                Anchors = [akLeft, akTop, akRight]
                Images = imlMain
                RightButton.ImageIndex = 10
                RightButton.Visible = True
                TabOrder = 1
                OnChange = edtDatabaseChange
                OnRightButtonClick = edtDatabaseRightButtonClick
              end
              object edtCatalog: TButtonedEdit
                Left = 76
                Top = 72
                Width = 405
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Images = imlMain
                RightButton.Enabled = False
                RightButton.ImageIndex = 10
                TabOrder = 2
                OnChange = edtCatalogChange
              end
              object grpDBMSUserLogin: TGroupBox
                Left = 8
                Top = 99
                Width = 474
                Height = 47
                Anchors = [akLeft, akTop, akRight]
                Caption = 'DBMS user &login'
                TabOrder = 3
                DesignSize = (
                  474
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
                  Width = 314
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
                      Value = 54.248366013071890000
                    end
                    item
                      SizeStyle = ssAbsolute
                      Value = 60.000000000000000000
                    end
                    item
                      Value = 45.751633986928110000
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
                  ParentBackground = False
                  RowCollection = <
                    item
                      Value = 100.000000000000000000
                    end
                    item
                      SizeStyle = ssAuto
                    end>
                  ShowCaption = False
                  TabOrder = 1
                  object edtUserName: TEdit
                    AlignWithMargins = True
                    Left = 66
                    Top = 1
                    Width = 100
                    Height = 21
                    Hint = 'The DBMS server login name.'
                    Margins.Left = 1
                    Margins.Top = 1
                    Margins.Right = 1
                    Margins.Bottom = 1
                    Align = alClient
                    TabOrder = 0
                    OnChange = edtUserNameChange
                  end
                  object lblPassword: TLabel
                    AlignWithMargins = True
                    Left = 169
                    Top = 2
                    Width = 56
                    Height = 19
                    Margins.Left = 2
                    Margins.Top = 2
                    Margins.Right = 2
                    Margins.Bottom = 2
                    Align = alClient
                    AutoSize = False
                    Caption = '&Password:'
                    Layout = tlCenter
                    ExplicitLeft = 184
                    ExplicitTop = -1
                    ExplicitWidth = 64
                  end
                  object edtPassword: TEdit
                    AlignWithMargins = True
                    Left = 228
                    Top = 1
                    Width = 85
                    Height = 21
                    Hint = 'The DBMS server login password.'
                    Margins.Left = 1
                    Margins.Top = 1
                    Margins.Right = 1
                    Margins.Bottom = 1
                    Align = alClient
                    TabOrder = 1
                    OnChange = edtPasswordChange
                  end
                  object lblUserName: TLabel
                    Left = 0
                    Top = 0
                    Width = 65
                    Height = 23
                    Align = alClient
                    Caption = '&Username:'
                    Layout = tlCenter
                    ExplicitWidth = 52
                    ExplicitHeight = 13
                  end
                end
              end
              object cbxConnectionDefs: TComboBox
                Left = 286
                Top = 17
                Width = 195
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 30
                TabOrder = 4
                OnChange = cbxConnectionDefsChange
                OnDropDown = cbxConnectionDefsDropDown
              end
            end
            object grpProfileSettings: TGroupBox
              Left = 3
              Top = 3
              Width = 488
              Height = 36
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              TabStop = True
              DesignSize = (
                488
                36)
              object lblProfileColor: TLabel
                Left = 369
                Top = 11
                Width = 60
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Profile c&olor:'
                FocusControl = btnProfileColor
              end
              object btnProfileColor: TKColorButton
                Left = 441
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
              end
              object edtProfileName: TLabeledEdit
                Left = 76
                Top = 7
                Width = 187
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 63
                EditLabel.Height = 13
                EditLabel.Caption = 'Profile &name:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                LabelPosition = lpLeft
                ParentFont = False
                TabOrder = 0
                OnChange = edtProfileNameChange
              end
              object chkSetAsDefault: TCheckBox
                Left = 269
                Top = 9
                Width = 94
                Height = 17
                Hint = 
                  'Make this connection profile the default one when the applicatio' +
                  'n is started.'
                Anchors = [akTop, akRight]
                Caption = 'Set as d&efault'
                TabOrder = 2
                OnClick = chkSetAsDefaultClick
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
        Height = 365
        Align = alLeft
        BevelOuter = bvNone
        Constraints.MinWidth = 137
        TabOrder = 1
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
      ImageIndex = 5
      DesignSize = (
        670
        365)
      object grpCellBackgroundColoring: TGroupBox
        Left = 2
        Top = 3
        Width = 127
        Height = 359
        Caption = 'Grid cell colors'
        TabOrder = 1
        object chkGridCellColoringEnabled: TCheckBox
          Left = 21
          Top = 20
          Width = 82
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
            ExplicitWidth = 36
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
            Height = 25
            Align = alClient
            Caption = 'Float'
            FocusControl = btnFloatColor
            Layout = tlCenter
            ExplicitWidth = 24
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
            Height = 25
            Align = alClient
            Caption = 'String'
            FocusControl = btnStringColor
            Layout = tlCenter
            ExplicitWidth = 28
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
            Height = 25
            Align = alClient
            Caption = 'Memo'
            FocusControl = btnMemoColor
            Layout = tlCenter
            ExplicitWidth = 28
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
            Height = 25
            Align = alClient
            Caption = 'Date'
            FocusControl = btnDateColor
            Layout = tlCenter
            ExplicitWidth = 23
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
            Height = 25
            Align = alClient
            Caption = 'Time'
            FocusControl = btnTimeColor
            Layout = tlCenter
            ExplicitWidth = 22
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
            Height = 25
            Align = alClient
            Caption = 'DateTime'
            FocusControl = btnDateTimeColor
            Layout = tlCenter
            ExplicitWidth = 45
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
            Height = 25
            Align = alClient
            Caption = 'NULL'
            FocusControl = btnNullColor
            Layout = tlCenter
            ExplicitWidth = 24
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
            Height = 25
            Align = alClient
            Caption = 'Boolean'
            FocusControl = btnBooleanColor
            Layout = tlCenter
            ExplicitWidth = 38
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
        Width = 276
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
          ButtonWidth = 75
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
          ButtonWidth = 122
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
            Action = actMRSHorizontally
            Wrap = True
          end
          object btnMRSVertically: TToolButton
            Left = 0
            Top = 44
            Action = actMRSVertically
            Wrap = True
          end
        end
      end
      object grpEditorSettings: TGroupBox
        Left = 387
        Top = 54
        Width = 276
        Height = 308
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Editor settings'
        TabOrder = 4
        object lblEditorFont: TLabel
          Left = 19
          Top = 22
          Width = 77
          Height = 13
          Caption = 'SQL editor font:'
          FocusControl = edtEditorFont
        end
        object edtEditorFont: TButtonedEdit
          Left = 112
          Top = 19
          Width = 153
          Height = 21
          Images = imlMain
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
        Height = 240
        Caption = 'Grid font settings'
        TabOrder = 5
        object lblGridFont: TLabel
          Left = 12
          Top = 27
          Width = 46
          Height = 13
          Caption = 'Grid font:'
          FocusControl = edtGridFont
        end
        object edtGridFont: TButtonedEdit
          Left = 64
          Top = 24
          Width = 162
          Height = 21
          Images = imlMain
          ReadOnly = True
          RightButton.ImageIndex = 23
          RightButton.Visible = True
          TabOrder = 0
          Text = 'Calibri'
          OnRightButtonClick = edtGridFontRightButtonClick
        end
      end
    end
    object tsSettings: TTabSheet
      AlignWithMargins = True
      Caption = 'Settings files'
      ImageIndex = 11
      OnEnter = tsSettingsEnter
      DesignSize = (
        664
        359)
      object pgcSettingsFiles: TPageControl
        Left = 0
        Top = 0
        Width = 664
        Height = 359
        ActivePage = tsDataGrabberSettings
        Align = alClient
        TabOrder = 0
        object tsDataGrabberSettings: TTabSheet
          Caption = 'Settings.json'
          object seSettings: TSynEdit
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 650
            Height = 325
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
            Gutter.ShowModification = True
            Gutter.Width = 15
            Gutter.Gradient = True
            Gutter.GradientStartColor = clWhite
            Gutter.GradientEndColor = clWhite
            Highlighter = synJScript
            Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ReadOnly = True
            RightEdgeColor = cl3DLight
            TabWidth = 2
            WordWrap = True
            OnChange = seSettingsChange
            FontSmoothing = fsmNone
          end
        end
        object tsFDConnectionDefs: TTabSheet
          Caption = 'FDConnectionDefs.ini'
          ImageIndex = 1
          object seFDConnectionDefs: TSynEdit
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 650
            Height = 325
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
            Gutter.ShowModification = True
            Gutter.Width = 15
            Gutter.Gradient = True
            Gutter.GradientStartColor = clWhite
            Gutter.GradientEndColor = clWhite
            Highlighter = synIni
            Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            RightEdgeColor = cl3DLight
            TabWidth = 2
            WordWrap = True
            OnChange = seFDConnectionDefsChange
            FontSmoothing = fsmNone
          end
        end
      end
      object tlbSettingsFiles: TToolBar
        Left = 592
        Top = -1
        Width = 72
        Height = 21
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
    Left = 214
    Top = 403
    Width = 150
    Height = 26
    Action = actApply
    Anchors = [akRight, akBottom]
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    TabOrder = 1
  end
  object btnClose: TButton
    Left = 526
    Top = 403
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
  end
  object btnCancel: TButton
    Left = 370
    Top = 403
    Width = 150
    Height = 26
    Action = actCancel
    Anchors = [akRight, akBottom]
    DisabledImageIndex = 1
    HotImageIndex = 1
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    ModalResult = 2
    TabOrder = 2
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 16
    Top = 272
    object actApply: TAction
      Caption = '&Apply'
      Hint = 'Apply changes'
      ImageIndex = 2
      OnExecute = actApplyExecute
    end
    object actClose: TAction
      Caption = 'Clos&e'
      Hint = 'Save changes and close'
      ImageIndex = 12
      OnExecute = actCloseExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      Hint = 'Cancel all changes and close'
      ImageIndex = 1
      OnExecute = actCancelExecute
    end
    object actAdd: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 3
      ShortCut = 16429
      OnExecute = actAddExecute
    end
    object actDelete: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 4
      ShortCut = 16430
      OnExecute = actDeleteExecute
    end
    object actMoveUp: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Up'
      Hint = 'Move up'
      ImageIndex = 8
      ShortCut = 16422
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Down'
      Hint = 'Move down'
      ImageIndex = 7
      ShortCut = 16424
      OnExecute = actMoveDownExecute
    end
    object actDuplicate: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Duplicate'
      Hint = 'Duplicate'
      ImageIndex = 13
      OnExecute = actDuplicateExecute
    end
    object actEditConnectionDef: TAction
      Category = 'ConnectionDef'
      ImageIndex = 11
      OnExecute = actEditConnectionDefExecute
    end
    object actGridlinesBoth: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Both'
      Checked = True
      GroupIndex = 1
      ImageIndex = 15
      OnExecute = actGridlinesBothExecute
    end
    object actGridlinesHorizontal: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Horizontal'
      GroupIndex = 1
      ImageIndex = 16
      OnExecute = actGridlinesHorizontalExecute
    end
    object actGridlinesVertical: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'Vertical'
      GroupIndex = 1
      ImageIndex = 17
      OnExecute = actGridlinesVerticalExecute
    end
    object actGridlinesNone: TAction
      Category = 'Gridlines'
      AutoCheck = True
      Caption = 'None'
      GroupIndex = 1
      ImageIndex = 14
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
    object actMRSHorizontally: TAction
      Category = 'DisplayMultipleResultsets'
      AutoCheck = True
      Caption = 'stacked horizontally'
      GroupIndex = 2
      ImageIndex = 20
      OnExecute = actMRSHorizontallyExecute
    end
    object actMRSVertically: TAction
      Category = 'DisplayMultipleResultsets'
      AutoCheck = True
      Caption = 'stacked vertically'
      GroupIndex = 2
      ImageIndex = 19
      OnExecute = actMRSVerticallyExecute
    end
    object actSaveFile: TAction
      Caption = 'Save'
      Hint = 'Save file to disk'
      ImageIndex = 0
      OnExecute = actSaveFileExecute
    end
    object actRefreshFile: TAction
      Caption = 'Refresh'
      Hint = 'Reload file from disk'
      ImageIndex = 22
      OnExecute = actRefreshFileExecute
    end
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 16
    Top = 200
    Bitmap = {
      494C010119003000C00010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000007000000001002000000000000070
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000002B0000009C000000E5000000FF000000E50000009C0000002B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0057000000FF02333EFF00191FFF000101FF04B0D9FF05677EFF000000FF0000
      0057000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002B0000
      00FF01242CFF000000FF000000FF000000FF08D0FFFF06CFFFFF078FAEFF0000
      00FF0000002B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000009C0334
      3EFF000000FF000000FF000000FF000000FF12D5FFFF0FD4FFFF0BD2FFFF0668
      7EFF0000009C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000E5021A
      1FFF131313FF282828FF303030FF2D2D2DFF3BDFFFFF25DAFFFF14D6FFFF10B6
      D9FF000000E50000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000FF000000FF2627
      27FF4F4F4FFF4D4D4DFF4A4A4AFF484848FF454545FF434343FF1E1E1EFF0001
      01FF000000FF000000FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000E57BD9
      E8FF86EFFFFF85EFFFFF83EFFFFF616161FF5E5E5EFF5C5C5CFF535353FF031B
      1FFF000000E50000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000009C84B3
      BDFF9EF3FFFF9DF4FFFF9CF4FFFF7A7A7AFF787878FF757575FF6B6B6BFF0535
      3EFF0000009C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002B4A4A
      4AFFA6DDE8FFB5F8FFFFB3F8FFFF949494FF929292FF8F8F8FFF445D62FF0000
      00FF0000002B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0057404040FF94BDC4FFC1EFF2FFA9A9A9FF889293FF41656BFF000000FF0000
      0057000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000002B0000009C000000E5000000FF000000E50000009C0000002B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000361D0CA2000000010000
      000000000000000000050E0703534B2912BE703E1BE981471EFA6C3B1AE43C21
      0EAA020100240000000000000000000000000000000000000000000000020000
      000E000000180000001A0000001A0000001A0000001A0000001A0000001A0000
      001900000010000000030000000000000000813623C0E6613FFFE6613FFFE661
      3FFFE6613FFFE6613FFFE6613FFFE6613FFFE6613FFFE6613FFFE6613FFFE661
      3FFFE6613FFFE6613FFF813623C000000000843824C0E96240FFE96240FFE962
      40FF391810800303038603030386030303860303038601010142030303860303
      0386030303860303038602020264000000000000000087491FFF2C180A930000
      00000E07035476411BEE85502BF3A46C43FFAA7045FFA6693AFF9F5F2FFF9255
      29FC7E451DF61109035B00000000000000000000000000000000000000040000
      001B000000BB000000EA000000EA000000EA000000EA000000EA000000EA0000
      00BB0000001F000000060000000000000000D65836FFECC3B6FFEAC1B4FFEAC1
      B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1
      B4FFEAC1B4FFECC3B6FFD65836FF00000000DD5C3AFFECC3B6FFEAC1B4FFEAC1
      B4FFDD5C3AFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FF0707077EE7E7E7FFE7E7
      E7FFE7E7E7FFEBEBEBFF0707077E0000000000000000864B21FE8E5B37F65D33
      16D4864F2AF4BF9575FFC8A285FFBF9370FFB6845CFFAB7246FF9E6030FF9C5C
      2DFF9E5E2FFF7E441DF50301002A000000000000000000000000000000000000
      0000000000E6695A5AFF685959FF675656FF665656FF665656FF675656FF0000
      00E600000000000000000000000000000000C44E2CFFEBC2B5FFE7BEB0FFE7BE
      B0FFE7BEB0FFE7BEB0FFE7BEB0FFE7BEB0FFE7BEB0FFE7BEB0FFE7BEB0FFE7BE
      B0FFE7BEB0FFEBC2B5FFC44E2CFF00000000CF5432FFEBC2B5FFE7BEB0FFE7BE
      B0FFCF5432FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1E1FF0A0A0A78E1E1E1FFE1E1
      E1FFE1E1E1FFE9E9E9FF0A0A0A780000000000000000864A22FDD3B59FFFAF82
      61FED5B9A3FFD4B69FFFC9A589FFA97852FE854F2AF583471EFB8B4D22FF8A4E
      22FE8A4C21FF8D4E23FF3B200EA8000000000000000000000000000000000000
      0000000000A8000000E02E2727F0645555FF655555FF2F2828F0000000E00000
      00A800000000000000000000000000000000B34624FFB34624FFB34624FFB346
      24FFB34624FFB34624FFB34624FFB34624FFB34624FFB34624FFB34624FFB346
      24FFB34624FFB34624FFB34624FF00000000C14C2AFFEDC4B6FFE8C0B2FFE8C0
      B2FFC14C2AFFC14C2AFFC14C2AFFC14C2AFFC14C2AFFC14C2AFFC14C2AFFC14C
      2AFFC14C2AFFC14C2AFFC14C2AFF0000000000000000864A21FDD9C0ACFFD8BE
      A9FFD8BEA9FFC5A185FF85502BF5552E14CA06030136000000001C11096A925B
      33EDB0754AFF8B4F24FE82481FFA000000000000000000000000000000000000
      00000000000000000037000000DA615555FF625555FF000000DA000000370000
      0000000000000000000000000000000000000B0B0B73EEEEEEFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFD2AEA2FFAB4321FFD2AEA2FFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFEEEEEEFF0B0B0B7300000000B54624FFEEC5B8FFEAC1B4FFEAC1
      B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1B4FFEAC1
      B4FFEAC1B4FFEEC5B8FFB54624FF000000000000000083471FFCD9C0ACFFCAA5
      89FFCAA990FF85491FFE170C056A0000000000000000000000000000000B8A54
      2BE93D2D218D65442CC287491FFF000000050000000000000000000000000000
      00000000000000000000000000D25B5252FF5C5252FF000000D2000000000000
      0000000000000000000000000000000000000B0B0B71F1F1F1FFEBEBEBFFEBEB
      EBFFEBEBEBFFD6B3A8FFAE4927FFEDC4B6FFAE4927FFD6B3A8FFEBEBEBFFEBEB
      EBFFEBEBEBFFF1F1F1FF0B0B0B7100000000AD4321FFF0C7B9FFEDC4B6FFEDC4
      B6FFEDC4B6FFEDC4B6FFEDC4B6FFEDC4B6FFEDC4B6FFEDC4B6FFEDC4B6FFEDC4
      B6FFEDC4B6FFF0C7B9FFAD4321FF000000000000000083471FFCD9BFABFFD9BF
      ABFFD8BDA9FFB28766FD683918E0000000100000000000000000000000000000
      00000201011A0A07053983471FFC0000000C0000000000000000000000000000
      00000000000000000000000000CC554F4FFF564F4FFF000000CC000000000000
      0000000000000000000000000000000000000C0C0C6FF3F3F3FFEFEFEFFFEFEF
      EFFFDEBBAFFFBB5634FFEDC4B6FFEDC4B6FFEDC4B6FFBB5634FFDEBBAFFFEFEF
      EFFFEFEFEFFFF3F3F3FF0C0C0C6F00000000AB4422FFF1C9BBFFEFC6B9FFEFC6
      B9FFEFC6B9FFEFC6B9FFEFC6B9FFEFC6B9FFEFC6B9FFEFC6B9FFEFC6B9FFEFC6
      B9FFEFC6B9FFF1C9BBFFAB4422FF000000000000000081461EF985491FFE8549
      1FFE85481FFD83471FFC83471EFB462710B90100001D01000018010000180100
      001801000018010000180100001C000000000000000000000000000000000000
      00000000000000000000000000C7514E4EFF514D4DFF000000C7000000000000
      0000000000000000000000000000000000000C0C0C6DF6F6F6FFF3F3F3FFF3F3
      F3FFD68B70FFCD6846FFCD6846FFEDC4B6FFCD6846FFCD6846FFD68B70FFF3F3
      F3FFF3F3F3FFF6F6F6FF0C0C0C6D00000000B04B29FFF3CBBDFFF1C9BBFFF1C9
      BBFFF1C9BBFFF1C9BBFFF1C9BBFFF1C9BBFFF1C9BBFFF1C9BBFFF1C9BBFFF1C9
      BBFFF1C9BBFFF3CBBDFFB04B29FF00000000000000000000000D000000000000
      000000000000000000000000000000000000351C0CA087491FFF864F2AF78752
      2DF788522EF788522EF787491FFF000000080000000000000000000000000000
      00000000000000000000000000C24F4E4EFF4E4C4CFF000000C2000000000000
      0000000000000000000000000000000000000C0C0C6BF9F9F9FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFE07D5DFFEEC6B9FFE07D5DFFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFF9F9F9FF0C0C0C6B00000000BA5634FFF5CCBEFFF3CBBDFFF3CB
      BDFFF3CBBDFFF3CBBDFFF3CBBDFFF3CBBDFFF3CBBDFFF3CBBDFFF3CBBDFFF3CB
      BDFFF3CBBDFFF5CCBEFFBA5634FF000000000000000083471FFC27190F7E0504
      032B0100001300000002000000000000000000000004502C13C4A67A5BF9DAC0
      ADFFD5B8A1FFDAC0ADFF854A22FB000000090000000000000000000000000000
      00000000000000000000000000BD555555FF525252FF000000BD000000000000
      0000000000000000000000000000000000000D0D0D69FCFCFCFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFEFA58DFFF0D3C9FFEFA58DFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF0D0D0D6900000000C86341FFF6CDC0FFF5CCBFFFF5CC
      BFFFF5CCBFFFF5CCBFFFF5CCBFFFF5CCBFFFF5CCBFFFF5CCBFFFF5CCBFFFF5CC
      BFFFF5CCBFFFF6CDC0FFC86341FF000000000000000085491FFE884C22FB4A36
      279D86532CE603010023000000000000000000000000130A046087491FFFC9A8
      8FFFCBA78BFFD8BEA9FF834922FA0000000A000000000000008B000000B90000
      00530000000000000000000000B95E5E5EFF5D5D5DFF000000B9000000000000
      000000000053000000B90000008B000000000D0D0D68FEFEFEFFFDFDFDFFFDFD
      FDFFFDFDFDFFFDFDFDFFF7CCBDFFF6E5DFFFF7CCBDFFFDFDFDFFFDFDFDFFFDFD
      FDFFFDFDFDFFFEFEFEFF0D0D0D6800000000D7714FFFF8CFC1FFF7CEC1FFF7CE
      C1FFF7CEC1FFF7CEC1FFF7CEC1FFF7CEC1FFF7CEC1FFF7CEC1FFF7CEC1FFF7CE
      C1FFF7CEC1FFF8CFC1FFD7714FFF00000000000000007C431CF38B4C21FF9054
      29FE955B30F522150A750100001909050245573015CD8E5B39F5CBAB91FFD7BB
      A6FFD9C0ACFFDAC0ADFF834922FA0000000B00000000000000B57E7E7EFF0000
      00B50000000000000000000000B5656565FF636363FF000000B5000000000000
      0000000000B5666666FF000000B5000000000D0D0D67FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDEEE9FFFDF6F4FFFDEEE9FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF0D0D0D6700000000E37D5BFFF8CFC2FFF8CFC2FFF8CF
      C2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CFC2FFF8CF
      C2FFF8CFC2FFF8CFC2FFE37D5BFF0000000000000000321B0B9B925428FD9758
      2AFF8A4C21FF884A1FFF854A23FC875633F2B28564FED0AF97FFD3B49DFFD4B7
      A1FFA87955FDCAA98FFF834822F90000000D00000000000000B17E7E7EFF4A4A
      4AEC000000B1000000B1000000B1737373FF707070FF000000B1000000B10000
      00B1313131EC606060FF000000B1000000000D0D0D68BBBBBBFFB9B9B9FFB8B8
      B8FFB5B5B5FFB2B2B2FFB0B0B0FFACACACFFAAAAAAFFA6A6A6FFA4A4A4FFA1A1
      A1FF9E9E9EFF9C9C9CFF0000006600000000ED8764FFED8764FFED8764FFED87
      64FFED8764FFED8764FFED8764FFED8764FFED8764FFED8764FFED8764FFED87
      64FFED8764FFED8764FFED8764FF0000000000000000030100297E441DF59D5E
      2EFFA46535FFA97044FFB6855DFFBF9471FFC9A386FFCEAC92FFC49F82FF854F
      29F54C2A12C0834C27F6834820FC0000000E00000000000000AE848484FF8080
      80FF808080FF808080FF7E7E7EFF7C7C7CFF7A7A7AFF777777FF757575FF7272
      72FF6F6F6FFF767676FF000000AE000000000A0A0A5A878787D3DBDBDBFFD8D8
      D8FFD3D4D4FFCED0D0FFCACBCBFFC6C7C7FFC6C5C5FFC8C5C5FFCCC5C5FFD1C7
      C7FFD6CACAFF867E7ED30A0A0A5A000000000A0A0A5A878787D3DBDBDBFFD8D8
      D8FFD3D4D4FFCED0D0FFCACBCBFFC6C7C7FFC6C5C5FFC8C5C5FFCCC5C5FFD1C7
      C7FFD6CACAFF867E7ED30A0A0A5A0000000000000000000000000D0703507E45
      1DF68F5327FAA2673AFFB07B51FFB3815AFFAC7751FF8A5633F378421CF01008
      035800000000150B056687491FFF0000000F0000000000000081000000AC0000
      00AC000000AC000000AC000000AC000000AC000000AC000000AC000000AC0000
      00AC000000AC000000AC0000008100000000010101220A0A0A590D0D0D660D0D
      0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D
      0D660D0D0D660A0A0A590101012200000000010101220A0A0A590D0D0D660D0D
      0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D
      0D660D0D0D660A0A0A5901010122000000000000000000000000000000000100
      001D341D0D9F663819DE7D441DF66D3B1AE54A2811BC0E070353000000050000
      000000000000000000001E100779000000100000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FB00000058000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000056000000F3000000550000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000202026403030386030303860303
      038603030386030303860303038603030386030303860303038603030386E962
      40FFE96240FFE96240FF843824C00000000000000000000000AC000000E60000
      00E6000000E6000000E6000000E6000000E6000000E6000000E6000000E60000
      00E6000000E6000000AC000000000000000000000000000000AC000000150000
      00A400000015000000A400000015000000E600000015000000A4000000150000
      00A400000015000000AC00000000000000000000000000000053000000EA0000
      005200000052000000EA00000052000000000000000000000000000000000000
      0000000000000000000000000000000000000707077EEBEBEBFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFDD5C
      3AFFEAC1B4FFECC3B6FFDD5C3AFF000000000000000000000014000000000000
      0000000000000000000000000000000000130000000000000000000000000000
      00000000000000000013000000000000000000000000000000D9000000000000
      0000000000000000000000000000000000D90000000000000000000000000000
      000000000000000000D900000000000000000000000000000000000000510000
      00E5000000E5655959FF000000E5000000500000000000000000000000000000
      0000000000000000000000000000000000000A0A0A78E9E9E9FFE1E1E1FFE1E1
      E1FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1E1FFE1E1E1FFCF54
      32FFE7BEB0FFEBC2B5FFCF5432FF000000000000000000000089000000000000
      0000000000000000000000000000000000890000000000000000000000000000
      00000000000000000089000000000000000000000000000000CC000000000000
      0000000000000000000000000000000000CC0000000000000000000000000000
      000000000000000000CC000000000000000000000000000000000000004F0000
      00E2625959FF544747FF5C5252FF000000E2000000E2000000E20000004F0000
      0000000000000000000000000000000000000A0A0A75EBEBEBFFE4E4E4FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFC97158FFD7AFA3FFE4E4E4FFE4E4E4FFC14C
      2AFFE8C0B2FFEDC4B6FFC14C2AFF000000000000000000000011000000000000
      0000000000000000000000000000000000110000000000000000000000000000
      00000000000000000011000000000000000000000000000000C7000000000000
      0000000000000000000000000000000000C70000000000000000000000000000
      000000000000000000C700000000000000000000000000000000000000E0605B
      5BFF4C4343FF463B3BFF463B3BFF4B4242FF4C4343FF4C4242FF0000002D0000
      004F000000000000000000000000000000000B0B0B73EEEEEEFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFB54624FFB54624FFD5AFA3FFE7E7E7FFB546
      24FFEAC1B4FFEEC5B8FFB54624FF000000000000000000000080000000000000
      0000000000000000000000000000000000800000000000000000000000000000
      00000000000000000080000000000000000000000000000000C4000000000000
      0000000000000000000000000000000000C40000000000000000000000000000
      000000000000000000C4000000000000000000000000000000000000004E0000
      00DD5A5656FF403939FF372F2FFF3D3535FF474141FF0000002C3F3838FF0000
      00DD000000000000000000000000000000000B0B0B71F1F1F1FFEBEBEBFFE3D3
      CFFFD1A597FFBE7159FFB04A2AFFAD4321FFEDC4B6FFAD4321FFD5B1A5FFAD43
      21FFEDC4B6FFF0C7B9FFAD4321FF000000000000000000000010000000000000
      0000000000000000000000000000000000100000000000000000000000000000
      00000000000000000010000000000000000000000000000000C0000000000000
      0000000000000000000000000000000000C00000000000000000000000000000
      000000000000000000C000000000000000000000000000000000000000000000
      004D000000DB3F3B3BFF322D2DFF474444FF000000583C3838FF322E2EFF0000
      00DB000000000000000000000000000000000C0C0C6FF3F3F3FFEFEFEFFFEFE9
      E7FFEFDED8FFEED0C6FFEDC6B9FFEDC4B6FFEDC4B6FFEDC4B6FFAB4422FFAB44
      22FFEFC6B9FFF1C9BBFFAB4422FF0000000000000000000000BC000000BC0000
      00BC000000BC000000BC000000BC000000BC000000BC000000BC000000BC0000
      00BC000000BC000000BC000000000000000000000000000000BC0000000F0000
      00780000000F000000780000000F000000BC0000000F000000780000000F0000
      00780000000F000000BC00000000000000000000000000000000000000000000
      0000000000D94C4B4BFF4D4D4DFF000000573F3E3EFF222121FF262424FF0000
      00D90000004C0000000000000000000000000C0C0C6DF6F6F6FFF3F3F3FFEADB
      D7FFD7AD9FFFC37961FFB35232FFB04B29FFEDC4B6FFB04B29FFDCB9ADFFB04B
      29FFF1C9BBFFF3CBBDFFB04B29FF00000000000000000000000F000000000000
      00000000000000000000000000000000000F0000000000000000000000000000
      0000000000000000000F000000000000000000000000000000B9000000000000
      0000000000000000000000000000000000B90000000000000000000000000000
      000000000000000000B900000000000000000000000000000000000000000000
      0000000000D7696969FF0000002B555555FF2C2B2BFF181818FF141313FF2C2C
      2CFF000000D70000004C00000000000000000C0C0C6BF9F9F9FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFBA5634FFBA5634FFE1BFB3FFF7F7F7FFBA56
      34FFF3CBBDFFF5CCBEFFBA5634FF000000000000000000000071000000000000
      0000000000000000000000000000000000710000000000000000000000000000
      00000000000000000071000000000000000000000000000000B6000000000000
      0000000000000000000000000000000000B60000000000000000000000000000
      000000000000000000B600000000000000000000000000000000000000000000
      00000000004A0000002A6B6B6BFF616161FF555555FF4C4C4CFF444444FF4343
      43FF505050FF000000D400000000000000000D0D0D69FCFCFCFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFAFAFAFFD4896FFFE8C5B9FFFAFAFAFFFAFAFAFFC863
      41FFF5CCBFFFF6CDC0FFC86341FF00000000000000000000000E000000000000
      00000000000000000000000000000000000E0000000000000000000000000000
      0000000000000000000E000000000000000000000000000000B3000000000000
      0000000000000000000000000000000000B30000000000000000000000000000
      000000000000000000B300000000000000000000000000000000000000000000
      0000000000000000004A000000D3000000D3000000D3666666FF525252FF5B5B
      5BFF000000D30000004A00000000000000000D0D0D68FEFEFEFFFDFDFDFFFDFD
      FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFD771
      4FFFF7CEC1FFF8CFC1FFD7714FFF00000000000000000000006B000000000000
      00000000000000000000000000000000006B0000000000000000000000000000
      0000000000000000006B000000000000000000000000000000B0000000000000
      0000000000000000000000000000000000B00000000000000000000000000000
      000000000000000000B000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000004A000000D16A6A6AFF0000
      00D1000000D10000004900000000000000000D0D0D67FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE37D
      5BFFF8CFC2FFF8CFC2FFE37D5BFF00000000000000000000000E000000000000
      00000000000000000000000000000000000D0000000000000000000000000000
      0000000000000000000D000000000000000000000000000000AE000000000000
      0000000000000000000000000000000000AE0000000000000000000000000000
      000000000000000000AE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000049000000CF0000
      004900000049000000CF00000049000000000D0D0D68BBBBBBFFB9B9B9FFB8B8
      B8FFB5B5B5FFB2B2B2FFB0B0B0FFACACACFFAAAAAAFFA6A6A6FFA4A4A4FFED87
      64FFED8764FFED8764FFED8764FF000000000000000000000081000000AC0000
      00AC000000AC000000AC000000AC000000AC000000AC000000AC000000AC0000
      00AC000000AC00000081000000000000000000000000000000810000000D0000
      00670000000D000000670000000D000000AC0000000D000000670000000D0000
      00670000000D0000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000049000000CE000000480A0A0A5A878787D3DBDBDBFFD8D8
      D8FFD3D4D4FFCED0D0FFCACBCBFFC6C7C7FFC6C5C5FFC8C5C5FFCCC5C5FFD1C7
      C7FFD6CACAFF867E7ED30A0A0A5A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000048000000CD010101220A0A0A590D0D0D660D0D
      0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D0D660D0D
      0D660D0D0D660A0A0A5901010122000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000070702650C0C03870C0C03870C0C03870C0C03870C0C03870C0C
      03870C0C03870C0C038707070265000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000012730000
      31BA00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC0000
      3ACC000031BA000012730000000C000000000000000000000000000000000000
      0000000000000F0F067FFFFFFFFFFFFFFFFFFFFFFEFFFEFEFDFFFEFEFCFFFDFD
      FAFFFCFCF9FFFEFEF9FF0F0F067F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000167306066DDD0F0F
      C2F91010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010
      D9FF0E0EC2F905056DDD00001673000000000000000000000000000000000000
      00000000000011110978FFFFFFFFFEFEFEFFFDFDFCFFFCFCFBFFFBFBF8FFFAFA
      F6FFF8F8F4FFFBFBF6FF1111097800000000000000000000007B000000150000
      00A400000015000000A400000015000000A400000015000000A4000000150000
      00A4000000150000007B000000000000000000000000000000AC000000E60000
      00E6000000E6000000E6000000E6000000E6000000E6000000E6000000E60000
      00E6000000E6000000AC000000000000000000000000000042BA1414C1F91010
      D1FF1010D1FF1010B6FF1010D1FF1010D1FF1010D1FF1010D1FF1010B6FF1010
      D1FF1010D1FF0E0EBEF9000042BA0000000000000000070702650C0C03870C0C
      03870C0C0387272714BEFFFFFEFFFDFDFCFFFCFCFBFFFBFBF8FFFAFAF6FFF8F8
      F4FFF7F7F1FFFAFAF4FF12120A76000000000000000000000014000000000000
      0000000000000000000000000000000000130000000000000000000000000000
      00000000000000000013000000000000000000000000000000D9000000000000
      0000000000000000000000000000000000D90000000000000000000000000000
      000000000000000000D9000000000000000000000000000054CC1B1BCEFF1010
      C8FF1010B2FFDCDCDCFF1010B2FF1010C8FF1010C8FF1010B2FFEEEEEEFF1010
      B2FF1010C8FF1010C8FF000054CC00000000000000000F0F067FFFFFFFFFFFFF
      FFFFFFFFFEFFB5B5A3FFFEFEFDFFFCFCFBFFFBFBF8FFFAFAF6FFF8F8F4FFF7F7
      F1FFF5F5EFFFFAFAF2FF13130B74000000000000000000000089000000000000
      0000000000000000000000000000000000890000000000000000000000000000
      00000000000000000089000000000000000000000000000000CC000000000000
      0000000000000000000000000000000000CC0000000000000000000000000000
      000000000000000000CC000000000000000000000000000057CC2121C8FF1010
      BEFFD1D1D1FFD6D6D6FFDCDCDCFF1010ADFF1010ADFFEAEAEAFFEEEEEEFFEEEE
      EEFF1010BEFF1111BEFF000057CC000000000000000011110978FFFFFFFFFEFE
      FEFFFDFDFCFFB8B8A6FFFEFEFCFFFBFBF8FFFAFAF6FFF8F8F4FFF7F7F1FFF5F5
      EFFFF4F4ECFFF9F9EFFF13130C72000000000000000000000011000000000000
      0000000000000000000000000000000000110000000000000000000000000000
      00000000000000000011000000000000000000000000000000C7000000000000
      0000000000000000000000000000000000C70000000000000000000000000000
      000000000000000000C700000000000000000000000000005ACC3333C7FF1111
      B4FF1010B4FFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FFE6E6E6FFEAEAEAFF1010
      B4FF1010B4FF1313B6FF00005ACC000000000000000012120A76FFFFFEFFFDFD
      FCFFFCFCFBFFBBBBA8FFFDFDFAFFFAFAF6FFF8F8F4FFF7F7F1FFF5F5EFFFF4F4
      ECFFF0F0E6FFF6F6E8FF13130C70000000000000000000000080000000000000
      0000000000000000000000000000000000800000000000000000000000000000
      00000000000000000080000000000000000000000000000000C4000000000000
      0000000000000000000000000000000000C40000000000000000000000000000
      000000000000000000C400000000000000000000000000005DCC4545CEFF2525
      B5FF1313ABFF1010AAFFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FF1010AAFF1010
      AAFF1010AAFF1717B0FF00005DCC000000000000000013130B74FEFEFDFFFCFC
      FBFFFBFBF8FFBDBDAAFFFCFCF9FFF8F8F4FFF7F7F1FFF5F5EFFFF4F4ECFFF0F0
      E6FFEBEBDDFFF4F4E3FF14140C6E000000000000000000000010000000000000
      0000000000000000000000000000000000100000000000000000000000000000
      00000000000000000010000000000000000000000000000000C0000000000000
      0000000000000000000000000000000000C00000000000000000000000000000
      000000000000000000C000000000000000000000000000005FCC4949D2FF3232
      BBFF2D2DB8FF12129FFFCECECEFFD1D1D1FFD6D6D6FFDCDCDCFF10109EFF1010
      A1FF1010A1FF1C1CACFF00005FCC000000000000000013130C72FEFEFCFFFBFB
      F8FFFAFAF6FFBFBFACFFFCFCF7FFF7F7F1FFF5F5EFFFF4F4ECFFF0F0E6FFEBEB
      DDFFE7E7D6FFF2F2E1FF14140C6C0000000000000000000000780000000F0000
      00780000000F000000780000000F000000780000000F000000780000000F0000
      00780000000F00000078000000000000000000000000000000BC000000BC0000
      00BC000000BC000000BC000000BC0000008E000000BC000000BC000000BC0000
      00BC000000BC000000BC000000000000000000000000000062CC4F4FD8FF3636
      BFFF2222ABFFFFFFFFFFF7F7F7FFE8E8E8FFDEDEDEFFDBDBDBFFDDDDDDFF1010
      9BFF1515A0FF2A2AB5FF000062CC000000000000000013130C70FDFDFAFFFAFA
      F6FFF8F8F4FFC1C1ACFFFBFBF5FFF5F5EFFFF4F4ECFFF0F0E6FFEBEBDDFFA4A4
      93FFA4A493FFA4A493FF1010087C00000000000000000000000F000000000000
      00000000000000000000000000000000000F0000000000000000000000000000
      0000000000000000000F000000000000000000000000000000B9000000000000
      0000000000000000000000000000000000B90000000000000000000000000000
      000000000000000000B9000000000000000000000000000064CC5959E2FF4141
      CAFFFFFFFFFFFFFFFFFFFFFFFFFF4141CAFF4141CAFFFFFFFFFFFFFFFFFFFFFF
      FFFF4141CAFF4D4DD6FF000064CC000000000000000014140C6EFCFCF9FFF8F8
      F4FFF7F7F1FFC2C2AEFFFAFAF4FFF4F4ECFFF0F0E6FFEBEBDDFFE7E7D6FFB6B6
      A5FFFFFFFFFF14140D6902020125000000000000000000000071000000000000
      0000000000000000000000000000000000710000000000000000000000000000
      00000000000000000071000000000000000000000000000000B6000000000000
      0000000000000000000000000000000000B60000000000000000000000000000
      000000000000000000B6000000000000000000000000000068CC6161EAFF4E4E
      D7FF4E4ED7FFFFFFFFFF4E4ED7FF4E4ED7FF4E4ED7FF4E4ED7FFFFFFFFFF4E4E
      D7FF4E4ED7FF5959E2FF000068CC000000000000000014140C6CFCFCF7FFF7F7
      F1FFF5F5EFFFC3C3AEFFFCFCF5FFF8F8EEFFF6F6E8FFF3F3E3FFF2F2E1FFC2C2
      B1FF14140D68020201250000000000000000000000000000000E000000000000
      00000000000000000000000000000000000E0000000000000000000000000000
      0000000000000000000E000000000000000000000000000000B3000000000000
      0000000000000000000000000000000000B30000000000000000000000000000
      000000000000000000B3000000000000000000000000000057BA5B5BE0F95A5A
      E3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5A
      E3FF5A5AE3FF5656DCF9000057BA000000000000000014140D6AFBFBF5FFF5F5
      EFFFF4F4ECFFCECEBBFFC0C0A7FF959579FF959579FF959579FF31311EB01414
      0D6602020124000000000000000000000000000000000000006B000000000000
      00000000000000000000000000000000006B0000000000000000000000000000
      0000000000000000006B000000000000000000000000000000B0000000000000
      0000000000000000000000000000000000B00000000000000000000000000000
      000000000000000000B0000000000000000000000000000022731F1F95DD5E5E
      E3F96C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6B6B
      F4FF5D5DE2F91F1F94DD00002273000000000000000014140D69FAFAF4FFF4F4
      ECFFF0F0E6FFEBEBDDFFE7E7D6FFB6B6A5FFFFFFFFFF14140D69020201250000
      000000000000000000000000000000000000000000000000000E000000000000
      00000000000000000000000000000000000D0000000000000000000000000000
      0000000000000000000D000000000000000000000000000000AE000000000000
      0000000000000000000000000000000000AE0000000000000000000000000000
      000000000000000000AE0000000000000000000000000000000C000022730000
      5ABA00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC0000
      6DCC00005ABA000022730000000C000000000000000014140D68FCFCF5FFF8F8
      EEFFF6F6E8FFF3F3E3FFF2F2E1FFC2C2B1FF14140D6802020125000000000000
      000000000000000000000000000000000000000000000000004D0000000D0000
      00670000000D000000670000000D000000670000000D000000670000000D0000
      00670000000D0000004D00000000000000000000000000000081000000AC0000
      00AC000000AC000000AC000000AC000000AC000000AC000000AC000000AC0000
      00AC000000AC0000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0B074D14140D661414
      0D6614140D6614140D6614140D6614140D660202012400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000874263BBA1494CE8A0474CE89E464CE89D454BE89D434AE89C424AE8883C
      5DC7000000000000000000000000000000000000005A000000FF0000005A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000808080D1D1D1D311A1A1A2F04040407000000000000
      0000000000000000000000000000000000000202026404040485040404850404
      0485040404850404048504040485040404850404048504040485040404850404
      0485040404850202026400000000000000000000000000000000000000000000
      0000964555D9EFAF66FFE79C60FFE3975AFFDF9354FFDB8F50FFE09D52FF9B42
      4CE600000000000000000000000000000000000000FF545454FF000000FF0000
      005A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001515
      15216363639D969696F0A1A1A1FFABABABFFA7A7A7FF959595FF808080E74A4A
      4A8A0C0C0C160000000000000000000000000909097BEBEBEBFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFEBEBEBFF0909097B00000000000000000000000000000000000000000000
      0000974655D9EFA769FFE58A63FFDF845BFFD97D53FFD4774CFFDB8F4FFF9B43
      4CE6000000000000000000000000000000000000005A000000FF545454FF0000
      00FF00000097000000FA00000057000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002F2F2F4A9E9E
      9EF4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFF4F4F4FFEFEFEFFFE2E2E2FFBABA
      BAFF7D7D7DE71C1C1C3500000000000000000A0A0A76EAEAEAFFE2E2E2FFE2E2
      E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
      E2FFEAEAEAFF0A0A0A7600000000000000000000000000000000000000000000
      0000994755D9F1AA6DFFE98F69FFE48962FFDE835AFFD97C52FFDE9253FF9C44
      4DE600000000000000000000000000000000000000000000005A000000FF5454
      54FF000000EE655959FF000000EE000000530000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9BEADEDE
      DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFD6D6D6FFC0C0C0FFC9C9C9FFE6E6
      E6FFC4C4C4FF7F7F7FEA00000000000000000B0B0B73EDEDEDFFFF9965FFE6E6
      E6FFBDBDBDFFACACACFFBDBDBDFFBDBDBDFFACACACFFBDBDBDFFC9C9C9FFE6E6
      E6FFEDEDEDFF0B0B0B7300000000000000000000000000000000000000000000
      0000994855D9F3AC70FFED936EFFE88E68FFE38861FFDE8259FFE29658FF9E46
      4DE6000000000000000000000000000000000000000000000000000000910000
      00E5615959FF544747FF5C5252FF000000E5000000D6000000B8000000870000
      00310000000000000000000000000000000000000000000000009E9E9EEAF0F0
      F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFD6D6D6FFBFBFBFFFB0B0B0FFB3B3
      B3FFDEDEDEFF848484EA00000000000000000B0B0B71F0F0F0FFEBEBEBFFEBEB
      EBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEB
      EBFFF0F0F0FF0B0B0B7100000000000000000000000000000000000000000000
      00009A4855D9F4AC72FFEF9573FFEC926DFFE88D67FFE38860FFE59A5DFF9E46
      4DE6000000000000000000000000000000000000000000000000000000E15F5F
      5FFF454141FF463C3CFF463B3BFF4B4242FF473F3FFE3C3434FA282323F40A09
      09E30000007000000000000000000000000000000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFD8D8D8FFC0C0C0FFB3B3B3FFB7B7
      B7FFE0E0E0FF8A8A8AEA00000000000000000C0C0C6EF3F3F3FF6D70C2FFEFEF
      EFFFBCBCBCFFC5C5C5FFC5C5C5FFC5C5C5FFBCBCBCFFC5C5C5FFBCBCBCFFC5C5
      C5FFF3F3F3FF0C0C0C6E00000000000000000000000000000000000000000000
      00009B4A55D9F4AC73FFF09675FFEF9572FFEC926DFFE78D66FFE99E62FF9F47
      4DE60000000000000000000000000000000000000000000000000000004E0000
      00DE5F5F5FFF3F3B3BFF383030FF372F2FFF372F2FFF372F2FFF3A3333FF3A33
      33FF000000DE0000004E00000000000000000000000000000000A2A2A2EAF3F3
      F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFFC4C4C4FFB8B8B8FFBBBB
      BBFFE1E1E1FF8E8E8EEA00000000000000000D0D0D6CF7F7F7FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF7F7F7FF0D0D0D6C000000000000000043344248563E5462563D5462573E
      5462A44D4BE8F3AB72FFF09676FFF09675FFEF9571FFEB916CFFECA267FFA74A
      45F0563D5462563D5462563D54624836474F0000000000000000000000000000
      004D000000DB535353FF2C3E44FF0E4152FF000000FF292323FF292323FF302B
      2BFF2E2929FF000000DB00000000000000000000000000000000A3A3A3EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FFBDBDBDFFBFBF
      BFFFE2E2E2FF919191EA00000000000000000D0D0D6AFAFAFAFF21AA32FFF8F8
      F8FFBBBBBBFFCCCCCCFFD4D4D4FFDADADAFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
      F8FFFAFAFAFF0D0D0D6A000000000000000066435F78BD5D38FEDA9253FFDA8F
      54FFE0995BFFEF9D73FFEF9576FFF09676FFF09675FFEE9471FFED9B6BFFDE97
      58FFD4894FFFD48A4EFFB24F36FF6E4365890000000000000000000000000000
      0000000000C93E5D62FD126F81FFD9F4FFFF154A5CFF1D1B1BFF282626FF2C2A
      2AFF000000D80202026F00000000000000000000000000000000A5A5A5EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC2C2C2FFC2C2
      C2FFE3E3E3FF939393EA00000000000000000D0D0D68FDFDFDFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFABC0D4FF326599FFABC0D4FFFCFC
      FCFFFDFDFDFF0D0D0D680000000000000000000000006944617DCE7B46FFE79A
      69FFE5896EFFE98E71FFED9274FFEF9576FFF09676FFF09674FFEE9470FFEA91
      6AFFEA9E65FFCB7947FF7143668D000000000000000000000000000000000000
      0000001317BB167484FE78E6F7FF168293FF3F5157FF494949FF454545FF0000
      00D5D0C2C2FF0909099401010132000000000000000000000000A6A6A6EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC3C3C3FFBCBCBCFFC0C0
      C0FFE5E5E5FF959595EA00000000000000000D0D0D67FFFFFFFFFFFFFFFFB0C5
      DAFF376BA0FFB0C5DAFFFFFFFFFFADC2D6FF326599FF81B4E6FF4376AAFFADC2
      D6FFFFFFFFFF0D0D0D67000000000000000000000000000000006944617DCE7C
      46FFE79A6AFFE5896EFFEA8E72FFED9375FFF09576FFF09676FFF09674FFF1A4
      6EFFCE7D4AFF7143668D00000000000000000000000000000000000000000007
      0944005260DE78E6F7FF1C8899FF3F656CFF5B5B5BFF616161FF000000D30404
      04660F0F0F7DD8CDCDFF0C0C0C850101012C0000000000000000A8A8A8EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC3C3
      C3FFE6E6E6FF979797EA00000000000000000D0D0D69BBBBBBFFB9B9B9FF2E62
      98FF8FC2F1FF3C70A4FF537496FF215589FF84B7E8FF4376AAFF84B7E8FF477A
      AEFF396694FF000F1FAA0000011A000000000000000000000000000000006944
      617DCE7C46FFE89B6AFFE68A6FFFEA8F72FFEE9375FFF09676FFF3A673FFD081
      4CFF7144668D0000000000000000000000000000000000000000000405410045
      52BE78E6F7FF066B7EF351777DFF646464FF6B6B6BFF000000D00202026D0000
      00000202022B14141477E1DADAFF141414750000000000000000AAAAAAEAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF999999EA00000000000000000A0A0A5B898989D4DBDBDBFF9BB1
      C6FF558ABDFF95C8F5FF386B9FFF89BCEBFF4376AAFF89BCEBFF5488BBFF89BC
      EBFF72A4D6FE3A6A9AED00274EBE0009135E0000000000000000000000000000
      00006944617DCF7C46FFE89B6BFFE68B6FFFEB9073FFF1A473FFD1824CFF7144
      668C00000000000000000000000000000000000000000000002E0808087D78E6
      F7FF004B58B50016198C000000CE727272FF000000CED0C2C2FF090909940101
      01320000000002020229181818710303032700000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA10000000000000000010101220A0A0A590D0D0D660D0D
      0D66101A248F25517CD89DD0F8FF5488BBFF92C5F0FF5488BBFF9DD0F8FF87BA
      E7FF75A9DAFF7CB0E0FF5485B3EA002446A80000000000000000000000000000
      0000000000006944617DCF7C46FFE99C6BFFEC9D6EFFD2834CFF7144668C0000
      00000000000000000000000000000000000000000C460707158EF7F7F7FF0D0D
      0D6C00090B3F0000000000000048000000CD040404640F0F0F7DD8CDCDFF0C0C
      0C850101012C00000000000000000000000000000000000000001F1F1F2A9999
      99D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC2C2
      C2FF848484C31212121B00000000000000000000000000000000000000000000
      000002080E4E0121419D689CCBFFA4D7FCFF5488BBFFA4D7FCFF8CBFEBFF70A4
      D5FF79ADDDFF84B7E5FF8CBFECFF0121419D0000000000000000000000000000
      000000000000000000006944617DCF7C47FFD2824AFF6F456588000000000000
      000000000000000000000000000000000000000065C18080FFFF0D0D1A7C0101
      012400000000000000000000000000000000000000000202022B14141477E1DA
      DAFF141414750000000000000000000000000000000000000000000000000505
      05073E3E3E557F7F7FAE9A9A9AD6B4B4B4FBB1B1B1F9909090CD777777A93131
      3146020202030000000000000000000000000000000000000000000000000000
      0000011F3C93A5D8FCFF7BAFDDFF699DCCFFA9DCFFFFA5D8FCFF8ABDE7FF8EC1
      EAFF93C6EFFF99CCF4FF011F3C9300070F4B0000000000000000000000000000
      000000000000000000000000000066435F786B45638000000000000000000000
      00000000000000000000000000000000000000000C40000068AF020207390000
      0000000000000000000000000000000000000000000000000000020202291818
      1871030303270000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000070E47011D398C011D398C011D398C011D398C011D398C011D398C011D
      398C011D398C011D398C00070E47000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202036868
      68CC7C7C7CFF777777FF727272FF6E6E6EFF535353CA00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006842617E7543679701000101000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000101015902020277020202770202
      0277020202770202027702020277020202770202027702020277020202770202
      02770202027702020277010101590000000000000000000000000C0C0C159191
      91FFD5D3D3FFE2E0DFFFDFDCDBFFE1DFDFFF696969F400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006B436382C46F43FFCB7C48FF7842689D010001010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000006060674EBEBEBFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFEBEBEBFF06060674000000000000000000000000000000003E3E
      3E6F666666BDC3C1C0FFD4CFCEFF757575F64848489C626262D2727272FF6E6E
      6EFF696969FF656565FF616161FF4F4F4FD70000000000000000000000000000
      0000000000006B446382C87346FFE6995EFFE08F58FFCB7C48FF7842689D0100
      0101000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B0B0B70E9E9E9FF59CE59FF63E5
      63FF63E563FFE1E1E1FFDDB159FFF5C463FFF5C463FFE1E1E1FFDD5959FFF563
      63FFF56363FFE9E9E9FF0B0B0B70000000005B5B5B8D818181CC7C7C7CCC7878
      78CC7D7D7DDE9D9D9DFF999999FF7D7D7DF8838383FF868685FF868584FFA19F
      9EFFD3CECDFFD3CECDFFE8E5E5FF626262FF0000000000000000000000000000
      00006B446383CC7949FFEDA169FFE48962FFDE835AFFDF8F57FFCB7B48FF7842
      689D010001010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000D0D0D6FEAEAEAFF5ED35EFF68EA
      68FF68EA68FFE3E3E3FFE1B65EFFFACA68FFFACA68FFE3E3E3FFE15E5EFFFA68
      68FFFA6868FFEAEAEAFF0D0D0D6F00000000ADADADFFE4E2E2FFD7D5D5FFD5D3
      D2FFD1CECDFFCAC3C2FFC8C2C1FFCDC9C8FFCCCACAFFCCCAC9FFD8D6D6FF7271
      71FFB8B0ADFFB6AEADFFD3CECDFF666666FF0000000000000000000000006D44
      6484CF7D4BFFF2A671FFED936EFFE88E68FFE38861FFDE8259FFDF8E56FFCB7B
      47FF7843679C01000101000000000000000004048AE80303A6FF0303A6FF0303
      A6FF0303A6FF0303A6FF0202A6FF0202A6FF0202A6FF0202A6FF0202A6FF0202
      A6FF0202A6FF0202A6FF0202A6FF04048AE80E0E0E6EEDEDEDFF5CCB5CFF61D6
      61FF61D661FFE6E6E6FFD8B05CFFE5BA61FFE5BA61FFE6E6E6FFD85C5CFFE561
      61FFE56161FFEDEDEDFF0E0E0E6E00000000B3B3B3FFE0DDDDFFA6704BFFA670
      4BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFDAD5D4FF7676
      76FFB9B1B0FFB8B0AEFFD4CFCEFF6B6B6BFF00000000000000006E456586D081
      4BFFF3A674FFF09775FFEF9573FFEC926DFFE88D67FFE38860FFDD8158FFDE8D
      55FFCB7B47FF7843679C01000101000000000B0BABFF5F5FD5FF5E5ED5FF5D5D
      D5FF5C5CD5FF5C5CD5FF5A5AD5FF5959D5FF5757D4FF5656D4FF5454D4FF5353
      D2FF5151D2FF4E4ED1FF4C4CD0FF0A0AABFF0E0E0E6DEFEFEFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFEFEFEFFF0E0E0E6D00000000B9B9B9FFDEDBDBFFB47E58FFCE97
      6EFFD8AD90FFD9AE90FFD9AE90FFDAAE90FFD69F75FFA6704BFFD7D3D1FF7C7C
      7CFFBAB3B2FFBAB2B0FFD4D0CFFF717171FF000000006F456588D1824BFFF0A4
      71FFEF9675FFF09676FFF09675FFEF9572FFEC926DFFE78D66FFE2875FFFDD82
      57FFDE8F55FFCB7B47FF7843679C010001011313B0FF6E6EDDFF4C4CD5FF4D4D
      D6FF4E4ED7FF4F4FD7FF5050D8FF5050D8FF5050D8FF5050D8FF5050D8FF4F4F
      D7FF4E4ED7FF4D4DD6FF5D5DD9FF1111B0FF1010106CF1F1F1FF59DDB1FF63F5
      C4FF63F5C4FFECECECFFDDDDDDFFF5F5F5FFF5F5F5FFECECECFFDD59B1FFF563
      C4FFF563C4FFF1F1F1FF1010106C00000000BFBFBFFFDFDCDCFFB37D57FFCB94
      6CFFCD966DFFCF986FFFD19A70FFD29B72FFD49D73FFA6704BFFD7D4D3FF8383
      83FF39A040FF359335FFD6D1D0FF767676FF6D456484BD5C36FFD88E50FFD78B
      51FFDD9457FFF2A274FFF09676FFF09675FFEF9571FFEB916CFFE99766FFD991
      52FFCE814AFFCD8148FFB04D35FF754267961A1AB5FF7D7DE4FF5858DDFF5959
      DEFF5B5BDFFF5C5CE0FF5D5DE0FF5E5EE1FF5E5EE1FF5E5EE1FF5D5DE0FF5C5C
      E0FF5B5BDFFF5959DEFF6D6DE1FF1919B5FF1010106BF3F3F3FF5EE1B6FF68FA
      CAFF68FACAFFEFEFEFFFE1E1E1FFFAFAFAFFFAFAFAFFEFEFEFFFE15EB6FFFA68
      CAFFFA68CAFFF3F3F3FF1010106B00000000C4C4C4FFE1DEDCFFB27B56FFC790
      69FFC9926BFFCB946CFFCD966EFFCF986FFFD19A71FFA6704BFFD9D5D4FF8A8A
      8AFF9FC8A3FF4EA854FFD7D3D1FF7C7C7CFF382D383B4B3849524B3849524B38
      49529F4A51E0F2AB71FFEF9576FFF09676FFF09675FFEE9471FFEFA46BFFAB4F
      42F34B3849524A3849524A3849523D303C412222BAFF8989E9FF8B8BE9FF8C8C
      E9FF8D8DE9FF8D8DE9FF8E8EE9FF8D8DE9FF8D8DEAFF8B8BE8FF8989E8FF8787
      E8FF8484E7FF8080E7FF7B7BE6FF2121BAFF1111116AF6F6F6FF5CD8B0FF61E5
      BAFF61E5BAFFF2F2F2FFD8D8D8FFE5E5E5FFE5E5E5FFF2F2F2FFD85CB0FFE561
      BAFFE561BAFFF6F6F6FF1111116A00000000CACACAFFE1DFDEFFB07A55FFC38D
      66FFC58F68FFC89169FFCA936BFFCC956DFFCE976EFFA6704BFFDBD6D6FF9090
      90FFC0BAB8FFBFB8B7FFD7D3D3FF838383FF0000000000000000000000000000
      000097495AD2F0A86FFFED9274FFEF9576FFF09676FFF09674FFF2A66FFFA94F
      48EE0000000000000000000000000000000025259FE82A2ABFFF2A2ABFFF2A2A
      BFFF2A2ABFFF2A2ABFFF2A2ABFFF2A2ABFFF2A2ABFFF2929BFFF2929BFFF2929
      BFFF2929BFFF2929BFFF2929BFFF24249FE812121269F8F8F8FFF5F5F5FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5
      F5FFF5F5F5FFF8F8F8FF1212126900000000CECECEFFE2DFDFFFAF7954FFB079
      54FFB07A55FFB17B56FFB27C56FFB37D57FFB47E58FFA6704BFFDCD8D7FF9797
      97FF8C8C8CFF898989FFD9D5D4FF8A8A8AFF0000000000000000000000000000
      0000984A5AD2EDA56DFFEA8E72FFED9375FFF09576FFF09676FFF3A872FFAA4F
      48EE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000012121269FAFAFAFF59B1DDFF63C4
      F5FF63C4F5FFF8F8F8FF5959DDFF6363F5FF6363F5FFF8F8F8FFB159DDFFC463
      F5FFC463F5FFFAFAFAFF1212126900000000D3D3D3FFF1EFEFFFE2DFDFFFE2DF
      DFFFE1DFDEFFE1DEDDFFE0DDDCFFDFDCDBFFDEDBDBFFDEDBD9FFEDECEBFF9D9D
      9DFFC4BEBDFFC2BCBAFFDAD6D5FF909090FF0000000000000000000000000000
      0000994B5AD2EAA16AFFE68A6FFFEA8F72FFEE9375FFF09676FFF3A873FFAB51
      48EE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000013131368FCFCFCFF5EB6E1FF68CA
      FAFF68CAFAFFFBFBFBFF5E5EE1FF6868FAFF6868FAFFFBFBFBFFB65EE1FFCA68
      FAFFCA68FAFFFCFCFCFF1313136800000000D1D1D1F9D3D3D3FFD0D0D0FFCCCC
      CCFFC8C8C8FFC2C2C2FFBEBEBEFFB9B9B9FFB4B4B4FFAFAFAFFFA9A9A9FFA4A4
      A4FF939292FF919090FFDBD7D6FF979797FF0000000000000000000000000000
      0000994C5AD2E69D67FFE1856BFFE68B6FFFEB9073FFEE9375FFF3A773FFAC53
      49EE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000013131367FEFEFEFF5CB0D8FF61BA
      E5FF61BAE5FFFDFDFDFF5C5CD8FF6161E5FF6161E5FFFDFDFDFFB05CD8FFBA61
      E5FFBA61E5FFFEFEFEFF13131367000000000000000000000000000000000000
      00000000000039393948B6B6B6E4DFDFDFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3
      C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D9DFF0000000000000000000000000000
      0000994D5AD2E39964FFDD8066FFE2856BFFE78B70FFEB9073FFF2A672FFAD54
      49EE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000014141467FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF14141467000000000000000000000000000000000000
      000000000000000000000C0C0C0F888888ABDCDCDCFFE6E3E3FFE1DEDCFFDFDC
      DCFFDFDCDBFFDEDBDBFFEEECECFFA4A4A4FF0000000000000000000000000000
      0000994D5AD2E6A563FFE39864FFE79D68FFEBA16CFFEEA56EFFF3B26FFFAD56
      49EE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B0B0B4D14141466141414661414
      1466141414661414146614141466141414661414146614141466141414661414
      146614141466141414660B0B0B4D000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F9FC6C6C6FBC3C3C3FFBFBF
      BFFFBABABAFFB5B5B5FFAFAFAFFF6E6E6EA70000000000000000000000000000
      0000814867A89E4F56D89D4E56D89C4E56D89C4D56D89C4D56D89C4C56D88C46
      62BF000000000000000000000000000000000000000022120881D38044FF5256
      54FF525654FF525654FF525654FF525654FF525654FF525654FF525654FF5256
      54FFD38044FFD38044FFD38044FF221208810000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B6338E80C7742FF0C7742FF0C7742FF0C7742FF0B6338E80000
      00000000000000000000000000000000000022120881D38044FFCCA98EFF927A
      62FFE3E3E3FFD6D6D6FFBABABAFFBABABAFFBABABAFFB9B9B9FFB9B9B9FF947B
      62FFCE9F71FFDCB898FFEAD2BEFFD38044FF0000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000000000
      0000000000000105010A00020007000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E7946FF43A581FF3FA380FF3DA27DFF3AA17CFF0E7946FF0000
      000000000000000000000000000000000000D38044FFCCA98EFFDCB898FF927A
      61FFE1E1E1FFE6E6E6FF525654FF525654FFC5C5C5FFC2C2C2FFB2B2B2FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000000000000000000
      00000105010A35823BFF2F7734FB000200070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000107C49FF4CAC8CFF2C9E77FF2C9E77FF45A988FF107C49FF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FF927A
      61FFC5C5C5FFE1E1E1FF525654FF525654FFC5C5C5FFC5C5C5FFBFBFBFFF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000005054DAC1616A4F53A3A
      C8FF3A3AC8FF1616A5F5050556B60000000400000004050556B61616A5F53B3B
      C8FF3A3AC8FF1616A5F505054DAC000000000000000000000000000000000306
      030A3E8D45FF52A25AFF4D9E55FF307A36FE0104020800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000127E4CFF56B494FF31A37CFF31A37CFF4EB08FFF127E4CFF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FF9279
      60FFC5C5C5FFC5C5C5FF525654FF525654FFE2E2E2FFC5C5C5FFB9B9B9FF9379
      60FFCE9E70FFDBB694FFEAD2BEFFD38044FF0000000006064EAC1A1AA7F54444
      CDFF4040CCFF4545CDFF1A1AA7F5070757B6070757B61A1AA7F54545CDFF4141
      CCFF4444CDFF1A1AA7F506064EAC0000000000000000000000000306030A4799
      4FFF59AB62FF75CA81FF72C87CFF4F9F57FF317B37FE01040208000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000148150FF60BA9CFF37A983FF37A983FF58B797FF148150FF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FF9C7D
      5FFFBABABAFFC5C5C5FFC5C5C5FFE1E1E1FFE7E7E7FFD7D7D7FFBABAB9FF997A
      5CFFCE9E6FFFDBB694FFEAD2BEFFD38044FF0000000000000004090958B61F1F
      ABF54F4FD1FF4949D0FF4F4FD1FF2121A9F32121A9F34F4FD1FF4949D0FF4F4F
      D1FF1F1FABF5090958B60000000400000000000000000307030A4FA558FF61B4
      6BFF7CCE88FF79CC86FF74CA80FF74C980FF50A158FF327C38FE010402080000
      000000000000000000000000000000000000136D46E8168453FF168453FF1684
      53FF168453FF198052F768C0A3FF3DAE89FF3DAE89FF61BD9FFF198052F71684
      53FF168453FF168453FF168453FF136D46E8D38044FFEAD2BEFFCF9F71FFB08C
      68FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FF947B61FFB08C
      68FFDBB694FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000040A0A
      59B62525ADF55858D6FF5151D4FF5A5AD7FF5A5AD7FF5252D4FF5858D6FF2525
      ADF50A0A59B60000000400000000000000000206030957AF61FF69BC74FF83D2
      8FFF78C984FF5EB168FF61B36BFF76C982FF76CB81FF51A25AFF327C38FD0204
      020800000000000000000000000000000000188657FF7BC8AFFF79C9AEFF77C8
      ADFF75C7ACFF73C7ACFF71C7AAFF42B48EFF42B48EFF6AC4A6FF67C3A4FF65C1
      A3FF61BFA0FF5EBE9EFF5BBC9CFF188656FFD38044FFEAD2BEFFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F71FFCF9F
      71FFCF9F71FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      00040C0C5BB62B2BAEF36464DBFF6262DBFF6262DBFF6464DBFF2B2BAEF30C0C
      5BB60000000400000000000000000000000019331C464FA159E477C985FF7ECE
      8CFF4EA357FC2046246F27542B8B5AAC65FF7ACC85FF77CB84FF52A35BFF337E
      39FC020402080000000000000000000000001A895AFF84CEB5FF44B690FF45B7
      91FF46B892FF47B993FF48B994FF48B994FF48B994FF48B994FF48B994FF47B9
      93FF46B892FF45B791FF63C2A3FF1A895AFFD38044FFEAD2BEFFCF9F71FFFEFE
      FEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFE
      FEFFFEFEFEFFCF9F71FFEAD2BEFFD38044FF0000000000000000000000000000
      00040E0E5DB63030B2F36D6DDFFF6969DFFF6969DFFF6D6DDFFF3131B2F30E0E
      5DB60000000400000000000000000000000000000000162C183C51A35AE66BBF
      77FF244A286F000000000000000029592D915CAD66FF7BCD88FF7ACD86FF54A4
      5DFF347E3AFC0204020800000000000000001C8B5DFF8BD3BBFF4ABB96FF4BBC
      97FF4CBD98FF4DBE99FF4DBF99FF4EBF9AFF4EBF9AFF4EBF9AFF4DBF99FF4DBE
      99FF4CBD98FF4BBC97FF6DC8AAFF1C8B5DFFD38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF0000000000000000000000040F0F
      5DB63333B5F57575E3FF6969E1FF7676E4FF7676E4FF6868E1FF7575E3FF3333
      B5F50F0F5DB6000000040000000000000000000000000000000018301C432348
      27650000000000000000000000000000000029592F915DAE67FF7DCE89FF7CCE
      88FF55A55EFF357F3BFC02040208000000001E8E61FF93D7C1FF91D7C0FF90D7
      C0FF8ED7BFFF8CD7BEFF8BD7BEFF53C59FFF53C59FFF84D5BAFF82D4B9FF7ED2
      B7FF7AD1B4FF78CFB3FF75CEB1FF1E8E60FFD38044FFEAD2BEFFCF9F71FFFEFE
      FEFFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7D7FFD7D7
      D7FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF000000000000000411115FB63737
      B9F67D7DE6FF7070E4FF7E7EE6FF3A3AB7F43A3AB7F47E7EE6FF7171E4FF7D7D
      E6FF3737B8F511115FB600000004000000000000000000000000000000000000
      00000000000000000000000000000000000000000000295A2F915EAF68FF80CF
      8CFF7DCF8AFF56A65FFF37843EFF020402081D7955E8209064FF209064FF2090
      64FF209064FF238B64F792DCC3FF59CAA5FF59CAA5FF8CDAC1FF238B64F72090
      64FF209064FF209064FF209064FF1D7955E8D38044FFEAD2BEFFCF9F71FFFEFE
      FEFFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFCF9F71FFEAD2BEFFD38044FF00000000111155AC3D3DBBF68686
      E7FF7777E5FF8888E7FF3E3EBBF613135FB613135FB63E3EBBF68888E7FF7878
      E5FF8787E7FF3D3DBBF6121255AC000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002B5A2F9160B1
      6AFF81D18EFF78C884FF55A55EFF1A3F1D7B0000000000000000000000000000
      000000000000229367FF9ADFC9FF5FCDA9FF5FCDA9FF94DDC5FF229367FF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
      E2FFF9F9F9FFCF9F71FFEAD2BEFFD38044FF00000000131356AC4242BDF68F8F
      E9FF9090E9FF4343BDF6151561B60000000400000004151561B64343BDF69090
      E9FF8F8FE9FF4242BDF6131356AC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002B5B
      2F9161B26BFF5DAE67FF1D442279000000000000000000000000000000000000
      00000000000024956BFFA1E2CDFF65CFADFF66CFADFF9BE0CAFF24956BFF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFCF9F71FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFCF9F71FFEAD2BEFFD38044FF0000000000000004161662B64646
      BEF64747BEF6161662B600000004000000000000000000000004171762B64848
      BFF64848BEF6171762B600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002B5C3091234E277F00000000000000000000000000000000000000000000
      00000000000026976EFFA9E4D0FF6BD1B0FF6CD1B0FFA3E2CDFF26976EFF0000
      000000000000000000000000000000000000D38044FFEAD2BEFFEED9C8FF3DAF
      FCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAFFCFF3DAF
      FCFF3DAFFCFFEAD2BEFFEAD2BEFFD38044FF0000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000289A71FFAFE6D4FFAEE6D3FFADE5D3FFAAE5D1FF289A71FF0000
      000000000000000000000000000000000000391F0DA7D38044FFD38044FF005B
      CEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005BCEFF005B
      CEFF005BCEFFD38044FFD38044FF391F0DA70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000268362E82B9C75FF2A9C75FF2A9C75FF2A9C75FF258362E80000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000700000000100010000000000800300000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F00F000000000000F00F000000000000
      F00F000000000000F00F000000000000F00F000000000000F00F000000000000
      F00F000000000000000000000000000000000000000000008001000000000000
      C003000000000000E007000000000000F00F000000000000F81F000000000000
      FC3F000000000000FE7F000000000000000000000000FE3F000000000000FC1F
      000000000000F80F000000000000F007000000000000E003000000000000C001
      000000000000800000000000000000000000000000000000000000000000F00F
      000000000000F00F000000000000F00F000000000000F00F000000000000F00F
      000000000000F00F000000000000F00F00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object dlgOpenFile: TOpenDialog
    Left = 16
    Top = 72
  end
  object synJScript: TSynJScriptSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold, fsItalic]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold, fsItalic]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    Left = 95
    Top = 76
  end
  object synIni: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    SectionAttri.Foreground = clMaroon
    SectionAttri.Style = [fsBold, fsUnderline]
    KeyAttri.Foreground = clBlue
    KeyAttri.Style = [fsBold]
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold, fsItalic]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold, fsItalic]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    Left = 14
    Top = 135
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 98
    Top = 130
  end
  object conTest: TFDConnection
    LoginPrompt = False
    OnError = conTestError
    Left = 94
    Top = 203
  end
end

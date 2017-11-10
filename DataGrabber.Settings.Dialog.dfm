object frmSettingsDialog: TfrmSettingsDialog
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 437
  ClientWidth = 721
  Color = clBtnFace
  Constraints.MinHeight = 476
  Constraints.MinWidth = 396
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  DesignSize = (
    721
    437)
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 715
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
        Left = 137
        Top = 0
        Width = 7
        Height = 365
        Color = clBtnHighlight
        ParentColor = False
        ExplicitLeft = 185
        ExplicitHeight = 377
      end
      object pnlConnectionProfileDetail: TPanel
        Left = 144
        Top = 0
        Width = 563
        Height = 365
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 192
        ExplicitWidth = 515
        object pgcConnectionProfile: TPageControl
          Left = 0
          Top = 0
          Width = 563
          Height = 365
          ActivePage = tsBasic
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 515
          object tsBasic: TTabSheet
            Caption = 'Basic'
            ExplicitWidth = 507
            DesignSize = (
              555
              337)
            object rgpConnectionType: TRadioGroup
              Left = 2
              Top = 45
              Width = 549
              Height = 39
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Connection type'
              Columns = 4
              ItemIndex = 0
              Items.Strings = (
                'ADO'
                'FireDAC'
                'dbExpress')
              TabOrder = 0
              OnClick = rgpConnectionTypeClick
              ExplicitWidth = 501
            end
            object grpClientSettings: TGroupBox
              Left = 3
              Top = 188
              Width = 549
              Height = 146
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Client settings'
              TabOrder = 1
              ExplicitWidth = 501
              object lblPacketrecords: TLabel
                Left = 152
                Top = 44
                Width = 75
                Height = 13
                Caption = 'Packet records:'
                FocusControl = edtPacketRecords
              end
              object chkProviderMode: TCheckBox
                Left = 16
                Top = 24
                Width = 116
                Height = 17
                Caption = 'Provider mode'
                Checked = True
                DoubleBuffered = False
                ParentDoubleBuffered = False
                State = cbChecked
                TabOrder = 0
                OnClick = chkProviderModeClick
              end
              object edtPacketRecords: TEdit
                Left = 239
                Top = 41
                Width = 58
                Height = 21
                Alignment = taCenter
                TabOrder = 1
                Text = '100'
              end
              object chkSeperateThreads: TCheckBox
                Left = 16
                Top = 69
                Width = 169
                Height = 17
                Caption = 'Execute in seperate threads'
                Enabled = False
                TabOrder = 2
              end
              object chkAllowMultipleInstances: TCheckBox
                Left = 16
                Top = 92
                Width = 169
                Height = 17
                Caption = 'Allow multiple instances'
                Enabled = False
                TabOrder = 3
              end
              object chkUseIDInUpdatableQueries: TCheckBox
                Left = 16
                Top = 115
                Width = 232
                Height = 17
                Caption = 'Use ID as primary key in updatable queries.'
                Enabled = False
                TabOrder = 4
              end
              object chkFetchOnDemand: TCheckBox
                Left = 37
                Top = 43
                Width = 100
                Height = 17
                Caption = 'Fetch on demand'
                Checked = True
                DoubleBuffered = False
                ParentDoubleBuffered = False
                State = cbChecked
                TabOrder = 5
                OnClick = chkFetchOnDemandClick
              end
            end
            object grpConnectionSettings: TGroupBox
              Left = 3
              Top = 90
              Width = 550
              Height = 94
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Connection settings'
              TabOrder = 2
              ExplicitWidth = 502
              DesignSize = (
                550
                94)
              object lblProtocols: TLabel
                Left = 10
                Top = 19
                Width = 39
                Height = 13
                Caption = 'Protocol'
                FocusControl = cbxProtocols
              end
              object lblDatabase: TLabel
                Left = 10
                Top = 45
                Width = 50
                Height = 13
                Caption = 'Database:'
                FocusControl = edtDatabase
              end
              object lblCatalog: TLabel
                Left = 10
                Top = 71
                Width = 41
                Height = 13
                Caption = 'Catalog:'
                FocusControl = edtCatalog
              end
              object cbxProtocols: TComboBox
                Left = 66
                Top = 16
                Width = 145
                Height = 21
                DropDownCount = 30
                TabOrder = 0
                OnChange = cbxProtocolsChange
              end
              object btnConnectionString: TButton
                Left = 217
                Top = 13
                Width = 150
                Height = 26
                Action = actConnectionString
                Images = imlMain
                TabOrder = 1
              end
              object edtDatabase: TButtonedEdit
                Left = 66
                Top = 42
                Width = 481
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Images = imlMain
                RightButton.ImageIndex = 10
                RightButton.Visible = True
                TabOrder = 2
                OnChange = edtDatabaseChange
                OnRightButtonClick = edtDatabaseRightButtonClick
                ExplicitWidth = 433
              end
              object edtCatalog: TButtonedEdit
                Left = 66
                Top = 68
                Width = 481
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Images = imlMain
                RightButton.ImageIndex = 10
                RightButton.Visible = True
                TabOrder = 3
                OnChange = edtCatalogChange
                OnRightButtonClick = edtDatabaseRightButtonClick
                ExplicitWidth = 433
              end
            end
            object grpProfileSettings: TGroupBox
              Left = 3
              Top = 3
              Width = 549
              Height = 36
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 3
              ExplicitWidth = 501
              DesignSize = (
                549
                36)
              object lblProfileColor: TLabel
                Left = 274
                Top = 10
                Width = 60
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'Profile color:'
                FocusControl = btnProfileColor
                ExplicitLeft = 220
              end
              object btnProfileColor: TKColorButton
                Left = 340
                Top = 5
                Width = 104
                Height = 25
                Anchors = [akTop, akRight]
                FocusRect = False
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clBtnText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                OnClick = btnProfileColorClick
                ColorDlgOptions = [cdAnyColor]
                ExplicitLeft = 292
              end
              object edtProfileName: TLabeledEdit
                Left = 76
                Top = 7
                Width = 191
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 63
                EditLabel.Height = 13
                EditLabel.Caption = 'Profile name:'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                LabelPosition = lpLeft
                ParentFont = False
                TabOrder = 1
                OnChange = edtProfileNameChange
                ExplicitWidth = 143
              end
              object chkSetAsDefault: TCheckBox
                Left = 450
                Top = 9
                Width = 97
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Set as default'
                TabOrder = 2
                OnClick = chkSetAsDefaultClick
                ExplicitLeft = 402
              end
            end
          end
          object tsAdvanced: TTabSheet
            Caption = 'Advanced'
            ImageIndex = 1
            ExplicitWidth = 507
          end
        end
      end
      object pnlConnectionProfilesList: TPanel
        Left = 0
        Top = 0
        Width = 137
        Height = 365
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object tlbConnectionProfiles: TToolBar
          Left = 0
          Top = 0
          Width = 137
          Height = 25
          Caption = 'tlbConnectionProfiles'
          Images = imlMain
          TabOrder = 0
          ExplicitWidth = 185
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
          object btn2: TToolButton
            Left = 46
            Top = 0
            Width = 10
            Caption = 'btn2'
            ImageIndex = 8
            Style = tbsSeparator
          end
          object btnDuplicate: TToolButton
            Left = 56
            Top = 0
            Action = actDuplicate
          end
          object btn1: TToolButton
            Left = 79
            Top = 0
            Width = 10
            Caption = 'btn1'
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
      Caption = '&Resultgrid display settings'
      ImageIndex = 5
      DesignSize = (
        707
        365)
      object grpCellBackgroundColoring: TGroupBox
        Left = 2
        Top = 56
        Width = 166
        Height = 302
        Caption = 'Grid cell background coloring'
        TabOrder = 0
        object chkGridCellColoringEnabled: TCheckBox
          Left = 19
          Top = 23
          Width = 64
          Height = 14
          Caption = 'Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = chkGridCellColoringEnabledClick
        end
        object pnlGridTypeColoring: TGridPanel
          Left = 12
          Top = 38
          Width = 145
          Height = 257
          BevelOuter = bvNone
          ColumnCollection = <
            item
              SizeStyle = ssAbsolute
              Value = 60.000000000000000000
            end
            item
              SizeStyle = ssAbsolute
              Value = 80.000000000000000000
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
            145
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
            Width = 74
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
        Left = 3
        Top = 1
        Width = 701
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Grid type'
        Columns = 4
        TabOrder = 1
      end
      object grpGridLines: TGroupBox
        Left = 175
        Top = 56
        Width = 90
        Height = 114
        Caption = 'Gridlines'
        TabOrder = 2
        object tlb1: TToolBar
          AlignWithMargins = True
          Left = 8
          Top = 18
          Width = 75
          Height = 88
          Align = alNone
          AutoSize = True
          ButtonWidth = 75
          Caption = 'tlb1'
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
    end
    object tsSettings: TTabSheet
      Caption = 'Settings file'
      ImageIndex = 11
      OnEnter = tsSettingsEnter
    end
  end
  object btnApply: TButton
    Left = 251
    Top = 403
    Width = 150
    Height = 26
    Action = actApply
    Anchors = [akRight, akBottom]
    Default = True
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    TabOrder = 1
  end
  object btnClose: TButton
    Left = 563
    Top = 403
    Width = 150
    Height = 26
    Action = actClose
    Anchors = [akRight, akBottom]
    ImageMargins.Left = 4
    ImageMargins.Right = 4
    Images = imlMain
    ModalResult = 11
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 407
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
    TabOrder = 3
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 600
    Top = 296
    object actApply: TAction
      Caption = '&Apply'
      Hint = 'Apply settings immediately'
      ImageIndex = 2
      OnExecute = actApplyExecute
    end
    object actClose: TAction
      Caption = 'Clos&e'
      Hint = 'Close dialog'
      ImageIndex = 12
      OnExecute = actCloseExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      Hint = 'Cancel all changes.'
      ImageIndex = 1
      OnExecute = actCancelExecute
    end
    object actAdd: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 3
      OnExecute = actAddExecute
    end
    object actDelete: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 4
      OnExecute = actDeleteExecute
    end
    object actMoveUp: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Up'
      Hint = 'Move up'
      ImageIndex = 8
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Down'
      Hint = 'Move down'
      ImageIndex = 7
      OnExecute = actMoveDownExecute
    end
    object actDuplicate: TAction
      Category = 'ConnectionProfiles'
      Caption = 'Copy'
      Hint = 'Duplicate'
      ImageIndex = 13
      OnExecute = actDuplicateExecute
    end
    object actConnectionString: TAction
      Caption = 'Build connectionstring'
      ImageIndex = 9
      OnExecute = actConnectionStringExecute
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
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 560
    Top = 248
    Bitmap = {
      494C010112001500040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005000000001002000000000000050
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
      00000000000000000000000000000000000000000000000000AC000000E60000
      00E6000000E6000000E6000000E6000000E6000000E6000000E6000000E60000
      00E6000000E6000000AC000000000000000000000000000000AC000000150000
      00A400000015000000A400000015000000E600000015000000A4000000150000
      00A400000015000000AC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000014000000000000
      0000000000000000000000000000000000130000000000000000000000000000
      00000000000000000013000000000000000000000000000000D9000000000000
      0000000000000000000000000000000000D90000000000000000000000000000
      000000000000000000D900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000089000000000000
      0000000000000000000000000000000000890000000000000000000000000000
      00000000000000000089000000000000000000000000000000CC000000000000
      0000000000000000000000000000000000CC0000000000000000000000000000
      000000000000000000CC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000011000000000000
      0000000000000000000000000000000000110000000000000000000000000000
      00000000000000000011000000000000000000000000000000C7000000000000
      0000000000000000000000000000000000C70000000000000000000000000000
      000000000000000000C700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000080000000000000
      0000000000000000000000000000000000800000000000000000000000000000
      00000000000000000080000000000000000000000000000000C4000000000000
      0000000000000000000000000000000000C40000000000000000000000000000
      000000000000000000C400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000010000000000000
      0000000000000000000000000000000000100000000000000000000000000000
      00000000000000000010000000000000000000000000000000C0000000000000
      0000000000000000000000000000000000C00000000000000000000000000000
      000000000000000000C000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000BC000000BC0000
      00BC000000BC000000BC000000BC000000BC000000BC000000BC000000BC0000
      00BC000000BC000000BC000000000000000000000000000000BC0000000F0000
      00780000000F000000780000000F000000BC0000000F000000780000000F0000
      00780000000F000000BC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000F000000000000
      00000000000000000000000000000000000F0000000000000000000000000000
      0000000000000000000F000000000000000000000000000000B9000000000000
      0000000000000000000000000000000000B90000000000000000000000000000
      000000000000000000B900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000071000000000000
      0000000000000000000000000000000000710000000000000000000000000000
      00000000000000000071000000000000000000000000000000B6000000000000
      0000000000000000000000000000000000B60000000000000000000000000000
      000000000000000000B600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000000000
      00000000000000000000000000000000000E0000000000000000000000000000
      0000000000000000000E000000000000000000000000000000B3000000000000
      0000000000000000000000000000000000B30000000000000000000000000000
      000000000000000000B300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000006B000000000000
      00000000000000000000000000000000006B0000000000000000000000000000
      0000000000000000006B000000000000000000000000000000B0000000000000
      0000000000000000000000000000000000B00000000000000000000000000000
      000000000000000000B000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000000000
      00000000000000000000000000000000000D0000000000000000000000000000
      0000000000000000000D000000000000000000000000000000AE000000000000
      0000000000000000000000000000000000AE0000000000000000000000000000
      000000000000000000AE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000081000000AC0000
      00AC000000AC000000AC000000AC000000AC000000AC000000AC000000AC0000
      00AC000000AC00000081000000000000000000000000000000810000000D0000
      00670000000D000000670000000D000000AC0000000D000000670000000D0000
      00670000000D0000008100000000000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000151616162E323539404B
      5362465A6A88416077A4185E92FC1D5D8CED0000000000000000000000000000
      000000000000070702650C0C03870C0C03870C0C03870C0C03870C0C03870C0C
      03870C0C03870C0C038707070265000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000989898FF727272FF555555FF5252
      52FF505050FF4D4D4DFF4B4B4BFF484848FF464646FF26689CFF3375A7FF3E7D
      AEFF4883B4FF4F89B9FF3F7FACFF1F5D8CEA0000000000000000000000000000
      0000000000000F0F067FFFFFFFFFFFFFFFFFFFFFFEFFFEFEFDFFFEFEFCFFFDFD
      FAFFFCFCF9FFFEFEF9FF0F0F067F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000595959FFA1A1
      A1FFA1A1A1FFA2A2A2FFA3A3A3FFA3A3A3FFA4A4A4FF3070A4FF79AAD2FF79AA
      D3FF74A6D1FF6A9FCDFF4180ADFF225E8EEA0000000000000000000000000000
      00000000000011110978FFFFFFFFFEFEFEFFFDFDFCFFFCFCFBFFFBFBF8FFFAFA
      F6FFF8F8F4FFFBFBF6FF1111097800000000000000000000007B000000150000
      00A400000015000000A400000015000000A400000015000000A4000000150000
      00A4000000150000007B000000000000000000000000000000AC000000E60000
      00E6000000E6000000E6000000E6000000E6000000E6000000E6000000E60000
      00E6000000E6000000AC000000000000000000000000000000005D5D5DFF9F9F
      9FFF3D7441FFA1A1A1FFA2A2A2FFA2A2A2FFA3A3A3FF3775A9FF7EAED4FF5C99
      C9FF5594C7FF5995C8FF4281ADFF256190EA00000000070702650C0C03870C0C
      03870C0C0387272714BEFFFFFEFFFDFDFCFFFCFCFBFFFBFBF8FFFAFAF6FFF8F8
      F4FFF7F7F1FFFAFAF4FF12120A76000000000000000000000014000000000000
      0000000000000000000000000000000000130000000000000000000000000000
      00000000000000000013000000000000000000000000000000D9000000000000
      0000000000000000000000000000000000D90000000000000000000000000000
      000000000000000000D900000000000000000000000000000000616161FF3B78
      40FF3E7742FFA0A0A0FFA1A1A1FFA1A1A1FFA2A2A2FF3E7AAFFF81B2D7FF639E
      CCFF5B99C9FF5F9ACAFF4481AEFF296593EA000000000F0F067FFFFFFFFFFFFF
      FFFFFFFFFEFFB5B5A3FFFEFEFDFFFCFCFBFFFBFBF8FFFAFAF6FFF8F8F4FFF7F7
      F1FFF5F5EFFFFAFAF2FF13130B74000000000000000000000089000000000000
      0000000000000000000000000000000000890000000000000000000000000000
      00000000000000000089000000000000000000000000000000CC000000000000
      0000000000000000000000000000000000CC0000000000000000000000000000
      000000000000000000CC000000000000000000000000000000003A773FFF4E94
      55FF4A9051FF296F2EFF276B2BFF246728FF226426FF467FB3FF87B6D9FF68A2
      CFFF629DCCFF649ECCFF4682B0FF2E6797EA0000000011110978FFFFFFFFFEFE
      FEFFFDFDFCFFB8B8A6FFFEFEFCFFFBFBF8FFFAFAF6FFF8F8F4FFF7F7F1FFF5F5
      EFFFF4F4ECFFF9F9EFFF13130C72000000000000000000000011000000000000
      0000000000000000000000000000000000110000000000000000000000000000
      00000000000000000011000000000000000000000000000000C7000000000000
      0000000000000000000000000000000000C70000000000000000000000000000
      000000000000000000C70000000000000000000000004C7F50D4579C5EFF81C5
      87FF7CC282FF78C080FF73BD7AFF70BB76FF256829FF4D83B9FF8CBADBFF6FA7
      D1FF67A5D1FF60B3DFFF4884B0FF336C9BEA0000000012120A76FFFFFEFFFDFD
      FCFFFCFCFBFFBBBBA8FFFDFDFAFFFAFAF6FFF8F8F4FFF7F7F1FFF5F5EFFFF4F4
      ECFFF0F0E6FFF6F6E8FF13130C70000000000000000000000080000000000000
      0000000000000000000000000000000000800000000000000000000000000000
      00000000000000000080000000000000000000000000000000C4000000000000
      0000000000000000000000000000000000C40000000000000000000000000000
      000000000000000000C40000000000000000557358A15FA467FF8ACC93FF7EC4
      85FF74BF7DFF6FBC78FF6ABA72FF76BE7DFF286D2DFF5588BEFF93BEDDFF76AC
      D4FF64B7E1FF4CD4FFFF438AB7FF39719FEA0000000013130B74FEFEFDFFFCFC
      FBFFFBFBF8FFBDBDAAFFFCFCF9FFF8F8F4FFF7F7F1FFF5F5EFFFF4F4ECFFF0F0
      E6FFEBEBDDFFF4F4E3FF14140C6E000000000000000000000010000000000000
      0000000000000000000000000000000000100000000000000000000000000000
      00000000000000000010000000000000000000000000000000C0000000000000
      0000000000000000000000000000000000C00000000000000000000000000000
      000000000000000000C0000000000000000000000000518455D260A568FF8CCD
      95FF88CB91FF83C88CFF81C587FF7CC282FF2B7131FF5B8DC3FF97C2E0FF7DB2
      D7FF75AED6FF5FC3EDFF4C87B2FF4074A3EA0000000013130C72FEFEFCFFFBFB
      F8FFFAFAF6FFBFBFACFFFCFCF7FFF7F7F1FFF5F5EFFFF4F4ECFFF0F0E6FFEBEB
      DDFFE7E7D6FFF2F2E1FF14140C6C0000000000000000000000780000000F0000
      00780000000F000000780000000F000000780000000F000000780000000F0000
      00780000000F00000078000000000000000000000000000000BC000000BC0000
      00BC000000BC000000BC000000BC0000008E000000BC000000BC000000BC0000
      00BC000000BC000000BC00000000000000000000000000000000488850FF61A6
      6AFF5EA366FF38813FFF357F3CFF327A38FF2F7635FF6191C9FF9DC7E2FF82B7
      DAFF7EB3D7FF7FB2D7FF5088B3FF467AA8EA0000000013130C70FDFDFAFFFAFA
      F6FFF8F8F4FFC1C1ACFFFBFBF5FFF5F5EFFFF4F4ECFFF0F0E6FFEBEBDDFFA4A4
      93FFA4A493FFA4A493FF1010087C00000000000000000000000F000000000000
      00000000000000000000000000000000000F0000000000000000000000000000
      0000000000000000000F000000000000000000000000000000B9000000000000
      0000000000000000000000000000000000B90000000000000000000000000000
      000000000000000000B900000000000000000000000000000000787878FF4E8F
      55FF3E8946FF9A9A9AFF9B9B9BFF9C9C9CFF9C9C9CFF6795CCFFA1CBE3FF88BC
      DCFF82B8DAFF83B8DAFF528AB4FF4C7DACEA0000000014140C6EFCFCF9FFF8F8
      F4FFF7F7F1FFC2C2AEFFFAFAF4FFF4F4ECFFF0F0E6FFEBEBDDFFE7E7D6FFB6B6
      A5FFFFFFFFFF14140D6902020125000000000000000000000071000000000000
      0000000000000000000000000000000000710000000000000000000000000000
      00000000000000000071000000000000000000000000000000B6000000000000
      0000000000000000000000000000000000B60000000000000000000000000000
      000000000000000000B6000000000000000000000000000000007B7B7BFF9798
      97FF53905AFF999999FF9A9A9AFF9B9B9BFF9B9B9BFF6D99D0FFA6CEE5FF8EC0
      DFFF88BCDCFF8ABCDCFF548CB5FF5383B1EA0000000014140C6CFCFCF7FFF7F7
      F1FFF5F5EFFFC3C3AEFFFCFCF5FFF8F8EEFFF6F6E8FFF3F3E3FFF2F2E1FFC2C2
      B1FF14140D68020201250000000000000000000000000000000E000000000000
      00000000000000000000000000000000000E0000000000000000000000000000
      0000000000000000000E000000000000000000000000000000B3000000000000
      0000000000000000000000000000000000B30000000000000000000000000000
      000000000000000000B3000000000000000000000000000000007E7E7EFF9898
      98FF989898FF999999FF999999FF9A9A9AFF9A9A9AFF709CD3FFA9D1E7FFAAD1
      E7FF97C7E1FF90C1DEFF578EB6FF5986B6EA0000000014140D6AFBFBF5FFF5F5
      EFFFF4F4ECFFCECEBBFFC0C0A7FF959579FF959579FF959579FF31311EB01414
      0D6602020124000000000000000000000000000000000000006B000000000000
      00000000000000000000000000000000006B0000000000000000000000000000
      0000000000000000006B000000000000000000000000000000B0000000000000
      0000000000000000000000000000000000B00000000000000000000000000000
      000000000000000000B000000000000000000000000000000000818181FF7F7F
      7FFF7D7D7DFF7B7B7BFF787878FF767676FF737373FF729DD4FF709DD6FF86B1
      DCFFAAD3E8FFA8D0E6FF598FB7FF5E8BBAEA0000000014140D69FAFAF4FFF4F4
      ECFFF0F0E6FFEBEBDDFFE7E7D6FFB6B6A5FFFFFFFFFF14140D69020201250000
      000000000000000000000000000000000000000000000000000E000000000000
      00000000000000000000000000000000000D0000000000000000000000000000
      0000000000000000000D000000000000000000000000000000AE000000000000
      0000000000000000000000000000000000AE0000000000000000000000000000
      000000000000000000AE00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007293
      BDDB6E9BD4FF84B0DAFF5B90B8FF658FBFEA0000000014140D68FCFCF5FFF8F8
      EEFFF6F6E8FFF3F3E3FFF2F2E1FFC2C2B1FF14140D6802020125000000000000
      000000000000000000000000000000000000000000000000004D0000000D0000
      00670000000D000000670000000D000000670000000D000000670000000D0000
      00670000000D0000004D00000000000000000000000000000081000000AC0000
      00AC000000AC000000AC000000AC000000AC000000AC000000AC000000AC0000
      00AC000000AC0000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000606D7D896B99D0FB6993C4EE000000000B0B074D14140D661414
      0D6614140D6614140D6614140D6614140D660202012400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005A000000FF0000005A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000808080D1D1D1D311A1A1A2F04040407000000000000
      00000000000000000000000000000000000000000000000000001B1B1B1D2A2A
      2A342C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C
      2C362C2C2C362C2C2C362A2A2A331B1B1B1D000000000000000C160000723A02
      00B8480300CB480300CB480300CB480300CB480300CB480300CB480300CB4803
      00CB3A0200B8160000720000000C00000000000000FF545454FF000000FF0000
      005A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001515
      15216363639D969696F0A1A1A1FFABABABFFA7A7A7FF959595FF808080E74A4A
      4A8A0C0C0C1600000000000000000000000000000000000000002A2A2A34EFEF
      EFF5FAFAFAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFDEDEDEDF32A2A2A3300000000180200707E2512DBDF62
      38F8FA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA73
      41FFDF6237F87E2411DB18020070000000000000005A000000FF545454FF0000
      00FF00000097000000FA00000057000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002F2F2F4A9E9E
      9EF4CECECEFFEDEDEDFFF4F4F4FFF5F5F5FFF4F4F4FFEFEFEFFFE2E2E2FFBABA
      BAFF7D7D7DE71C1C1C35000000000000000000000000020202012C2C2C36FBFB
      FBFEFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFAFAFAFD2C2C2C3600000000460B00B3DA6238F8F16E
      3DFFF16E3DFFF16E3DFFF16E3DFFCF5930FFCF5930FFF16E3DFFF16E3DFFF16E
      3DFFF16E3DFFD95E34F8460B00B300000000000000000000005A000000FF5454
      54FF000000EE655959FF000000EE000000530000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9BEADEDE
      DEFFF3F3F3FFDBDBDBFFD2D2D2FFDBDBDBFFD6D6D6FFC0C0C0FFC9C9C9FFE6E6
      E6FFC4C4C4FF7F7F7FEA000000000000000000000000020202012C2C2C36FCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF2C2C2C3600000000581001C4E86D41FFE566
      39FFE56639FFE56639FFD75E33FFE8E8E8FFECECECFFD75E33FFE56639FFE566
      39FFE56639FFE56639FF581001C4000000000000000000000000000000910000
      00E5615959FF544747FF5C5252FF000000E5000000D6000000B8000000870000
      00310000000000000000000000000000000000000000000000009E9E9EEAF0F0
      F0FFDEDEDEFFD4D4D4FFD2D2D2FFDBDBDBFFD6D6D6FFBFBFBFFFB0B0B0FFB3B3
      B3FFDEDEDEFF848484EA0000000000000000000000000202020135333342E194
      66FFE08F56FFFCFCFCFFE3995AFFE39868FFF8F1EEFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFFCFCFCFF2C2C2C36000000005A1101C3DE6B41FFD85F
      34FFD85F34FFD85F34FFCD582FFFE4E4E4FFE8E8E8FFCD582FFFD85F34FFD85F
      34FFD85F34FFD85F34FF5A1101C3000000000000000000000000000000E15F5F
      5FFF454141FF463C3CFF463B3BFF4B4242FF473F3FFE3C3434FA282323F40A09
      09E30000007000000000000000000000000000000000000000009F9F9FEAF2F2
      F2FFE2E2E2FFD8D8D8FFD5D5D5FFDCDCDCFFD8D8D8FFC0C0C0FFB3B3B3FFB7B7
      B7FFE0E0E0FF8A8A8AEA00000000000000000000000014131313B2774CD2E5A2
      66FFE1935EFFFCFCFCFFE59F62FFE7A769FFE39A68FFF8F0EBFFFAFAFAFFFAFA
      FAFFF8F8F8FFF8F8F8FFFCFCFCFF2C2C2C36000000005B1201C2D86E4AFFCB58
      2FFFBC4E28FFC4532BFFC3532BFFE0E0E0FFE4E4E4FFC3532BFFC4532BFFBC4E
      28FFCB572EFFCC5A31FF5B1201C20000000000000000000000000000004E0000
      00DE5F5F5FFF3F3B3BFF383030FF372F2FFF372F2FFF372F2FFF3A3333FF3A33
      33FF000000DE0000004E00000000000000000000000000000000A2A2A2EAF3F3
      F3FFE7E7E7FFDDDDDDFFD9D9D9FFE0E0E0FFDBDBDBFFC4C4C4FFB8B8B8FFBBBB
      BBFFE1E1E1FF8E8E8EEA000000000000000055483F58DF9C58F7E8A96BFFE49B
      60FFF8EBE5FFFCFCFCFFF8EDE5FFE5A163FFE8A96BFFE39B58FFEDC4AEFFF9F9
      F9FFF9F9F9FFF8F8F8FFFCFCFCFF2C2C2C36000000005C1301C1DA7856FFC85E
      3AFFD4D4D4FFC9AFA7FFB54A26FFDCDCDCFFE0E0E0FFB54A26FFD9BFB7FFECEC
      ECFFBE4F29FFC35530FF5C1301C1000000000000000000000000000000000000
      004D000000DB535353FF2C3E44FF0E4152FF000000FF292323FF292323FF302B
      2BFF2E2929FF000000DB00000000000000000000000000000000A3A3A3EAF4F4
      F4FFEAEAEAFFE1E1E1FFDDDDDDFFE3E3E3FFDEDEDEFFC9C9C9FFBDBDBDFFBFBF
      BFFFE2E2E2FF919191EA0000000000000000DBA25BF1ECB77AFFE2A25AFCF4DF
      D1FFFCFCFCFFFCFCFCFFFCFCFCFFF5E1D2FFE6A55CFFE9B176FFE49D5BFFF8F5
      F4FFF6F6F6FFF6F6F6FFFCFCFCFF2C2C2C36000000005E1501C1DD7C5AFFCC65
      43FFF8F8F8FFE1E1E1FFCAB1A8FFD7D7D7FFDCDCDCFFD3B9B1FFE4E4E4FFE8E8
      E8FFB44925FFBD5431FF5E1501C1000000000000000000000000000000000000
      0000000000C93E5D62FD126F81FFD9F4FFFF154A5CFF1D1B1BFF282626FF2C2A
      2AFF000000D80202026F00000000000000000000000000000000A5A5A5EAF5F5
      F5FFEEEEEEFFE6E6E6FFE2E2E2FFE6E6E6FFE1E1E1FFCDCDCDFFC2C2C2FFC2C2
      C2FFE3E3E3FF939393EA0000000000000000554A4058E3A85EF7EAB472FFE8A6
      66FFF8EDE5FFFCFCFCFFF9EFE6FFE9AD67FFEAB472FFE8A75EFFEDC9AFFFF6F6
      F6FFF3F3F3FFF2F2F2FFFCFCFCFF2C2C2C3600000000611501C0E2825FFFCF68
      46FFCF6846FFFFFFFFFFF8F8F8FFEBEBEBFFE2E2E2FFE0E0E0FFE1E1E1FFAD45
      23FFB04926FFC25E3BFF611501C0000000000000000000000000000000000000
      0000001317BB167484FE78E6F7FF168293FF3F5157FF494949FF454545FF0000
      00D5D0C2C2FF0909099401010132000000000000000000000000A6A6A6EAF6F6
      F6FFEBEBEBFFDEDEDEFFD6D6D6FFD5D5D5FFD1D1D1FFC3C3C3FFBCBCBCFFC0C0
      C0FFE5E5E5FF959595EA00000000000000000000000014131313B88854D2EBB5
      73FFE8A866FFFCFCFCFFEBB46BFFECBA77FFEAAF71FFF6EFEAFFF5F5F5FFF2F2
      F2FFEFEFEFFFEDEDEDFFFCFCFCFF2C2C2C3600000000621701BFE88A67FFD66F
      4DFFD66F4DFFD66F4DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD66F4DFFD66F
      4DFFD66F4DFFE07D5BFF621701BF000000000000000000000000000000000007
      0944005260DE78E6F7FF1C8899FF3F656CFF5B5B5BFF616161FF000000D30404
      04660F0F0F7DD8CDCDFF0C0C0C850101012C0000000000000000A8A8A8EAF7F7
      F7FFE7E7E7FFEFEFEFFFF6F6F6FFFBFBFBFFFAFAFAFFF0F0F0FFDEDEDEFFC3C3
      C3FFE6E6E6FF979797EA0000000000000000000000000202020135343342EAB2
      72FFE8AE63FFFCFCFCFFECB668FFEBB673FFF7F1ECFFF5F5F5FFF1F1F1FFECEC
      ECFFEAEAEAFFE6E6E6FFFCFCFCFF2C2C2C3600000000631801BFEF906DFFDF78
      56FFDF7856FFDF7856FFDF7856FFFFFFFFFFFFFFFFFFDF7856FFDF7856FFDF78
      56FFDF7856FFE88764FF631801BF000000000000000000000000000405410045
      52BE78E6F7FF066B7EF351777DFF646464FF6B6B6BFF000000D00202026D0000
      00000202022B14141477E1DADAFF141414750000000000000000AAAAAAEAF8F8
      F8FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFEAEAEAFF999999EA000000000000000000000000020202012C2C2C36FAF5
      F0FFF9F9F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFF2C2C2C3600000000531401ADE08765F7E781
      5EFFE7815EFFE7815EFFE7815EFFD8714FFFD8714FFFE7815EFFE7815EFFE781
      5EFFE7815EFFDC805EF7531401AD00000000000000000000002E0808087D78E6
      F7FF004B58B50016198C000000CE727272FF000000CED0C2C2FF090909940101
      01320000000002020229181818710303032700000000000000008B8B8BBFE1E1
      E1FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFB
      FBFFCFCFCFFF6A6A6AA1000000000000000000000000020202012C2C2C36FCFC
      FCFFF7F7F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFC
      FCFFF6F6F6FFF4F4F4FF717171911D1D1D20000000002008006B903D22D3E188
      67F7F69875FFF69875FFF69875FFF69875FFF59875FFF59774FFF59774FFF596
      73FFE08664F78F3C21D32008006B0000000000000C460707158EF7F7F7FF0D0D
      0D6C00090B3F0000000000000048000000CD040404640F0F0F7DD8CDCDFF0C0C
      0C850101012C00000000000000000000000000000000000000001F1F1F2A9999
      99D1D0D0D0FFE8E8E8FFF3F3F3FFFDFDFDFFFCFCFCFFEDEDEDFFE0E0E0FFC2C2
      C2FF848484C31212121B000000000000000000000000000000002C2C2C36F9F9
      F9FDF4F4F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFC
      FCFFE7E7E7FF6F6F6F911D1D1D2003030302000000000000000B1F08006A5315
      01AC661A01BD661A01BD661A01BD661A01BD661A01BD661A01BD661A01BD661A
      01BD531501AC1F08006A0000000B00000000000065C18080FFFF0D0D1A7C0101
      012400000000000000000000000000000000000000000202022B14141477E1DA
      DAFF141414750000000000000000000000000000000000000000000000000505
      05073E3E3E557F7F7FAE9A9A9AD6B4B4B4FBB1B1B1F9909090CD777777A93131
      31460202020300000000000000000000000000000000000000002A2A2A33EAEA
      EAF0F9F9F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8
      F8FF6F6F6F911D1D1D2003030302000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000C40000068AF020207390000
      0000000000000000000000000000000000000000000000000000020202291818
      1871030303270000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001A1A1A1C2A2A
      2A332C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C2C362C2C
      2C361D1D1D200303030200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202036868
      68CC7C7C7CFF777777FF727272FF6E6E6EFF535353CA00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C000012730000
      31BA00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC00003ACC0000
      3ACC000031BA000012730000000C000000000101015902020277020202770202
      0277020202770202027702020277020202770202027702020277020202770202
      02770202027702020277010101590000000000000000000000000C0C0C159191
      91FFD5D3D3FFE2E0DFFFDFDCDBFFE1DFDFFF696969F400000000000000000000
      000000000000000000000000000000000000000000000000000C160000723A02
      00B8480300CB480300CB480300CB480300CB480300CB480300CB480300CB4803
      00CB3A0200B8160000720000000C00000000000000000000167306066DDD0F0F
      C2F91010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010D9FF1010
      D9FF0E0EC2F905056DDD000016730000000006060674EBEBEBFFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE7E7E7FFEBEBEBFF06060674000000000000000000000000000000003E3E
      3E6F666666BDC3C1C0FFD4CFCEFF757575F64848489C626262D2727272FF6E6E
      6EFF696969FF656565FF616161FF4F4F4FD700000000180200707E2512DBDF62
      38F8FA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA7341FFFA73
      41FFDF6237F87E2411DB180200700000000000000000000042BA1414C1F91010
      D1FF1010D1FF1010B6FF1010D1FF1010D1FF1010D1FF1010D1FF1010B6FF1010
      D1FF1010D1FF0E0EBEF9000042BA000000000B0B0B70E9E9E9FF59CE59FF63E5
      63FF63E563FFE1E1E1FFDDB159FFF5C463FFF5C463FFE1E1E1FFDD5959FFF563
      63FFF56363FFE9E9E9FF0B0B0B70000000005B5B5B8D818181CC7C7C7CCC7878
      78CC7D7D7DDE9D9D9DFF999999FF7D7D7DF8838383FF868685FF868584FFA19F
      9EFFD3CECDFFD3CECDFFE8E5E5FF626262FF00000000460B00B3DA6238F8F16E
      3DFFF16E3DFFF16E3DFFF16E3DFFCF5930FFCF5930FFF16E3DFFF16E3DFFF16E
      3DFFF16E3DFFD95E34F8460B00B30000000000000000000054CC1B1BCEFF1010
      C8FF1010B2FFDCDCDCFF1010B2FF1010C8FF1010C8FF1010B2FFEEEEEEFF1010
      B2FF1010C8FF1010C8FF000054CC000000000D0D0D6FEAEAEAFF5ED35EFF68EA
      68FF68EA68FFE3E3E3FFE1B65EFFFACA68FFFACA68FFE3E3E3FFE15E5EFFFA68
      68FFFA6868FFEAEAEAFF0D0D0D6F00000000ADADADFFE4E2E2FFD7D5D5FFD5D3
      D2FFD1CECDFFCAC3C2FFC8C2C1FFCDC9C8FFCCCACAFFCCCAC9FFD8D6D6FF7271
      71FFB8B0ADFFB6AEADFFD3CECDFF666666FF00000000581001C4E86D41FFE566
      39FFE56639FFE56639FFC9562EFFE8E8E8FFECECECFFC9562EFFE56639FFE566
      39FFE56639FFE56639FF581001C40000000000000000000057CC2121C8FF1010
      BEFFD1D1D1FFD6D6D6FFDCDCDCFF1010ADFF1010ADFFEAEAEAFFEEEEEEFFEEEE
      EEFF1010BEFF1111BEFF000057CC000000000E0E0E6EEDEDEDFF5CCB5CFF61D6
      61FF61D661FFE6E6E6FFD8B05CFFE5BA61FFE5BA61FFE6E6E6FFD85C5CFFE561
      61FFE56161FFEDEDEDFF0E0E0E6E00000000B3B3B3FFE0DDDDFFA6704BFFA670
      4BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFA6704BFFDAD5D4FF7676
      76FFB9B1B0FFB8B0AEFFD4CFCEFF6B6B6BFF000000005A1101C3DE6B41FFD85F
      34FFD85F34FFC3522BFFE0E0E0FFE4E4E4FFE8E8E8FFECECECFFC3522BFFD85F
      34FFD85F34FFD85F34FF5A1101C3000000000000000000005ACC3333C7FF1111
      B4FF1010B4FFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FFE6E6E6FFEAEAEAFF1010
      B4FF1010B4FF1313B6FF00005ACC000000000E0E0E6DEFEFEFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFEFEFEFFF0E0E0E6D00000000B9B9B9FFDEDBDBFFB47E58FFCE97
      6EFFD8AD90FFD9AE90FFD9AE90FFDAAE90FFD69F75FFA6704BFFD7D3D1FF7C7C
      7CFFBAB3B2FFBAB2B0FFD4D0CFFF717171FF000000005B1201C2D86E4AFFCB58
      2FFFBC4E28FFD7D7D7FFDCDCDCFFE0E0E0FFE4E4E4FFE8E8E8FFECECECFFBC4E
      28FFCB572EFFCC5A31FF5B1201C2000000000000000000005DCC4545CEFF2525
      B5FF1313ABFF1010AAFFD1D1D1FFD6D6D6FFDCDCDCFFE2E2E2FF1010AAFF1010
      AAFF1010AAFF1717B0FF00005DCC000000001010106CF1F1F1FF59DDB1FF63F5
      C4FF63F5C4FFECECECFFDDDDDDFFF5F5F5FFF5F5F5FFECECECFFDD59B1FFF563
      C4FFF563C4FFF1F1F1FF1010106C00000000BFBFBFFFDFDCDCFFB37D57FFCB94
      6CFFCD966DFFCF986FFFD19A70FFD29B72FFD49D73FFA6704BFFD7D4D3FF8383
      83FF39A040FF359335FFD6D1D0FF767676FF000000005C1301C1DA7856FFC85E
      3AFFD4D4D4FFD3D3D3FFCEB4ABFFDCDCDCFFE0E0E0FFD8BEB5FFE8E8E8FFECEC
      ECFFBE4F29FFC35530FF5C1301C1000000000000000000005FCC4949D2FF3232
      BBFF2D2DB8FF12129FFFCECECEFFD1D1D1FFD6D6D6FFDCDCDCFF10109EFF1010
      A1FF1010A1FF1C1CACFF00005FCC000000001010106BF3F3F3FF5EE1B6FF68FA
      CAFF68FACAFFEFEFEFFFE1E1E1FFFAFAFAFFFAFAFAFFEFEFEFFFE15EB6FFFA68
      CAFFFA68CAFFF3F3F3FF1010106B00000000C4C4C4FFE1DEDCFFB27B56FFC790
      69FFC9926BFFCB946CFFCD966EFFCF986FFFD19A71FFA6704BFFD9D5D4FF8A8A
      8AFF9FC8A3FF4EA854FFD7D3D1FF7C7C7CFF000000005E1501C1DD7C5AFFCC65
      43FFF8F8F8FFD7BDB4FFB24825FFD7D7D7FFDCDCDCFFB14724FFD8BEB5FFE8E8
      E8FFB44925FFBD5431FF5E1501C10000000000000000000062CC4F4FD8FF3636
      BFFF2222ABFFFFFFFFFFF7F7F7FFE8E8E8FFDEDEDEFFDBDBDBFFDDDDDDFF1010
      9BFF1515A0FF2A2AB5FF000062CC000000001111116AF6F6F6FF5CD8B0FF61E5
      BAFF61E5BAFFF2F2F2FFD8D8D8FFE5E5E5FFE5E5E5FFF2F2F2FFD85CB0FFE561
      BAFFE561BAFFF6F6F6FF1111116A00000000CACACAFFE1DFDEFFB07A55FFC38D
      66FFC58F68FFC89169FFCA936BFFCC956DFFCE976EFFA6704BFFDBD6D6FF9090
      90FFC0BAB8FFBFB8B7FFD7D3D3FF838383FF00000000611501C0E2825FFFCF68
      46FFCF6846FFCF6846FFC35C3AFFEBEBEBFFE2E2E2FFAE4725FFAD4523FFAD45
      23FFB04926FFC25E3BFF611501C00000000000000000000064CC5959E2FF4141
      CAFFFFFFFFFFFFFFFFFFFFFFFFFF4141CAFF4141CAFFFFFFFFFFFFFFFFFFFFFF
      FFFF4141CAFF4D4DD6FF000064CC0000000012121269F8F8F8FFF5F5F5FFF5F5
      F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5F5FFF5F5
      F5FFF5F5F5FFF8F8F8FF1212126900000000CECECEFFE2DFDFFFAF7954FFB079
      54FFB07A55FFB17B56FFB27C56FFB37D57FFB47E58FFA6704BFFDCD8D7FF9797
      97FF8C8C8CFF898989FFD9D5D4FF8A8A8AFF00000000621701BFE88A67FFD66F
      4DFFD66F4DFFD66F4DFFCC6543FFFFFFFFFFFFFFFFFFCC6543FFD66F4DFFD66F
      4DFFD66F4DFFE07D5BFF621701BF0000000000000000000068CC6161EAFF4E4E
      D7FF4E4ED7FFFFFFFFFF4E4ED7FF4E4ED7FF4E4ED7FF4E4ED7FFFFFFFFFF4E4E
      D7FF4E4ED7FF5959E2FF000068CC0000000012121269FAFAFAFF59B1DDFF63C4
      F5FF63C4F5FFF8F8F8FF5959DDFF6363F5FF6363F5FFF8F8F8FFB159DDFFC463
      F5FFC463F5FFFAFAFAFF1212126900000000D3D3D3FFF1EFEFFFE2DFDFFFE2DF
      DFFFE1DFDEFFE1DEDDFFE0DDDCFFDFDCDBFFDEDBDBFFDEDBD9FFEDECEBFF9D9D
      9DFFC4BEBDFFC2BCBAFFDAD6D5FF909090FF00000000631801BFEF906DFFDF78
      56FFDF7856FFDF7856FFD26B49FFFFFFFFFFFFFFFFFFD26B49FFDF7856FFDF78
      56FFDF7856FFE88764FF631801BF0000000000000000000057BA5B5BE0F95A5A
      E3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5AE3FF5A5A
      E3FF5A5AE3FF5656DCF9000057BA0000000013131368FCFCFCFF5EB6E1FF68CA
      FAFF68CAFAFFFBFBFBFF5E5EE1FF6868FAFF6868FAFFFBFBFBFFB65EE1FFCA68
      FAFFCA68FAFFFCFCFCFF1313136800000000D1D1D1F9D3D3D3FFD0D0D0FFCCCC
      CCFFC8C8C8FFC2C2C2FFBEBEBEFFB9B9B9FFB4B4B4FFAFAFAFFFA9A9A9FFA4A4
      A4FF939292FF919090FFDBD7D6FF979797FF00000000531401ADE08765F7E781
      5EFFE7815EFFE7815EFFE7815EFFD8714FFFD8714FFFE7815EFFE7815EFFE781
      5EFFE7815EFFDC805EF7531401AD0000000000000000000022731F1F95DD5E5E
      E3F96C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6C6CF5FF6B6B
      F4FF5D5DE2F91F1F94DD000022730000000013131367FEFEFEFF5CB0D8FF61BA
      E5FF61BAE5FFFDFDFDFF5C5CD8FF6161E5FF6161E5FFFDFDFDFFB05CD8FFBA61
      E5FFBA61E5FFFEFEFEFF13131367000000000000000000000000000000000000
      00000000000039393948B6B6B6E4DFDFDFFFEAEAEAFFCFCAC9FFCBC5C4FFCAC3
      C2FFC8C2C0FFC7C0C0FFDCD9D8FF9D9D9DFF000000002008006B903D22D3E188
      67F7F69875FFF69875FFF69875FFF69875FFF59875FFF59774FFF59774FFF596
      73FFE08664F78F3C21D32008006B00000000000000000000000C000022730000
      5ABA00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC00006DCC0000
      6DCC00005ABA000022730000000C0000000014141467FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF14141467000000000000000000000000000000000000
      000000000000000000000C0C0C0F888888ABDCDCDCFFE6E3E3FFE1DEDCFFDFDC
      DCFFDFDCDBFFDEDBDBFFEEECECFFA4A4A4FF000000000000000B1F08006A5315
      01AC661A01BD661A01BD661A01BD661A01BD661A01BD661A01BD661A01BD661A
      01BD531501AC1F08006A0000000B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B0B0B4D14141466141414661414
      1466141414661414146614141466141414661414146614141466141414661414
      146614141466141414660B0B0B4D000000000000000000000000000000000000
      0000000000000000000000000000000000007F7F7F9FC6C6C6FBC3C3C3FFBFBF
      BFFFBABABAFFB5B5B5FFAFAFAFFF6E6E6EA70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000510000
      00E70101018A0101018A0101018A0101018A0101018A0101018A000000E70000
      00E7000000E7000000E7000000AD000000000000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000040101
      4BAC01014BAC0000000400000000000000000000000000000000000000000000
      0000000000000105010A00020007000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C001200730031
      00BA013A00CC013A00CC013A00CC013A00CC013A00CC013A00CC013A00CC013A
      00CC003100BA001200730000000C00000000000000000000004C000000DB2B2B
      2BF7B9ABABFF424242FF424242FFB5A7A7FFB5A7A7FFB9ABABFF424242FF5454
      54FF515151FF676767FF000000DB000000000000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000004030355B61111
      A1F51111A1F5030355B600000004000000000000000000000000000000000000
      00000105010A35823BFF2F7734FB000200070000000000000000000000000000
      00000000000000000000000000000000000000000000011600730D6D06DD1EC2
      0FF921D910FF21D910FF21D910FF21D910FF21D910FF21D910FF21D910FF21D9
      10FF1DC20EF90C6D05DD011600730000000000000000000000CE656565FF3E3E
      3EFFBAB1B1FF3E3E3EFF3E3E3EFFB1A8A8FFB1A8A8FFBAB1B1FF3E3E3EFF5454
      54FF494949FF696969FF000000CE000000000000000005054DAC1616A4F53A3A
      C8FF3A3AC8FF1616A5F5050556B60000000400000004050556B61616A5F53B3B
      C8FF3A3AC8FF1616A5F505054DAC000000000000000000000000000000000306
      030A3E8D45FF52A25AFF4D9E55FF307A36FE0104020800000000000000000000
      00000000000000000000000000000000000000000000054200BA23C014F921D1
      10FF21D110FF21D110FF21D110FF21B610FF21B610FF21D110FF21D110FF21D1
      10FF21D110FF1EBE0EF9054200BA0000000000000000000000C9636363FF3A3A
      3AFFC0BBBBFF262626FF262626FFB7B2B2FFB7B2B2FFC0BBBBFF3A3A3AFF5454
      54FF414141FF6B6B6BFF000000C9000000000000000006064EAC1A1AA7F54444
      CDFF4040CCFF4545CDFF1A1AA7F5070757B6070757B61A1AA7F54545CDFF4141
      CCFF4444CDFF1A1AA7F506064EAC0000000000000000000000000306030A4799
      4FFF59AB62FF75CA81FF72C87CFF4F9F57FF317B37FE01040208000000000000
      00000000000000000000000000000000000000000000075400CC2ACC19FF21C8
      10FF21C810FF21C810FF21BC10FFE8E8E8FFECECECFF21BC10FF21C810FF21C8
      10FF21C810FF21C810FF075400CC0000000000000000000000C5666666FF3636
      36FFC8C7C7FFC3C2C2FFC3C2C2FFC3C2C2FFC3C2C2FFC8C7C7FF363636FF5454
      54FF393939FF6E6E6EFF000000C5000000000000000000000004090958B61F1F
      ABF54F4FD1FF4949D0FF4F4FD1FF2121A9F32121A9F34F4FD1FF4949D0FF4F4F
      D1FF1F1FABF5090958B60000000400000000000000000307030A4FA558FF61B4
      6BFF7CCE88FF79CC86FF74CA80FF74C980FF50A158FF327C38FE010402080000
      00000000000000000000000000000000000000000000075700CC30C61FFF21BE
      10FF21BE10FF21BE10FF21B510FFE4E4E4FFE8E8E8FF21B510FF21BE10FF21BE
      10FF21BE10FF22BE11FF075700CC0000000000000000000000C16A6A6AFF3333
      33FF323232FF323232FF323232FF323232FF323232FF323232FF333333FF3333
      33FF333333FF727272FF000000C1000000000000000000000000000000040A0A
      59B62525ADF55858D6FF5151D4FF5A5AD7FF5A5AD7FF5252D4FF5858D6FF2525
      ADF50A0A59B60000000400000000000000000206030957AF61FF69BC74FF83D2
      8FFF78C984FF5EB168FF61B36BFF76C982FF76CB81FF51A25AFF327C38FD0204
      02080000000000000000000000000000000000000000085A00CC40C32FFF22B4
      11FF21A810FF21A810FF21A410FFE0E0E0FFE4E4E4FF21A410FF21A810FF21A8
      10FF21B410FF24B513FF085A00CC0000000000000000000000BE6D6D6DFF6464
      64FF646464FF646464FF646464FF646464FF646464FF646464FF646464FF6464
      64FF646464FF6D6D6DFF000000BE000000000000000000000000000000000000
      00040C0C5BB62B2BAEF36464DBFF6262DBFF6262DBFF6464DBFF2B2BAEF30C0C
      5BB60000000400000000000000000000000019331C464FA159E477C985FF7ECE
      8CFF4EA357FC2046246F27542B8B5AAC65FF7ACC85FF77CB84FF52A35BFF337E
      39FC0204020800000000000000000000000000000000095D00CC51C940FF32AF
      21FFD4D4D4FFD3D3D3FFD7D7D7FFDCDCDCFFE0E0E0FFE4E4E4FFE8E8E8FFECEC
      ECFF21A610FF27AF16FF095D00CC0000000000000000000000BB717171FFD4D4
      C9FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4E4FFF4F4
      E4FFD4D4C9FF717171FF000000BB000000000000000000000000000000000000
      00040E0E5DB63030B2F36D6DDFFF6969DFFF6969DFFF6D6DDFFF3131B2F30E0E
      5DB60000000400000000000000000000000000000000162C183C51A35AE66BBF
      77FF244A286F000000000000000029592D915CAD66FF7BCD88FF7ACD86FF54A4
      5DFF347E3AFC020402080000000000000000000000000A5F00CC54CC43FF3BB3
      2AFFF8F8F8FFE1E1E1FFD5D5D5FFD7D7D7FFDCDCDCFFE0E0E0FFE4E4E4FFE8E8
      E8FF21A010FF2BAA1AFF0A5F00CC0000000000000000000000B8747474FFF6F6
      E9FFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECECDFFFECEC
      DFFFF6F6E9FF747474FF000000B8000000000000000000000000000000040F0F
      5DB63333B5F57575E3FF6969E1FF7676E4FF7676E4FF6868E1FF7575E3FF3333
      B5F50F0F5DB6000000040000000000000000000000000000000018301C432348
      27650000000000000000000000000000000029592F915DAE67FF7DCE89FF7CCE
      88FF55A55EFF357F3BFC0204020800000000000000000B6200CC59D148FF46BE
      35FF3DB52CFF3DB52CFF36AE25FFEBEBEBFFE2E2E2FF249D13FF229B11FF229B
      11FF269F15FF38B127FF0B6200CC0000000000000000000000B5787878FFF8F8
      EFFFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1E7FFF1F1
      E7FFF8F8EFFF787878FF000000B500000000000000000000000411115FB63737
      B9F67D7DE6FF7070E4FF7E7EE6FF3A3AB7F43A3AB7F47E7EE6FF7171E4FF7D7D
      E6FF3737B8F511115FB600000004000000000000000000000000000000000000
      00000000000000000000000000000000000000000000295A2F915EAF68FF80CF
      8CFF7DCF8AFF56A65FFF37843EFF02040208000000000B6400CC5FD74EFF4DC5
      3CFF4DC53CFF4DC53CFF43BB32FFFFFFFFFFFFFFFFFF43BB32FF4DC53CFF4DC5
      3CFF4DC53CFF57CF46FF0B6400CC0000000000000000000000B27B7B7BFFFBFB
      F5FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6F0FFF6F6
      F0FFFBFBF5FF7B7B7BFF000000B20000000000000000111155AC3D3DBBF68686
      E7FF7777E5FF8888E7FF3E3EBBF613135FB613135FB63E3EBBF68888E7FF7878
      E5FF8787E7FF3D3DBBF6121255AC000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002B5A2F9160B1
      6AFF81D18EFF78C884FF55A55EFF1A3F1D7B000000000B6800CC66DE55FF56CE
      45FF56CE45FF56CE45FF49C138FFFFFFFFFFFFFFFFFF49C138FF56CE45FF56CE
      45FF56CE45FF5FD74EFF0B6800CC0000000000000000000000B07D7D7DFFFEFE
      FBFFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFBF8FFFBFB
      F8FFFEFEFBFF7D7D7DFF000000B00000000000000000131356AC4242BDF68F8F
      E9FF9090E9FF4343BDF6151561B60000000400000004151561B64343BDF69090
      E9FF8F8FE9FF4242BDF6131356AC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002B5B
      2F9161B26BFF5DAE67FF1D44227900000000000000000A5700BA5FD44EF95ED6
      4DFF5ED64DFF5ED64DFF5ED64DFF4FC73EFF4FC73EFF5ED64DFF5ED64DFF5ED6
      4DFF5ED64DFF5BD04AF90A5700BA0000000000000000000000AE848484FFFFFF
      FFFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFFFEFFFFFF
      FEFFFFFFFFFF848484FF000000AE000000000000000000000004161662B64646
      BEF64747BEF6161662B600000004000000000000000000000004171762B64848
      BFF64848BEF6171762B600000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002B5C3091234E277F0000000000000000000000000422007329911ADD61D5
      4FF96DE55CFF6DE55CFF6DE55CFF6DE55CFF6CE45BFF6CE45BFF6CE45BFF6CE4
      5BFF5FD54EF9289019DD04220073000000000000000000000081000000AC1414
      0D6614140D6614140D6614140D6614140D6614140D6614140D6614140D661414
      0D6614140D66000000AC00000081000000000000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000041616
      58AC161658AC0000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000C042200730A5A
      00BA0D6D00CC0D6D00CC0D6D00CC0D6D00CC0D6D00CC0D6D00CC0D6D00CC0D6D
      00CC0A5A00BA042200730000000C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500000000100010000000000800200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF800000000000000000000000000000
      C000000000000000C000000000000000C000000000000000C000000000000000
      800000000000000000000000000000008000000000000000C000000000000000
      C000000000000000C000000000000000C000000000000000C000000000000000
      FFE0000000000000FFF800000000000000000000000000000000000000000000
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
      000000000000}
  end
end

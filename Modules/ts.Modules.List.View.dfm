inherited frmCustomList: TfrmCustomList
  ClientHeight = 462
  ClientWidth = 746
  ExplicitWidth = 754
  ExplicitHeight = 489
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFooter: TPanel [0]
    Left = 0
    Top = 415
    Width = 746
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      746
      47)
    object grpFooterLine: TGroupBox
      Left = 8
      Top = 0
      Width = 731
      Height = 7
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object pnlButtons: TFlowPanel
      Left = 0
      Top = 13
      Width = 746
      Height = 34
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      FlowStyle = fsBottomTopRightLeft
      Padding.Left = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 1
      DesignSize = (
        746
        34)
      object btnClose: TJvImgBtn
        AlignWithMargins = True
        Left = 589
        Top = 2
        Width = 150
        Height = 25
        Action = actClose
        Anchors = [akRight, akBottom]
        BiDiMode = bdLeftToRight
        Caption = 'Sluiten                   (F9)'
        ImageIndex = 2
        Images = imlMain
        ParentBiDiMode = False
        TabOrder = 0
        Alignment = taLeftJustify
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'Tahoma'
        HotTrackFont.Style = []
        Margin = 4
        Spacing = 10
      end
    end
  end
  object pnlGrid: TPanel [1]
    Left = 0
    Top = 65
    Width = 746
    Height = 350
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object grdMain: TDBGridView
      Left = 8
      Top = 6
      Width = 731
      Height = 319
      Align = alCustom
      AlwaysSelected = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      CheckBoxes = True
      CheckStyle = cs3D
      ColumnClick = True
      ColumnsFullDrag = True
      CursorKeys = [gkArrows, gkTabs, gkReturn, gkMouse, gkMouseWheel]
      DataSource = dscMain
      DoubleBuffered = True
      GridLines = False
      Header.FullSynchronizing = True
      Header.Synchronized = True
      ParentShowHint = False
      PopupMenu = ppmMain
      ShowCellTips = True
      ShowHint = True
      TabOrder = 0
      FitColsToClient = False
      MultiSelect = False
      OnCellAcceptCursor = grdMainCellAcceptCursor
      OnChanging = grdMainChanging
      OnCheckClick = grdMainCheckClick
      OnClick = grdMainClick
      OnEditButtonPress = grdMainEditButtonPress
      OnEditCanModify = grdMainEditCanModify
      OnEnter = grdMainEnter
      OnGetCellColors = grdMainGetCellColors
      OnGetCellReadOnly = grdMainGetCellReadOnly
      OnGetCellText = grdMainGetCellText
      OnKeyPress = grdMainKeyPress
      OnRowMultiSelect = grdMainRowMultiSelect
      OnClearMultiSelect = grdMainClearMultiSelect
    end
  end
  object pnlHeader: TPanel [2]
    Left = 0
    Top = 0
    Width = 746
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBusy: TPanel [3]
    Left = 32
    Top = 316
    Width = 217
    Height = 65
    TabOrder = 2
    Visible = False
    object lblBusy: TLabel
      Left = 1
      Top = 1
      Width = 183
      Height = 63
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Layout = tlCenter
    end
    object aniBusy: TAnimate
      Left = 184
      Top = 1
      Width = 32
      Height = 63
      Align = alRight
      AutoSize = False
      BorderWidth = 10
      CommonAVI = aviFindFile
      StopFrame = 8
    end
  end
  inherited aclMain: TActionList
    Left = 456
    Top = 384
    inherited actClose: TAction
      Caption = 'Sluiten'
      Hint = 'Applicatie afsluiten'
    end
    object actExecute: TAction
      Caption = 'Lijst ophalen'
      Hint = 'Haal de lijstgegevens op.'
      ImageIndex = 4
      ShortCut = 116
      OnExecute = actExecuteExecute
    end
    object actExecuteShowSQL: TAction
      Caption = 'SQL lijst ophalen'
      ImageIndex = 4
      ShortCut = 49268
      OnExecute = actExecuteShowSQLExecute
    end
    object actClear: TAction
      Caption = 'Wis criteria'
      Hint = 'Lijstcriteria wissen.'
      ImageIndex = 3
    end
    object actInsertRecord: TAction
      Caption = 'Record invoegen'
      ShortCut = 118
      OnExecute = actInsertRecordExecute
    end
    object actDeleteRecord: TAction
      Caption = 'Record verwijderen'
      ShortCut = 117
      OnExecute = actDeleteRecordExecute
    end
    object actToggleGridFocus: TAction
      ShortCut = 113
      OnExecute = actToggleGridFocusExecute
    end
  end
  inherited imlMain: TImageList
    Left = 552
    Top = 384
  end
  object dscMain: TDataSource
    Left = 392
    Top = 384
  end
  object ppmMain: TPopupMenu
    Left = 360
    Top = 384
    object mniExecute: TMenuItem
      Action = actExecute
    end
    object mniN1: TMenuItem
      Caption = '-'
    end
    object mniClose: TMenuItem
      Caption = 'Sluiten'
      Hint = 'Applicatie afsluiten.'
      ImageIndex = 1
      ShortCut = 120
      OnClick = actCloseExecute
    end
  end
end

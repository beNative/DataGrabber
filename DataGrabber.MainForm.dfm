object frmMain: TfrmMain
  Left = 350
  Top = 119
  Caption = 'DataGrabber'
  ClientHeight = 593
  ClientWidth = 1082
  Color = clBtnFace
  TransparentColorValue = clGray
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Calibri'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object tlbMain: TToolBar
    Left = 0
    Top = 0
    Width = 1082
    Height = 19
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 70
    Color = clBtnFace
    Customizable = True
    DrawingStyle = dsGradient
    EdgeBorders = [ebBottom]
    EdgeInner = esNone
    EdgeOuter = esNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GradientEndColor = clBtnFace
    GradientStartColor = clBtnFace
    List = True
    GradientDirection = gdHorizontal
    GradientDrawingOptions = [gdoGradient]
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 0
    Transparent = False
    OnCustomDraw = tlbMainCustomDraw
  end
  object pnlStatusBar: TPanel
    Left = 0
    Top = 572
    Width = 1082
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlStatus: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 48
      Height = 19
      Hint = 'Status'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditMode: TPanel
      AlignWithMargins = True
      Left = 467
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Data mode'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlElapsedTime: TPanel
      AlignWithMargins = True
      Left = 311
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Elapsed time'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 2
    end
    object pnlProviderMode: TPanel
      AlignWithMargins = True
      Left = 415
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Provider mode'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 3
    end
    object pnlConnectionType: TPanel
      AlignWithMargins = True
      Left = 363
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Connection type'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      PopupMenu = ppmConnectionTypes
      TabOrder = 4
    end
    object pnlGridType: TPanel
      AlignWithMargins = True
      Left = 519
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Grid type'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      PopupMenu = ppmGridTypes
      TabOrder = 5
    end
    object pnlConnectionStatus: TPanel
      AlignWithMargins = True
      Left = 51
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Connection status'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 6
    end
    object pnlRecordCount: TPanel
      AlignWithMargins = True
      Left = 103
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Recordcount'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 7
    end
    object pnlFieldCount: TPanel
      AlignWithMargins = True
      Left = 155
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Field count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 8
    end
    object pnlConstantFieldsCount: TPanel
      AlignWithMargins = True
      Left = 259
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Constant field count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 9
    end
    object pnlEmptyFieldsCount: TPanel
      AlignWithMargins = True
      Left = 207
      Top = 1
      Width = 50
      Height = 19
      Hint = 'Empty fields count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 10
    end
  end
  object pnlConnectionViews: TPanel
    Left = 0
    Top = 19
    Width = 1082
    Height = 553
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
  object aclMain: TActionList
    Left = 80
    Top = 8
    object actAddConnectionView: TAction
      Caption = 'Add connectionview'
      ShortCut = 16462
      OnExecute = actAddConnectionViewExecute
    end
  end
  object ppm: TPopupMenu
    Left = 16
    Top = 8
    object mniGroupBySelection: TMenuItem
      Caption = 'Group by selected columns'
      ShortCut = 16455
    end
    object mniHideSelectedColumns: TMenuItem
      Caption = 'Hide selected'
      ShortCut = 16456
    end
    object mniHideEmptyColumns: TMenuItem
      AutoCheck = True
      Caption = 'Hide empty'
      ShortCut = 16453
    end
    object mniHideConstantColumns: TMenuItem
      AutoCheck = True
      Caption = 'Hide constant'
      ShortCut = 16459
    end
    object mniShowAllColumns: TMenuItem
      Caption = 'Show All'
    end
    object mniN1: TMenuItem
      Caption = '-'
    end
    object mniMergeColumns: TMenuItem
      AutoCheck = True
      Caption = 'Merge cells'
      ShortCut = 49229
    end
    object mniN2: TMenuItem
      Caption = '-'
    end
    object mniCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
    end
    object mniCopyWikiTable: TMenuItem
      Caption = 'Selection as Wiki'
    end
    object mniCopyTextTable: TMenuItem
      Caption = 'Selection as text'
    end
    object mniSelectionAsTextTable: TMenuItem
      Caption = 'Selection as texttable'
    end
    object mniSelectionAsCommaText: TMenuItem
      Caption = 'Selection as commatext'
    end
    object mniSelectionAsQuotedCommaText: TMenuItem
      Caption = 'Selection as quoted commatext'
    end
    object mniSelectionAsFields: TMenuItem
      Caption = 'Selection as fields'
    end
    object mniSelectionAsQuotedFields: TMenuItem
      Caption = 'Selection as quoted fields'
    end
    object mniN4: TMenuItem
      Caption = '-'
    end
    object mniFormatSQL: TMenuItem
      Caption = 'Format SQL'
      ShortCut = 119
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mniInspectConnection: TMenuItem
      Caption = 'Inspect connection'
    end
    object mniInspectDataSet: TMenuItem
      Caption = 'Inspect dataset'
    end
    object mniInspectFields: TMenuItem
      Caption = 'Inspect fields'
    end
    object mniInspectGrid: TMenuItem
      Caption = 'Inspect grid'
    end
    object mniN3: TMenuItem
      Caption = '-'
    end
    object mniSettings: TMenuItem
      Caption = 'Settings'
    end
  end
  object ppmConnectionTypes: TPopupMenu
    Left = 16
    Top = 88
    object mniADO: TMenuItem
      AutoCheck = True
      Caption = 'ADO'
    end
    object mniDBX: TMenuItem
      AutoCheck = True
      Caption = 'DBX'
    end
    object mniZEOS: TMenuItem
      AutoCheck = True
      Caption = 'ZEOS'
    end
    object mniUNI: TMenuItem
      AutoCheck = True
      Caption = 'UNIDAC'
    end
  end
  object ppmGridTypes: TPopupMenu
    Left = 128
    Top = 88
    object mnicxGrid: TMenuItem
      AutoCheck = True
      Caption = 'TcxGrid'
    end
    object mniGridView: TMenuItem
      AutoCheck = True
      Caption = 'TGridView'
    end
    object mniKGrid: TMenuItem
      AutoCheck = True
      Caption = 'TKGrid'
    end
    object mniVirtualDBGrid: TMenuItem
      AutoCheck = True
      Caption = 'VirtualDBGrid'
    end
  end
end

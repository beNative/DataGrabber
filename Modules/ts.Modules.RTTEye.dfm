object frmRTTEye: TfrmRTTEye
  Left = 350
  Top = 170
  ActiveControl = btnLoad
  Caption = 'RTTEye'
  ClientHeight = 517
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 413
    Height = 485
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object splVertical: TSplitter
      Left = 398
      Top = 5
      Width = 10
      Height = 475
      Align = alRight
      ExplicitLeft = 732
      ExplicitTop = 21
    end
    object tvRtti: TTreeView
      Left = 5
      Top = 5
      Width = 393
      Height = 475
      Align = alClient
      BevelOuter = bvNone
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      Indent = 19
      ParentDoubleBuffered = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      OnChange = tvRttiChange
      OnCustomDrawItem = tvRttiCustomDrawItem
      OnDblClick = tvRttiDblClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 485
    Width = 748
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    DesignSize = (
      748
      32)
    object btnLoad: TButton
      Left = 0
      Top = 1
      Width = 81
      Height = 27
      Action = actLoad
      TabOrder = 0
    end
    object btnExpand1: TButton
      Left = 87
      Top = 1
      Width = 81
      Height = 27
      Action = actExpand
      TabOrder = 1
    end
    object btnCollapse1: TButton
      Left = 174
      Top = 1
      Width = 81
      Height = 27
      Action = actCollapse
      TabOrder = 2
    end
    object EditSearch: TEdit
      Left = 421
      Top = 4
      Width = 234
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 3
    end
    object btnSearch: TButton
      Left = 661
      Top = 1
      Width = 81
      Height = 27
      Action = actSearch
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 4
    end
  end
  object lvRtti: TListView
    Left = 413
    Top = 0
    Width = 335
    Height = 485
    Align = alRight
    BevelOuter = bvNone
    Color = clBtnFace
    Columns = <
      item
        Caption = 'Name'
        Width = 162
      end
      item
        Caption = 'Value'
        Width = 162
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object aclMain: TActionList
    Left = 384
    Top = 128
    object actLoad: TAction
      Caption = 'Load'
      OnExecute = actLoadExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      OnExecute = actCollapseExecute
    end
    object actExpand: TAction
      Caption = 'Expand'
      OnExecute = actExpandExecute
    end
    object actSearch: TAction
      Caption = 'Search'
      OnExecute = actSearchExecute
    end
  end
end

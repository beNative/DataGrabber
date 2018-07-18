object frmConnectionView: TfrmConnectionView
  AlignWithMargins = True
  Left = 0
  Top = 0
  ClientHeight = 487
  ClientWidth = 906
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  ShowHint = True
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 906
    Height = 487
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = pnlTop
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = pnlBottom
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    SplitterSize = 8
    Align = alClient
    TabOrder = 0
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 906
      Height = 244
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splVertical: TSplitter
        Left = 185
        Top = 0
        Width = 8
        Height = 244
        OnMoved = splVerticalMoved
        ExplicitHeight = 232
      end
      object pnlVST: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 244
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlTopRight: TPanel
        Left = 193
        Top = 0
        Width = 713
        Height = 244
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 252
      Width = 906
      Height = 235
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
end

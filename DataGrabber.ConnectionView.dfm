object frmConnectionView: TfrmConnectionView
  Left = 0
  Top = 0
  ClientHeight = 487
  ClientWidth = 906
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
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
        Position = 0.300000000000000000
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
    SplitterSize = 4
    SplitterColor = clScrollBar
    SplitterHoverColor = clScrollBar
    Align = alClient
    BevelEdges = []
    TabOrder = 0
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 906
      Height = 146
      ParentCustomHint = False
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splVertical: TSplitter
        Left = 185
        Top = 0
        Width = 4
        Height = 146
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Color = clScrollBar
        ParentColor = False
        ResizeStyle = rsLine
        StyleElements = [seClient, seBorder]
        OnMoved = splVerticalMoved
        ExplicitHeight = 244
      end
      object pnlVST: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 146
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlTopRight: TPanel
        Left = 189
        Top = 0
        Width = 717
        Height = 146
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 150
      Width = 906
      Height = 337
      Align = alClient
      BevelEdges = []
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
end

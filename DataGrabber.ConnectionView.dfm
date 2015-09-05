object frmConnectionView: TfrmConnectionView
  Left = 0
  Top = 0
  ClientHeight = 612
  ClientWidth = 829
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splHorizontal: TSplitter
    Left = 0
    Top = 249
    Width = 829
    Height = 8
    Cursor = crVSplit
    Align = alTop
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 829
    Height = 249
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object splVertical: TSplitter
      Left = 209
      Top = 0
      Width = 8
      Height = 249
      ExplicitLeft = 204
    end
    object pnlProfiles: TPanel
      Left = 0
      Top = 0
      Width = 209
      Height = 249
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 217
      Top = 0
      Width = 612
      Height = 249
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 257
    Width = 829
    Height = 355
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object pnlGrid: TPanel
      Left = 0
      Top = 0
      Width = 829
      Height = 355
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pcGrid: TcxPageControl
        Left = 0
        Top = 0
        Width = 829
        Height = 355
        Align = alClient
        TabOrder = 0
        Properties.CustomButtons.Buttons = <>
        ClientRectBottom = 351
        ClientRectLeft = 4
        ClientRectRight = 825
        ClientRectTop = 4
      end
      object dxdckst1: TdxDockSite
        Left = 480
        Top = 48
        Width = 300
        Height = 200
        DockingType = 5
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object dsGrids: TdxDockSite
        Left = 0
        Top = 0
        Width = 829
        Height = 355
        Align = alClient
        DockingType = 5
        OriginalWidth = 829
        OriginalHeight = 355
        object ldsGrid: TdxLayoutDockSite
          Left = 0
          Top = 0
          Width = 829
          Height = 355
          DockingType = 0
          OriginalWidth = 300
          OriginalHeight = 200
        end
        object pnlGrid1: TdxDockPanel
          Left = 0
          Top = 0
          Width = 829
          Height = 355
          AllowFloating = False
          AutoHide = False
          Caption = 'pnlGrid1'
          CustomCaptionButtons.Buttons = <>
          ShowCaption = False
          TabsProperties.CloseButtonMode = cbmActiveTab
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 0
          OriginalWidth = 185
          OriginalHeight = 140
        end
      end
    end
  end
end

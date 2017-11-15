object frmConnectionView: TfrmConnectionView
  AlignWithMargins = True
  Left = 0
  Top = 0
  ClientHeight = 612
  ClientWidth = 829
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
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
      Left = 193
      Top = 0
      Width = 8
      Height = 249
      ExplicitLeft = 209
    end
    object pnlProfiles: TPanel
      Left = 0
      Top = 0
      Width = 193
      Height = 249
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlEditor: TPanel
      Left = 201
      Top = 0
      Width = 628
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
    end
  end
end

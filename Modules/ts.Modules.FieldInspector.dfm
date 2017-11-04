object frmFieldInspector: TfrmFieldInspector
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 575
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 185
    Top = 0
    Width = 8
    Height = 575
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 575
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    Left = 193
    Top = 0
    Width = 418
    Height = 575
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end

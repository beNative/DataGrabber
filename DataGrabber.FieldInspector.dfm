object frmFieldInspector: TfrmFieldInspector
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsSizeToolWin
  Caption = 'Field inspector'
  ClientHeight = 909
  ClientWidth = 1035
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  PixelsPerInch = 144
  TextHeight = 23
  object splVertical: TSplitter
    Left = 513
    Top = 0
    Width = 6
    Height = 909
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    MinSize = 45
    ExplicitLeft = 515
  end
  object pnlLeft: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 503
    Height = 899
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlRight: TPanel
    AlignWithMargins = True
    Left = 524
    Top = 5
    Width = 506
    Height = 899
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 16
    Top = 8
    object actInspect: TAction
      Caption = 'actInspect'
      ShortCut = 16457
      OnExecute = actInspectExecute
    end
  end
end

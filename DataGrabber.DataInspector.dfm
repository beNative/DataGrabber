object frmDataInspector: TfrmDataInspector
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsSizeToolWin
  Caption = 'Datainspector'
  ClientHeight = 338
  ClientWidth = 359
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 23
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 359
    Height = 338
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object dscMain: TDataSource
    OnDataChange = dscMainDataChange
    Left = 16
    Top = 8
  end
  object aclMain: TActionList
    Left = 128
    Top = 8
    object actHideEmptyFields: TAction
      AutoCheck = True
      Caption = 'Hide empty fields'
      Checked = True
      OnExecute = actHideEmptyFieldsExecute
    end
  end
  object ppmMain: TPopupMenu
    Left = 72
    Top = 8
    object mniHideEmptyFields: TMenuItem
      Action = actHideEmptyFields
      AutoCheck = True
    end
  end
end

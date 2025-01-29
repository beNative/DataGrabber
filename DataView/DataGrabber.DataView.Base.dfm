object BaseDataView: TBaseDataView
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'BaseDataView'
  ClientHeight = 242
  ClientWidth = 435
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object dscMain: TDataSource
    Left = 8
    Top = 16
  end
  object imlMain: TVirtualImageList
    AutoFill = True
    Images = <>
    ImageNameAvailable = False
    Left = 224
    Top = 32
  end
end

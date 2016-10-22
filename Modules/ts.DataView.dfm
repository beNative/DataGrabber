object frmDataView: TfrmDataView
  Left = 0
  Top = 0
  ClientHeight = 103
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object dscMain: TDataSource
    OnStateChange = dscMainStateChange
    OnDataChange = dscMainDataChange
    OnUpdateData = dscMainUpdateData
    Left = 16
    Top = 8
  end
end

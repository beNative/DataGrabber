object frmMetaData: TfrmMetaData
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 768
  ClientWidth = 1296
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 23
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 1296
    Height = 768
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    PanelCollection = <
      item
        Control = lstCatalogs
        Position = 0.250000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = lstSchemas
        Position = 0.500000000000000000
        Visible = True
        Index = 1
      end
      item
        Control = lstTables
        Position = 0.750000000000000000
        Visible = True
        Index = 2
      end
      item
        Control = lstFields
        Position = 1.000000000000000000
        Visible = True
        Index = 3
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    DesignSize = (
      1296
      768)
    object lstCatalogs: TListBox
      Left = 0
      Top = 0
      Width = 324
      Height = 768
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 23
      TabOrder = 0
      OnClick = lstCatalogsClick
    end
    object lstSchemas: TListBox
      Left = 327
      Top = 0
      Width = 321
      Height = 768
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 23
      TabOrder = 1
      OnClick = lstSchemasClick
      OnEnter = lstSchemasEnter
    end
    object lstTables: TListBox
      Left = 651
      Top = 0
      Width = 321
      Height = 768
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 23
      TabOrder = 2
      OnClick = lstTablesClick
    end
    object lstFields: TListBox
      Left = 975
      Top = 0
      Width = 321
      Height = 768
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 23
      TabOrder = 3
    end
  end
end

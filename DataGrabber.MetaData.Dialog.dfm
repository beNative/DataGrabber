object frmMetaData: TfrmMetaData
  Left = 0
  Top = 0
  ClientHeight = 512
  ClientWidth = 858
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 858
    Height = 512
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
      858
      512)
    object lstCatalogs: TListBox
      Left = 0
      Top = 0
      Width = 214
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstCatalogsClick
    end
    object lstSchemas: TListBox
      Left = 217
      Top = 0
      Width = 212
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 1
      OnClick = lstSchemasClick
      OnEnter = lstSchemasEnter
    end
    object lstTables: TListBox
      Left = 432
      Top = 0
      Width = 212
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 2
      OnClick = lstTablesClick
    end
    object lstFields: TListBox
      Left = 647
      Top = 0
      Width = 211
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 3
    end
  end
end

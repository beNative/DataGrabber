object frmMetaData: TfrmMetaData
  Left = 0
  Top = 0
  ClientHeight = 512
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 13
  object pnlMain: TOMultiPanel
    Left = 0
    Top = 0
    Width = 860
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
    ExplicitWidth = 854
    ExplicitHeight = 495
    DesignSize = (
      860
      512)
    object lstCatalogs: TListBox
      Left = 0
      Top = 0
      Width = 215
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstCatalogsClick
    end
    object lstSchemas: TListBox
      Left = 218
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
      Left = 433
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
      Left = 648
      Top = 0
      Width = 212
      Height = 512
      Anchors = []
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 3
    end
  end
end

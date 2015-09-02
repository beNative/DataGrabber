object dmModelData: TdmModelData
  OldCreateOrder = False
  Height = 274
  Width = 372
  object conMain: TZConnection
    ControlsCodePage = cCP_UTF16
    UTF8StringsAsWideField = True
    Catalog = ''
    HostName = ''
    Port = 0
    Database = 'DG.db'
    User = ''
    Password = ''
    Protocol = 'sqlite-3'
    Left = 48
    Top = 24
  end
  object tblModel: TZTable
    Connection = conMain
    TableName = 'Model'
    Left = 48
    Top = 88
  end
  object tblTable: TZTable
    Connection = conMain
    TableName = '"Table"'
    MasterFields = 'ID'
    MasterSource = dscModel
    LinkedFields = 'ModelID'
    Left = 168
    Top = 96
  end
  object tblField: TZTable
    Connection = conMain
    TableName = 'Field'
    MasterFields = 'ID'
    MasterSource = dscTable
    LinkedFields = 'TableID'
    Left = 288
    Top = 112
  end
  object dscModel: TDataSource
    DataSet = tblModel
    Left = 128
    Top = 64
  end
  object dscTable: TDataSource
    DataSet = tblTable
    Left = 240
    Top = 64
  end
end

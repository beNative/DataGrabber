object dmCustomModule: TdmCustomModule
  OldCreateOrder = True
  Height = 232
  Width = 357
  object dspMaster: TDataSetProvider
    Options = [poPropogateChanges, poAllowCommandText, poUseQuoteChar]
    UpdateMode = upWhereKeyOnly
    OnUpdateData = dspMasterUpdateData
    OnUpdateError = dspMasterUpdateError
    AfterUpdateRecord = dspMasterAfterUpdateRecord
    BeforeUpdateRecord = dspMasterBeforeUpdateRecord
    OnGetTableName = dspMasterGetTableName
    Left = 120
    Top = 24
  end
  object cdsMaster: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'cdsListField1'
        DataType = ftString
        Size = 200
      end>
    IndexDefs = <>
    Params = <>
    ProviderName = 'dspMaster'
    StoreDefs = True
    BeforeOpen = cdsMasterBeforeOpen
    AfterOpen = cdsMasterAfterOpen
    AfterPost = cdsMasterAfterPost
    AfterDelete = cdsMasterAfterDelete
    AfterScroll = cdsMasterAfterScroll
    Left = 184
    Top = 24
  end
  object dscMaster: TDataSource
    OnDataChange = dscMasterDataChange
    Left = 264
    Top = 24
  end
end

object dmDataFireDAC: TdmDataFireDAC
  OldCreateOrder = False
  Height = 338
  Width = 429
  object conMain: TFDConnection
    Left = 48
    Top = 40
  end
  object qryMain: TFDQuery
    Connection = conMain
    Left = 152
    Top = 40
  end
  object dsMemTable: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 256
    Top = 48
  end
end

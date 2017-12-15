object dmDataFireDAC: TdmDataFireDAC
  OldCreateOrder = False
  Height = 88
  Width = 294
  object conMain: TFDConnection
    Left = 40
    Top = 16
  end
  object qryMain: TFDQuery
    AfterOpen = qryMainAfterOpen
    AfterRefresh = qryMainAfterRefresh
    BeforeApplyUpdates = qryMainBeforeApplyUpdates
    AfterApplyUpdates = qryMainAfterApplyUpdates
    AfterGetRecords = qryMainAfterGetRecords
    BeforeRowRequest = qryMainBeforeRowRequest
    AfterRowRequest = qryMainAfterRowRequest
    OnMasterSetValues = qryMainMasterSetValues
    Connection = conMain
    OnError = qryMainError
    OnExecuteError = qryMainExecuteError
    OnCommandChanged = qryMainCommandChanged
    Left = 120
    Top = 16
  end
end

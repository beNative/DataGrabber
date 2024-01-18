object dmData: TdmData
  Height = 88
  Width = 232
  object conMain: TFDConnection
    OnError = conMainError
    OnLost = conMainLost
    OnRestored = conMainRestored
    OnRecover = conMainRecover
    AfterConnect = conMainAfterConnect
    BeforeConnect = conMainBeforeConnect
    AfterDisconnect = conMainAfterDisconnect
    BeforeDisconnect = conMainBeforeDisconnect
    BeforeStartTransaction = conMainBeforeStartTransaction
    AfterStartTransaction = conMainAfterStartTransaction
    Left = 40
    Top = 16
  end
  object qryMain: TFDQuery
    BeforeOpen = qryMainBeforeOpen
    AfterOpen = qryMainAfterOpen
    BeforeClose = qryMainBeforeClose
    AfterClose = qryMainAfterClose
    BeforeRefresh = qryMainBeforeRefresh
    AfterRefresh = qryMainAfterRefresh
    BeforeExecute = qryMainBeforeExecute
    AfterExecute = qryMainAfterExecute
    BeforeApplyUpdates = qryMainBeforeApplyUpdates
    AfterApplyUpdates = qryMainAfterApplyUpdates
    BeforeGetRecords = qryMainBeforeGetRecords
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

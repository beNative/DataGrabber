inherited dmUNIConnection: TdmUNIConnection
  OldCreateOrder = True
  Height = 96
  Width = 279
  object conUni: TUniConnection
    ProviderName = 'SQL Server'
    Database = 'ITC'
    Options.LocalFailover = True
    Pooling = True
    Username = 'TSI'
    Server = 'DEVELOP'
    LoginPrompt = False
    Left = 80
    Top = 40
  end
end

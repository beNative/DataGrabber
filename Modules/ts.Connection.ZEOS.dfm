inherited dmZEOSConnection: TdmZEOSConnection
  OldCreateOrder = True
  object conZEOS: TZConnection
    Catalog = 'ITC'
    ReadOnly = True
    DesignConnection = True
    Protocol = 'mssql'
    HostName = 'DEVELOP'
    Database = 'ITC'
    User = 'TSI'
    Left = 48
    Top = 16
  end
  object zsqlmntr1: TZSQLMonitor
    MaxTraceCount = 100
    Left = 48
    Top = 72
  end
  object zsqlprcsr1: TZSQLProcessor
    Params = <>
    Connection = conZEOS
    DelimiterType = dtEmptyLine
    Delimiter = ';'
    CleanupStatements = True
    Left = 112
    Top = 48
  end
  object zsqlmtdt1: TZSQLMetadata
    Connection = conZEOS
    MetadataType = mdVersionColumns
    Catalog = 'ITC'
    Schema = 'TSI'
    ProcedureName = 'sp_who;1'
    Left = 112
    Top = 8
  end
end

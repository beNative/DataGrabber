inherited dmDBXConnection: TdmDBXConnection
  OldCreateOrder = True
  object conDBX: TSQLConnection
    ConnectionName = 'SQLITECONNECTION'
    DriverName = 'Sqlite'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Sqlite'
      
        'Database=D:\development\database\Chinook\Chinook_Sqlite_AutoIncr' +
        'ementPKs.sqlite')
    Left = 48
    Top = 40
  end
end

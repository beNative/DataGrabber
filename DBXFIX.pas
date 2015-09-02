unit DBXFIX;

interface

implementation

uses DBXCommon, DBXDynalinkNative;

type
  TDBXInternalDriver = class(TDBXDynalinkDriverNative)
  public
    constructor Create(DriverDef: TDBXDriverDef); override;
  end;

  TDBXInternalProperties = class(TDBXProperties)
  public
    constructor Create(DBXContext: TDBXContext); override;
  end; { TDBXInternalDriver }

constructor TDBXInternalDriver.Create(DriverDef: TDBXDriverDef);
begin
  inherited Create(DriverDef, TDBXDynalinkDriverLoader);
  InitDriverProperties(TDBXInternalProperties.Create(DriverDef.FDBXContext));
end; { TDBXInternalProperties }

constructor TDBXInternalProperties.Create(DBXContext: TDBXContext);
begin
  inherited Create(DBXContext);
  Values[TDBXPropertyNames.DriverUnit] := ''; // This is for the IDE only
  Values[TDBXPropertyNames.GetDriverFunc] := 'getSQLDriverMYSQL';
  Values[TDBXPropertyNames.LibraryName] := 'dbxmys.dll';
  Values[TDBXPropertyNames.VendorLib] := 'LIBMYSQL.dll';
  Values[TDBXPropertyNames.Database] := 'Database Name';
  Values[TDBXPropertyNames.UserName] := 'user';
  Values[TDBXPropertyNames.Password] := 'password';
  Values[TDBXPropertyNames.MaxBlobSize] := '-1';
end;

var
  InternalConnectionFactory: TDBXMemoryConnectionFactory;

initialization
  TDBXDriverRegistry.RegisterDriverClass('MySQL', TDBXInternalDriver);
  InternalConnectionFactory := TDBXMemoryConnectionFactory.Create;
  InternalConnectionFactory.Open;
  TDBXConnectionFactory.SetConnectionFactory(InternalConnectionFactory);

end.

unit ts.Connection.UNIDac;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB,

  UniProvider, DBAccess, UniDacVcl, Uni,

  AccessUniProvider,
  AdvantageUniProvider,
  ASEUniProvider,
  DB2UniProvider,
  InterBaseUniProvider,
  MySQLUniProvider,
  ODBCUniProvider,
  OracleUniProvider,
  PostgreSQLUniProvider,
  SQLiteUniProvider,
  SQLServerUniProvider,

  ts.Interfaces, ts.Connection;

//=============================================================================

type
  TdmUNIConnection = class(TdmConnection, IConnection)
    conUni     : TUniConnection;

  protected
    function GetConnectionType: string; override;
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    function GetConnectionString: string; override;
    function GetConnection: TComponent; override;

    procedure AssignConnectionString(const AValue: string); override;
    procedure AssignConnectionSettings; override;

    function CreateNativeDataSet: INativeDataSet;
    function Execute(const ACommandText: string): Boolean; override;

  public
    procedure AfterConstruction; override;

  end;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  ts.Data.NativeUNI;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TdmUNIConnection.AfterConstruction;
begin
  inherited;
  UniProviders.GetProviderNames(Protocols);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmUNIConnection.GetConnected: Boolean;
begin
  Result := conUni.Connected;
end;

procedure TdmUNIConnection.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    conUni.Connected := Value;
  end;
end;

//-----------------------------------------------------------------------------

function TdmUNIConnection.GetConnection: TComponent;
begin
  Result := conUni;
end;

//-----------------------------------------------------------------------------

function TdmUNIConnection.GetConnectionString: string;
begin
//
end;

//-----------------------------------------------------------------------------

function TdmUNIConnection.GetConnectionType: string;
begin
  Result := 'UNI';
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TdmUNIConnection.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    if ConnectionSettings.Database <> '' then
    begin
      Connected := False;
      conUni.Port         := ConnectionSettings.Port;
      conUni.Database     := ConnectionSettings.Database;
      conUni.Username     := ConnectionSettings.User;
      conUni.Password     := ConnectionSettings.Password;
      conUni.Server       := ConnectionSettings.HostName;
      conUni.ProviderName := ConnectionSettings.Protocol;
      conUni.Options.DisconnectedMode := ConnectionSettings.DisconnectedMode;
    end;
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

procedure TdmUNIConnection.AssignConnectionString(const AValue: string);
begin
  inherited;
// TODO
end;

//-----------------------------------------------------------------------------

function TdmUNIConnection.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeUNIDataSet.Create(Self);
end;

//-----------------------------------------------------------------------------

function TdmUNIConnection.Execute(const ACommandText: string): Boolean;
begin
  Result := conUni.ExecSQL(ACommandText, []);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

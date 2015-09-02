unit ts.Connection.UNIConnectionAdaptor;

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
  //OracleUniProvider,
  PostgreSQLUniProvider,
  //SQLiteUniProvider,
  SQLServerUniProvider,

  ts.Interfaces,

  ts.Connection.CustomConnectionAdaptor;

//=============================================================================

type
  TUNIConnectionAdaptor = class(TCustomConnectionAdaptor, IConnection)
  private
    FConnection: TUniConnection;

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
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

uses
  ts.Data.NativeUNI;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TUNIConnectionAdaptor.AfterConstruction;
begin
  inherited;
  FConnection := TUniConnection.Create(nil);
  UniProviders.GetProviderNames(Protocols);
end;

procedure TUNIConnectionAdaptor.BeforeDestruction;
begin
  FConnection.Free;
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TUNIConnectionAdaptor.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TUNIConnectionAdaptor.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    FConnection.Connected := Value;
  end;
end;

//-----------------------------------------------------------------------------

function TUNIConnectionAdaptor.GetConnection: TComponent;
begin
  Result := FConnection;
end;

//-----------------------------------------------------------------------------

function TUNIConnectionAdaptor.GetConnectionString: string;
begin
//
end;

//-----------------------------------------------------------------------------

function TUNIConnectionAdaptor.GetConnectionType: string;
begin
  Result := 'UNI';
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TUNIConnectionAdaptor.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    if ConnectionSettings.Database <> '' then
    begin
      Connected := False;
      FConnection.Port         := ConnectionSettings.Port;
      FConnection.Database     := ConnectionSettings.Database;
      FConnection.Username     := ConnectionSettings.User;
      FConnection.Password     := ConnectionSettings.Password;
      FConnection.Server       := ConnectionSettings.HostName;
      FConnection.ProviderName := ConnectionSettings.Protocol;
      FConnection.Options.DisconnectedMode := ConnectionSettings.DisconnectedMode;
    end;
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

procedure TUNIConnectionAdaptor.AssignConnectionString(const AValue: string);
begin
  inherited;
// TODO
end;

//-----------------------------------------------------------------------------

function TUNIConnectionAdaptor.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeUNIDataSet.Create(Self);
end;

//-----------------------------------------------------------------------------

function TUNIConnectionAdaptor.Execute(const ACommandText: string): Boolean;
begin
  Result := FConnection.ExecSQL(ACommandText, []);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

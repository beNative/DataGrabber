unit ts.Connection.ZEOS;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB,

  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZSqlMetadata,
  ZSqlProcessor, ZSqlMonitor,

  ts.Interfaces, ts.Connection;

//=============================================================================

type
  TdmZEOSConnection = class(TdmConnection, IConnection)
    conZEOS: TZConnection;
    zsqlmntr1: TZSQLMonitor;
    zsqlprcsr1: TZSQLProcessor;
    zsqlmtdt1: TZSQLMetadata;
  protected
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    function GetConnectionString: string; override;
    function GetConnectionType: string; override;
    function GetConnection: TComponent; override;

    procedure AssignConnectionString(const AValue: string); override;
    procedure AssignConnectionSettings; override;

    function CreateNativeDataSet: INativeDataSet; override;
    function Execute(const ACommandText: string): Boolean; override;

  public
    procedure AfterConstruction; override;

  end;

function ZEOSConnection: IConnection;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  ts.Data.NativeZEOS;

var
  FConnection: TdmZEOSConnection;

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

function ZEOSConnection: IConnection;
begin
  if not Assigned(FConnection) then
    FConnection := TdmZEOSConnection.Create(Application);
  Result := FConnection;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TdmZEOSConnection.AfterConstruction;
begin
  inherited;
  conZEOS.GetProtocolNames(Protocols);
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmZEOSConnection.GetConnected: Boolean;
begin
  Result := conZEOS.Connected;
end;

//-----------------------------------------------------------------------------

function TdmZEOSConnection.GetConnection: TComponent;
begin
  Result := conZEOS;
end;

//-----------------------------------------------------------------------------

function TdmZEOSConnection.GetConnectionString: string;
begin
  Result := conZEOS.Properties.Text;
end;

//-----------------------------------------------------------------------------

function TdmZEOSConnection.GetConnectionType: string;
begin
  Result := 'ZEOS';
end;

//-----------------------------------------------------------------------------

procedure TdmZEOSConnection.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    conZEOS.Connected := Value;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TdmZEOSConnection.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    Connected := False;
    conZEOS.Protocol := ConnectionSettings.Protocol;
    conZEOS.HostName := ConnectionSettings.HostName;
    conZEOS.Database := ConnectionSettings.Database;
    conZEOS.Port     := ConnectionSettings.Port;
    conZEOS.User     := ConnectionSettings.User;
    conZEOS.Password := ConnectionSettings.Password;
    conZEOS.Catalog  := ConnectionSettings.Catalog;
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

procedure TdmZEOSConnection.AssignConnectionString(const AValue: string);
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    conZEOS.Properties.Text := AValue
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

function TdmZEOSConnection.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeZEOSDataSet.Create(Self);
end;

//-----------------------------------------------------------------------------

function TdmZEOSConnection.Execute(const ACommandText: string): Boolean;
begin
  Result := conZEOS.ExecuteDirect(ACommandText);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

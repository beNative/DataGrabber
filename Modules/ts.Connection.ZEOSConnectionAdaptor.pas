unit ts.Connection.ZEOSConnectionAdaptor;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB,

  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZSqlMetadata,
  ZSqlProcessor, ZSqlMonitor,

  ts.Interfaces,

  ts.Connection.CustomConnectionAdaptor;

//=============================================================================

type
  TZEOSConnectionAdaptor = class(TCustomConnectionAdaptor, IConnection)
  private
    FConnection: TZConnection;
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
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

uses
  ts.Data.NativeZEOS;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TZEOSConnectionAdaptor.AfterConstruction;
begin
  inherited;
  FConnection := TZConnection.Create(nil);
  FConnection.GetProtocolNames(Protocols);
end;

procedure TZEOSConnectionAdaptor.BeforeDestruction;
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

function TZEOSConnectionAdaptor.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

//-----------------------------------------------------------------------------

function TZEOSConnectionAdaptor.GetConnection: TComponent;
begin
  Result := FConnection;
end;

//-----------------------------------------------------------------------------

function TZEOSConnectionAdaptor.GetConnectionString: string;
begin
  Result := FConnection.Properties.Text;
end;

//-----------------------------------------------------------------------------

function TZEOSConnectionAdaptor.GetConnectionType: string;
begin
  Result := 'ZEOS';
end;

//-----------------------------------------------------------------------------

procedure TZEOSConnectionAdaptor.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    FConnection.Connected := Value;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TZEOSConnectionAdaptor.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    Connected := False;
    FConnection.Protocol := ConnectionSettings.Protocol;
    FConnection.HostName := ConnectionSettings.HostName;
    FConnection.Database := ConnectionSettings.Database;
    FConnection.Port     := ConnectionSettings.Port;
    FConnection.User     := ConnectionSettings.User;
    FConnection.Password := ConnectionSettings.Password;
    FConnection.Catalog  := ConnectionSettings.Catalog;
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

procedure TZEOSConnectionAdaptor.AssignConnectionString(const AValue: string);
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    FConnection.Properties.Text := AValue
  finally
    Connected := B;
  end;
end;

//-----------------------------------------------------------------------------

function TZEOSConnectionAdaptor.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeZEOSDataSet.Create(Self);
end;

//-----------------------------------------------------------------------------

function TZEOSConnectionAdaptor.Execute(const ACommandText: string): Boolean;
begin
  Result := FConnection.ExecuteDirect(ACommandText);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

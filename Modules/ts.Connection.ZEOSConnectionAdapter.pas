{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Connection.ZEOSConnectionAdapter;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB,

  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZSqlMetadata,
  ZSqlProcessor, ZSqlMonitor,

  ts.Interfaces,

  ts.Connection.CustomConnectionAdapter;

type
  TZEOSConnectionAdapter = class(TCustomConnectionAdapter, IConnection)
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

implementation

uses
  ts.Data.NativeZEOS;

{$REGION 'construction and destruction'}
procedure TZEOSConnectionAdapter.AfterConstruction;
begin
  inherited AfterConstruction;
  FConnection := TZConnection.Create(nil);
  FConnection.GetProtocolNames(Protocols);
end;

procedure TZEOSConnectionAdapter.BeforeDestruction;
begin
  FConnection.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TZEOSConnectionAdapter.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TZEOSConnectionAdapter.GetConnection: TComponent;
begin
  Result := FConnection;
end;

function TZEOSConnectionAdapter.GetConnectionString: string;
begin
  Result := FConnection.Properties.Text;
end;

function TZEOSConnectionAdapter.GetConnectionType: string;
begin
  Result := 'ZEOS';
end;

procedure TZEOSConnectionAdapter.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    FConnection.Connected := Value;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TZEOSConnectionAdapter.AssignConnectionSettings;
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

procedure TZEOSConnectionAdapter.AssignConnectionString(const AValue: string);
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

function TZEOSConnectionAdapter.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeZEOSDataSet.Create(Self);
end;

function TZEOSConnectionAdapter.Execute(const ACommandText: string): Boolean;
begin
  Result := FConnection.ExecuteDirect(ACommandText);
end;
{$ENDREGION}

end.

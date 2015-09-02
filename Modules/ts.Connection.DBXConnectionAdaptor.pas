{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Connection.DBXConnectionAdaptor;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB, Data.SqlExpr,

  DBXMSSQL,
  Data.DBXMySQL,
  Data.DbxFirebird,
  Data.DbxInformix,
  Data.DBXInterbase,
  Data.DBXOdbc,
  Data.DbxOracle,
  Data.DbxSybaseASA,
  Data.DbxSybaseASE,

  ts.Interfaces,

  ts.Connection.CustomConnectionAdaptor;

type
  TDBXConnectionAdaptor = class(TCustomConnectionAdaptor, IConnection)
  private
    FConnection: TSQLConnection;

  protected
    function GetConnectionType: string; override;
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    function GetConnectionString: string; override;
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
  ts.Data.NativeDBX;

{$REGION 'construction and destruction'}
procedure TDBXConnectionAdaptor.AfterConstruction;
begin
  inherited;
  FConnection := TSQLConnection.Create(nil);
  GetDriverNames(Protocols);
end;

procedure TDBXConnectionAdaptor.BeforeDestruction;
begin
  FConnection.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDBXConnectionAdaptor.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TDBXConnectionAdaptor.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    try
      FConnection.Connected := Value;
    except
      FConnection.Connected := False;
    end;
  end;
end;

function TDBXConnectionAdaptor.GetConnection: TComponent;
begin
  Result := FConnection;
end;

function TDBXConnectionAdaptor.GetConnectionString: string;
begin
  Result := FConnection.ConnectionData.Properties.Properties.Text;
end;

function TDBXConnectionAdaptor.GetConnectionType: string;
begin
  Result := 'DBX';
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TDBXConnectionAdaptor.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    if ConnectionSettings.Protocol <> '' then
    begin
      Connected := False;
      FConnection.DriverName := ConnectionSettings.Protocol;
      FConnection.KeepConnection := not ConnectionSettings.DisconnectedMode;
      with FConnection.ConnectionData.Properties do
      begin
        Values['HostName']  := ConnectionSettings.HostName;
        Values['Port']      := IntToStr(ConnectionSettings.Port);
        Values['Database']  := ConnectionSettings.Database;
        Values['User_Name'] := ConnectionSettings.User;
        Values['Password']  := ConnectionSettings.Password;
        Values['OS Authentication'] := 'True';
      end;
    end;
  finally
    Connected := B;
  end;
end;

procedure TDBXConnectionAdaptor.AssignConnectionString(const AValue: string);
begin
  FConnection.ConnectionData.Properties.Properties.Text := AValue;
end;

function TDBXConnectionAdaptor.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeDBXDataSet.Create(Self);
end;

function TDBXConnectionAdaptor.Execute(const ACommandText: string): Boolean;
begin
  FConnection.ExecuteDirect(ACommandText);
  Result := True;
end;
{$ENDREGION}

end.

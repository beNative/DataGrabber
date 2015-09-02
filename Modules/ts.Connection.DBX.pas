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

unit ts.Connection.DBX;

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

  ts.Interfaces, ts.Connection;

type
  TdmDBXConnection = class(TdmConnection, IConnection)
    conDBX: TSQLConnection;

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

  end;

function DBXConnection: IConnection;

implementation

uses
  ts.Data.NativeDBX;

{$R *.dfm}

var
  FConnection: TdmDBXConnection;

{$REGION 'interfaced routines'}
function DBXConnection: IConnection;
begin
  if not Assigned(FConnection) then
    FConnection := TdmDBXConnection.Create(Application);
  Result := FConnection;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TdmDBXConnection.AfterConstruction;
begin
  inherited;
  GetDriverNames(Protocols);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmDBXConnection.GetConnected: Boolean;
begin
  Result := conDBX.Connected;
end;

procedure TdmDBXConnection.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    try
      conDBX.Connected := Value;
    except
      conDBX.Connected := False;
    end;
  end;
end;

function TdmDBXConnection.GetConnection: TComponent;
begin
  Result := conDBX;
end;

function TdmDBXConnection.GetConnectionString: string;
begin
  Result := conDBX.ConnectionData.Properties.Properties.Text;
end;

function TdmDBXConnection.GetConnectionType: string;
begin
  Result := 'DBX';
end;


{$REGION 'protected methods'}
procedure TdmDBXConnection.AssignConnectionSettings;
var
  B: Boolean;
begin
  inherited;
  B := Connected;
  try
    if ConnectionSettings.Protocol <> '' then
    begin
      Connected := False;
      conDBX.DriverName := ConnectionSettings.Protocol;
      conDBX.KeepConnection := not ConnectionSettings.DisconnectedMode;
      with conDBX.ConnectionData.Properties do
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

procedure TdmDBXConnection.AssignConnectionString(const AValue: string);
begin
  conDBX.ConnectionData.Properties.Properties.Text := AValue;
end;

function TdmDBXConnection.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeDBXDataSet.Create(Self);
end;

//-----------------------------------------------------------------------------

function TdmDBXConnection.Execute(const ACommandText: string): Boolean;
begin
  conDBX.ExecuteDirect(ACommandText);
  Result := True;
end;
{$ENDREGION}

end.

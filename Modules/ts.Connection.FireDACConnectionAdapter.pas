{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Connection.FireDACConnectionAdapter;

interface

uses
  System.Classes,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Stan.Def,
  FireDAC.Comp.Client,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.Oracle, FireDAC.Phys.MSSQL, FireDAC.Phys.ADS, FireDAC.Phys.ASA,
  FireDAC.Phys.DB2, FireDAC.Phys.DS, FireDAC.Phys.FB, FireDAC.Phys.IB,
  FireDAC.Phys.Infx, FireDAC.Phys.MongoDB, FireDAC.Phys.MSAcc,
  FireDAC.Phys.MySQL, FireDAC.Phys.ODBC, FireDAC.Phys.PG, FireDAC.Phys.SQLite,
  FireDAC.Phys.TData, FireDAC.Phys.TDBX,

  FireDAC.VCLUI.Async,
  FireDAC.Comp.UI,

  ts.Interfaces,
  ts.Data.NativeFireDAC,



  ts.Connection.CustomConnectionAdapter;

type
  TFireDACConnectionAdapter = class(TCustomConnectionAdapter, IConnection, IMetaData)
  private
    FConnection         : TFDConnection;
    FAsyncExecuteDialog : TFDGUIxAsyncExecuteDialog;

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

    { IMetaData }
    procedure GetTableNames(
      AList             : TStrings;
      const ASchemaName : string = ''
    );
    procedure GetFieldNames(
      AList             : TStrings;
      const ATableName  : string;
      const ASchemaName : string = ''
    );
    procedure GetSchemaNames(AList: TStrings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  Vcl.Forms;

{$REGION 'construction and destruction'}
procedure TFireDACConnectionAdapter.AfterConstruction;
begin
  inherited AfterConstruction;
  FConnection := TFDConnection.Create(nil);
  FConnection.FetchOptions.Mode := fmAll;
  FConnection.FetchOptions.RecordCountMode := cmTotal;
  FConnection.ResourceOptions.CmdExecMode := amCancelDialog;
  // send no prepare command to the database
  FConnection.ResourceOptions.DirectExecute  := True;

  // cancel dialog
  FAsyncExecuteDialog := TFDGUIxAsyncExecuteDialog.Create(FConnection);
  FAsyncExecuteDialog.Provider  := 'Forms';
  FAsyncExecuteDialog.Caption   := Application.Title;
  FAsyncExecuteDialog.ShowDelay := 100;

  FDManager.GetDriverNames(Protocols);
end;

procedure TFireDACConnectionAdapter.BeforeDestruction;
begin
  FConnection.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TFireDACConnectionAdapter.GetConnectionType: string;
begin
  Result := 'FireDAC';
end;

function TFireDACConnectionAdapter.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TFireDACConnectionAdapter.SetConnected(const Value: Boolean);
begin
  FConnection.Connected := Value;
end;

function TFireDACConnectionAdapter.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TFireDACConnectionAdapter.GetConnection: TComponent;
begin
  Result := FConnection;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TFireDACConnectionAdapter.AssignConnectionString(
  const AValue: string);
begin
  FConnection.ConnectionString := AValue;
end;

procedure TFireDACConnectionAdapter.AssignConnectionSettings;
var
  B: Boolean;
begin
  B := Connected;
  try
    if ConnectionSettings.Protocol <> '' then
    begin
      Connected := False;
      FConnection.DriverName := ConnectionSettings.Protocol;
      FConnection.Offlined := ConnectionSettings.DisconnectedMode;
      with FConnection.Params do
      begin
        Values['Server']    := ConnectionSettings.HostName;
        Values['Database']  := ConnectionSettings.Database;
        Values['User_Name'] := ConnectionSettings.User;
        Values['Password']  := ConnectionSettings.Password;
        Values['OSAuthent'] := 'Yes';
      end;
      FConnection.LoginPrompt := False;
      // Set this to prevent issues with FireDac's need for a cursor object﻿
      FConnection.ResourceOptions.SilentMode := True;
      FConnection.ResourceOptions.AutoReconnect := True;
    end;
  finally
    Connected := B;
  end;
end;

function TFireDACConnectionAdapter.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeFireDACDataSet.Create(Self);
end;

function TFireDACConnectionAdapter.Execute(const ACommandText: string): Boolean;
begin
  Result := FConnection.ExecSQL(ACommandText) <> 0;
end;

procedure TFireDACConnectionAdapter.GetTableNames(AList: TStrings;
  const ASchemaName: string);
begin
  AList.Clear;
  FConnection.GetTableNames(
    FConnection.CurrentCatalog,
    ASchemaName,
    '*',
    AList
  );
end;

procedure TFireDACConnectionAdapter.GetFieldNames(AList: TStrings;
  const ATableName, ASchemaName: string);
begin
  AList.Clear;
  FConnection.GetFieldNames(
    FConnection.CurrentCatalog,
    ASchemaName,
    ATableName,
    '*',
    AList
  );
end;

procedure TFireDACConnectionAdapter.GetSchemaNames(AList: TStrings);
begin
  AList.Clear;
  FConnection.GetSchemaNames(
    FConnection.CurrentCatalog,
    '*',
    AList
  );
end;
{$ENDREGION}

end.

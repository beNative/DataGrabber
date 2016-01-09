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

unit ts.Connection.ADOConnectionAdaptor;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB, Data.Win.ADODB,

  ts.Interfaces,

  ts.Connection.CustomConnectionAdaptor;

type
  TADOConnectionAdaptor = class(TCustomConnectionAdaptor, IConnection, IMetaData)
  private
    FConnection       : TADOConnection;
    FConnectionString : string;

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
    procedure GetTableNames(AList: TStrings; const ASchemaName: string = '');
    procedure GetFieldNames(AList: TStrings; const ATableName: string; const ASchemaName: string = '');
    procedure GetSchemaNames(AList : TStrings);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.StrUtils,

  ts.Data.NativeADO;

const
  CONNECTION_STRING =
    'Provider=%s;' +  // SQLOLEDB.1   SQLNCLI10.1  SQLNCLI.1  MSDASQL.1
    'Integrated Security=SSPI;' +
    'Persist Security Info=False;' +
    'User ID=%s;' +                   // "" when empty
    'Password=%s;' +                  // "" when empty
    'Initial Catalog=%s;' +           // database name
    'Data Source=%s';                 // Servername

{$REGION 'construction and destruction'}
procedure TADOConnectionAdaptor.AfterConstruction;
begin
  inherited;
  FConnection := TADOConnection.Create(nil);
  GetProviderNames(Protocols);
end;

procedure TADOConnectionAdaptor.BeforeDestruction;
begin
  FConnection.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TADOConnectionAdaptor.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TADOConnectionAdaptor.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    FConnection.Connected := Value;
  end;
end;

function TADOConnectionAdaptor.GetConnection: TComponent;
begin
  Result := FConnection;
end;

function TADOConnectionAdaptor.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TADOConnectionAdaptor.GetConnectionType: string;
begin
  Result := 'ADO';
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TADOConnectionAdaptor.AssignConnectionSettings;
var
  S: string;
begin
  inherited;
  S := Format(CONNECTION_STRING, [
    ConnectionSettings.Protocol,
    IfThen(ConnectionSettings.User = '', '""', ConnectionSettings.User),
    IfThen(ConnectionSettings.Password = '', '""', ConnectionSettings.Password),
    ConnectionSettings.Database,
    ConnectionSettings.HostName
  ]);
  if ConnectionSettings.Protocol <> '' then
    AssignConnectionString(S);
end;

procedure TADOConnectionAdaptor.AssignConnectionString(const AValue: string);
var
  B: Boolean;
  S: WideString;
begin
  if FConnectionString <> AValue then
  begin
    // connectionstring is stored locally because the ADO connection alters the
    // original connectionstring.
    FConnectionString := AValue;
    B := Connected;
    try
      S := AValue;
      Connected := False;
      FConnection.ConnectionString := S;
    finally
      Connected := B;
    end;
  end;
end;

function TADOConnectionAdaptor.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeADODataSet.Create(Self);
end;

function TADOConnectionAdaptor.Execute(const ACommandText: string): Boolean;
begin
  FConnection.Execute(ACommandText);
  Result := True;
end;

procedure TADOConnectionAdaptor.GetFieldNames(AList: TStrings;
  const ATableName: string; const ASchemaName: string);
var
  DS : TADODataSet;
begin
  AList.Clear;
  DS := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(
      siColumns,
      VarArrayOf([Unassigned, ASchemaName, ATableName, Unassigned]),
      EmptyParam,
      DS
    );
    DS.First;
    while not DS.Eof do
    begin
      AList.Add(Trim(DS.FieldByName('COLUMN_NAME').AsString));
      DS.Next;
    end
  finally
    DS.Free;
  end;
end;

procedure TADOConnectionAdaptor.GetSchemaNames(AList: TStrings);
var
  DS : TADODataSet;
begin
  AList.Clear;
  DS := TADODataSet.Create(nil);
  try
    FConnection.OpenSchema(siSchemata, EmptyParam, EmptyParam, DS);
    DS.First;
    while not DS.Eof do
    begin
      AList.Add(Trim(DS.FieldByName('SCHEMA_NAME').AsString));
      DS.Next;
    end
  finally
    DS.Free;
  end;
end;

procedure TADOConnectionAdaptor.GetTableNames(AList: TStrings;
  const ASchemaName: string);
var
  DS : TADODataSet;
  V  : Variant;
begin
  AList.Clear;
  DS := TADODataSet.Create(nil);
  try
    if ASchemaName = '' then
      V := Unassigned
    else
      V := ASchemaName;
    FConnection.OpenSchema(
      siTables,
      VarArrayOf([Unassigned, V, Unassigned, Unassigned]),
      EmptyParam,
      DS
    );
    DS.First;
    while not DS.Eof do
    begin
      if DS.FieldByName('TABLE_TYPE').AsString = 'TABLE' then
      begin
        AList.Add(DS.FieldByName('TABLE_NAME').AsString);
      end;
      DS.Next;
    end
  finally
    DS.Free;
  end;
end;
{$ENDREGION}

end.

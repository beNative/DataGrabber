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

unit ts.Connection.ADO;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB, Data.Win.ADODB,

  ts.Interfaces, ts.Connection;

type
  TdmADOConnection = class(TdmConnection, IConnection, IMetaData)
    conADO: TADOConnection;

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
    procedure GetFieldNames(
            AList       : TStrings;
      const ATableName  : string;
      const ASchemaName : string = ''
    );
    procedure GetSchemaNames(AList : TStrings);

  public
    procedure AfterConstruction; override;
    constructor Create; reintroduce; virtual;

  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,

  ts.Data.NativeADO;

var
  FConnection: TdmADOConnection;

const
  CONNECTION_STRING =
    'Provider=%s;' +  // SQLOLEDB.1   SQLNCLI10.1  SQLNCLI.1  MSDASQL.1
    'Integrated Security=SSPI;' +
    'Persist Security Info=False;' +
    'User ID=%s;' +                   // "" when empty
    'Password=%s;' +                  // "" when empty
    'Initial Catalog=%s;' +           // database name
    'Data Source=%s';                  // Servername

//Provider=SQLOLEDB.1;Password=balta;Persist Security Info=True;User ID=TSI;Initial Catalog=ITC;Data Source=DEVELOP
//Provider=SQLOLEDB.1;Persist Security Info=False;User ID=TSI;Initial Catalog=ITC;Data Source=DEVELOP
//Provider=MSDASQL.1;Persist Security Info=False;Data Source=itc;Initial Catalog=ITC
//Provider=SQLNCLI10.1;Integrated Security=SSPI;Persist Security Info=False;User ID="";Initial Catalog=ITC;Data Source=DEVELOP;Initial File Name="";Server SPN=""
//Provider=SQLNCLI.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=ITC;Data Source=DEVELOP
//Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=ITC;Data Source=DEVELOP

{$REGION 'interfaced routines'}
function ADOConnection: IConnection;
begin
  if not Assigned(FConnection) then
    FConnection := TdmADOConnection.Create;
  Result := FConnection;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TdmADOConnection.AfterConstruction;
begin
  inherited AfterConstruction;
  GetProviderNames(Protocols);
end;

constructor TdmADOConnection.Create;
begin
  inherited Create(Application);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmADOConnection.GetConnected: Boolean;
begin
  Result := conADO.Connected;
end;

procedure TdmADOConnection.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    conADO.Connected := Value;
  end;
end;

function TdmADOConnection.GetConnection: TComponent;
begin
  Result := conADO;
end;

function TdmADOConnection.GetConnectionString: string;
begin
  Result := conADO.ConnectionString;
end;

function TdmADOConnection.GetConnectionType: string;
begin
  Result := 'ADO';
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmADOConnection.AssignConnectionSettings;
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

procedure TdmADOConnection.AssignConnectionString(const AValue: string);
var
  B: Boolean;
  S: WideString;
begin
  B := Connected;
  try
    S := AValue;
    Connected := False;
    conADO.ConnectionString := S;
  finally
    Connected := B;
  end;
end;

function TdmADOConnection.CreateNativeDataSet: INativeDataSet;
begin
  Result := TNativeADODataSet.Create(Self);
end;

function TdmADOConnection.Execute(const ACommandText: string): Boolean;
begin
  conADO.Execute(ACommandText);
  Result := True;
end;

procedure TdmADOConnection.GetFieldNames(AList: TStrings;
  const ATableName: string; const ASchemaName: string);
var
  DS : TADODataSet;
begin
  AList.Clear;
  DS := TADODataSet.Create(nil);
  try
    conADO.OpenSchema(
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

procedure TdmADOConnection.GetSchemaNames(AList: TStrings);
var
  DS : TADODataSet;
begin
  AList.Clear;
  DS := TADODataSet.Create(nil);
  try
    conADO.OpenSchema(siSchemata, EmptyParam, EmptyParam, DS);
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

procedure TdmADOConnection.GetTableNames(AList: TStrings;
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
    conADO.OpenSchema(
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

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

unit ts.Classes.SQL.Statement;

interface

uses
  System.Classes,

  ts.Classes.SQL.Condition;

type
  TSQLTable = class(TPersistent)
  private
    FAlias: string;
    FName: string;
    function GetAlias: string;
    function GetName: string;
    procedure SetAlias(const Value: string);
    procedure SetName(const Value: string);
  public
    property Name: string
      read GetName write SetName;

    property Alias: string
      read GetAlias write SetAlias;
  end;

  TSQLField = class(TPersistent)
  private
    FAlias: string;
    FName: string;
    FTable: TSQLTable;
    function GetAlias: string;
    function GetName: string;
    function GetTable: TSQLTable;
    procedure SetAlias(const Value: string);
    procedure SetName(const Value: string);
    procedure SetTable(const Value: TSQLTable);
    property Table: TSQLTable
      read GetTable write SetTable;

    property Name: string
      read GetName write SetName;

    property Alias: string
      read GetAlias write SetAlias;
  end;

type
  TSQLStatement = class(TPersistent)

  public
//    function Select: TSQLStatement;
//    function From: TSQLStatement;
//    function Where: TSQLStatement;
//    function OrderBy: TSQLStatement;
//    function GroupBy: TSQLStatement;
//    function Having: TSQLStatement;

  end;

implementation

{ TSQLTable }

function TSQLTable.GetAlias: string;
begin
  Result := FAlias;
end;

function TSQLTable.GetName: string;
begin
  Result := FName;
end;

procedure TSQLTable.SetAlias(const Value: string);
begin
  FAlias := Value;
end;

procedure TSQLTable.SetName(const Value: string);
begin
  FName := Value;
end;

{ TSQLField }

function TSQLField.GetAlias: string;
begin
  Result := FAlias;
end;

procedure TSQLField.SetAlias(const Value: string);
begin
  FAlias := Value;
end;

function TSQLField.GetName: string;
begin
  Result := FName;
end;

procedure TSQLField.SetName(const Value: string);
begin
  FName := Value;
end;

function TSQLField.GetTable: TSQLTable;
begin
  Result := FTable;
end;

procedure TSQLField.SetTable(const Value: TSQLTable);
begin
  FTable := Value;
end;

end.

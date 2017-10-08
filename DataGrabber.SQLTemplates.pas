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

unit DataGrabber.SQLTemplates;

interface

uses
  System.Classes,
  Data.DB;

{
  SELECT
    *
  FROM
    <table>
  WHERE
    <selectedfield> = <selectedvalue>
}

type
  TSQLTemplates = class(TComponent)
  private
    FSelectTemplate     : string;
    FCallTemplate       : string;
    FInsertTemplate     : string;
    FDeleteTemplate     : string;
    FUpdateTemplate     : string;
    FAssignmentTemplate : string;
    FWhereTemplate      : string;

  public
    constructor Create(AOwner: TComponent); override;

    function BuildSQLSelect(const ATable: string): string; overload;

    function BuildSQLSelect(
      const ATable : string;
      AFields      : TStrings
    ): string; overload;

    function BuildSQLSelect(
      const ATable : string;
      AFields      : TStrings;
      AValues      : Variant
    ): string; overload;

    function BuildSQLSelect(
      const ATable   : string;
      const ADataSet : TDataSet
    ): string; overload;

    function BuildSQLInsert(
      const ATable : string;
      AFields      : TStrings
    ): string;

    function BuildSQLDelete(const ATable: string) : string;

    function BuildSQLUpdate(
      const ATable : string;
      AFields      : TStrings
    ): string;

    function BuildSQLCall(
      const AProcedure : string;
      AParameters      : TStrings): string;

  published
    property SelectTemplate: string
      read FSelectTemplate write FSelectTemplate;

    property InsertTemplate: string
      read FInsertTemplate write FInsertTemplate;

    property DeleteTemplate: string
      read FDeleteTemplate write FDeleteTemplate;

    property CallTemplate: string
      read FCallTemplate write FCallTemplate;

    property UpdateTemplate: string
      read FUpdateTemplate write FUpdateTemplate;

    property AssignmentTemplate: string
      read FAssignmentTemplate write FAssignmentTemplate;

    property WhereTemplate: string
      read FWhereTemplate write FWhereTemplate;

  end;

implementation

uses
  System.SysUtils, System.Variants;

const
  DEFAULT_SELECT_TEMPLATE = 'SELECT'#13#10 +
                            '  %s'#13#10 +
                            'FROM'#13#10 +
                            '  %s';

  DEFAULT_WHERE_TEMPLATE = #13#10 +
                           'WHERE'#13#10 +
                           '%s';

  DEFAULT_DELETE_TEMPLATE = 'DELETE'#13#10 +
                            'FROM'#13#10 +
                            '  %s';

  DEFAULT_UPDATE_TEMPLATE = 'UPDATE'#13#10 +
                            '  %s'#13#10 +
                            'SET'#13#10 +
                            '%s'#13#10 +
                            'WHERE'#13#10 +
                            '  <condition>';

  DEFAULT_INSERT_TEMPLATE = 'INSERT INTO %s'#13#10 +
                            '  (%s)'#13#10 +
                            'VALUES'#13#10 +
                            '  (%s)';

  DEFAULT_CALL_TEMPLATE = 'CALL %s(%s)';

  DEFAULT_ASSIGNMENT_TEMPLATE = '  %s = <%0:s>';

  ASSIGNMENT = '  %s = %s';
  ISNULL     = '  %s IS NULL';

  // ODBC date - and time formats
  DATE_FORMAT     = 'yyyy-mm-dd';
  TIME_FORMAT     = 'hh:mm';
  DATETIME_FORMAT = DATE_FORMAT + ' ' + TIME_FORMAT;

{$REGION 'construction and destruction'}
constructor TSQLTemplates.Create(AOwner: TComponent);
begin
  inherited;
  FSelectTemplate     := DEFAULT_SELECT_TEMPLATE;
  FInsertTemplate     := DEFAULT_INSERT_TEMPLATE;
  FCallTemplate       := DEFAULT_CALL_TEMPLATE;
  FDeleteTemplate     := DEFAULT_DELETE_TEMPLATE;
  FUpdateTemplate     := DEFAULT_UPDATE_TEMPLATE;
  FWhereTemplate      := DEFAULT_WHERE_TEMPLATE;
  FAssignmentTemplate := DEFAULT_ASSIGNMENT_TEMPLATE;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TSQLTemplates.BuildSQLCall(const AProcedure: string;
  AParameters: TStrings): string;
var
  S : string;
  I : Integer;
begin
  for I := 0 to AParameters.Count - 1 do
  begin
    if I < AParameters.Count -1 then
      S := S + '<' + AParameters[I] + '>' + ', '
    else
      S := S + '<' + AParameters[I] + '>';
  end;
  Result := Format(CallTemplate, [AProcedure, S])
end;

function TSQLTemplates.BuildSQLDelete(const ATable: string): string;
begin
  Result := Format(DeleteTemplate, [ATable]);
end;

function TSQLTemplates.BuildSQLInsert(const ATable: string;
  AFields: TStrings): string;
var
  sFields : string;
  sValues : string;
  I       : Integer;
begin
  for I := 0 to AFields.Count - 1 do
  begin
   if I < AFields.Count -1  then
   begin
     sFields := sFields + AFields[I] + ', ';
     sValues := sValues + '<' + AFields[I] + '>' + ', ';
   end
   else
   begin
     sFields := sFields + AFields[I];
     sValues := sValues + '<' + AFields[I] + '>';
   end;
  end;
  Result := Format(InsertTemplate, [ATable, sFields, sValues]);
end;

function TSQLTemplates.BuildSQLSelect(const ATable: string): string;
begin
  Result := Format(SelectTemplate, ['*', ATable]);
end;

function TSQLTemplates.BuildSQLSelect(const ATable: string;
  AFields: TStrings): string;
var
  S : string;
  I : Integer;
begin
  if AFields.Count = 1 then
    Result := Format(SelectTemplate, [AFields[0], ATable])
  else
  begin
    for I := 0 to AFields.Count - 1 do
    begin
      if I = 0 then
        S := AFields[I] + ','
      else if I < AFields.Count - 1 then
        S := S + #13#10 + '  ' + AFields[I] + ','
      else
        S := S + #13#10 + '  ' + AFields[I];
    end;
    Result :=  Format(SelectTemplate, [S, ATable]);
  end;
end;

function TSQLTemplates.BuildSQLSelect(const ATable: string;
  AFields: TStrings; AValues: Variant): string;
var
  S      : string;
  sValue : string;
  I      : Integer;
begin
  if VarIsArray(AValues) then
  begin
    for I := 0 to AFields.Count - 1 do
    begin
      if AValues[I] <> Null then
      begin
        if VarIsType(AValues[I], varDate) then
        begin
          sValue := FormatDateTime('yyyy-mm-dd hh:mm:ss', AValues[i])
        end
        else
          sValue := AValues[I];
        S := S + Format(ASSIGNMENT, [AFields[I], QuotedStr(sValue)])
      end
      else
        S := S + Format(ISNULL, [AFields[I]]);
      if I < AFields.Count - 1 then
        S := S + ' AND' + #13#10
    end;
  end;
  Result := BuildSQLSelect(ATable, AFields) + Format(WhereTemplate, [S]);
end;

function TSQLTemplates.BuildSQLSelect(const ATable: string;
  const ADataSet: TDataSet): string;
var
  I      : Integer;
  S      : string;
  sValue : string;
  SL     : TStrings;
begin
  for I := 0 to ADataSet.Fields.Count - 1 do
  begin
    if not ADataSet.Fields[I].IsNull then
    begin
      case ADataSet.Fields[I].DataType of
        ftDateTime :
          sValue := FormatDateTime(DATETIME_FORMAT,
                                   ADataSet.Fields[I].AsDateTime);
        ftDate :
          sValue := FormatDateTime(DATE_FORMAT, ADataSet.Fields[I].AsDateTime);
        ftTime :
          sValue := FormatDateTime(DATE_FORMAT, ADataSet.Fields[I].AsDateTime);
        else
          sValue := ADataSet.Fields[I].AsString;
      end; // case ADataSet.Fields[I].DataType of
      S := S + Format(ASSIGNMENT, [ADataSet.Fields[I].FieldName,
                                   QuotedStr(sValue)]);
    end  // if not ADataSet.Fields[I].IsNull then
    else
      S := S + Format(ISNULL, [ADataSet.Fields[I].FieldName]);
    if I < ADataSet.Fields.Count - 1 then
      S := S + ' AND' + #13#10;
  end;
  SL := TStringList.Create;
  SL.Assign(ADataSet.FieldList);
  try
    Result := BuildSQLSelect(ATable, SL) + Format(WhereTemplate, [S]);
  finally
    SL.Free;
  end;
end;

function TSQLTemplates.BuildSQLUpdate(const ATable: string;
  AFields: TStrings): string;
var
  I: Integer;
  S : string;
begin
  for I := 0 to AFields.Count - 1 do
  begin
    if I = 0 then
      S := Format(AssignmentTemplate, [AFields[I]])
    else
      S := S + #13#10 + Format(AssignmentTemplate, [AFields[I]]);
    if I < AFields.Count - 1 then
      S := S + ',';
  end;
  Result := Format(UpdateTemplate, [ATable, S]);
end;
{$ENDREGION}

end.

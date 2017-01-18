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

unit ts.Classes.SQL.ComparisonCondition;

interface

uses
  System.Classes, System.SysUtils,

  ts.Classes.SQL.Condition, ts.Classes.SQL.Params;

type
  ESQLComparisonCondition = class(Exception);

// comparison condition
// like_condition
// null_condition

//not supported:
//| logical_condition (on boolean fields or expressions)

// TODO : custom comparisontype? (with user defined comparisontemplate)

type
  TSQLComparisonType = (
    sctEquals,            // L = R
    sctNotEquals,         // L <> R
    sctLike,              // L LIKE R
    sctNotLike,           // L NOT LIKE R
    sctContains,          // L LIKE '%R%'
    sctNotContains,       // L NOT LIKE '%R%'
    sctStartsWith,        // L LIKE 'R%'
    sctNotStartsWith,     // L LIKE 'R%'
    sctEndsWith,          // L LIKE '%R'
    sctNotEndsWith,
    sctMoreThan,          // L > R
    sctNotMoreThan,       // L <= R
    sctLessThan,          // L < R
    sctNotLessThan,       // L >= R
    sctIsNull,            // L IS NULL
    sctIsNotNull,         // L IS NOT NULL
    sctIn                 // L IN (R)
  );

const
  SQLComparisonTemplates : array[TSQLComparisonType] of string = (
    '%s = %s',
    '%s <> %s',
    '%s LIKE %s',
    'NOT(%s LIKE %s)',
    '%s LIKE %s',
    '(NOT(%s LIKE %s) OR (%0:s IS NULL))',
    '%s LIKE %s',
    '(NOT(%s LIKE %s) OR (%0:s IS NULL))',
    '%s LIKE %s',
    '(NOT(%s LIKE %s) OR (%0:s IS NULL))',
    '%s > %s',
    '%s <= %s',
    '%s < %s',
    '%s >= %s',
    '%s IS NULL',
    '%s IS NOT NULL',
    '%s IN (%s)'
  );

type
  TCustomSQLComparisonCondition = class(TCustomSQLCondition)
  private
    FFieldName         : string;
    FComparisonType    : TSQLComparisonType;
    FCollationFunction : string;
    FFieldValue        : Variant;

    procedure SetFieldName(const Value: string);
    procedure SetParamName(const Value: string);
    procedure SetComparisonType(const Value: TSQLComparisonType);
    function GetParamName: string;
    procedure SetCollationFunction(const Value: string);
    procedure SetFieldValue(const Value: Variant);
    function GetFieldValue: Variant;
  protected

    procedure UpdateConditionString; override;

  public
    { The SQL fieldname used in the expression. }
    property FieldName : string
      read FFieldName write SetFieldName;

    property FieldValue : Variant
      read GetFieldValue write SetFieldValue;

    { References a parameter in the <Params> collection. }
    property ParamName : string
      read GetParamName write SetParamName;

    { Indicates which type of comparison expression is represented by this
      condition. }
    property ComparisonType : TSQLComparisonType
      read FComparisonType write SetComparisonType;

    { Optional server function that can be applied to the <FieldName> in the
      SQL expression. }
    property CollationFunction : string
      read FCollationFunction write SetCollationFunction;
  end;

  TSQLComparisonCondition = class(TCustomSQLComparisonCondition)
  published
    property CollationFunction;
    property ComparisonType;
    property Enabled;
    property FieldName;
    property FieldValue;
    property ParamName;
    property QuoteValues;
  end;

implementation

uses
  System.Variants, System.TypInfo;

{$REGION 'property access methods'}
//---|ComparisonType|----------------------------------------------------------

procedure TCustomSQLComparisonCondition.SetComparisonType(const Value:
  TSQLComparisonType);
begin
  if Value <> ComparisonType then
  begin
    FComparisonType := Value;
    Changed;
  end;
end;

//---|CollationFunction|-------------------------------------------------------

procedure TCustomSQLComparisonCondition.SetCollationFunction(const Value: string);
begin
  if Value <> CollationFunction then
  begin
    FCollationFunction := Value;
    Changed;
  end;
end;

//---|FieldName|---------------------------------------------------------------

procedure TCustomSQLComparisonCondition.SetFieldName(const Value: string);
begin
  if Value <> FieldName then
  begin
    FFieldName := Value;
    Changed;
  end;
end;

//---|FieldValue|--------------------------------------------------------------

function TCustomSQLComparisonCondition.GetFieldValue: Variant;
begin
  if ParamIndex = -1 then
    Result := FFieldValue
  else
    Result := Params.Items[ParamIndex].Value;
end;

procedure TCustomSQLComparisonCondition.SetFieldValue(const Value: Variant);
begin
  if Value <> FieldValue then
  begin
    if ParamIndex = -1 then
    begin
      FFieldValue := Value;
    end
    else
    begin
      FFieldValue := Unassigned;
      Params.Items[ParamIndex].Value := Value;
    end;
  end;
end;
//---|ParamName|---------------------------------------------------------------

function TCustomSQLComparisonCondition.GetParamName: string;
begin
  if (ParamIndex > -1) and (ParamIndex < Params.Count) then
    Result := Params.Items[ParamIndex].Name
  else
    Result := '';
end;

procedure TCustomSQLComparisonCondition.SetParamName(const Value: string);
begin
  if Value <> ParamName then
  begin
    ParamIndex := Params.IndexOf(Value);
    if ParamIndex = -1 then
      raise ESQLComparisonCondition.CreateFmt('Param %s does not exist', [Value]);
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomSQLComparisonCondition.UpdateConditionString;
var
  sLeft      : string;  // left operand of the condition expression
  sRight     : string;  // right operand of the condition expression
  sCondition : string;
  ParamValue : Variant;
  CT         : TSQLComparisonType;
begin
  inherited;
  if ComparisonType in [sctIsNull, sctIsNotNull] then
    ParamValue := Null
  else
    ParamValue := FieldValue;
  if (not VarIsEmpty(ParamValue)) and
      not (VarIsStr(ParamValue) and (ParamValue = '')) then
  begin
    if CollationFunction <> '' then
      sLeft := Format('%s(%s)', [CollationFunction, FieldName])
    else
      sLeft := FieldName;

    CT := ComparisonType;
    case ComparisonType of
      sctEquals, sctNotEquals, sctLike, sctNotLike, sctMoreThan,
      sctNotMoreThan, sctLessThan, sctNotLessThan:
      begin
        if ParamValue = Null then
          CT := sctIsNull
        else
        begin
          if QuoteValues then
            sRight := AnsiQuotedStr(ParamValue, '''')
          else
            sRight := ParamValue;
        end;
      end;

      sctContains, sctNotContains:
        sRight := AnsiQuotedStr('%' +  VarToStr(ParamValue) + '%', '''');

      sctStartsWith, sctNotStartsWith:
        sRight := AnsiQuotedStr(VarToStr(ParamValue) + '%', '''');

      sctEndsWith, sctNotEndsWith:
        sRight := AnsiQuotedStr('%' + VarToStr(ParamValue), '''');

      sctIsNull, sctIsNotNull: ; // do nothing

      // TODO : Support for quoted strings on this level needs to be supported
      sctIn:
        sRight := FieldValue;

      else
        raise ESQLComparisonCondition.CreateFmt(
               'ComparisonType %s not supported',
               [GetEnumName(TypeInfo(TSQLComparisonType), Ord(ComparisonType))]);
    end;

    if CT in [sctIsNull, sctIsNotNull] then
      sCondition := Format(SQLComparisonTemplates[CT], [sLeft])
    else
      sCondition := Format(SQLComparisonTemplates[CT], [sLeft, sRight]);
  end
  else
    sCondition := '';
  SetConditionString(sCondition);
end;
{$ENDREGION}

end.

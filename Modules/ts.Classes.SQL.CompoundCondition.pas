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

unit ts.Classes.SQL.CompoundCondition;

{
  REMARK : never pass 0 to a Variant, because the Variant will get the value
  Unassigned. If you need to use the value zero, pass it as a string '0' or
  '0.00' etc.
}

interface

uses
  System.SysUtils, System.Classes, System.Contnrs,

  ts.Classes.SQL.Params, ts.Classes.SQL.Condition,
  ts.Classes.SQL.ComparisonCondition;

type
  ESQLCompoundCondition = class(Exception);

// compound_condition

// AND conjunction
// OR  disjunction

{
| membership_condition
| range_condition
| exists_condition
}

type
  TSQLJunctionType = (
    sjtAND,
    sjtOR
  );

const
  SQLJunctionStrings : array[TSQLJunctionType] of string = (
    'AND',
    'OR'
  );

type
  TCustomSQLCompoundCondition = class(TCustomSQLCondition)
  private
    FConditions   : TObjectList;
    FJunctionType : TSQLJunctionType;

  protected
    function GetItem(Index: Integer): TCustomSQLCondition;
    procedure SetItem(Index: Integer; const Value: TCustomSQLCondition);
    function GetCount: Integer;
    function GetEnabledCount: Integer;
    procedure SetJunctionType(const Value: TSQLJunctionType);

    procedure OnConditionChanged(Sender: TObject);

  public
    constructor Create(ASQLParams: TSQLParams; AOwnsConditions: Boolean = True);
      reintroduce; virtual;
    destructor Destroy; override;

    function Add(ACondition: TCustomSQLCondition): Integer;

    function AddComparisonCondition(
      const AFieldName         : string;
      const AParamName         : string;
            AComparisonType    : TSQLComparisonType = sctEquals;
            AQuoteValues       : Boolean = True) : Integer; overload;

    function AddComparisonCondition(
      const AFieldName      : string;
            AComparisonType : TSQLComparisonType = sctIsNull) : Integer;
              overload;

    function AddComparisonCondition(
      const AFieldName      : string;
            AComparisonType : TSQLComparisonType;
            AFieldValue     : Variant;
            AQuoteValues    : Boolean = True) : Integer; overload;

    function AddComparisonCondition(
      const AFieldName      : string;
            AComparisonType : TSQLComparisonType;
      const AFieldValues    : array of const;
            AQuoteValues    : Boolean = True) : Integer; overload;

    procedure Update; override;
    procedure UpdateConditionString; override;

    procedure Clear;

    function Extract(Item: TCustomSQLCondition): TCustomSQLCondition;
    function Remove(ACondition: TCustomSQLCondition): Integer;
    procedure Delete(Index: Integer);
    function IndexOf(ACondition: TCustomSQLCondition): Integer;
    function FindInstanceOf(AClass   : TClass;
                            AExact   : Boolean = True;
                            AStartAt : Integer = 0): Integer;
    procedure Insert(Index: Integer; ACondition: TCustomSQLCondition);

    property Items[Index: Integer]: TCustomSQLCondition
      read GetItem write SetItem; default;

    property Count: Integer
      read GetCount;

    property EnabledCount: Integer
      read GetEnabledCount;
    property JunctionType: TSQLJunctionType
      read FJunctionType write SetJunctionType default sjtAND;
  end;

  TSQLCompoundCondition = class(TCustomSQLCompoundCondition)
  published
    property QuoteValues;
    property Enabled;
    property JunctionType;
  end;

implementation

uses
  System.Variants, System.Types,

  ts.Utils;

{$REGION 'non-interfaced routines'}
function ArrayToStr(const AArray: array of const; const ASep: string;
  const AAllowEmptyString: Boolean; const AQuoteValues : Boolean): string;
var
  I, L: Integer;
  S   : string;
begin
  Result := '';
  for I := Low(AArray) to High(AArray) do
  begin
    S := VarRecToString(AArray[I]);
    if (S <> '') or AAllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      if AQuoteValues then
        Result := Result + QuotedStr(S)
      else
        Result := Result + S;
      Result := Result + ASep;
    end;
  end;
  // remove terminating separator
  if Length(AArray) <> 0 then
  begin
    L := Length(ASep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TCustomSQLCompoundCondition.Create(ASQLParams : TSQLParams;
  AOwnsConditions : Boolean);
begin
  inherited Create(ASQLParams);
  FJunctionType := sjtAND;
  FConditions   := TObjectList.Create(AOwnsConditions);
end;

destructor TCustomSQLCompoundCondition.Destroy;
begin
  FreeAndNil(FConditions);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Count|-------------------------------------------------------------------

function TCustomSQLCompoundCondition.GetCount: Integer;
begin
  Result := FConditions.Count;
end;

//---|EnabledCount|------------------------------------------------------------

function TCustomSQLCompoundCondition.GetEnabledCount: Integer;
var
  I : Integer;
  N : Integer;
begin
  N := 0;
  for I := 0 to Count - 1 do
    if Items[I].Enabled then
      Inc(N);
  Result := N;
end;

//---|Items|-------------------------------------------------------------------

function TCustomSQLCompoundCondition.GetItem(Index: Integer): TCustomSQLCondition;
begin
  Result := FConditions[Index] as TCustomSQLCondition;
end;

procedure TCustomSQLCompoundCondition.SetItem(Index: Integer;
  const Value: TCustomSQLCondition);
begin
  if FConditions[Index] <> Value then
  begin
    FConditions[Index] := Value;
    Changed;
  end;
end;

//---|JunctionType|------------------------------------------------------------

procedure TCustomSQLCompoundCondition.SetJunctionType(const Value:
  TSQLJunctionType);
begin
  if Value <> JunctionType then
  begin
    FJunctionType := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TCustomSQLCompoundCondition.OnConditionChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomSQLCompoundCondition.UpdateConditionString;
const
  BINARY_JUNCTION = '(%s) %s (%s)';
  MULTI_JUNCTION  = '%s %s (%s)';
var
  I, N             : Integer;
  S                : string;
  sConditionString : string;
  C                : TCustomSQLCondition;
begin
  inherited;
  sConditionString := '';
  N := 0;
  for I := 0 to FConditions.Count - 1 do
  begin
    C := Items[I];
    C.Update;
    S := C.ConditionString;
    if S <> '' then
    begin
      Inc(N);
      case N of
        1 : sConditionString := S;
        2 : sConditionString :=
              Format(BINARY_JUNCTION,
                    [sConditionString, SQLJunctionStrings[JunctionType], S]);
        else
          sConditionString :=
            Format(MULTI_JUNCTION,
                   [sConditionString, SQLJunctionStrings[JunctionType], S]);
      end; // case
    end; // if S <> '' then
  end; // for I := 0 to FConditions.Count - 1 do
  SetConditionString(sConditionString);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TCustomSQLCompoundCondition.Add(ACondition: TCustomSQLCondition):
  Integer;
begin
  Result := FConditions.Add(ACondition);
  ACondition.OnChanged := OnConditionChanged;
  Changed;
end;

function TCustomSQLCompoundCondition.AddComparisonCondition(const AFieldName,
  AParamName: string; AComparisonType: TSQLComparisonType;
  AQuoteValues : Boolean): Integer;
var
  CC : TCustomSQLComparisonCondition;
begin
  CC := TCustomSQLComparisonCondition.Create(Params);
  CC.FieldName := AFieldName;

  // if the parameter does not exist, create it
  if not Assigned(Params.Find(AParamName)) then
    Params[AParamName] := Unassigned;

  CC.ParamName := AParamName;
  CC.ComparisonType := AComparisonType;
  CC.QuoteValues := AQuoteValues;
  Result := Add(CC);
end;

function TCustomSQLCompoundCondition.AddComparisonCondition(
  const AFieldName: string; AComparisonType: TSQLComparisonType;
  const AFieldValues: array of const; AQuoteValues: Boolean): Integer;
var
  CC : TCustomSQLComparisonCondition;
begin
  if AComparisonType in [sctIn] then
  begin
    CC := TCustomSQLComparisonCondition.Create(Params);
    CC.FieldName := AFieldName;
    CC.ComparisonType := AComparisonType;
    CC.FieldValue := ArrayToStr(AFieldValues, ',', False, AQuoteValues);
    CC.QuoteValues := AQuoteValues;
    Result := Add(CC);
  end
  else
    raise Exception.Create('Invalid ComparisonType parameter specified');
end;

function TCustomSQLCompoundCondition.AddComparisonCondition(
  const AFieldName: string; AComparisonType: TSQLComparisonType;
  AFieldValue: Variant; AQuoteValues: Boolean): Integer;
var
  CC : TCustomSQLComparisonCondition;
begin
  if not (AComparisonType in [sctIsNull, sctIsNotNull]) then
  begin
    CC := TCustomSQLComparisonCondition.Create(Params);
    CC.FieldName := AFieldName;
    CC.FieldValue := AFieldValue;
    CC.QuoteValues := AQuoteValues;
    CC.ComparisonType := AComparisonType;
    Result := Add(CC);
  end
  else
    raise Exception.Create('Invalid ComparisonType');
end;

function TCustomSQLCompoundCondition.AddComparisonCondition(
  const AFieldName: string; AComparisonType: TSQLComparisonType): Integer;
var
  CC : TCustomSQLComparisonCondition;
begin
  if AComparisonType in [sctIsNull, sctIsNotNull] then
  begin
    CC := TCustomSQLComparisonCondition.Create(Params);
    CC.FieldName := AFieldName;
    CC.ComparisonType := AComparisonType;
    Result := Add(CC);
  end
  else
    raise Exception.Create('Invalid ComparisonType parameter specified');
end;

procedure TCustomSQLCompoundCondition.Delete(Index: Integer);
begin
  FConditions.Delete(Index);
end;

function TCustomSQLCompoundCondition.IndexOf(ACondition: TCustomSQLCondition)
  : Integer;
begin
  Result := FConditions.IndexOf(ACondition);
end;

procedure TCustomSQLCompoundCondition.Insert(Index: Integer;
  ACondition: TCustomSQLCondition);
begin
  FConditions.Insert(index, ACondition);
end;

function TCustomSQLCompoundCondition.Remove(ACondition: TCustomSQLCondition)
  : Integer;
begin
  Result := FConditions.Remove(ACondition);
end;

function TCustomSQLCompoundCondition.Extract(Item: TCustomSQLCondition)
  : TCustomSQLCondition;
begin
  Result := FConditions.Extract(Item) as TCustomSQLCondition;
end;

function TCustomSQLCompoundCondition.FindInstanceOf(AClass: TClass;
  AExact: Boolean; AStartAt: Integer): Integer;
begin
  Result := FConditions.FindInstanceOf(AClass, AExact, AStartAt);
end;

procedure TCustomSQLCompoundCondition.Update;
begin
  inherited;
  UpdateConditionString;
end;

procedure TCustomSQLCompoundCondition.Clear;
begin
  FConditions.Clear;
end;
{$ENDREGION}

end.

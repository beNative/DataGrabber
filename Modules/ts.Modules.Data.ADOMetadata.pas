unit ts.Modules.Data.ADOMetadata;

//*****************************************************************************

interface

uses
  Classes,

  ts.Interfaces;

//=============================================================================

type
  TtsADOMetaData = class(TComponent)
  private
    FConnecion : IConnection;

    function MetaDataFieldLookup(const AMetaField, ATable, AField: string)
      : Variant;
    function MetaDataLookup(const AMetaField, AMetaTable, ATable,
      AField: string) : Variant;

    function GetConnection: IConnection;
    procedure SetConnection(const Value: IConnection);

  public
    constructor Create(AOwner: TComponent; AConnection: IConnection); reintroduce;

    function IsStorable(const ATable, AField : string) : Boolean;
    function IsCalculated(const ATable, AField : string) : Boolean;
    function IsRelation(const ATable, AField : string) : Boolean;
    function IsRequired(const ATable, AField : string) : Boolean;
    function IsUnique(const ATable, AField : string) : Boolean;

    { IMetaData }
    procedure GetTableNames(const ASchemaName: string; AList: TStrings); overload;
    procedure GetTableNames(AList : TStrings); overload;
    procedure GetFieldNames(const ATableName: string; AList: TStrings);
    procedure GetSchemaNames(AList : TStrings);

    function TableExists(const ATableName : string) : Boolean;
    function StoredProcedureExists(const AProcedureName : string) : Boolean;

    property Connection : IConnection
      read GetConnection write SetConnection;
  end;

//*****************************************************************************

implementation

uses
  SysUtils, Variants, StrUtils,

  JclStrings,

  ADODB,

  ts.DBUtils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TtsADOMetaData.Create(AOwner: TComponent;
  AConnection: IConnection);
begin
  inherited Create(AOwner);
  FConnecion := AConnection;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Connection|--------------------------------------------------------------

function TtsADOMetaData.GetConnection: IConnection;
begin
  Result := FConnecion;
end;

procedure TtsADOMetaData.SetConnection(const Value: IConnection);
begin
  FConnecion := Value;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

function TtsADOMetaData.MetaDataFieldLookup(const AMetaField, ATable,
  AField: string): Variant;
begin
  Result :=
    MetaDataLookup(AMetaField, '%Dictionary.CompiledProperty', ATable, AField);
end;

//-----------------------------------------------------------------------------

function TtsADOMetaData.MetaDataLookup(const AMetaField, AMetaTable, ATable,
  AField: string): Variant;
const
  QUERY = 'SELECT %s FROM %s WHERE SqlFieldName = %s AND parent = %s';
begin
  Result := QueryLookup(
    (Connection as IADOConnection).ADOConnection,
    QUERY,
    [AMetaField, AMetaTable, QuotedStr(AField), QuotedStr(ATable)]
  );
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Returns True if the field is calculated. }

function TtsADOMetaData.IsCalculated(const ATable, AField: string): Boolean;
begin
  Result :=
    MetaDataFieldLookup('SqlComputed | Calculated', ATable, AField) = '1';
end;

//-----------------------------------------------------------------------------

{ Returns True if the field represents a relationship (foreign key). }

function TtsADOMetaData.IsRelation(const ATable, AField: string): Boolean;
begin
  Result := MetaDataFieldLookup('Relationship', ATable, AField) = '1';
end;

//-----------------------------------------------------------------------------

{ Returns True if the field is required. }

function TtsADOMetaData.IsRequired(const ATable, AField: string): Boolean;
begin
  Result := MetaDataFieldLookup('Required', ATable, AField) = '1';
end;

//-----------------------------------------------------------------------------

{ Returns True if the field can be persisted. }

function TtsADOMetaData.IsStorable(const ATable, AField: string): Boolean;
begin
  Result := MetaDataFieldLookup('Storable', ATable, AField) = '1';
end;

//-----------------------------------------------------------------------------

{ Returns True if the field has a unique index. }

function TtsADOMetaData.IsUnique(const ATable, AField: string): Boolean;
const
  QUERY = 'SELECT _Unique' + #13#10 +
          'FROM %%Dictionary.CompiledIndex' + #13#10 +
          'WHERE Properties = %s AND parent = %s';
begin
  Result := QueryLookup(
    (Connection as IADOConnection).ADOConnection,
    QUERY,
    [QuotedStr(AField), QuotedStr(ATable)]) = '1';
end;

//-----------------------------------------------------------------------------

{ Checks if the given stored procedure name exists in the database. The name is
  case sensitive. }

function TtsADOMetaData.StoredProcedureExists(
  const AProcedureName: string): Boolean;
begin
  try
    Result := QueryLookup(
      (Connection as IADOConnection).ADOConnection,
      '%Dictionary.MethodDefinition',
      'COUNT(*)',
      AProcedureName,
      '(parent || ''_'' || Name)') > 0;
  except
    Result := False;
  end;
end;

//-----------------------------------------------------------------------------

function TtsADOMetaData.TableExists(const ATableName: string): Boolean;
begin
//
  Result := False;
end;

//-----------------------------------------------------------------------------

procedure TtsADOMetaData.GetFieldNames(const ATableName: string;
  AList: TStrings);
var
  sSchema : string;
  sTable  : string;
  TBL     : TADODataSet;
  iPos    : Integer;
begin
  iPos := Pos('.', ATableName) + 1;
  if iPos <> 0 then
  begin
    sSchema := Copy(ATableName, 1, iPos - 2);
    sTable  := Copy(ATableName, iPos, Length(ATableName) - iPos + 1);
    if MatchText(sSchema, ['dbo']) then
      sSchema := '';
  end
  else
  begin
    sSchema := '';
    sTable  := ATableName;
  end;

  // raises EOLEException when no dataprovider is assigned
  TBL := TADODataSet.Create(nil);
  AList.Clear;
  AList.BeginUpdate;
  (Connection as IADOConnection).ADOConnection.GetFieldNames(sTable, AList);
  try
//    try
//      Connection.ADOConnection.OpenSchema(siColumns,
//        VarArrayOf([Unassigned, sSchema, sTable, Unassigned]), EmptyParam, TBL);
//      TBL.First;
//      while not TBL.Eof do
//      begin
//        sField := Trim(TBL.FieldByName('COLUMN_NAME').AsString);
//        if StrContainsChars(sField, [' ', '-'], False) then
//          sField := Format('[%s]', [sField]);
//        if sField <> '' then
//          AList.Add(sField);
//        TBL.Next;
//      end
//    except
//      Connection.ADOConnection.GetFieldNames(sTable, AList);
//    end;

  finally
    TBL.Free;
    AList.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsADOMetaData.GetSchemaNames(AList: TStrings);
var
  TBL     : TADODataSet;
  sSchema : string;
begin
  TBL := TADODataSet.Create(nil);
  AList.Clear;
  AList.BeginUpdate;
  try
    (Connection as IADOConnection).ADOConnection.OpenSchema(siSchemata, EmptyParam, EmptyParam, TBL);
    TBL.First;
    while not TBL.Eof do
    begin
      sSchema := Trim(TBL.FieldByName('SCHEMA_NAME').AsString);
      if sSchema <> '' then
        AList.Add(sSchema);
      TBL.Next;
    end
  finally
    TBL.Free;
    AList.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsADOMetaData.GetTableNames(const ASchemaName: string;
  AList: TStrings);
var
  TBL     : TADODataSet;
  sSchema : string;
  sTable  : string;
begin
  TBL := TADODataSet.Create(nil);
  AList.Clear;
  try
    (Connection as IADOConnection).ADOConnection.OpenSchema(siTables, EmptyParam, EmptyParam, TBL);
    TBL.First;
    while not TBL.Eof do
    begin
      sSchema := Trim(TBL.FieldByName('TABLE_SCHEMA').AsString);
      if sSchema = ASchemaName then
      begin
        sTable := sSchema + '.'  + TBL.FieldByName('TABLE_NAME').AsString;
        if StrContainsChars(sTable, [' ', '-'], False) then
          sTable := Format('[%s]', [sTable]);
        AList.Add(sTable);
      end;
      TBL.Next;
    end
  finally
    TBL.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsADOMetaData.GetTableNames(AList: TStrings);
var
  TBL     : TADODataSet;
  sSchema : string;
  sTable  : string;
begin
  TBL := TADODataSet.Create(nil);
  AList.Clear;
  try
    (Connection as IADOConnection).ADOConnection.OpenSchema(siTables, EmptyParam, EmptyParam, TBL);
    TBL.First;
    while not TBL.Eof do
    begin
      sSchema := Trim(TBL.FieldByName('TABLE_SCHEMA').AsString);
      if not MatchText(sSchema, ['', 'dbo']) then
        sTable := sSchema + '.'  + TBL.FieldByName('TABLE_NAME').AsString
      else
        sTable := TBL.FieldByName('TABLE_NAME').AsString;
      if StrContainsChars(sTable, [' ', '-'], False) then
          sTable := Format('[%s]', [sTable]);
      AList.Add(sTable);
      TBL.Next;
    end
  finally
    TBL.Free;
  end;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.


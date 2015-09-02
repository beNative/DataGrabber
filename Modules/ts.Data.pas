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

unit ts.Data;

{ Author: Tim Sinaeve }

{
  Base module for generic SQL table administration (SELECT/INSERT/DELETE/UPDATE).

  This unit incorporates all functionality required for applications that need
  to do operations on SQL database tables.
    1. Simple listings, custom selections and for building reports.
    2. Table administration (SELECT/INSERT/DELETE/UPDATE of a single record)
    3. The combination of 1 & 2 : editable lists that support data operations on
       multiple records (using MultiSelect).

  This unit incorporates all the functionality needed to manage a database table
  using a typical TDataSet-TDataSetProvider-TClientDataSet combination to
  support following basic operations:
    - Runtime generation of SQL statements with full control over dynamic
      conditions and parameters.
    - Select/Insert/Delete/Update
    - Incremental fetch (for large tables)
    - Cached updates (currently not yet supported, but possible thanks to the
      TDataSetProvider-TClientDataSet combination.)

  IData : the interface to communicate with the GUI (or View). Communication
          between both is done using the IDataView interface. One IData instance
          can support multiple IDataView instances.

  TdmCustomModule defines a set of virtual methods, that are intended to be
  overridden in descendants to give the developer full control of how the
  data has to be fetched from the database table(s) by providing hooks into the
  dynamic generation of the SQL statements.
    - <Initialize>
    - <DefineSQLConditions>

  -----------------------------------------------------------------------------
  Typical implementation steps for creating derived datamodules:

    1. Override Initialize
        - Build the FieldList property (using BuildFieldList) by assigning the
          selected fields.
        - Assign DisplayLabels for each field.

            procedure Initialize;
            begin
              inherited;
              BuildFieldList(SELECTED_FIELDS_ARRAY);

              Displaylabels[<fieldname>] := <resourcestring>;
              ...
            end;

    2. Override DefineSQLConditions
    3. Override GetFromClause, [GetGroupByClause], [GetOrderByClause]

  NEW:
  - Simple mode using the new SQL property
  - ProviderMode:
       True : DataProvider - ClientDataSet is connected to the (native) dataset.
       False: direct mode. Operating directly on native dataset.
              In this case a bi-directional cursor is required.

  -----------------------------------------------------------------------------
  REMARKS
    - Date parameters should always be assigned a native Delphi TDateTime or
      TDate value. Conversions to the expected DB-type need to be done in the
      DefineSQLConditions method.

    - Fields should always be dynamically created in all descendant modules.
      This makes all listings more flexible and most changes to field
      definitions in the DBMS won't require modifications in the listing
      modules.
  -----------------------------------------------------------------------------
  Typical call sequence (can differ in descendant implementations):

    Execute -> GenerateSQL -> DefineSQLConditions
                           -> GetFromClause
                           -> GetGroupByClause
                           -> GetHavingClause
                           -> GetOrderByClause
            -> InternalExecute            (executes the generated SQL statement)
                 -> InitFields -> InitField (for every dataset-field)
                 -> ExecuteDetails -> InternalExecuteDetails

                 (place where dependant data modules
                 can update their data when the
                 current record of the dataset changes)
            -> UpdateDisplayValues        (fetches extra data to display in the
                                           attached view)
            -> AssignRecordCount          (assigns the RecordCount property)
}

{
  TODO
     - Handle OnReconcileErrors event to show errors when applying updates

     - if we provide a simple SQL statement in GenerateSQL, the class should
       check this and automatically parse this statement and provide the
       appropriate values.
       => maybe using gaSQLParser?

     - OnGetTableName of the provider needs to get assigned the TableName
       property.

     - Support for stored procedures that return a dataset.

     - The native dataset (source dataset) should be called by an interfaced object.

     - Replace FUpdating with UpdateLock

     - Transaction support
         - Start Transaction
         - Commit Transaction
         - Rollback Transaction

     - execute in a seperate thread
}

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  Data.DB,
  Datasnap.DBClient, DataSnap.Provider,

  ts.Classes.SQL.CompoundCondition, ts.Classes.SQL.Params,
  ts.Classes.KeyValues, ts.Classes.ModuleManager,

  ts.Data.Selection, ts.Data.Report, ts.Data.Native,

  ts.Interfaces;

const
  DEFAULT_KEYNAME = 'ID';
  // for MS-SQL $IDENTITY can be used

type
  TDataViewMethod = procedure(AIndex: Integer) of object;

// REMARK: IData is REQUIRED in the header of any module that inherits from this
// base module. If we ommit this we end up with
// hard to trace bugs!!!

type
  TdmCustomModule = class (TDataModule, IConnection,
                                        IData,
                                        IUpdatable,
                                        IDataSelection,
                                        IDataReport)
    cdsMaster : TClientDataSet;
    dspMaster : TDataSetProvider;
    dscMaster : TDataSource;

    procedure cdsMasterBeforeOpen(ADataSet: TDataSet);
    procedure cdsMasterAfterOpen(ADataSet: TDataSet);
    procedure cdsMasterAfterScroll(ADataSet: TDataSet);
    procedure cdsMasterAfterPost(ADataSet: TDataSet);
    procedure dspMasterUpdateData(Sender: TObject; DataSet: TCustomClientDataSet);
    procedure dspMasterAfterUpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
    procedure cdsMasterAfterDelete(ADataSet: TDataSet);
    procedure dscMasterDataChange(Sender: TObject; Field: TField);
    procedure dspMasterGetTableName(Sender: TObject; DataSet: TDataSet;
      var TableName: WideString);
    procedure dspMasterBeforeUpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
      var Applied: Boolean);
    procedure dspMasterUpdateError(Sender: TObject;
      DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
      var Response: TResolverResponse);

  private
    // True when the list is executed for the current user criteria
    FUpdating          : Boolean;  // status flags (can be combined in a
    FRefreshed         : Boolean;  // Status set property).
    FExecuted          : Boolean;  //
    FProviderMode      : Boolean;
    FConnection        : IConnection;
    FParams            : TSQLParams;            // condition value parameters
    FConditions        : TSQLCompoundCondition;
    FRecordCount       : Integer;
    FModuleItem        : TtsModuleItem;
    FKeyName           : string;
    FFieldNames        : TStrings;
    // holds the corresponding display values for the given parameter
    FDisplayValues     : TtsKeyValues;
    FDisplayLabels     : TtsKeyValues;
    FMaxRecords        : Integer;
    FTableName         : string;
    FOnAfterUpdateData : TAfterUpdateDataEvent;
    FDataViews         : TInterfaceList;
    FSQL               : string;
    FPacketRecords     : Integer;
    FSelection         : TDataSelection;
    FReport            : TDataReport;
    FNativeDataSet     : INativeDataSet;

    // private property access methods
    function GetFields: TFields;
    function GetActive: Boolean;
    function GetClientDataSet: TClientDataSet;
    function GetConnection: IConnection;
    procedure SetRecordCount(const Value: Integer);
    function GetExecuted: Boolean;
    function GetCanModify: Boolean;
    function GetMaxRecords: Integer;
    procedure SetMaxRecords(const Value: Integer);
    function GetFieldNames: TStrings;
    function GetConditions: TSQLCompoundCondition;
    function GetParams: TSQLParams;
    function GetDisplayValues: TtsKeyValues;
    function GetDisplayLabels : TtsKeyValues;
    function GetModuleItem: TtsModuleItem;
    function GetFetchOnDemand: Boolean;
    procedure SetFetchOnDemand(const Value: Boolean);
    function GetPacketRecords: Integer;
    procedure SetPacketRecords(const Value: Integer);
    function GetSQL: string;
    procedure SetSQL(const Value: string);

    // SourceDataSet Events
    procedure SourceDataSetAfterOpen(ADataSet: TDataSet);

  protected
    // protected property access methods
    function GetDataSet: TDataSet; virtual;
    function GetRecordCount: Integer; virtual;

    procedure SetExecuted(const Value: Boolean); virtual;
    function GetFromClause : string; virtual;
    function GetGroupByClause : string; virtual;
    function GetHavingClause: string; virtual;
    function GetOrderByClause : string; virtual;
    function GetSourceDataSet: TDataSet; virtual;
    function GetProviderMode: Boolean; virtual;
    procedure SetProviderMode(const Value: Boolean); virtual;

    { IUpdatable }
    function GetKeyName: string;
    procedure SetKeyName(const Value: string);
    function GetTableName : string;
    procedure SetTableName(const Value: string);
    function GetOnAfterUpdateData : TAfterUpdateDataEvent;
    procedure SetOnAfterUpdateData(const Value : TAfterUpdateDataEvent);

    procedure DataSetFieldChanged(AField : TField); virtual;
    function GetRowID : Variant; virtual;
    procedure UpdateTableName(const ASQL: string);

    // protected virtual methods
    procedure AssignConnection; virtual;
    procedure AssignKeyValues(AKeyValues : TtsKeyValues); virtual;
    procedure AssignRecordCount; virtual;
    procedure AssignProviderMode; virtual;

    { IUpdatable }
    procedure BeginUpdate;
    procedure EndUpdate;
    function Post: Boolean;

    procedure DoAfterUpdateData(AUpdateKind : TUpdateKind); dynamic;

    function IsLookupField(const AFieldName : string) : Boolean; virtual;
    function IsCheckBoxField(const AFieldName : string) : Boolean; virtual;
    function IsMemoField(const AFieldName : string) : Boolean; virtual;
    function IsRequiredField(const AFieldName : string) : Boolean; virtual;
    procedure RefreshRecord(ADataSet: TCustomClientDataSet); virtual;

    procedure BeforeExecute; virtual;
    procedure AfterExecute; virtual;

    procedure Initialize; virtual;
    procedure PostUpdates(AClientDataSet : TCustomClientDataSet); overload;
    procedure PostUpdates; overload; virtual;
    procedure DefineSQLConditions; virtual;

    procedure InitField(AField : TField); virtual;
    procedure InitProviderField(AField : TField); virtual;
    function GenerateSQL : string; virtual;
    procedure UpdateDisplayValue(const AName : string); virtual;
    procedure InternalExecuteDetails; virtual;

    // protected methods
    procedure BuildFieldList(const AFieldNames : array of string);
    procedure InitFields(ADataSet : TDataSet); virtual;
    procedure InitProviderFields(ADataSet : TDataSet);
    procedure UpdateDisplayValues;
    procedure InternalExecute(const ACommandText : string); virtual;
    procedure ExecuteDetails;

    procedure EnumerateDataViews(const AMethod: TDataViewMethod);
    procedure UpdateDataView(AIndex: Integer);
    procedure DataViewBeforeExecute(AIndex: Integer);
    procedure DataViewAfterExecute(AIndex: Integer);

     // protected properties
    { IData }
    property FieldNames : TStrings
      read GetFieldNames;

    property FromClause : string
      read GetFromClause;

    property GroupByClause : string
      read GetGroupByClause;

    property HavingClause : string
      read GetHavingClause;

    property OrderByClause : string
      read GetOrderByClause;

    property SQL: string
      read GetSQL write SetSQL;

    property ModuleItem : TtsModuleItem
      read GetModuleItem;

    property ProviderMode: Boolean
      read GetProviderMode write SetProviderMode;

    property Active: Boolean
      read GetActive;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;

    property PacketRecords : Integer
      read GetPacketRecords write SetPacketRecords;

    { IUpdatable }
    property TableName : string
      read GetTableName write SetTableName;

    property KeyName : string
      read GetKeyName write SetKeyName;

    property OnAfterUpdateRecord : TAfterUpdateDataEvent
      read GetOnAfterUpdateData write SetOnAfterUpdateData;

    // not interfaced
    property Refreshed : Boolean
      read FRefreshed write FRefreshed;

    { Native dataset that is usually dependent on the connection strategy. }
    property SourceDataSet: TDataSet
      read GetSourceDataSet;

    { ClientDataSet that is used in ProviderMode. }
    property ClientDataSet: TClientDataSet
      read GetClientDataSet;

  public
    // construction and destruction
    constructor Create(
      AOwner      : TComponent;
      AConnection : IConnection;
      AKeyValues  : TtsKeyValues = nil;
      AModuleItem : TtsModuleItem = nil
    ); reintroduce; virtual;

    destructor Destroy; override;

    // public methods
    procedure Execute; virtual;
    procedure AfterConstruction; override;

    procedure RegisterDataView(ADataView: IDataView);
    procedure UnregisterDataView(ADataView: IDataView);
    procedure UpdateDataViews;

    // public properties
    property Conditions : TSQLCompoundCondition
      read GetConditions;

    property Connection : IConnection
      read GetConnection implements IConnection;

    property Executed : Boolean
      read GetExecuted write SetExecuted;

    property DataSet : TDataSet
      read GetDataSet;

    property RecordCount : Integer
      read GetRecordCount write SetRecordCount;

    property CanModify: Boolean
      read GetCanModify;

    { Limits the maximum amount of records to fetch from the database. When
      the value is 0, all records are returned. }
    property MaxRecords : Integer
      read GetMaxRecords write SetMaxRecords default 0;

    property Fields: TFields
      read GetFields;

    property Params : TSQLParams
      read GetParams;

    { Values retrieved from the database to show on the form. These values are
      intended to be assigned to the corresponding controls on the form in the
      method UpdateActions. }
    property DisplayValues : TtsKeyValues
      read GetDisplayValues;

    { Localizable field descriptions that will be shown to the user. }
    property DisplayLabels: TtsKeyValues
      read GetDisplayLabels;

    property Selection: TDataSelection
      read FSelection implements IDataSelection;

    property Report: TDataReport
      read FReport implements IDataReport;
  end;

implementation

{$R *.dfm}

uses
  System.Variants, System.StrUtils,
  Vcl.Forms, Vcl.Controls, Vcl.Dialogs,
  Data.DBCommon,

  ts.Modules.Memo,

  ts.Utils, ts.DBUtils;

const
  CR             = #13#10;

  SQL_SELECT     = 'SELECT' + CR +
                   '  %s';
  SQL_SELECT_TOP = 'SELECT TOP %d' + CR +
                   '  %s';
  SQL_FROM       = 'FROM' + CR +
                   '  %s';
  SQL_WHERE      = 'WHERE' + CR +
                   '  %s';
  SQL_GROUP_BY   = 'GROUP BY' + CR +
                   '  %s';
  SQL_HAVING     = 'HAVING' + CR +
                   '  %s';
  SQL_ORDER_BY   = 'ORDER BY' + CR +
                   '  %s';

{$REGION 'construction and destruction'}
constructor TdmCustomModule.Create(AOwner: TComponent; AConnection : IConnection;
  AKeyValues : TtsKeyValues; AModuleItem : TtsModuleItem);
begin
  inherited Create(AOwner);
  FConnection := AConnection;
  FModuleItem := AModuleItem;
  if Assigned(AKeyValues) then
    AssignKeyValues(AKeyValues);
end;

destructor TdmCustomModule.Destroy;
begin
  FConnection := nil;
  FNativeDataSet := nil;
  FreeAndNil(FParams);
  FreeAndNil(FConditions);
  FreeAndNil(FFieldNames);
  FreeAndNil(FDisplayValues);
  FreeAndNil(FDisplayLabels);
  FreeAndNil(FSelection);
  FreeAndNil(FReport);
  FreeAndNil(FDataViews);
  inherited;
end;

{ AfterConstruction is called automatically after the object’s last constructor
  has executed. }

procedure TdmCustomModule.AfterConstruction;
begin
  inherited;
  if not Assigned(FConnection) then
    raise Exception.Create('Connection is not assigned!');
  FNativeDataSet := FConnection.CreateNativeDataSet;
  FDataViews  := TInterfaceList.Create;
  FReport     := TDataReport.Create(Self);
  FSelection  := TDataSelection.Create(Self);
  FRecordCount                := 0;
  FMaxRecords                 := 0;
  FKeyName                    := DEFAULT_KEYNAME;
  FFieldNames                 := TStringList.Create;
  FFieldNames.QuoteChar       := ' ';
  FFieldNames.StrictDelimiter := True;
  FFieldNames.Delimiter       := ',';
  FParams        := TSQLParams.Create(Self);
  FConditions    := TSQLCompoundCondition.Create(FParams);
  FDisplayValues := TtsKeyValues.Create;
  FDisplayLabels := TtsKeyValues.Create;
  FProviderMode := True;
  dscMaster.DataSet := ClientDataSet;
  Initialize;
  UpdateDisplayValues;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmCustomModule.GetActive: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.Active;
end;

function TdmCustomModule.GetCanModify: Boolean;
begin
  Result := DataSet.CanModify and (FTableName <> '') and (FKeyName <> '');
end;

function TdmCustomModule.GetClientDataSet: TClientDataSet;
begin
  Result := cdsMaster;
end;

function TdmCustomModule.GetConditions: TSQLCompoundCondition;
begin
  Result := FConditions;
end;

function TdmCustomModule.GetConnection: IConnection;
begin
  Result := FConnection;
end;

function TdmCustomModule.GetDataSet: TDataSet;
begin
  if ProviderMode then
    Result := ClientDataSet
  else
    Result := SourceDataSet;
end;

function TdmCustomModule.GetDisplayLabels: TtsKeyValues;
begin
  Result := FDisplayLabels;
end;

function TdmCustomModule.GetDisplayValues: TtsKeyValues;
begin
  Result := FDisplayValues;
end;

function TdmCustomModule.GetExecuted: Boolean;
begin
  Result := FExecuted;
end;

procedure TdmCustomModule.SetExecuted(const Value: Boolean);
begin
  if Value <> Executed then
  begin
    FExecuted := Value;
    if not FExecuted then
      FRecordCount := 0
  end;
end;

function TdmCustomModule.GetFieldNames: TStrings;
begin
  Result := FFieldNames;
end;

function TdmCustomModule.GetFields: TFields;
begin
  Result := DataSet.Fields;
end;

function TdmCustomModule.GetFromClause: string;
begin
  Result := '';
end;

function TdmCustomModule.GetGroupByClause: string;
begin
  Result := '';
end;

function TdmCustomModule.GetHavingClause: string;
begin
  Result := '';
end;

function TdmCustomModule.GetFetchOnDemand: Boolean;
begin
  Result := ClientDataSet.FetchOnDemand;
end;

procedure TdmCustomModule.SetFetchOnDemand(const Value: Boolean);
begin
  if Value <> FetchOnDemand then
  begin
    ClientDataSet.FetchOnDemand := Value;
  end;
end;

function TdmCustomModule.GetKeyName: string;
begin
  Result := FKeyName;
end;

procedure TdmCustomModule.SetKeyName(const Value: string);
begin
  FKeyName := Value;
end;

function TdmCustomModule.GetMaxRecords: Integer;
begin
  Result := FMaxRecords;
end;

procedure TdmCustomModule.SetMaxRecords(const Value: Integer);
begin
  FMaxRecords := Value;
end;

function TdmCustomModule.GetModuleItem: TtsModuleItem;
begin
  Result := FModuleItem;
end;

function TdmCustomModule.GetOnAfterUpdateData: TAfterUpdateDataEvent;
begin
  Result := FOnAfterUpdateData;
end;

procedure TdmCustomModule.SetOnAfterUpdateData(
  const Value: TAfterUpdateDataEvent);
begin
  FOnAfterUpdateData := Value;
end;

function TdmCustomModule.GetOrderByClause: string;
begin
  Result := '';
end;

function TdmCustomModule.GetPacketRecords: Integer;
begin
  Result := ClientDataSet.PacketRecords;
end;

procedure TdmCustomModule.SetPacketRecords(const Value: Integer);
begin
  FPacketRecords := Value;
  if FetchOnDemand then
    ClientDataSet.PacketRecords := Value;
end;

function TdmCustomModule.GetParams: TSQLParams;
begin
  Result := FParams;
end;

function TdmCustomModule.GetProviderMode: Boolean;
begin
  Result := FProviderMode;
end;

procedure TdmCustomModule.SetProviderMode(const Value: Boolean);
begin
  BeginUpdate;
  try
    FProviderMode := Value;
    ClientDataSet.Active := False;
    SourceDataSet.Active := False;
    AssignProviderMode;
    if ProviderMode then
    begin
      dspMaster.DataSet := SourceDataSet;
      dscMaster.DataSet := ClientDataSet;
    end
    else
    begin
      dspMaster.DataSet := nil;
      dscMaster.DataSet := SourceDataSet;
    end;
  finally
    EndUpdate;
  end;
  UpdateDataViews;
end;

function TdmCustomModule.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

procedure TdmCustomModule.SetRecordCount(const Value: Integer);
begin
  FRecordCount := Value;
end;

function TdmCustomModule.GetSourceDataSet: TDataSet;
begin
  Result := FNativeDataSet.DataSet;
end;

procedure TdmCustomModule.SetSQL(const Value: string);
begin
  if Value <> SQL then
  begin
    FSQL := Value;
    UpdateTableName(Value);
  end;
end;

function TdmCustomModule.GetSQL: string;
begin
  Result := FSQL;
end;

function TdmCustomModule.GetTableName: string;
begin
  Result := FTableName;
end;

procedure TdmCustomModule.SetTableName(const Value: string);
begin
  FTableName := Value;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TdmCustomModule.DoAfterUpdateData(AUpdateKind: TUpdateKind);
begin
  if Assigned(FOnAfterUpdateData) then
    FOnAfterUpdateData(Self, AUpdateKind);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmCustomModule.SourceDataSetAfterOpen(ADataSet: TDataSet);
begin
  InitFields(ADataSet);
end;

procedure TdmCustomModule.cdsMasterBeforeOpen(ADataSet: TDataSet);
begin
  SourceDataSet.Active := False;
  cdsMaster.IndexName := '';
end;

procedure TdmCustomModule.cdsMasterAfterDelete(ADataSet: TDataSet);
begin
  PostUpdates;
  ExecuteDetails;
end;

procedure TdmCustomModule.cdsMasterAfterOpen(ADataSet: TDataSet);
begin
  InitFields(ADataSet);
  ExecuteDetails;
end;

procedure TdmCustomModule.cdsMasterAfterPost(ADataSet: TDataSet);
begin
  inherited;
  PostUpdates;
end;

procedure TdmCustomModule.cdsMasterAfterScroll(ADataSet: TDataSet);
begin
  if ADataSet.RecNo > RecordCount then
    RecordCount := ADataSet.RecNo;
  ExecuteDetails;
end;

procedure TdmCustomModule.dspMasterAfterUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
begin
  if UpdateKind <> ukDelete then
    RefreshRecord(DeltaDS);
  DoAfterUpdateData(UpdateKind);
end;

procedure TdmCustomModule.dspMasterBeforeUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
  var Applied: Boolean);
begin
//  can be used to apply updates to multiple tables
end;

procedure TdmCustomModule.dspMasterGetTableName(Sender: TObject;
  DataSet: TDataSet; var TableName: WideString);
begin
  TableName := Self.TableName;
end;

procedure TdmCustomModule.dspMasterUpdateData(Sender: TObject;
  DataSet: TCustomClientDataSet);
begin
  InitProviderFields(DataSet);
end;

procedure TdmCustomModule.dspMasterUpdateError(Sender: TObject;
  DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  // needs to be raised, or we won't see the error at runtime!
  raise E;
end;

//---|dscMaster|---------------------------------------------------------------

procedure TdmCustomModule.dscMasterDataChange(Sender: TObject; Field: TField);
begin
  if Assigned(Field) then
  begin
  // Needed to prevent recursive calls when the fieldvalue of Field is changed
  // inside DataSetFieldChanged.
    Field.DataSet.DisableControls;
    try
      DataSetFieldChanged(Field);
    finally
      Field.DataSet.EnableControls;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Assigns the given set of keyvalues to the corresponding properties (eg.
  SQLParams). These keyvalues serve as a container which holds data that is
  shared between the datamodule and the calling module. }

procedure TdmCustomModule.AssignKeyValues(AKeyValues: TtsKeyValues);
var
  I : Integer;
begin
  for I := 0 to AKeyValues.Count - 1 do
    Params[AKeyValues.Items[I].Name] := AKeyValues.Items[I].Value;
end;

procedure TdmCustomModule.AfterExecute;
begin
//  EnumerateDataViews(DataViewAfterExecute);
end;

{ Assignes all dataset components to the given connection. }

procedure TdmCustomModule.AssignConnection;
begin
//
end;

{ Intended to be overridden for custom connection types. This allows to apply
  changes to the cursortype and other connection specific parameters before
  the TDataSetProvider - TClientDataSet combination is connected to the native
  TDataSet descendant.

  A Lightweight forward cursor is recommended on the native dataset when a
  TDataSetProvider is used to prevent that data is buffered twice on the client.
}

procedure TdmCustomModule.AssignProviderMode;
begin
  // both SourceDataSet and ClientDataSet are closed here.
  FNativeDataSet.UpdateProviderMode(FProviderMode);
end;

procedure TdmCustomModule.AssignRecordCount;
begin
//  if (GroupByClause = '') and (FromClause <> '') then
//    RecordCount :=
//      GetQueryRecordCount((Connection as IADOConnection).ADOConnection,
//                          FromClause,
//                          Conditions.ConditionString)
//  else
  begin
    if DataSet.Active then
      RecordCount := DataSet.RecordCount;
  end;
end;

procedure TdmCustomModule.BeforeExecute;
begin
  EnumerateDataViews(DataViewBeforeExecute);
end;

procedure TdmCustomModule.BeginUpdate;
begin
  FUpdating := True;
  DataSet.DisableControls;
end;

{ Builds the FieldNames stringlist from a given array of strings. }

procedure TdmCustomModule.BuildFieldList(const AFieldNames: array of string);
var
  I : Integer;
begin
  FFieldNames.Clear;
  for I := Low(AFieldNames) to High(AFieldNames) do
    FFieldNames.Add(AFieldNames[I]);
end;

{ Used to handle changes of field values in the dataset.

  REMARKS:
    In this method the DataSet is in EditMode. Never do a Post inside this
    method!
}

procedure TdmCustomModule.DataSetFieldChanged(AField: TField);
begin
//
end;

procedure TdmCustomModule.DataViewAfterExecute(AIndex: Integer);
begin
  //(FDataViews[AIndex] as IDataView).AfterExecute;
end;

procedure TdmCustomModule.DataViewBeforeExecute(AIndex: Integer);
begin
//  (FDataViews[AIndex] as IDataView).BeforeExecute;
end;

{ Used in descendants to define the SQL conditions that will be applied to the
  list query. This method gets called before execution of the list query. }

procedure TdmCustomModule.DefineSQLConditions;
begin
  FConditions.Clear;
end;

procedure TdmCustomModule.EndUpdate;
begin
  DataSet.EnableControls;
  FUpdating := False;
end;

procedure TdmCustomModule.EnumerateDataViews(const AMethod: TDataViewMethod);
var
  I : Integer;
begin
  for I := 0 to FDataViews.Count - 1 do
  begin
    AMethod(I);
  end;
end;

{ Returns the ID of the last inserted record in the current table (using the
  TableName property).

  TODO: make this configurable

  }

function TdmCustomModule.GetRowID: Variant;
//const
//  QUERY = 'SELECT TOP 1 LAST_IDENTITY() FROM %s';
begin
  //Result := QueryLookup((Connection as IADOConnection).ADOConnection, QUERY, [TableName]);
end;

{ Generates the SQL statement corresponding to the current set of:
     - field names (FieldNames property) => defines the SELECT clause.
     - SQL conditions (Conditions property) => defines the WHERE clause.
     - statement block properties (FromClause, GroupByClause, OrderByClause)
     - parameter values (Params property)
}

function TdmCustomModule.GenerateSQL: string;
var
  S          : string;
  SQLSelect  : string;
  SQLFrom    : string;
  SQLWhere   : string;
  SQLGroupBy : string;
  SQLHaving  : string;
  SQLOrderBy : string;
begin
  if FSQL <> '' then
  begin
    Result := FSQL;
  end
  else
  begin
    DefineSQLConditions;

    S := StringsToStr(FFieldNames, ', ');
    if MaxRecords <= 0 then
      SQLSelect := Format(SQL_SELECT, [S])
    else
      SQLSelect := Format(SQL_SELECT_TOP, [MaxRecords, S]);

    SQLFrom := CR + Format(SQL_FROM, [FromClause]);

    S := Conditions.ConditionString;
    if S <> '' then
      SQLWhere := CR + Format(SQL_WHERE, [S]);

    S := GroupByClause;
    if S <> '' then
      SQLGroupBy := CR + Format(SQL_GROUP_BY, [S]);

    S := HavingClause;
    if (S <> '') and (GroupByClause <> '') then
      SQLHaving := CR + Format(SQL_HAVING, [S]);

    S := OrderByClause;
    if S <> '' then
      SQLOrderBy := CR + Format(SQL_ORDER_BY, [S]);

    Result := SQLSelect + SQLFrom + SQLWhere + SQLGroupBy + SQLHaving + SQLOrderBy;
  end;
end;

{ Executes the given SQL-statement. }

procedure TdmCustomModule.InternalExecute(const ACommandText : string);
var
  N : Integer;
begin
  if Trim(ACommandText) <> '' then
  begin
    SourceDataSet.Active := False;
    ClientDataSet.Active := False;
    FNativeDataSet.BeforeExecute; // TS
    if ProviderMode then
    begin
      ClientDataSet.CommandText := ACommandText;
      if FetchOnDemand then
      begin
        ClientDataSet.PacketRecords := PacketRecords;
      end
      else
      begin
        ClientDataSet.PacketRecords := -1; // fetch all records
      end;
      ClientDataSet.Active := True;
//      if ClientDataSet.PacketRecords > 0 then
//        N := ClientDataSet.PacketRecords
//      else
//        N := 100;
    end
    else
    begin
      (SourceDataSet as IProviderSupportNG).PSSetCommandText(ACommandText);
      SourceDataSet.Active := True;
    end;
    FNativeDataSet.AfterExecute; // TS
    //AutoSizeDisplayWidths(DataSet, N - 1); // N causes more records to be fetched
  end;
end;

{ Intended to be overridden (hook method). The typical use of this method is to
  pass parameters to dependent modules and execute them when the current record
  changes.  }

procedure TdmCustomModule.InternalExecuteDetails;
begin
//
end;

{ Returns True if the given field should be represented by a checkbox. }

function TdmCustomModule.IsCheckBoxField(const AFieldName: string): Boolean;
var
  F : TField;
begin
  F := DataSet.FindField(AFieldName);
  Result := Assigned(F) and (F is TBooleanField);
end;

{ Returns True if the current field represents a lookup value. }

function TdmCustomModule.IsLookupField(const AFieldName: string): Boolean;
begin
  Result := False;
end;

{ Returns True if the field typically represents multiple lines of text. }

function TdmCustomModule.IsMemoField(const AFieldName: string): Boolean;
var
  F : TField;
begin
  F := DataSet.FindField(AFieldName);
  Result := Assigned(F) and (IsBlobFieldType(F.DataType)
    or (IsStringFieldType(F.DataType) and (F.Size > 50)));
end;

{ Returns True if a value is required for the current field. }

function TdmCustomModule.IsRequiredField(const AFieldName: string): Boolean;
begin
  Result := False;
end;

{ This is the place where adjustments to the field definitions can be defined.

  REMARK:
    Fields are always created at runtime, so the list dataset will never have
    any persistent fields. }

procedure TdmCustomModule.InitField(AField: TField);
begin
  if AField is TFloatField then
  begin
    TFloatField(AField).DisplayFormat := '#,##0.00';
  end
  else if AField is TDateTimeField then
  begin
    AField.Alignment := taCenter;
  end;

  AField.DisplayLabel := DisplayLabels[AField.FieldName];
end;

{ Calls InitField for every field in the given dataset. }

procedure TdmCustomModule.InitFields(ADataSet: TDataSet);
var
  Field : TField;
begin
  for Field in ADataSet.Fields do
    InitField(Field);
end;

{ Used in overridden classes to do custom initializations. }

{ Typical overridden versions of Initialize look like this:

    begin
      // Assign the string array of all selected field names.
      BuildFieldList(SELECTED_FIELDS);
      // Assign resourcestrings used for the field captions.
      DisplayLabels['Fieldname1'] := SFieldName1;
      DisplayLabels['Fieldname2'] := SFieldName2;
      DisplayLabels['Fieldname3'] := SFieldName3;
      ...
    end;
 }

procedure TdmCustomModule.Initialize;
begin
  AssignConnection;
  FExecuted  := False;
  FTableName := FromClause;
  SourceDataSet.AfterOpen := SourceDataSetAfterOpen;
end;

{ Intended to be overridden in descendants to specify the provider properties
  for each field. }

procedure TdmCustomModule.InitProviderField(AField: TField);
begin
  if MatchText(AField.FieldName, [FKeyName]) then
  begin
    AField.ProviderFlags := [pfInKey];
  end
end;

procedure TdmCustomModule.InitProviderFields(ADataSet: TDataSet);
var
  Field : TField;
begin
  for Field in ADataSet.Fields do
    InitProviderField(Field);
end;

{ Post all pending updates in AClientDataSet to the database. }

procedure TdmCustomModule.PostUpdates(AClientDataSet : TCustomClientDataSet);
begin
  BeginUpdate;
  try
    if AClientDataSet.ApplyUpdates(0) <> 0 then
    begin
      AClientDataSet.CancelUpdates;
      FRefreshed :=  True;
    end;
    // if the changed record(s) cannot be refreshed for some reason, the
    // whole table is refreshed
    if not FRefreshed then
      AClientDataSet.Refresh;
  finally
    EndUpdate;
  end;
end;

{ Refreshes the current record in the client dataset (cdsMaster) after posting
  changes in the current record to the database.

  When one or more fields are updated in the current record, this method is
  called to get all field data for the current record.
  By doing this we make sure that in cdsMaster:
    - we have the ID (or primary key value) in case of an INSERT.
    - updated values for calculated fields (on the DBMS) are fetched.

  ADataSet : DeltaDS dataset passed by the TDataSetProvider's OnAfterUpdateData
             event handler.
}

procedure TdmCustomModule.RefreshRecord(ADataSet: TCustomClientDataSet);
var
  NDS       : INativeDataSet;
  DS        : TDataSet;
  I         : Integer;
  S         : string;
  SQLSelect : string;
  SQLFrom   : string;
  SQLWhere  : string;
  F         : TField;
  ID        : Integer;
begin
  NDS := Connection.CreateNativeDataSet;
  DS := NDS.DataSet;
  try
    Refreshed := False;
    if FFieldNames.Count > 0 then
    begin
      SQLSelect := Format(SQL_SELECT, [StringsToStr(FFieldNames, ', ')]);
      SQLFrom   := CR + Format(SQL_FROM, [FromClause]);
    end;

    SQLSelect := Format('select * from (%s) t ', [GenerateSQL]);

    if not DataSet.FieldByName(FKeyName).IsNull then
      ID := DataSet[FKeyName]
    else
      ID := GetRowID;

    if (ID <> 0)  then
    begin
      SQLWhere  := CR + Format(SQL_WHERE,
        [Format('%s = %d', [FKeyName,  ID])]);
      (DS as IProviderSupportNG).PSSetCommandText(SQLSelect + SQLFrom + SQLWhere);
      try
        DS.Active := True;
        for I := 0 to DS.FieldCount - 1 do
        begin
          S := DS.Fields[I].FieldName;
          F := ADataSet.FieldByName(S);
          if DS.Fields[I].CanModify and not SameText(DS.Fields[I].FieldName, FKeyName) then
            F.NewValue := DS.Fields[I].Value
          else
          begin
          // this is done for calculated fields, which would otherwise not get updated
            if not (pfInKey in F.ProviderFlags) and
              (F.Value <> DS.Fields[I].Value) then
            begin
              F.ReadOnly := False;
              F.NewValue := DS.Fields[I].Value;
              F.ReadOnly := True;
            end;
          end;
        end;
        DS.Active := False;
        Refreshed := True;
      except
        Refreshed := False;
      end;
    end;
  finally
    DS.Free;
  end;
end;

procedure TdmCustomModule.RegisterDataView(ADataView: IDataView);
begin
  if FDataViews.IndexOf(ADataView) < 0 then
    FDataViews.Add(ADataView);
end;

procedure TdmCustomModule.UnregisterDataView(ADataView: IDataView);
var
  N : Integer;
begin
  N := FDataViews.IndexOf(ADataView);
  if N >= 0  then
    FDataViews.Delete(N);
end;

procedure TdmCustomModule.UpdateDataView(AIndex: Integer);
begin
  (FDataViews[AIndex] as IDataView).UpdateView;
end;

procedure TdmCustomModule.UpdateDataViews;
begin
  EnumerateDataViews(UpdateDataView);
end;

procedure TdmCustomModule.UpdateTableName(const ASQL: string);
begin
  if not IsMultiTableQuery(ASQL) then
    FTableName := GetTableNameFromQuery(ASQL)
  else
    FTableName := '';
end;

{ Updates the DisplayValues list, which holds the textual representation for
  the corresponding key value. }

procedure TdmCustomModule.UpdateDisplayValue(const AName: string);
begin
  // intended to be overridden in descendants
end;

procedure TdmCustomModule.UpdateDisplayValues;
var
  I : Integer;
begin
  for I := 0 to DisplayValues.Count - 1 do
  begin
    UpdateDisplayValue(DisplayValues.Items[I].Name);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Executes the table query and resets the Executed property. }

procedure TdmCustomModule.Execute;
var
  S  : string;
begin
  BeginUpdate;
  try
    BeforeExecute;
    try
      DataSet.Active := False;
      S := GenerateSQL;
      InternalExecute(S);
      UpdateDisplayValues;
      AssignRecordCount;
      FExecuted := True;
    finally
      AfterExecute;
    end;
  finally
    EndUpdate;
  end;
  UpdateDataViews;
end;

{ Executes dependent (IData) modules when the current record is changed. }

procedure TdmCustomModule.ExecuteDetails;
begin
  InternalExecuteDetails;
end;

{ Test for required fields here! }

function TdmCustomModule.Post: Boolean;
var
  B : Boolean;
  I : Integer;
begin
  if DataSet.State in dsEditModes then
  begin
  // check for required fields
    B := True;
    I := 0;
    while B and (I < DataSet.Fields.Count) do
    begin
      B := B and not (DataSet.Fields[I].Required and DataSet.Fields[I].IsNull);
      Inc(I);
    end;
    if B then
      try
        DataSet.Post;
      except
        Abort;
        raise Exception.Create('Error while trying to post!');
      end;
    Result := B;
  end
  else
    Result := False;
end;

{ Post all pending updates in ClientDataSet to the database. }

procedure TdmCustomModule.PostUpdates;
begin
  PostUpdates(ClientDataSet);
  UpdateDisplayValues;
end;
{$ENDREGION}

end.

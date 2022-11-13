{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DataGrabber.inc}

unit DataGrabber.Data;

{ Represents all fetched data for a connectionview. }

{
  We decided not to share connections between IData instances, so if many
  connectionviews are created of a given connectionprofile, they both will
  have their own private connection.

  By default FireDAC searches for the driver configuration file (FDDrivers.ini)
  in the application EXE folder.
  If this file is not found, it looks for the file specified in the registry key
  HKCU\Software\Embarcadero\FireDAC\DriverFile.

  For ODBC based drivers, if the ODBCDriver is specified, FireDAC uses the
  specified driver. If it is not specified, a default driver name is used.
}

{
  TODO
    - support for local SQL on TFDMemTable objects?
}

interface

uses
  System.SysUtils, System.Classes,

  Data.DB,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param,
  FireDAC.UI.Intf,
  FireDAC.Phys, FireDAC.Phys.Intf,
  FireDAC.Phys.Oracle, FireDAC.Phys.MSSQL, FireDAC.Phys.ADS, FireDAC.Phys.ASA,
  FireDAC.Phys.DB2, FireDAC.Phys.DS, FireDAC.Phys.FB, FireDAC.Phys.IB,
  FireDAC.Phys.Infx, FireDAC.Phys.MongoDB, FireDAC.Phys.MSAcc,
  FireDAC.Phys.MySQL, FireDAC.Phys.ODBC, FireDAC.Phys.PG, FireDAC.Phys.SQLite,
  FireDAC.Phys.TData, FireDAC.Phys.DSDef, FireDAC.Phys.TDBXDef,
  FireDAC.Phys.TDBX, FireDAC.Phys.TDBXBase,
  FireDAC.VCLUI.Async, FireDAC.VCLUI.Wait,
  FireDAC.DApt, FireDAC.DApt.Intf,
  FireDAC.DatS,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  Spring, Spring.Collections,

  DataGrabber.ConnectionSettings, DataGrabber.Interfaces,
  DataGrabber.Data.ResultSet;

type
  TdmData = class(TDataModule, IData, IResultSet)
    conMain : TFDConnection;
    qryMain : TFDQuery;

    {$REGION 'event handlers'}
    procedure qryMainAfterGetRecords(DataSet: TFDDataSet);
    procedure qryMainAfterOpen(DataSet: TDataSet);
    procedure qryMainAfterRefresh(DataSet: TDataSet);
    procedure qryMainAfterRowRequest(DataSet: TFDDataSet);
    procedure qryMainBeforeApplyUpdates(DataSet: TFDDataSet);
    procedure qryMainBeforeRowRequest(DataSet: TFDDataSet);
    procedure qryMainCommandChanged(Sender: TObject);
    procedure qryMainError(ASender, AInitiator: TObject;
      var AException: Exception);
    procedure qryMainExecuteError(ASender: TObject; ATimes, AOffset: Integer;
      AError: EFDDBEngineException; var AAction: TFDErrorAction);
    procedure qryMainMasterSetValues(DataSet: TFDDataSet);
    procedure qryMainAfterApplyUpdates(DataSet: TFDDataSet; AErrors: Integer);
    procedure qryMainAfterExecute(DataSet: TFDDataSet);
    procedure qryMainBeforeClose(DataSet: TDataSet);
    procedure qryMainBeforeExecute(DataSet: TFDDataSet);
    procedure qryMainBeforeGetRecords(DataSet: TFDDataSet);
    procedure qryMainBeforeRefresh(DataSet: TDataSet);
    procedure qryMainBeforeOpen(DataSet: TDataSet);
    procedure qryMainAfterClose(DataSet: TDataSet);
    procedure conMainAfterConnect(Sender: TObject);
    procedure conMainAfterDisconnect(Sender: TObject);
    procedure conMainBeforeConnect(Sender: TObject);
    procedure conMainBeforeDisconnect(Sender: TObject);
    procedure conMainBeforeStartTransaction(Sender: TObject);
    procedure conMainAfterStartTransaction(Sender: TObject);
    procedure conMainLost(Sender: TObject);
    procedure conMainRecover(ASender, AInitiator: TObject;
      AException: Exception; var AAction: TFDPhysConnectionRecoverAction);
    procedure conMainRestored(Sender: TObject);
    procedure conMainError(ASender, AInitiator: TObject;
      var AException: Exception);
    {$ENDREGION}

  private
    FConstantFields         : IList<TField>;
    FEmptyFields            : IList<TField>;
    FNonEmptyFields         : IList<TField>;
    FHiddenFields           : IList<TField>;
    FFavoriteFields         : IList<TField>;
    FConstantFieldsVisible  : Boolean;
    FEmptyFieldsVisible     : Boolean;
    FShowFavoriteFieldsOnly : Boolean;
    FSQL                    : string;
    FConnectionSettings     : TConnectionSettings;
    FDriverNames            : TStrings;
    FOnAfterExecute         : Event<TNotifyEvent>;
    FOnBeforeExecute        : Event<TNotifyEvent>;
    FMultipleResultSets     : Boolean;
    FStopWatch              : TStopwatch;
    FFieldListsUpdated      : Boolean;
    FDataEditMode           : Boolean;
    FResultSets             : TResultSets;

    {$REGION 'property access methods'}
    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
    function GetHiddenFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFieldsVisible: Boolean;
    function GetShowFavoriteFieldsOnly: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    procedure SetShowFavoriteFieldsOnly(const Value: Boolean);
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetDriverNames: TStrings;
    function GetConnectionSettings: TConnectionSettings;
    function GetDataSet : TFDDataSet;
    function GetRecordCount : Integer;
    function GetActive: Boolean;
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetOnAfterExecute: IEvent<TNotifyEvent>;
    function GetConnection: TFDConnection;
    function GetItem(AIndex: Integer): IResultSet;
    function GetDataSetCount: Integer;
    function GetOnBeforeExecute: IEvent<TNotifyEvent>;
    function GetMultipleResultSets: Boolean;
    procedure SetMultipleResultSets(const Value: Boolean);
    function GetElapsedTime: TTimeSpan;
    function GetFieldListsUpdated: Boolean;
    function GetDataEditMode: Boolean;
    procedure SetDataEditMode(const Value: Boolean);
    function GetResultSet: IResultSet;
    function GetData: IData;
    {$ENDREGION}

    procedure FConnectionSettingsChanged(Sender: TObject);

  protected
    procedure Execute;
    procedure InternalExecute(const ACommandText : string); virtual;

    procedure InitializeConnection;
    procedure InitFields(ADataSet: TDataSet);
    procedure InitField(AField: TField);

    procedure HideField(
      ADataSet         : TDataSet;
      const AFieldName : string
    );
    procedure UpdateFieldLists;

    procedure SaveToFile(
      ADataSet        : TDataSet;
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;
    procedure LoadFromFile(
      ADataSet        : TDataSet;
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;
    procedure SaveToFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;
    procedure LoadFromFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;

    procedure Sort(
      const AFieldName : string;
      ADescending      : Boolean = False
    ); overload;

    procedure Sort(
      ADataSet         : TDataSet;
      const AFieldName : string;
      ADescending      : Boolean = False
    ); overload;

    property DataEditMode: Boolean
      read GetDataEditMode write SetDataEditMode;

    property Connection: TFDConnection
      read GetConnection;

    property ElapsedTime: TTimeSpan
      read GetElapsedTime;

    { Indicated if there is an active connection to the database. }
    property Connected: Boolean
      read GetConnected write SetConnected;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    { List of all supported drivernames (by FireDAC). }
    property DriverNames: TStrings
      read GetDriverNames;

    property SQL: string
      read GetSQL write SetSQL;

    property DataSet: TFDDataSet
      read GetDataSet;

    property Items[AIndex: Integer]: IResultSet
      read GetItem; default;

    property Active: Boolean
      read GetActive;

    property CanModify: Boolean
      read GetCanModify;

    property RecordCount: Integer
      read GetRecordCount;

    { Returns the listcount of the internal list of fetched datasets
      (FDataSets). }
    property DataSetCount: Integer
      read GetDataSetCount;

    property FieldListsUpdated: Boolean
      read GetFieldListsUpdated;

    { Multiple resultsets will be fetched and stored in a local list of
      TFDMemtable instances (FDataSets). }
    property MultipleResultSets: Boolean
      read GetMultipleResultSets write SetMultipleResultSets;

    { Called after the SQL statement or script is executed. }
    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;

    { Called before the SQL statement or script is executed. }
    property OnBeforeExecute: IEvent<TNotifyEvent>
      read GetOnBeforeExecute;
    {$ENDREGION}

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TConnectionSettings
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    {$REGION 'IResultSet'}
    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;

    property HiddenFields: IList<TField>
      read GetHiddenFields;

    function ShowAllFields: Boolean;

    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;
    {$ENDREGION}
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,

  DDuce.Logger;

{$REGION 'construction and destruction'}
constructor TdmData.Create(AOwner: TComponent;
  ASettings: TConnectionSettings);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  FConnectionSettings := TConnectionSettings.Create;
  FConnectionSettings.Assign(ASettings);
  FConnectionSettings.OnChanged.Add(FConnectionSettingsChanged);
end;

procedure TdmData.AfterConstruction;
begin
  Logger.Track(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FConstantFields := TCollections.CreateObjectList<TField>(False);
  FEmptyFields    := TCollections.CreateObjectList<TField>(False);
  FNonEmptyFields := TCollections.CreateObjectList<TField>(False);
  FFavoriteFields := TCollections.CreateObjectList<TField>(False);
  FHiddenFields   := TCollections.CreateObjectList<TField>(False);
  FConstantFieldsVisible := True;
  FEmptyFieldsVisible    := True;
  FDriverNames := TStringList.Create;
  FResultSets  := TResultSets.Create;

  FDManager.ResourceOptions.AutoReconnect := True;
  // Blocking with cancel dialog.
  FDManager.ResourceOptions.CmdExecMode := amCancelDialog;
  // Enable dialogs.
  FDManager.ResourceOptions.SilentMode := False;
  FDManager.GetDriverNames(DriverNames);
  InitializeConnection;
  FStopWatch := TStopwatch.Create;
end;

procedure TdmData.BeforeDestruction;
begin
  Logger.Track(Self, 'BeforeDestruction');
  FConnectionSettings.Free;
  FDriverNames.Free;
  FOnBeforeExecute.RemoveAll(Self);
  FOnAfterExecute.RemoveAll(Self);
  FResultSets.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
{$REGION 'IFieldLists'}
function TdmData.GetConstantFields: IList<TField>;
begin
  Result := FConstantFields;
end;

function TdmData.GetEmptyFields: IList<TField>;
begin
  Result := FEmptyFields;
end;

function TdmData.GetNonEmptyFields: IList<TField>;
begin
  Result := FNonEmptyFields;
end;
{$ENDREGION}

{$REGION 'IFieldVisibility'}
function TdmData.GetConstantFieldsVisible: Boolean;
begin
  Result := FConstantFieldsVisible;
end;

procedure TdmData.SetConstantFieldsVisible(const Value: Boolean);
begin
  if Value <> ConstantFieldsVisible then
  begin
    FConstantFieldsVisible := Value;
    UpdateFieldLists;
    InitFields(DataSet);
  end;
end;

function TdmData.GetEmptyFieldsVisible: Boolean;
begin
  Result := FEmptyFieldsVisible;
end;

procedure TdmData.SetEmptyFieldsVisible(const Value: Boolean);
begin
  if Value <> EmptyFieldsVisible then
  begin
    FEmptyFieldsVisible := Value;
    UpdateFieldLists;
    InitFields(DataSet);
  end;
end;

function TdmData.GetShowFavoriteFieldsOnly: Boolean;
begin
  Result := FShowFavoriteFieldsOnly;
end;

procedure TdmData.SetShowFavoriteFieldsOnly(const Value: Boolean);
begin
  if Value <> ShowFavoriteFieldsOnly then
  begin
    FShowFavoriteFieldsOnly := Value;
    InitFields(DataSet);
  end;
end;

function TdmData.GetFieldListsUpdated: Boolean;
begin
  Result := FFieldListsUpdated;
end;

function TdmData.GetHiddenFields: IList<TField>;
begin
  Result := FHiddenFields;
end;
{$ENDREGION}

function TdmData.GetElapsedTime: TTimeSpan;
begin
  Result := FStopWatch.Elapsed;
end;

function TdmData.GetItem(AIndex: Integer): IResultSet;
begin
  if not FResultSets.IsEmpty and (AIndex < FResultSets.Count) then
    Result := FResultSets[AIndex]
  else
    Result := nil;
end;

function TdmData.GetMultipleResultSets: Boolean;
begin
  Result := FMultipleResultSets;
end;

procedure TdmData.SetMultipleResultSets(const Value: Boolean);
begin
  if Value <> MultipleResultSets then
  begin
    FMultipleResultSets := Value;
  end;
end;

function TdmData.GetDriverNames: TStrings;
begin
  Result := FDriverNames;
end;

function TdmData.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TdmData.GetResultSet: IResultSet;
begin
  Result := Self as IResultSet;
end;

function TdmData.GetData: IData;
begin
  Result := Self;
end;

function TdmData.GetDataEditMode: Boolean;
begin
  Result := FDataEditMode;
end;

procedure TdmData.SetDataEditMode(const Value: Boolean);
begin
  if Value <> DataEditMode then
  begin
    FDataEditMode := Value;
    InitializeConnection;
  end;
end;

function TdmData.GetDataSet: TFDDataSet;
begin
  if FResultSets.Any then
    Result := FResultSets.First.DataSet
  else
    Result := qryMain;
end;

function TdmData.GetDataSetCount: Integer;
begin
  Result := FResultSets.Count;
end;

function TdmData.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TdmData.GetCanModify: Boolean;
begin
  Result := qryMain.CanModify;
end;

function TdmData.GetConnected: Boolean;
begin
  Result := Connection.Connected;
end;

procedure TdmData.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    Connection.Connected := Value;
  end;
end;

function TdmData.GetConnection: TFDConnection;
begin
  Result := conMain;
end;

function TdmData.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

function TdmData.GetOnBeforeExecute: IEvent<TNotifyEvent>;
begin
  Result := FOnBeforeExecute;
end;

function TdmData.GetOnAfterExecute: IEvent<TNotifyEvent>;
begin
  Result := FOnAfterExecute;
end;

function TdmData.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TdmData.SetSQL(const Value: string);
begin
  if Value <> SQL then
  begin
    FSQL := Value;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmData.FConnectionSettingsChanged(Sender: TObject);
begin
  if not DataEditMode then
  begin
    InitializeConnection;
  end;
end;

{$REGION 'conMain'}
procedure TdmData.conMainAfterConnect(Sender: TObject);
begin
  Logger.Track(Self, 'conMainAfterConnect');
  FResultSets.Clear;
end;

procedure TdmData.conMainAfterDisconnect(Sender: TObject);
begin
  Logger.Track(Self, 'conMainAfterDisconnect');
end;

procedure TdmData.conMainAfterStartTransaction(Sender: TObject);
begin
  Logger.Track(Self, 'conMainAfterStartTransaction');
end;

procedure TdmData.conMainBeforeConnect(Sender: TObject);
begin
  Logger.Track(Self, 'conMainBeforeConnect');
end;

procedure TdmData.conMainBeforeDisconnect(Sender: TObject);
begin
  Logger.Track(Self, 'conMainBeforeDisconnect');
end;

procedure TdmData.conMainBeforeStartTransaction(Sender: TObject);
begin
  Logger.Track(Self, 'conMainBeforeStartTransaction');
end;

procedure TdmData.conMainError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  Logger.Track(Self, 'conMainError');
end;

procedure TdmData.conMainLost(Sender: TObject);
begin
  Logger.Track(Self, 'conMainLost');
end;

procedure TdmData.conMainRecover(ASender, AInitiator: TObject;
  AException: Exception; var AAction: TFDPhysConnectionRecoverAction);
begin
  Logger.Track(Self, 'conMainRecover');
end;

procedure TdmData.conMainRestored(Sender: TObject);
begin
  Logger.Track(Self, 'conMainRestored');
end;
{$ENDREGION}

{$REGION 'qryMain'}
procedure TdmData.qryMainAfterApplyUpdates(DataSet: TFDDataSet;
  AErrors: Integer);
begin
  Logger.Track(Self, 'qryMainAfterApplyUpdates');
end;

procedure TdmData.qryMainAfterClose(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainAfterClose');
end;

procedure TdmData.qryMainAfterExecute(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainAfterExecute');
end;

procedure TdmData.qryMainAfterGetRecords(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainAfterGetRecords');
end;

procedure TdmData.qryMainAfterOpen(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainAfterOpen');
end;

procedure TdmData.qryMainAfterRefresh(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainAfterRefresh');
end;

procedure TdmData.qryMainAfterRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainAfterRowRequest');
end;

procedure TdmData.qryMainBeforeApplyUpdates(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeApplyUpdates');
end;

procedure TdmData.qryMainBeforeClose(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeClose');
end;

procedure TdmData.qryMainBeforeExecute(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeExecute');
end;

procedure TdmData.qryMainBeforeGetRecords(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeGetRecords');
end;

procedure TdmData.qryMainBeforeOpen(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeOpen');
  FConstantFieldsVisible := True;
  FEmptyFieldsVisible    := True;
end;

procedure TdmData.qryMainBeforeRefresh(DataSet: TDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeRefresh');
end;

procedure TdmData.qryMainBeforeRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainBeforeRowRequest');
end;

procedure TdmData.qryMainCommandChanged(Sender: TObject);
begin
  Logger.Track(Self, 'qryMainCommandChanged');
end;

procedure TdmData.qryMainError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  Logger.Track(Self, 'qryMainError');
end;

procedure TdmData.qryMainExecuteError(ASender: TObject; ATimes,
  AOffset: Integer; AError: EFDDBEngineException; var AAction: TFDErrorAction);
begin
  Logger.Track(Self, 'qryMainExecuteError');
end;

procedure TdmData.qryMainMasterSetValues(DataSet: TFDDataSet);
begin
  Logger.Track(Self, 'qryMainMasterSetValues');
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmData.HideField(ADataSet: TDataSet; const AFieldName: string);
var
  F : TField;
begin
  Guard.CheckNotNull(ADataSet, 'ADataSet');
  F := ADataSet.FieldByName(AFieldName);
  Guard.CheckNotNull(F, 'Field not found!');
  F.Visible := False;
  if not FHiddenFields.Contains(F) then
    FHiddenFields.Add(F);
end;

procedure TdmData.InitField(AField: TField);
var
  B : Boolean;
begin
  B := True;
  if ShowFavoriteFieldsOnly then
    B := FFavoriteFields.Contains(AField);
  if B and not ConstantFieldsVisible then
    B := not FConstantFields.Contains(AField);
  if B and not EmptyFieldsVisible then
    B := not FEmptyFields.Contains(AField);
  if not B and not FHiddenFields.Contains(AField) then
    FHiddenFields.Add(AField);
  B := not FHiddenFields.Contains(AField);
  AField.Visible := B;
end;

procedure TdmData.InitFields(ADataSet: TDataSet);
var
  LField : TField;
begin
  Logger.Track(Self, 'InitFields');
  for LField in ADataSet.Fields do
  begin
    InitField(LField);
  end;
end;

{ Assigns application level connection settings (TConnectionSettings object) to
  the FireDAC connection component. }

procedure TdmData.InitializeConnection;
begin
  Logger.Track('InitializeConnection');
  Logger.Send('DriverName', ConnectionSettings.DriverName);
  if DataEditMode then
  begin
    MultipleResultSets := False;
    Connection.FetchOptions.AutoClose := True;
    conMain.UpdateOptions.ReadOnly := False;
    Connection.FetchOptions.Mode := fmAll;
  end
  else
  begin
    MultipleResultSets := FConnectionSettings.MultipleResultSets;
    Connection.FetchOptions.RowsetSize := ConnectionSettings.PacketRecords;
    if ConnectionSettings.FetchOnDemand then
    begin
      Connection.FetchOptions.Mode := fmOnDemand
    end
    else
    begin
      Connection.FetchOptions.Mode := fmAll;
    end;
    if MultipleResultSets then
    begin
      // This setting is required to support multiple resultsets.
      Connection.FetchOptions.AutoClose := False;
    end
    else
    begin
      Connection.FetchOptions.AutoClose := True;
    end;
    conMain.UpdateOptions.ReadOnly := ConnectionSettings.ReadOnly;
  end;
  Connection.DriverName := ConnectionSettings.DriverName;
  if ConnectionSettings.DriverName <> '' then
  begin
    Connected := False;
    Connection.DriverName := ConnectionSettings.DriverName;
    Connection.Offlined   := ConnectionSettings.DisconnectedMode;
    with Connection.Params do
    begin
      Values['Server']    := ConnectionSettings.HostName;
      Values['Database']  := ConnectionSettings.Database;
      Values['User_Name'] := ConnectionSettings.UserName;
      Values['Password']  := ConnectionSettings.Password;
      Values['OSAuthent'] := IfThen(ConnectionSettings.OSAuthent, 'Yes', 'No');
    end;
    Connection.LoginPrompt := False;
    // Set this to prevent issues with FireDac's need for a cursor object﻿.
    Connection.ResourceOptions.SilentMode    := True;
    Connection.ResourceOptions.AutoReconnect := ConnectionSettings.AutoReconnect;
  end;
  FResultSets.Clear;
end;

procedure TdmData.InternalExecute(const ACommandText: string);
var
  B : Boolean;
begin
  Logger.Track(Self, 'InternalExecute');
  if Trim(ACommandText) <> '' then
  begin
    FConstantFields.Clear;
    FHiddenFields.Clear;
    FEmptyFields.Clear;
    FFavoriteFields.Clear;
    qryMain.Close;
    FStopWatch.Start;
    Logger.SendSQL('ACommandText', ACommandText);
    qryMain.Open(ACommandText);
    FStopWatch.Stop;
    if MultipleResultSets then
    begin
      FResultSets.Add(TResultSet.Create(Self, qryMain.Data));
      B := qryMain.Active;
      while B do
      begin
        qryMain.NextRecordSet;
        if qryMain.Active then
        begin
          FResultSets.Add(TResultSet.Create(Self, qryMain.Data));
        end
        else
        begin
          B := False;
        end;
      end;
    end;
  end;
end;

function TdmData.ShowAllFields: Boolean;
var
  F : TField;
begin
  Logger.Track(Self, 'ShowAllFields');
  Result := False;
  DataSet.DisableControls;
  try
    FConstantFieldsVisible := True;
    FEmptyFieldsVisible    := True;
    for F in DataSet.Fields do
    begin
      if not F.Visible then
      begin
        F.Visible := True;
        Result := True;
      end;
    end;
    FHiddenFields.Clear;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TdmData.Sort(const AFieldName: string; ADescending: Boolean);
begin
  Sort(DataSet, AFieldName, ADescending);
end;

{ Sorts the TFDDataSet (or any of its descendants) on a given fieldname by
  assigning an expression to the IndexFieldNames property. }

procedure TdmData.Sort(ADataSet: TDataSet; const AFieldName: string;
  ADescending: Boolean);
var
  C : Char;
begin
  if ADescending then
    C := 'D'
  else
    C := 'A';
  (ADataSet as TFDDataSet).IndexFieldNames := Format('%s:%s', [AFieldName, C]);
end;

procedure TdmData.UpdateFieldLists;
var
  S        : string;
  T        : string;
  F        : TField;
  LIsEmpty : Boolean;
  LIsConst : Boolean;
begin
  Logger.Track(Self, 'UpdateFieldLists');
  Logger.Watch('FFieldListsUpdated', FFieldListsUpdated);
  if not FFieldListsUpdated then
  begin
    DataSet.DisableControls;
    FConstantFields.Clear;
    FEmptyFields.Clear;
    FNonEmptyFields.Clear;
    try
      if DataSet.FindFirst then
      begin
        for F in DataSet.Fields do
        begin
          // constant fields
          DataSet.FindFirst;
          S := F.AsString;
          LIsConst := True;
          LIsEmpty := F.IsNull or F.AsString.IsEmpty;
          while (LIsConst or LIsEmpty) and DataSet.FindNext do
          begin
            T := F.AsString;
            LIsConst := LIsConst and (S = T);
            LIsEmpty := LIsEmpty and (F.IsNull or T.IsEmpty);
          end;
          if LIsConst then
            FConstantFields.Add(F);
          if LIsEmpty then
            FEmptyFields.Add(F)
          else
            FNonEmptyFields.Add(F);
        end;
      end;
      FFieldListsUpdated := True;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

procedure TdmData.Execute;
begin
  Logger.Track(Self, 'Execute');
  FStopWatch.Reset;
  InitializeConnection;
  FStopWatch.Start;
  if OnBeforeExecute.CanInvoke then
    OnBeforeExecute.Invoke(Self);
  InternalExecute(SQL);
  if OnAfterExecute.CanInvoke then
    OnAfterExecute.Invoke(Self);
  FStopWatch.Stop;
  FConstantFields.Clear;
  FEmptyFields.Clear;
  FNonEmptyFields.Clear;
  FHiddenFields.Clear;
  FFieldListsUpdated := False;
  UpdateFieldLists;
end;

procedure TdmData.LoadFromFile(ADataSet: TDataSet; const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  (ADataSet as TFDDataSet).LoadFromFile(AFileName, AFormat);
end;

procedure TdmData.LoadFromFile(const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  DataSet.LoadFromFile(AFileName, AFormat);
end;

procedure TdmData.SaveToFile(const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  DataSet.SaveToFile(AFileName, AFormat);
end;

procedure TdmData.SaveToFile(ADataSet: TDataSet; const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  (ADataSet as TFDDataSet).SaveToFile(AFileName, AFormat);
end;
{$ENDREGION}

end.


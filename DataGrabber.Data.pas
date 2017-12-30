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
    - Fieldlists are only built based on the primary (first) resultset if
      multiple resultsets are returned.
    - support for cached updates on (multiple) resultsets that are assigned to
      TFDMemTable objects.
    - support for local SQL on TFDMemTable objects.
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

  DataGrabber.ConnectionSettings, DataGrabber.Interfaces;

type
  TdmData = class(
    TDataModule,
    IData,
    IFieldLists,
    IFieldVisiblity
  )
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
    procedure conMainLost(Sender: TObject);
    procedure conMainRecover(ASender, AInitiator: TObject;
      AException: Exception; var AAction: TFDPhysConnectionRecoverAction);
    procedure conMainRestored(Sender: TObject);
    procedure conMainError(ASender, AInitiator: TObject;
      var AException: Exception);
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
    {$ENDREGION}

  private
    FConstantFields         : IList<TField>;
    FEmptyFields            : IList<TField>;
    FNonEmptyFields         : IList<TField>;
    FFavoriteFields         : IList<TField>;
    FConstantFieldsVisible  : Boolean;
    FEmptyFieldsVisible     : Boolean;
    FShowFavoriteFieldsOnly : Boolean;
    FExecuted               : Boolean;
    FSQL                    : string;
    FConnectionSettings     : TConnectionSettings;
    FDriverNames            : TStrings;
    FOnAfterExecute         : Event<TNotifyEvent>;
    FOnBeforeExecute        : Event<TNotifyEvent>;
    FDataSets               : IList<TFDMemTable>;
    FMultipleResultSets     : Boolean;

    {$REGION 'property access methods'}
    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
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
    function GetItem(AIndex: Integer): TFDMemTable;
    function GetDataSetCount: Integer;
    function GetOnBeforeExecute: IEvent<TNotifyEvent>;
    function GetMultipleResultSets: Boolean;
    procedure SetMultipleResultSets(const Value: Boolean);
    {$ENDREGION}

    procedure FConnectionSettingsChanged(Sender: TObject);

  protected
    procedure Execute;
    procedure InternalExecute(const ACommandText : string); virtual;

    procedure InitializeConnection;
    procedure InitFields(ADataSet: TDataSet);
    procedure InitField(AField: TField);

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

    property Connection: TFDConnection
      read GetConnection;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property DriverNames: TStrings
      read GetDriverNames;

    property SQL: string
      read GetSQL write SetSQL;

    property DataSet: TFDDataSet
      read GetDataSet;

    property Items[AIndex: Integer]: TFDMemTable
      read GetItem; default;

    property Active: Boolean
      read GetActive;

    property CanModify: Boolean
      read GetCanModify;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSetCount: Integer
      read GetDataSetCount;

    property MultipleResultSets: Boolean
      read GetMultipleResultSets write SetMultipleResultSets;

    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;

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

    {$REGION 'IFieldLists'}
    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;
    {$ENDREGION}

    {$REGION 'IFieldVisibility'}
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
  inherited AfterConstruction;
  FConstantFields := TCollections.CreateObjectList<TField>(False);
  FEmptyFields    := TCollections.CreateObjectList<TField>(False);
  FNonEmptyFields := TCollections.CreateObjectList<TField>(False);
  FFavoriteFields := TCollections.CreateObjectList<TField>(False);
  FConstantFieldsVisible := True;
  FEmptyFieldsVisible    := True;
  FDriverNames := TStringList.Create;
  FDataSets := TCollections.CreateObjectList<TFDMemTable>;
  FDManager.ResourceOptions.AutoReconnect := True;
  // Blocking with cancel dialog.
  FDManager.ResourceOptions.CmdExecMode := amCancelDialog;
  // Enable dialogs.
  FDManager.ResourceOptions.SilentMode  := False;
  FDManager.GetDriverNames(DriverNames);
  InitializeConnection;
end;

procedure TdmData.BeforeDestruction;
begin
  FConnectionSettings.Free;
  FDriverNames.Free;
  FOnAfterExecute.RemoveAll(Self);
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
{$ENDREGION}

function TdmData.GetItem(AIndex: Integer): TFDMemTable;
begin
  Result := FDataSets[AIndex];
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

function TdmData.GetDataSet: TFDDataSet;
begin
  if FDataSets.Any then
    Result := FDataSets.First
  else
    Result := qryMain;
end;

function TdmData.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

function TdmData.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TdmData.GetCanModify: Boolean;
begin
  Result := not qryMain.UpdateOptions.ReadOnly;
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
  InitializeConnection;
end;

{$REGION 'conMain'}
procedure TdmData.conMainAfterConnect(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainAfterConnect');
end;

procedure TdmData.conMainAfterDisconnect(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainAfterDisconnect');
end;

procedure TdmData.conMainAfterStartTransaction(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainAfterStartTransaction');
end;

procedure TdmData.conMainBeforeConnect(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainBeforeConnect');
end;

procedure TdmData.conMainBeforeDisconnect(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainBeforeDisconnect');
end;

procedure TdmData.conMainBeforeStartTransaction(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainBeforeStartTransaction');
end;

procedure TdmData.conMainError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  Logger.Track('TdmDataFireDAC.conMainError');
end;

procedure TdmData.conMainLost(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainLost');
end;

procedure TdmData.conMainRecover(ASender, AInitiator: TObject;
  AException: Exception; var AAction: TFDPhysConnectionRecoverAction);
begin
  Logger.Track('TdmDataFireDAC.conMainRecover');
end;

procedure TdmData.conMainRestored(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.conMainRestored');
end;
{$ENDREGION}

{$REGION 'qryMain'}
procedure TdmData.qryMainAfterApplyUpdates(DataSet: TFDDataSet;
  AErrors: Integer);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterApplyUpdates');
end;

procedure TdmData.qryMainAfterClose(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterClose');
end;

procedure TdmData.qryMainAfterExecute(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterExecute');
end;

procedure TdmData.qryMainAfterGetRecords(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterGetRecords');
end;

procedure TdmData.qryMainAfterOpen(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterOpen');
end;

procedure TdmData.qryMainAfterRefresh(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterRefresh');
end;

procedure TdmData.qryMainAfterRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterRowRequest');
end;

procedure TdmData.qryMainBeforeApplyUpdates(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeApplyUpdates');
end;

procedure TdmData.qryMainBeforeClose(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeClose');
end;

procedure TdmData.qryMainBeforeExecute(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeExecute');
end;

procedure TdmData.qryMainBeforeGetRecords(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeGetRecords');
end;

procedure TdmData.qryMainBeforeOpen(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeOpen');
end;

procedure TdmData.qryMainBeforeRefresh(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeRefresh');
end;

procedure TdmData.qryMainBeforeRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeRowRequest');
end;

procedure TdmData.qryMainCommandChanged(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.qryMainCommandChanged');
end;

procedure TdmData.qryMainError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  Logger.Track('TdmDataFireDAC.qryMainError');
end;

procedure TdmData.qryMainExecuteError(ASender: TObject; ATimes,
  AOffset: Integer; AError: EFDDBEngineException; var AAction: TFDErrorAction);
begin
  Logger.Track('TdmDataFireDAC.qryMainExecuteError');
end;

procedure TdmData.qryMainMasterSetValues(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainMasterSetValues');
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmData.InitField(AField: TField);
var
  B: Boolean;
begin
  B := True;
  if ShowFavoriteFieldsOnly then
    B := FFavoriteFields.Contains(AField);
  if B and not ConstantFieldsVisible then
    B := not FConstantFields.Contains(AField);
  if B and not EmptyFieldsVisible then
    B := not FEmptyFields.Contains(AField);

  AField.Visible := B;
end;

procedure TdmData.InitFields(ADataSet: TDataSet);
var
  Field : TField;
begin
  for Field in ADataSet.Fields do
    InitField(Field);
end;

{ Assigns application level connection settings (TConnectionSettings object) to
  the FireDAC connection component. }

procedure TdmData.InitializeConnection;
begin
  Logger.Track('InitializeConnection');
//  Logger.Send('ConnectionString', Connection.ConnectionString);

  Logger.Send('DriverName', ConnectionSettings.DriverName);
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
//  Connection.ConnectionString := ConnectionSettings.ConnectionString;
  Connection.DriverName       := ConnectionSettings.DriverName;
  if MultipleResultSets then
  begin
    Connection.FetchOptions.AutoClose := False; // required to support multiple resultsets
  end
  else
  begin
    Connection.FetchOptions.AutoClose := True;
  end;
  conMain.UpdateOptions.ReadOnly := ConnectionSettings.ReadOnly;
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
    Logger.Send('ConnectionString', Connection.ConnectionString);
    Connection.LoginPrompt := False;
    // Set this to prevent issues with FireDac's need for a cursor object﻿
    Connection.ResourceOptions.SilentMode := True;
    Connection.ResourceOptions.AutoReconnect := ConnectionSettings.AutoReconnect;
    Logger.Send('ConnectionString', Connection.ConnectionString);
  end;
end;

procedure TdmData.InternalExecute(const ACommandText: string);
var
  LMemTable : TFDMemTable;
  B         : Boolean;
begin
  if Trim(ACommandText) <> '' then
  begin
    FDataSets.Clear;
    qryMain.Close;
    qryMain.Open(ACommandText);
    if MultipleResultSets then
    begin
      LMemTable := TFDMemTable.Create(nil);
      LMemTable.Data := qryMain.Data;
      FDataSets.Add(LMemTable);
      B := qryMain.Active;
      while B do
      begin
        qryMain.NextRecordSet;
        if qryMain.Active then
        begin
          LMemTable := TFDMemTable.Create(nil);
          LMemTable.Data := qryMain.Data;
          FDataSets.Add(LMemTable);
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
  Result := False;
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
end;

procedure TdmData.Sort(const AFieldName: string; ADescending: Boolean);
begin
  Sort(DataSet, AFieldName, ADescending);
end;

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
  finally
    DataSet.EnableControls;
  end;
end;

procedure TdmData.Execute;
begin
  InitializeConnection;
  InternalExecute(SQL);
  UpdateFieldLists;
  if OnAfterExecute.CanInvoke then
    OnAfterExecute.Invoke(Self);
  FExecuted := True;
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


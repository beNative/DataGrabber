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
  FireDAC.Phys.TData, FireDAC.Phys.TDBX,

  FireDAC.VCLUI.Async,
  FireDAC.Comp.UI,

  FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.DatS,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait,

  Spring, Spring.Collections,

  DataGrabber.ConnectionSettings, DataGrabber.Interfaces;

  {
    Connection.OnLost, OnRecover
  }

type
  TdmDataFireDAC = class(
    TDataModule,
    IData,
    IFieldLists,
    IFieldVisiblity
  )
    conMain : TFDConnection;
    qryMain : TFDQuery;

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

  private
    FConstantFields         : IList<TField>;
    FEmptyFields            : IList<TField>;
    FNonEmptyFields         : IList<TField>;
    FFavoriteFields         : IList<TField>;
    FConstantFieldsVisible  : Boolean;
    FEmptyFieldsVisible     : Boolean;
    FShowFavoriteFieldsOnly : Boolean;
    FExecuted               : Boolean;
    FFetchOnDemand          : Boolean;
    FPacketRecords          : Integer;
    FSQL                    : string;
    FConnectionSettings     : TConnectionSettings;
    FMaxRecords             : Integer;
    FDriverNames            : TStrings;
    FOnAfterExecute         : Event<TNotifyEvent>;
    FDataSets               : IList<TFDMemTable>;

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
    function GetDataSet : TDataSet;
    function GetRecordCount : Integer;
    function GetExecuted: Boolean;
    function GetActive: Boolean;
    procedure SetExecuted(const Value: Boolean);
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetFDQuery: TFDQuery;
    function GetOnAfterExecute: IEvent<TNotifyEvent>;
    function GetConnection: TFDConnection;
    function GetItem(AIndex: Integer): TFDMemTable;
    function GetDataSetCount: Integer;
    {$ENDREGION}

    procedure FConnectionSettingsChanged(Sender: TObject);


  protected
    procedure Execute;
    procedure InternalExecute(const ACommandText : string); virtual;

    procedure InitializeConnection;
    procedure InitFields(ADataSet: TDataSet);

    procedure UpdateFieldLists;

    property FDQuery: TFDQuery
      read GetFDQuery;

    property Connection: TFDConnection
      read GetConnection;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property DriverNames: TStrings
      read GetDriverNames;

    {$REGION 'IData'}
    property SQL: string
      read GetSQL write SetSQL;

    property DataSet: TDataSet
      read GetDataSet;

    property Items[AIndex: Integer]: TFDMemTable
      read GetItem; default;

    property Active: Boolean
      read GetActive;

    property CanModify: Boolean
      read GetCanModify;

    property Executed: Boolean
      read GetExecuted write SetExecuted;

    property RecordCount: Integer
      read GetRecordCount;

    property DataSetCount: Integer
      read GetDataSetCount;
    {$ENDREGION}

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TConnectionSettings
    ); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure InitField(AField: TField);

    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;

    {$REGION 'IDataPersistable'}
    procedure SaveToFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    );

    procedure LoadFromFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    );
    {$ENDREGION}

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
constructor TdmDataFireDAC.Create(AOwner: TComponent;
  ASettings: TConnectionSettings);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(ASettings, 'ASettings');
  FConnectionSettings := TConnectionSettings.Create;
  FConnectionSettings.Assign(ASettings);
  FConnectionSettings.OnChanged.Add(FConnectionSettingsChanged);
end;

procedure TdmDataFireDAC.AfterConstruction;
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
  FDManager.ResourceOptions.AutoReconnect := True; // default set to False
  // Blocking with cancel dialog.
  FDManager.ResourceOptions.CmdExecMode := amCancelDialog;
  // Enable dialogs.
  FDManager.ResourceOptions.SilentMode  := False;
  FDManager.GetDriverNames(DriverNames);
  InitializeConnection;
end;

procedure TdmDataFireDAC.BeforeDestruction;
begin
  FConnectionSettings.Free;
  FDriverNames.Free;
  FOnAfterExecute.RemoveAll(Self);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
{$REGION 'IFieldLists'}
function TdmDataFireDAC.GetConstantFields: IList<TField>;
begin
  Result := FConstantFields;
end;

function TdmDataFireDAC.GetEmptyFields: IList<TField>;
begin
  Result := FEmptyFields;
end;

function TdmDataFireDAC.GetNonEmptyFields: IList<TField>;
begin
  Result := FNonEmptyFields;
end;
{$ENDREGION}

{$REGION 'IFieldVisibility'}
function TdmDataFireDAC.GetConstantFieldsVisible: Boolean;
begin
  Result := FConstantFieldsVisible;
end;

procedure TdmDataFireDAC.SetConstantFieldsVisible(const Value: Boolean);
begin
  if Value <> ConstantFieldsVisible then
  begin
    FConstantFieldsVisible := Value;
    InitFields(DataSet);
  end;
end;

function TdmDataFireDAC.GetEmptyFieldsVisible: Boolean;
begin
  Result := FEmptyFieldsVisible;
end;

procedure TdmDataFireDAC.SetEmptyFieldsVisible(const Value: Boolean);
begin
  if Value <> EmptyFieldsVisible then
  begin
    FEmptyFieldsVisible := Value;
    InitFields(DataSet);
  end;
end;

function TdmDataFireDAC.GetShowFavoriteFieldsOnly: Boolean;
begin
  Result := FShowFavoriteFieldsOnly;
end;

procedure TdmDataFireDAC.SetShowFavoriteFieldsOnly(const Value: Boolean);
begin
  if Value <> ShowFavoriteFieldsOnly then
  begin
    FShowFavoriteFieldsOnly := Value;
    InitFields(DataSet);
  end;
end;
{$ENDREGION}

function TdmDataFireDAC.GetFDQuery: TFDQuery;
begin
  Result := qryMain;
end;

function TdmDataFireDAC.GetItem(AIndex: Integer): TFDMemTable;
begin
  Result := FDataSets[AIndex];
end;

function TdmDataFireDAC.GetExecuted: Boolean;
begin
  Result := FExecuted;
end;

procedure TdmDataFireDAC.SetExecuted(const Value: Boolean);
begin
  if Value <> Executed then
  begin
    FExecuted := Value;
  end;
end;

function TdmDataFireDAC.GetDriverNames: TStrings;
begin
  Result := FDriverNames;
end;

function TdmDataFireDAC.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TdmDataFireDAC.GetDataSet: TDataSet;
begin
  if FDataSets.Any then
    Result := FDataSets.First
  else
    Result := qryMain;
end;

function TdmDataFireDAC.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

function TdmDataFireDAC.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TdmDataFireDAC.GetCanModify: Boolean;
begin
  Result := True;
end;

function TdmDataFireDAC.GetConnected: Boolean;
begin
  Result := Connection.Connected;
end;

procedure TdmDataFireDAC.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    Connection.Connected := Value;
  end;
end;

function TdmDataFireDAC.GetConnection: TFDConnection;
begin
  Result := conMain;
end;

function TdmDataFireDAC.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

function TdmDataFireDAC.GetOnAfterExecute: IEvent<TNotifyEvent>;
begin
  Result := FOnAfterExecute;
end;

function TdmDataFireDAC.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TdmDataFireDAC.SetSQL(const Value: string);
begin
  if Value <> SQL then
  begin
    FSQL := Value;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmDataFireDAC.FConnectionSettingsChanged(Sender: TObject);
begin
  InitializeConnection;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmDataFireDAC.InitField(AField: TField);
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

procedure TdmDataFireDAC.InitFields(ADataSet: TDataSet);
var
  Field : TField;
begin
  for Field in ADataSet.Fields do
    InitField(Field);
end;

{ Assigns application level connection settings (TConnectionSettings object) to
  the FireDAC connection component. }

procedure TdmDataFireDAC.InitializeConnection;
begin
  Logger.Track('InitializeConnection');
//  Logger.Send('ConnectionString', Connection.ConnectionString);

  Logger.Send('DriverName', ConnectionSettings.DriverName);

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
  Connection.FetchOptions.AutoClose := False; // required to support multiple resultsets

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

procedure TdmDataFireDAC.InternalExecute(const ACommandText: string);
var
  LMemTable : TFDMemTable;
  B         : Boolean;
begin
  if Trim(ACommandText) <> '' then
  begin
    FDataSets.Clear;
    FDQuery.Close;
    FDQuery.Open(ACommandText);
    LMemTable := TFDMemTable.Create(nil);
    FDQuery.FetchAll;
    LMemTable.Data := FDQuery.Data;
    FDataSets.Add(LMemTable);
    B := FDQuery.Active;
    while B do
    begin
      FDQuery.NextRecordSet;
      //FDQuery.FetchAll;
      if FDQuery.Active then
      begin
        LMemTable := TFDMemTable.Create(nil);
        FDQuery.FetchAll;
        LMemTable.Data := FDQuery.Data;
        FDataSets.Add(LMemTable);
      end
      else
      begin
        B := False;
      end;
    end;
  end;
end;

function TdmDataFireDAC.ShowAllFields: Boolean;
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

procedure TdmDataFireDAC.UpdateFieldLists;
var
  S        : string;
  T        : string;
  F        : TField;
  LIsEmpty : Boolean;
  LIsConst : Boolean;
begin
  Logger.Track('UpdateFieldLists');
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

procedure TdmDataFireDAC.Execute;
begin
  DataSet.Active := False;
  InitializeConnection;
  InternalExecute(SQL);
  UpdateFieldLists;
  if OnAfterExecute.CanInvoke then
    OnAfterExecute.Invoke(Self);
  FExecuted := True;
end;

{$REGION 'IDataPersistable'}
procedure TdmDataFireDAC.LoadFromFile(const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  FDQuery.LoadFromFile(AFileName, AFormat);
end;


procedure TdmDataFireDAC.qryMainAfterApplyUpdates(DataSet: TFDDataSet;
  AErrors: Integer);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterApplyUpdates');
end;

procedure TdmDataFireDAC.qryMainAfterGetRecords(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterGetRecords');
end;

procedure TdmDataFireDAC.qryMainAfterOpen(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterOpen');
end;

procedure TdmDataFireDAC.qryMainAfterRefresh(DataSet: TDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterRefresh');
end;

procedure TdmDataFireDAC.qryMainAfterRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainAfterRowRequest');
end;

procedure TdmDataFireDAC.qryMainBeforeApplyUpdates(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeApplyUpdates');
end;

procedure TdmDataFireDAC.qryMainBeforeRowRequest(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainBeforeRowRequest');
end;

procedure TdmDataFireDAC.qryMainCommandChanged(Sender: TObject);
begin
  Logger.Track('TdmDataFireDAC.qryMainCommandChanged');
end;

procedure TdmDataFireDAC.qryMainError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  Logger.Track('TdmDataFireDAC.qryMainError');
end;

procedure TdmDataFireDAC.qryMainExecuteError(ASender: TObject; ATimes,
  AOffset: Integer; AError: EFDDBEngineException; var AAction: TFDErrorAction);
begin
  Logger.Track('TdmDataFireDAC.qryMainExecuteError');
end;

procedure TdmDataFireDAC.qryMainMasterSetValues(DataSet: TFDDataSet);
begin
  Logger.Track('TdmDataFireDAC.qryMainMasterSetValues');
end;

procedure TdmDataFireDAC.SaveToFile(const AFileName: string;
  AFormat: TFDStorageFormat);
begin
  FDQuery.SaveToFile(AFileName, AFormat);
end;
{$ENDREGION}
{$ENDREGION}

end.


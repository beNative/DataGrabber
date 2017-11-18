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

unit DataGrabber.Data.FireDAC;

{ Represents all fetched data for a connectionview. }

{
  We decided not to share connections between IData instances, so if many
  connectionviews are created of a given connectionprofile, they both will
  have their own private connection.
}

interface

uses
  System.SysUtils, System.Classes,

  Data.DB,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param,
  FireDAC.UI.Intf,
  FireDAC.Phys, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.DatS,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait,

  Spring, Spring.Collections,

  DataGrabber.ConnectionSettings, DataGrabber.Interfaces;

type
  TdmDataFireDAC = class(TDataModule, IData, IFieldLists, IFieldVisiblity,
    IDataEvents)
    conMain    : TFDConnection;
    qryMain    : TFDQuery;
    dsMemTable : TFDMemTable;

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
    FConnectionString       : string;
    FPacketRecords          : Integer;
    FSQL                    : string;
    FConnectionSettings     : TConnectionSettings;
    FMaxRecords             : Integer;
    FProtocols              : TStrings;
    FOnAfterExecute         : Event<TNotifyEvent>;
    FSettings               : IDGSettings;

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
    {$ENDREGION}

    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    function GetProtocols: TStrings;
    function GetConnectionSettings: TConnectionSettings;

    {$REGION 'property access methods'}
    function GetDataSet : TDataSet;
    function GetRecordCount : Integer;
    function GetExecuted: Boolean;
    function GetActive: Boolean;
    //function GetConnection: IConnection;
    procedure SetExecuted(const Value: Boolean);
    function GetMaxRecords: Integer;
    procedure SetMaxRecords(const Value: Integer);
    function GetPacketRecords: Integer;
    procedure SetPacketRecords(const Value: Integer);
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetFetchOnDemand: Boolean;
    procedure SetFetchOnDemand(const Value: Boolean);
    {$ENDREGION}

    procedure Execute; overload;

    procedure InternalExecute(const ACommandText : string); virtual;

    function Execute(const ACommandText: string): Boolean; overload;
    function GetConnection: TFDConnection;
    function GetFDQuery: TFDQuery;
    function GetOnAfterExecute: IEvent<TNotifyEvent>;

  protected
    procedure InitFields(ADataSet: TDataSet);

    procedure UpdateFieldLists;

    property FDQuery: TFDQuery
      read GetFDQuery;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property Connection: TFDConnection
      read GetConnection;

    property ConnectionString: string
      read GetConnectionString write SetConnectionString;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property Protocols: TStrings
      read GetProtocols;

    {$REGION 'IData'}
    property SQL: string
      read GetSQL write SetSQL;

    property DataSet: TDataSet
      read GetDataSet;

    property Active: Boolean
      read GetActive;

    property CanModify: Boolean
      read GetCanModify;

    property Executed: Boolean
      read GetExecuted write SetExecuted;

    property RecordCount: Integer
      read GetRecordCount;

    property MaxRecords: Integer
      read GetMaxRecords write SetMaxRecords;

    property PacketRecords: Integer
      read GetPacketRecords write SetPacketRecords;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;
    {$ENDREGION}

  public
    constructor Create(AOwner: TComponent; ASettings: IDGSettings); reintroduce;
    procedure AfterConstruction; override;

    function ShowAllFields: Boolean;
    procedure InitField(AField: TField);
    procedure BeforeDestruction; override;

    { IFieldLists }
    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;

    { IFieldVisibility }
    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;

    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmDataFireDAC.AfterConstruction;
begin
  inherited AfterConstruction;
  FConstantFields := TCollections.CreateObjectList<TField>(False);
  FEmptyFields    := TCollections.CreateObjectList<TField>(False);
  FNonEmptyFields := TCollections.CreateObjectList<TField>(False);
  FFavoriteFields := TCollections.CreateObjectList<TField>(False);
  FConstantFieldsVisible := True;
  FEmptyFieldsVisible    := True;
  FPacketRecords         := -1; // fetch all
  FMaxRecords            := -1; // fetch all
  FProtocols := TStringList.Create;
  FDManager.GetDriverNames(Protocols);
end;

procedure TdmDataFireDAC.BeforeDestruction;
begin
  FProtocols.Free;
  FOnAfterExecute.RemoveAll(Self);
  inherited BeforeDestruction;
end;
constructor TdmDataFireDAC.Create(AOwner: TComponent; ASettings: IDGSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
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

{$REGION 'IConnection'}
function TdmDataFireDAC.GetFDQuery: TFDQuery;
begin
  Result := qryMain;
end;

function TdmDataFireDAC.GetFetchOnDemand: Boolean;
begin
  Result := FFetchOnDemand;
end;

procedure TdmDataFireDAC.SetFetchOnDemand(const Value: Boolean);
begin
  if Value <> FetchOnDemand then
  begin
    FFetchOnDemand := Value;
  end;
end;

function TdmDataFireDAC.GetMaxRecords: Integer;
begin
  Result := FMaxRecords;
end;

procedure TdmDataFireDAC.SetMaxRecords(const Value: Integer);
begin
  if Value <> MaxRecords then
  begin
    qryMain.FetchOptions.RecsMax := Value;
  end;
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

function TdmDataFireDAC.GetPacketRecords: Integer;
begin
  Result := FPacketRecords;
end;

procedure TdmDataFireDAC.SetPacketRecords(const Value: Integer);
begin
  if Value <> PacketRecords then
  begin
    FPacketRecords := Value;
  end;
end;

function TdmDataFireDAC.GetProtocols: TStrings;
begin
  Result := FProtocols;
end;

function TdmDataFireDAC.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TdmDataFireDAC.GetDataSet: TDataSet;
begin
  Result := qryMain;
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

function TdmDataFireDAC.GetConnection: TFDConnection;
begin
  Result := conMain;
end;

function TdmDataFireDAC.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;
{$ENDREGION}

{$REGION 'IData'}

{$ENDREGION}

function TdmDataFireDAC.GetOnAfterExecute: IEvent<TNotifyEvent>;
begin
  Result := FOnAfterExecute;
end;

procedure TdmDataFireDAC.SetConnected(const Value: Boolean);
begin
  if Value <> Connected then
  begin
    Connection.Connected := Value;
  end;
end;

function TdmDataFireDAC.GetConnectionString: string;
begin
  Result := Connection.ConnectionString;
end;

procedure TdmDataFireDAC.SetConnectionString(const Value: string);
begin
  if Value <> ConnectionString then
  begin
    Connection.ConnectionString := Value;
  end;
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

procedure TdmDataFireDAC.InternalExecute(const ACommandText: string);
begin
  if Trim(ACommandText) <> '' then
  begin
    FDQuery.Active := False;
    FDQuery.FetchOptions.RecsMax := MaxRecords;
    if FetchOnDemand then
      FDQuery.FetchOptions.Mode := fmOnDemand
    else
      FDQuery.FetchOptions.Mode := fmAll;
    FDQuery.FetchOptions.RowsetSize := PacketRecords;
    FDQuery.SQL.Text := ACommandText;
    FDQuery.ExecSQL;
  end;
end;

function TdmDataFireDAC.ShowAllFields: Boolean;
var
  B : Boolean;
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
{$ENDREGION}


function TdmDataFireDAC.Execute(const ACommandText: string): Boolean;
begin
  SQL := ACommandText;
  Execute;
end;

procedure TdmDataFireDAC.Execute;
begin
  DataSet.Active := False;
  InternalExecute(SQL);
  UpdateFieldLists;
  if OnAfterExecute.CanInvoke then
    OnAfterExecute.Invoke(Self);
  FExecuted := True;
end;

end.


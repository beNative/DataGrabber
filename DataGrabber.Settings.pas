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

unit DataGrabber.Settings;

interface

uses
  System.Classes, System.Generics.Collections,
  Vcl.Graphics,
  Data.DB,

  ts.Classes.ConnectionSettings, ts.Interfaces,

  DataGrabber.Interfaces, DataGrabber.SQLTemplates, DataGrabber.FormSettings,
  DataGrabber.ConnectionProfiles;

const
  SETTINGS_FILE = 'Settings.json';

type
  TDGSettings = class(TComponent, IDGSettings, IDataViewSettings)
  private
    FSQLTemplates             : TSQLTemplates;
    FFormSettings             : TFormSettings;
    FProviderMode             : Boolean;
    FDataTypeColors           : TDictionary<TDataType, TColor>;
    FGridCellColoring         : Boolean;
    FConnectionString         : string;
    FConnectionProfiles       : TConnectionProfiles;
    FPacketRecords            : Integer;
    FFileName                 : string;
    FFetchOnDemand            : Boolean;
    FConnectionType           : string;
    FGridType                 : string;
    FDefaultConnectionProfile : string;
    FConnectionSettings       : TConnectionSettings;
    FRepositoryVisible        : Boolean;
    FDataInspectorVisible     : Boolean;

    function GetGridCellColoring: Boolean;
    procedure SetGridCellColoring(const Value: Boolean);
    function GetFieldTypeColor(Index: TFieldType): TColor;
    function GetDataTypeColor(Index: TDataType): TColor;
    procedure SetDataTypeColor(Index: TDataType; const Value: TColor);
    function GetConnectionProfiles: TConnectionProfiles;
    procedure SetConnectionProfiles(const Value: TConnectionProfiles);
    function GetConnectionString: string;
    function GetConnectionType: string;
    function GetFetchOnDemand: Boolean;
    function GetFormSettings: TFormSettings;
    function GetPacketRecords: Integer;
    function GetProviderMode: Boolean;
    procedure SetConnectionString(const Value: string);
    procedure SetConnectionType(const Value: string);
    procedure SetFetchOnDemand(const Value: Boolean);
    procedure SetFormSettings(const Value: TFormSettings);
    procedure SetPacketRecords(const Value: Integer);
    procedure SetProviderMode(const Value: Boolean);
    function GetConnectionSettings: TConnectionSettings;
    function GetDataInspectorVisible: Boolean;
    function GetDefaultConnectionProfile: string;
    function GetRepositoryVisible: Boolean;
    procedure SetConnectionSettings(const Value: TConnectionSettings);
    procedure SetDataInspectorVisible(const Value: Boolean);
    procedure SetDefaultConnectionProfile(const Value: string);
    procedure SetRepositoryVisible(const Value: Boolean);
    function GetGridType: string;
    procedure SetGridType(const Value: string);

  protected
    procedure AssignStandardSettings;

  public
    constructor Create(AOwner: TComponent); override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Load;
    procedure Save;

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

  published
    { Only supported in combination with a ClientDataSet/Provider }
    property ProviderMode: Boolean
      read GetProviderMode write SetProviderMode;

    property PacketRecords: Integer
      read GetPacketRecords write SetPacketRecords;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring default True;

    property ConnectionString: string
      read GetConnectionString write SetConnectionString;

    property SQLTemplates: TSQLTemplates
      read FSQLTemplates write FSQLTemplates;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property ConnectionProfiles: TConnectionProfiles
      read GetConnectionProfiles write SetConnectionProfiles;

    property ConnectionType: string
      read GetConnectionType write SetConnectionType;

    property GridType: string
      read GetGridType write SetGridType;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings write SetConnectionSettings;

    property DefaultConnectionProfile: string
      read GetDefaultConnectionProfile write SetDefaultConnectionProfile;

    property RepositoryVisible: Boolean
      read GetRepositoryVisible write SetRepositoryVisible;

    property DataInspectorVisible: Boolean
      read GetDataInspectorVisible write SetDataInspectorVisible;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms, Vcl.GraphUtil, Vcl.Dialogs,

  JsonDataObjects;

{$REGION 'construction and destruction'}
constructor TDGSettings.Create(AOwner: TComponent);
begin
  inherited Create(Application);
end;

procedure TDGSettings.AfterConstruction;
var
  I : TDataType;
begin
  inherited AfterConstruction;
  Name := 'Settings';
  FDataTypeColors := TDictionary<TDataType, TColor>.Create;
  FConnectionSettings := TConnectionSettings.Create;
  for I := Low(TDataType) to High(TDataType) do
    FDataTypeColors.Add(I, ColorAdjustLuma(DEFAULT_DATATYPE_COLORS[I], 6, False));
  FFormSettings := TFormSettings.Create;
  FConnectionProfiles := TConnectionProfiles.Create(Self);
  FFileName := SETTINGS_FILE;
  FGridCellColoring := True;
end;

procedure TDGSettings.BeforeDestruction;
begin
  FreeAndNil(FConnectionProfiles);
  FreeAndNil(FFormSettings);
  FreeAndNil(FDataTypeColors);
  FreeAndNil(FConnectionSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TDGSettings.GetFieldTypeColor(Index: TFieldType): TColor;
begin
  case Index of
    ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo, ftMemo,
    ftFmtMemo:
      Result := DataTypeColors[dtString];

    ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint, ftLongWord,
    ftShortint, ftByte:
      Result := DataTypeColors[dtInteger];

    ftBoolean:
      Result := DataTypeColors[dtBoolean];

    ftFloat, ftCurrency, ftBCD, ftExtended, ftFMTBcd:
      Result := DataTypeColors[dtFloat];

    ftDate:
      Result := DataTypeColors[dtDate];

    ftTime:
      Result := DataTypeColors[dtTime];

    ftDateTime, ftTimeStamp, ftOraTimeStamp:
      Result := DataTypeColors[dtDateTime];

    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftUnknown, ftParadoxOle, ftGuid,
    ftDBaseOle, ftTypedBinary, ftCursor, ftADT, ftArray, ftReference, ftStream,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface, ftIDispatch,
    ftOraInterval, ftConnection, ftParams:
      Result := clWhite;
    else
      Result := clWhite;
  end;
end;

function TDGSettings.GetGridCellColoring: Boolean;
begin
  Result := FGridCellColoring;
end;

procedure TDGSettings.SetGridCellColoring(const Value: Boolean);
begin
  FGridCellColoring := Value;
end;

function TDGSettings.GetGridType: string;
begin
  Result := FGridType;
end;

procedure TDGSettings.SetGridType(const Value: string);
begin
  FGridType := Value;
end;

function TDGSettings.GetConnectionProfiles: TConnectionProfiles;
begin
  Result := FConnectionProfiles;
end;

procedure TDGSettings.SetConnectionProfiles(const Value: TConnectionProfiles);
begin
  FConnectionProfiles := Value;
end;

function TDGSettings.GetPacketRecords: Integer;
begin
  Result := FPacketRecords;
end;

procedure TDGSettings.SetPacketRecords(const Value: Integer);
begin
  FPacketRecords := Value;
end;

function TDGSettings.GetProviderMode: Boolean;
begin
  Result := FProviderMode;
end;

procedure TDGSettings.SetProviderMode(const Value: Boolean);
begin
  FProviderMode := Value;
end;

function TDGSettings.GetRepositoryVisible: Boolean;
begin
  Result := FRepositoryVisible;
end;

procedure TDGSettings.SetRepositoryVisible(const Value: Boolean);
begin
  FRepositoryVisible := Value;
end;

function TDGSettings.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

procedure TDGSettings.SetConnectionSettings(const Value: TConnectionSettings);
begin
  FConnectionSettings := Value;
end;

function TDGSettings.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

procedure TDGSettings.SetConnectionString(const Value: string);
begin
  FConnectionString := Value;
end;

function TDGSettings.GetConnectionType: string;
begin
  Result := FConnectionType;
end;

procedure TDGSettings.SetConnectionType(const Value: string);
begin
  FConnectionType := Value;
end;

function TDGSettings.GetDataTypeColor(Index: TDataType): TColor;
begin
  Result := FDataTypeColors[Index];
end;

procedure TDGSettings.SetDataTypeColor(Index: TDataType; const Value: TColor);
begin
  FDataTypeColors[Index] := Value;
end;

function TDGSettings.GetDefaultConnectionProfile: string;
begin
  Result := FDefaultConnectionProfile;
end;

procedure TDGSettings.SetDefaultConnectionProfile(const Value: string);
begin
  FDefaultConnectionProfile := Value;
end;

function TDGSettings.GetDataInspectorVisible: Boolean;
begin
  Result := FDataInspectorVisible;
end;

procedure TDGSettings.SetDataInspectorVisible(const Value: Boolean);
begin
  FDataInspectorVisible := Value;
end;

function TDGSettings.GetFetchOnDemand: Boolean;
begin
  Result := FFetchOnDemand;
end;

procedure TDGSettings.SetFetchOnDemand(const Value: Boolean);
begin
  FFetchOnDemand := Value;
end;

function TDGSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

procedure TDGSettings.SetFormSettings(const Value: TFormSettings);
begin
  FFormSettings := Value;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TDGSettings.AssignStandardSettings;
begin
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TDGSettings.Load;
var
  JO  : TJsonObject;
  I   : Integer;
  CP  : TConnectionProfile;
begin
  if FileExists('settings.json') then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile('settings.json');
      JO.ToSimpleObject(Self);
      JO['FormSettings'].ObjectValue.ToSimpleObject(FFormSettings);
      for I := 0 to JO['ConnectionProfiles'].ArrayValue.Count - 1 do
      begin
        CP := FConnectionProfiles.Add;
        JO['ConnectionProfiles'].ArrayValue[I].ObjectValue.ToSimpleObject(CP);
        JO['ConnectionProfiles']
          .ArrayValue[I].O['ConnectionSettings']
          .ObjectValue.ToSimpleObject(CP.ConnectionSettings);
      end;
    finally
      JO.Free;
    end;
  end;
end;

procedure TDGSettings.Save;
var
  JO : TJsonObject;
  I  : Integer;
begin
  JO := TJsonObject.Create;
  try
    JO.FromSimpleObject(Self);
    JO['FormSettings'].ObjectValue.FromSimpleObject(FormSettings);
    for I := 0 to ConnectionProfiles.Count - 1 do
    begin
      JO['ConnectionProfiles'].ArrayValue
        .AddObject
        .FromSimpleObject(ConnectionProfiles[I]);
      JO['ConnectionProfiles'].ArrayValue
        .Values[I]
        .O['ConnectionSettings']
        .ObjectValue
        .FromSimpleObject(ConnectionProfiles[I].ConnectionSettings);
    end;
    JO.SaveToFile('settings.json', False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

initialization
  TDGSettings.ClassName;

end.

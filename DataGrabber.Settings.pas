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

  DataGrabber.Interfaces, DataGrabber.FormSettings,
  DataGrabber.ConnectionSettings, DataGrabber.ConnectionProfiles;

type
  TSettings = class(TComponent, ISettings, IDataViewSettings)
  private
    FFormSettings             : TFormSettings;
    FDataTypeColors           : TDictionary<TDataType, TColor>;
    FGridCellColoring         : Boolean;
    FConnectionProfiles       : TConnectionProfiles;
    FFileName                 : string;
    FGridType                 : string;
    FDefaultConnectionProfile : string;
    FConnectionSettings       : TConnectionSettings;
    FDataInspectorVisible     : Boolean;
    FShowVerticalGridLines    : Boolean;
    FShowHorizontalGridLines  : Boolean;
    FResultDisplayLayout      : TResultDisplayLayout;

    {$REGION 'property access methods'}
    function GetGridCellColoring: Boolean;
    procedure SetGridCellColoring(const Value: Boolean);
    function GetFieldTypeColor(Index: TFieldType): TColor;
    function GetDataTypeColor(Index: TDataType): TColor;
    procedure SetDataTypeColor(Index: TDataType; const Value: TColor);
    function GetConnectionProfiles: TConnectionProfiles;
    procedure SetConnectionProfiles(const Value: TConnectionProfiles);
    function GetFormSettings: TFormSettings;
    procedure SetFormSettings(const Value: TFormSettings);
    function GetConnectionSettings: TConnectionSettings;
    function GetDataInspectorVisible: Boolean;
    function GetDefaultConnectionProfile: string;
    procedure SetConnectionSettings(const Value: TConnectionSettings);
    procedure SetDataInspectorVisible(const Value: Boolean);
    procedure SetDefaultConnectionProfile(const Value: string);
    function GetGridType: string;
    procedure SetGridType(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetShowHorizontalGridLines: Boolean;
    function GetShowVerticalGridLines: Boolean;
    procedure SetShowHorizontalGridLines(const Value: Boolean);
    procedure SetShowVerticalGridLines(const Value: Boolean);
    function GetResultDisplayLayout: TResultDisplayLayout;
    procedure SetResultDisplayLayout(const Value: TResultDisplayLayout);
    {$ENDREGION}

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

    property FileName: string
      read GetFileName write SetFileName;

  published
    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring default True;

    property ShowHorizontalGridLines: Boolean
      read GetShowHorizontalGridLines write SetShowHorizontalGridLines default True;

    property ShowVerticalGridLines: Boolean
      read GetShowVerticalGridLines write SetShowVerticalGridLines default True;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property ConnectionProfiles: TConnectionProfiles
      read GetConnectionProfiles write SetConnectionProfiles;

    property GridType: string
      read GetGridType write SetGridType;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings write SetConnectionSettings;

    property DefaultConnectionProfile: string
      read GetDefaultConnectionProfile write SetDefaultConnectionProfile;

    property DataInspectorVisible: Boolean
      read GetDataInspectorVisible write SetDataInspectorVisible;

    property ResultDisplayLayout: TResultDisplayLayout
      read GetResultDisplayLayout
      write SetResultDisplayLayout
      default TResultDisplayLayout.Horizontal;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Forms, Vcl.GraphUtil, Vcl.Dialogs,

  JsonDataObjects,

  DataGrabber.Resources;

{$REGION 'construction and destruction'}
constructor TSettings.Create(AOwner: TComponent);
begin
  if not Assigned(AOwner) then
    inherited Create(Application)
  else
    inherited Create(AOwner);
end;

procedure TSettings.AfterConstruction;
var
  I : TDataType;
begin
  inherited AfterConstruction;
  Name := 'Settings';
  FShowVerticalGridLines := True;
  FShowHorizontalGridLines := True;
  FDataTypeColors := TDictionary<TDataType, TColor>.Create;
  FConnectionSettings := TConnectionSettings.Create;
  for I := Low(TDataType) to High(TDataType) do
    FDataTypeColors.Add(I, ColorAdjustLuma(DEFAULT_DATATYPE_COLORS[I], 6, False));
  FFormSettings := TFormSettings.Create;
  FConnectionProfiles := TConnectionProfiles.Create(Self);
  FFileName := SETTINGS_FILE;
  FGridCellColoring := True;
  FResultDisplayLayout := TResultDisplayLayout.Horizontal;
end;

procedure TSettings.BeforeDestruction;
begin
  FreeAndNil(FConnectionProfiles);
  FreeAndNil(FFormSettings);
  FreeAndNil(FDataTypeColors);
  FreeAndNil(FConnectionSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TSettings.GetFieldTypeColor(Index: TFieldType): TColor;
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

function TSettings.GetGridCellColoring: Boolean;
begin
  Result := FGridCellColoring;
end;

procedure TSettings.SetGridCellColoring(const Value: Boolean);
begin
  FGridCellColoring := Value;
end;

function TSettings.GetGridType: string;
begin
  Result := FGridType;
end;

procedure TSettings.SetGridType(const Value: string);
begin
  FGridType := Value;
end;

function TSettings.GetConnectionProfiles: TConnectionProfiles;
begin
  Result := FConnectionProfiles;
end;

procedure TSettings.SetConnectionProfiles(const Value: TConnectionProfiles);
begin
  FConnectionProfiles := Value;
end;

function TSettings.GetResultDisplayLayout: TResultDisplayLayout;
begin
  Result := FResultDisplayLayout;
end;

function TSettings.GetShowHorizontalGridLines: Boolean;
begin
  Result := FShowHorizontalGridLines;
end;

procedure TSettings.SetShowHorizontalGridLines(const Value: Boolean);
begin
  FShowHorizontalGridLines := Value;
end;

function TSettings.GetShowVerticalGridLines: Boolean;
begin
  Result := FShowVerticalGridLines;
end;

procedure TSettings.SetShowVerticalGridLines(const Value: Boolean);
begin
  FShowVerticalGridLines := Value;
end;

procedure TSettings.SetResultDisplayLayout(const Value: TResultDisplayLayout);
begin
  FResultDisplayLayout := Value;
end;

function TSettings.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

procedure TSettings.SetConnectionSettings(const Value: TConnectionSettings);
begin
  FConnectionSettings := Value;
end;

function TSettings.GetDataTypeColor(Index: TDataType): TColor;
begin
  Result := FDataTypeColors[Index];
end;

procedure TSettings.SetDataTypeColor(Index: TDataType; const Value: TColor);
begin
  FDataTypeColors[Index] := Value;
end;

function TSettings.GetDefaultConnectionProfile: string;
begin
  Result := FDefaultConnectionProfile;
end;

procedure TSettings.SetDefaultConnectionProfile(const Value: string);
begin
  FDefaultConnectionProfile := Value;
end;

function TSettings.GetDataInspectorVisible: Boolean;
begin
  Result := FDataInspectorVisible;
end;

procedure TSettings.SetDataInspectorVisible(const Value: Boolean);
begin
  FDataInspectorVisible := Value;
end;

function TSettings.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TSettings.SetFileName(const Value: string);
begin
  if Value <> FileName then
  begin
    FFileName := Value;
  end;
end;

function TSettings.GetFormSettings: TFormSettings;
begin
  Result := FFormSettings;
end;

procedure TSettings.SetFormSettings(const Value: TFormSettings);
begin
  FFormSettings := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TSettings.Load;
var
  JO : TJsonObject;
  I  : Integer;
  CP : TConnectionProfile;
begin
  if FileExists(FileName) then
  begin
    JO := TJsonObject.Create;
    try
      JO.LoadFromFile(FileName);
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

procedure TSettings.Save;
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
    JO.SaveToFile(FileName, False);
  finally
    JO.Free;
  end;
end;
{$ENDREGION}

initialization
  TSettings.ClassName;

end.

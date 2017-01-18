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

unit ts.Interfaces;

interface

uses
  System.Classes,
  Vcl.Graphics, Vcl.Forms,
  Data.DB, Data.Win.ADODB, Data.SqlExpr,
  Datasnap.DBClient,

  // ZEOS
  //ZConnection,

  // UNIDAC
  //Uni,

  DDuce.Components.DBGridView,

  ts.Classes.SQL.CompoundCondition, ts.Classes.SQL.Params,
  ts.Classes.ConnectionSettings,
  //ts.Classes.ListReport,
  ts.Classes.KeyValues,

  ts.Modules.List.Columns;

type
  TDataType = (
    dtBoolean,
    dtString,
    dtInteger,
    dtFloat,
    dtDate,
    dtTime,
    dtDateTime,
    dtNULL
  );

type
  IData = interface;
  INativeDataSet = interface;

  // Interface to be supported by the connection adaptors.
  IConnection = interface
  ['{37E7E015-A3AA-4193-9936-98BEB9D4C9A0}']
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    function GetConnectionType: string;
    function GetProtocols: TStrings;
    function GetConnectionSettings: TConnectionSettings;
    function GetConnection: TComponent;
    function CreateNativeDataSet: INativeDataSet;
    function Execute(const ACommandText: string): Boolean;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property Connection: TComponent
      read GetConnection;

    property ConnectionString: string
      read GetConnectionString write SetConnectionString;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property ConnectionType: string
      read GetConnectionType;

    property Protocols: TStrings
      read GetProtocols;
  end;

  ILogging = interface
  ['{4AB29FDF-0CFE-48FC-B7FB-9829DC58335B}']
    function GetLog: TStrings;
    function GetLoggingEnabled: Boolean;
    procedure SetLoggingEnabled(const Value: Boolean);

    property Log : TStrings
      read GetLog;

    property LoggingEnabled : Boolean
      read GetLoggingEnabled write SetLoggingEnabled;
  end;

  IUserData = interface
  ['{441FE01D-768F-4B56-8AD3-0B292DB9C0E2}']
    function GetUserID: string;
    function GetUserName: string;

    property UserID : string
      read GetUserID;

    property UserName : string
      read GetUserName;
  end;

  ILocale = interface
  ['{52115E15-3D78-4370-A871-D071479ED8FE}']
    function GetLanguageCode: string;

    property LanguageCode : string
      read GetLanguageCode;
  end;

{ TODO : - ChangeCount property? }

{ Common interfaces for ts applications.
  These interfaces make the form modules and the datamodules loosely coupled. }

  TAfterUpdateDataEvent = procedure (
    ASource     : IData;
    AUpdateKind : TUpdateKind
  ) of object;

  IDataViewSettings = interface
  ['{5820CB42-DF7E-4DB9-8BDE-F13E9F7C0B2F}']
    function GetDataTypeColor(Index: TDataType): TColor;
    function GetFieldTypeColor(Index: TFieldType): TColor;
    procedure SetDataTypeColor(Index: TDataType; const Value: TColor);
    function GetGridCellColoring: Boolean;
    procedure SetGridCellColoring(const Value: Boolean);

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;
    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring;
  end;

  IDataView = interface
  ['{66617CAF-874A-4637-878B-93B8B73C5129}']
    procedure UpdateView;
//    procedure BeforeExecute;
//    procedure AfterExecute;

    function GetName: string;
    function GetGridType: string;
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);

    property Name: string
      read GetName;

    property GridType: string
      read GetGridType;
  end;

  IData = interface
  ['{3DFA4682-620B-406F-BE20-F9C04719965B}']
    function GetDataSet : TDataSet;
    function GetRecordCount : Integer;
    function GetExecuted: Boolean;
    function GetActive: Boolean;
    function GetConnection: IConnection;
    procedure SetExecuted(const Value: Boolean);
    function GetMaxRecords: Integer;
    procedure SetMaxRecords(const Value: Integer);
    function GetParams: TSQLParams;
    function GetDisplayValues : TtsKeyValues;
    function GetDisplayLabels : TtsKeyValues;
    function GetPacketRecords: Integer;
    procedure SetPacketRecords(const Value: Integer);
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetProviderMode: Boolean;
    procedure SetProviderMode(const Value: Boolean);
    function GetFetchOnDemand: Boolean;
    procedure SetFetchOnDemand(const Value: Boolean);

    procedure Execute;

    procedure RefreshRecord(ADataSet: TCustomClientDataSet);

    procedure RegisterDataView(ADataView: IDataView);
    procedure UnregisterDataView(ADataView: IDataView);
    procedure UpdateDataViews;

    function IsLookupField(const AFieldName: string): Boolean;
    function IsCheckBoxField(const AFieldName: string): Boolean;
    function IsRequiredField(const AFieldName: string): Boolean;

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

    property Params: TSQLParams
      read GetParams;

    property RecordCount: Integer
      read GetRecordCount;

    property MaxRecords: Integer
      read GetMaxRecords write SetMaxRecords;

    property PacketRecords: Integer
      read GetPacketRecords write SetPacketRecords;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;

    property DisplayLabels: TtsKeyValues
      read GetDisplayLabels;

    property DisplayValues: TtsKeyValues
      read GetDisplayValues;

    property ProviderMode: Boolean
      read GetProviderMode write SetProviderMode;

    property Connection: IConnection
      read GetConnection;
  end;

  INativeDataSet = interface
  ['{875C8E14-93BF-4508-8830-D1873789C78D}']
    function GetDataSet: TDataSet;

    procedure BeforeExecute;
    procedure AfterExecute;

    procedure UpdateProviderMode(var AEnabled: Boolean);

    property DataSet: TDataSet
      read GetDataSet;
  end;

  IUpdatable = interface
  ['{8343E0B7-9AB1-4F84-85EE-705D83AAD110}']
    function GetKeyName: string;
    procedure SetKeyName(const Value: string);
    function GetTableName : string;
    procedure SetTableName(const Value: string);
    procedure SetOnAfterUpdateData(const Value : TAfterUpdateDataEvent);
    function GetOnafterUpdateData : TAfterUpdateDataEvent;

    procedure BeginUpdate;
    procedure EndUpdate;
    function Post: Boolean;

    property KeyName : string
      read GetKeyName write SetKeyName;

    property TableName : string
      read GetTableName write SetTableName;

    property OnAfterUpdateData : TAfterUpdateDataEvent
      read GetOnAfterUpdateData write SetOnAfterUpdateData;
  end;

  IDataSelection = interface
  ['{0E93F473-9BED-459B-AFF6-7FCCEB921991}']
    function GetSelectionDataSet : TDataSet;
    function GetSelectedRecords: TtsKeyValues;
    function GetMaxSelectionCount: Integer;
    procedure SetMaxSelectionCount(const Value: Integer);

    procedure ResetSelectionDataSet;
    procedure PrepareSelectionDataSet;
    procedure Update(const AFieldName  : string;
                     const AFieldValue : Variant);

    property SelectionDataSet : TDataSet
      read GetSelectionDataSet;

    property SelectedRecords : TtsKeyValues
      read GetSelectedRecords;

    property MaxSelectionCount : Integer
      read GetMaxSelectionCount write SetMaxSelectionCount;
  end;
(*
type
  IDataReport = interface
  ['{E59E2ED1-FCAB-4AD4-BD4C-8FC0A3DC25B6}']
    procedure PrintReport;
    procedure PreviewReport;
    procedure DesignReport;
    procedure LayoutReport;
    procedure ExportReport(      AExportFilter : TExportFilter;
                           const AFileName     : string = '');
    procedure EditProperties;

    function GetReportTitle: string;
    procedure SetReportTitle(const Value: string);
    function GetPrepared: Boolean;
    procedure SetPrepared(const Value: Boolean);

    property ReportTitle : string
      read GetReportTitle write SetReportTitle;

    property Prepared : Boolean
      read GetPrepared write SetPrepared;
  end;
  *)

  { currently targeted at TDBGridView, should be more generic... }
  IListSettings = interface
  ['{46C83D0D-135A-48E2-AA6C-72E79EDB21B9}']
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    function GetColumns : TtsListColumns;
    function GetDBGridView: TDBGridView;
    procedure SetDBGridView(const Value: TDBGridView);
    function GetAutoSizeColumns: Boolean;
    procedure SetAutoSizeColumns(const Value: Boolean);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetGridFontSize: Integer;
    procedure SetGridFontSize(const Value: Integer);

    procedure AssignToGridView(AGridView : TDBGridView);
    procedure AssignFromGridView(AGridView : TDBGridView);

    procedure AssignToDataSet(ADataSet : TDataSet);
    procedure AssignFromDataSet(ADataSet: TDataSet);

//    procedure AssignToReport(AReport : TtsFRListReport);
//    procedure AssignFromReport(AReport : TtsFRListReport);

    procedure Save;
    function Load: Boolean;

    property Columns : TtsListColumns
      read GetColumns;

    property AutoSizeColumns : Boolean
      read GetAutoSizeColumns write SetAutoSizeColumns;

    property Modified: Boolean
      read GetModified write SetModified;

    property DBGridView : TDBGridView
      read GetDBGridView write SetDBGridView;

    property FileName : string
      read GetFileName write SetFileName;

    property GridFontSize : Integer
      read GetGridFontSize write SetGridFontSize;
  end;

type
  IDataExport = interface
  ['{1B5E7994-8F6E-4BA7-BC10-AAC289BCE750}']
    procedure ExportExcel;
    procedure ExportText;
  end;

  { Fetch table metadata. }
  IGetTables = interface
  ['{BD71302D-34D4-4DD2-8FBB-BEBB075DE16A}']
    function GetTableDataSet : TDataSet;
  end;

  IMetaData = interface
  ['{A5921438-2613-47DF-832E-0DF33A8DC567}']
    procedure GetSchemaNames(AList : TStrings);
    procedure GetTableNames(AList: TStrings; const ASchemaName: string = '');
    procedure GetFieldNames(AList: TStrings; const ATableName: string;
      const ASchemaName: string = '');
  end;

implementation

end.


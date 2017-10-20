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

unit DataGrabber.Interfaces;

interface

uses
  System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Menus, Vcl.Forms, Vcl.ActnList,
  Data.DB,

  Spring.Collections,

  DataGrabber.ConnectionProfiles, DataGrabber.FormSettings,

  BCEditor.Editor.Base, BCEditor.Editor,

  ts.Classes.ConnectionSettings, ts.Interfaces;

const
  DEFAULT_DATATYPE_COLORS : array [TDataType] of TColor = (
    $00DFDFDF,
    $00E1FFE1, // Green
    $00DFDFFF, // Red
    $00DFDFFF,
    $00FFEBD7, // Blue
    $00FFEBD7,
    $00FFEBD7,
    clSilver
  );
  WHERE_IN = 'where' + #13#10 + '  %s in (%s)';

type
  TConnectionViewList = TInterfaceList;
  IEditorView = interface;

  IDGDataView = interface(IDataView)
    ['{B88F97B2-35BA-42A3-A35A-8122604E482B}']
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetRecordCount: Integer;
    function GetConstantColumnsVisible: Boolean;
    procedure SetConstantColumnsVisible(const Value: Boolean);
    function GetEmptyColumnsVisible: Boolean;
    procedure SetEmptyColumnsVisible(const Value: Boolean);
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);

    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;
    function SelectionToFields(AQuoteItems: Boolean = True): string;
    procedure AutoSizeColumns;
    procedure Copy;
    procedure HideSelectedColumns;
    procedure ShowAllColumns;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Inspect;

    procedure AssignParent(AParent: TWinControl);

    property Data: IData
      read GetData write SetData;

    property RecordCount: Integer
      read GetRecordCount;

    property ConstantColumnsVisible: Boolean
      read GetConstantColumnsVisible write SetConstantColumnsVisible;

    property EmptyColumnsVisible: Boolean
      read GetEmptyColumnsVisible write SetEmptyColumnsVisible;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

  IDGSettings = interface
  ['{C6E48393-6FBA-451B-A565-921F11E433F0}']
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

    property ConnectionProfiles: TConnectionProfiles
      read GetConnectionProfiles write SetConnectionProfiles;

    property ProviderMode: Boolean
      read GetProviderMode write SetProviderMode;

    property PacketRecords: Integer
      read GetPacketRecords write SetPacketRecords;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring;

    property ConnectionString: string
      read GetConnectionString write SetConnectionString;

//    property SQLTemplates: TSQLTemplates
//      read FSQLTemplates write FSQLTemplates;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

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

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

    procedure Load;
    procedure Save;
  end;

  IConnectionViews = interface
  ['{5D11A4DE-8D5F-41A1-9B98-A5EEAB1EE17C}']
  end;

  IConnectionView = interface
  ['{52F9CB1D-68C8-4E74-B3B7-0A9DDF93A818}']
    procedure Copy;
    procedure ApplySettings;

    function GetForm: TForm;
    function GetActiveConnectionProfile: TConnectionProfile;
    function GetEditorView: IEditorView;

    property EditorView: IEditorView
      read GetEditorView;

    property Form: TForm
      read GetForm;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;
  end;

  IConnectionViewManager = interface
  ['{E71C1D68-F201-4A95-9802-1D5B77445FEC}']
    function GetActiveConnectionView: IConnectionView;
    procedure SetActiveConnectionView(const Value: IConnectionView);
    function GetSettings: IDGSettings;
    function GetActiveDataView: IDGDataView;
    function GetActiveData: IData;
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;
    function GetConnectionViewPopupMenu: TPopupMenu;

    function AddConnectionView: IConnectionView;
    procedure UpdateActions;

    property ActiveConnectionView: IConnectionView
      read GetActiveConnectionView write SetActiveConnectionView;

    property ActiveDataView: IDGDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

    property Settings: IDGSettings
      read GetSettings;

    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ConnectionViewPopupMenu: TPopupMenu
      read GetConnectionViewPopupMenu;

  end;

  IEditorView = interface
  ['{E5E24E2A-AAFB-46EB-8E0F-E9BAC2E114FB}']
    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEditorFocused: Boolean;
//    function GetOnStatusChange: TStatusChangeEvent;
//    procedure SetOnStatusChange(const Value: TStatusChangeEvent);

    procedure FillCompletionLists(ATables, AAttributes : TStrings);
    procedure CopyToClipboard;
    procedure SetFocus;

    property EditorFocused: Boolean
      read GetEditorFocused;

    property Color: TColor
      read GetColor write SetColor;

    property Text: string
      read GetText write SetText;

//    property OnStatusChange: TStatusChangeEvent
//      read GetOnStatusChange write SetOnStatusChange;
  end;

  ISelection = interface
  ['{5E7F286D-7975-411F-9559-49616F1445B8}']

  end;

  IGroupable = interface
  ['{54AE6150-862B-4F69-A877-7D72B88C4FB4}']
    procedure GroupBySelectedColumns;
  end;

  IMergable = interface
  ['{713C0738-96A5-4A98-B5E4-71A1C96B0D88}']
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    procedure MergeAllColumnCells(AActive: Boolean);

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;
  end;

  IFieldLists = interface
  ['{DB8D457B-101A-4994-9CBB-CAB24EE27ECF}']
    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;

    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;
  end;

  IFieldVisiblity = interface
  ['{E7D8A634-78EE-442F-8098-155D0C0763B3}']
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFieldsVisible: Boolean;
    function GetShowFavoriteFieldsOnly: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    procedure SetShowFavoriteFieldsOnly(const Value: Boolean);

    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;
  end;

implementation

end.

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

unit DataGrabber.Interfaces;

{ Application interfaces }

interface

uses
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Menus, Vcl.Forms, Vcl.ActnList,
  Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Comp.DataSet,

  Spring, Spring.Collections,

  DataGrabber.ConnectionProfiles, DataGrabber.FormSettings,
  DataGrabber.ConnectionSettings;

type
// TODO: dtMemo?
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

  TResultDisplayLayout = (
    Tabbed,
    Horizontal,
    Vertical
  );

type
  IEditorView = interface;
  IData       = interface;

  IResultSet = interface
  ['{2A65FFAE-DA27-4088-B9E5-EB055A0E46A0}']
    {$REGION 'property access methods'}
    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;
    function GetHiddenFields: IList<TField>;
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFieldsVisible: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    function GetDataSet: TFDDataSet;
    function GetData: IData;
    {$ENDREGION}

    function ShowAllFields: Boolean;
    procedure UpdateFieldLists;

    property Data: IData
      read GetData;

    property DataSet: TFDDataSet
      read GetDataSet;

    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;

    property HiddenFields: IList<TField>
      read GetHiddenFields;

    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;
  end;

  IData = interface
  ['{0E8958C3-CECD-4E3F-A990-B73635E50F26}']
    {$REGION 'property access methods'}
    function GetDataSet : TFDDataSet;
    function GetRecordCount : Integer;
    function GetActive: Boolean;
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetConnectionSettings: TConnectionSettings;
    function GetConnection: TFDConnection;
    function GetItem(AIndex: Integer): IResultSet;
    function GetDataSetCount: Integer;
    function GetOnAfterExecute: IEvent<TNotifyEvent>;
    function GetOnBeforeExecute: IEvent<TNotifyEvent>;
    function GetMultipleResultSets: Boolean;
    procedure SetMultipleResultSets(const Value: Boolean);
    function GetElapsedTime: TTimeSpan;
    function GetFieldListsUpdated: Boolean;
    function GetDataEditMode: Boolean;
    procedure SetDataEditMode(const Value: Boolean);
    function GetResultSet: IResultSet;
    {$ENDREGION}

    procedure Execute;

    procedure HideField(
      ADataSet         : TDataSet;
      const AFieldName : string
    );
    procedure SaveToFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;
    procedure SaveToFile(
      ADataSet        : TDataSet;
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;

    procedure LoadFromFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    ); overload;
    procedure LoadFromFile(
      ADataSet        : TDataSet;
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

    property SQL: string
      read GetSQL write SetSQL;

    property ResultSet: IResultSet
      read GetResultSet;

    property Connection: TFDConnection
      read GetConnection;

    property Active: Boolean
      read GetActive;

    property CanModify: Boolean
      read GetCanModify;

    property DataEditMode: Boolean
      read GetDataEditMode write SetDataEditMode;

    property RecordCount: Integer
      read GetRecordCount;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property ElapsedTime: TTimeSpan
      read GetElapsedTime;

    property Items[AIndex: Integer]: IResultSet
      read GetItem; default;

    property DataSetCount: Integer
      read GetDataSetCount;

    property MultipleResultSets: Boolean
      read GetMultipleResultSets write SetMultipleResultSets;

    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;

    property OnBeforeExecute: IEvent<TNotifyEvent>
      read GetOnBeforeExecute;
  end;

  IDataViewSettings = interface
  ['{62EF3A18-73C5-4FAA-BBA1-D203AD028F38}']
    {$REGION 'property access methods'}
    function GetDataTypeColor(Index: TDataType): TColor;
    function GetFieldTypeColor(Index: TFieldType): TColor;
    procedure SetDataTypeColor(Index: TDataType; const Value: TColor);
    function GetGridCellColoring: Boolean;
    procedure SetGridCellColoring(const Value: Boolean);
    function GetShowHorizontalGridLines: Boolean;
    function GetShowVerticalGridLines: Boolean;
    procedure SetShowHorizontalGridLines(const Value: Boolean);
    procedure SetShowVerticalGridLines(const Value: Boolean);
    function GetGroupByBoxVisible: Boolean;
    procedure SetGroupByBoxVisible(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    function GetGridFont: TFont;
    procedure SetGridFont(const Value: TFont);
    {$ENDREGION}

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property GridFont: TFont
      read GetGridFont write SetGridFont;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring;

    property ShowHorizontalGridLines: Boolean
      read GetShowHorizontalGridLines write SetShowHorizontalGridLines;

    property ShowVerticalGridLines: Boolean
      read GetShowVerticalGridLines write SetShowVerticalGridLines;

    property GroupByBoxVisible: Boolean
      read GetGroupByBoxVisible write SetGroupByBoxVisible;

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

  IDataView = interface
  ['{50D61670-EBD1-4A52-8915-C8006053E2B2}']
    {$REGION 'property access methods'}
    function GetName: string;
    function GetGridType: string;
    function GetSettings: IDataViewSettings;
    function GetData: IData;
    function GetRecordCount: Integer;
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetDataSet: TDataSet;
    function GetResultSet: IResultSet;
    {$ENDREGION}

    procedure AssignParent(AParent: TWinControl);
    procedure UpdateView;
    procedure HideSelectedColumns;
    function IsActiveDataView: Boolean;
    procedure Inspect;
    procedure AutoSizeColumns;
    procedure Copy;
    procedure Close;

    procedure BeginUpdate;
    procedure EndUpdate;

    function ResultsToWikiTable(AIncludeHeader: Boolean = False): string;
    function ResultsToTextTable(AIncludeHeader: Boolean = False): string;

    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;
    function SelectionToFields(AQuoteItems: Boolean = True): string;
    // TODO: ABreakItems (insert new lines after comma)

    property Name: string
      read GetName;

    property GridType: string
      read GetGridType;

    property DataSet: TDataSet
      read GetDataSet;

    property ResultSet: IResultSet
      read GetResultSet;

    property RecordCount: Integer
      read GetRecordCount;

    property Settings: IDataViewSettings
      read GetSettings;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

  ISettings = interface
  ['{C6E48393-6FBA-451B-A565-921F11E433F0}']
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
    function GetGroupByBoxVisible: Boolean;
    procedure SetGroupByBoxVisible(const Value: Boolean);
    function GetOnChanged: IEvent<TNotifyEvent>;
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    function GetEditorFont: TFont;
    procedure SetEditorFont(const Value: TFont);
    function GetGridFont: TFont;
    procedure SetGridFont(const Value: TFont);
    function GetEmitLogMessages: Boolean;
    procedure SetEmitLogMessages(const Value: Boolean);
    {$ENDREGION}

    procedure Load;
    procedure Save;

    property FileName: string
      read GetFileName write SetFileName;

    property ConnectionProfiles: TConnectionProfiles
      read GetConnectionProfiles write SetConnectionProfiles;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property GridFont: TFont
      read GetGridFont write SetGridFont;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring;

    property FormSettings: TFormSettings
      read GetFormSettings write SetFormSettings;

    property GridType: string
      read GetGridType write SetGridType;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings write SetConnectionSettings;

    property DefaultConnectionProfile: string
      read GetDefaultConnectionProfile write SetDefaultConnectionProfile;

    property DataInspectorVisible: Boolean
      read GetDataInspectorVisible write SetDataInspectorVisible;

    property EmitLogMessages: Boolean
      read GetEmitLogMessages write SetEmitLogMessages;

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

    property GroupByBoxVisible: Boolean
      read GetGroupByBoxVisible write SetGroupByBoxVisible;

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;

    property ShowHorizontalGridLines: Boolean
      read GetShowHorizontalGridLines write SetShowHorizontalGridLines;

    property ShowVerticalGridLines: Boolean
      read GetShowVerticalGridLines write SetShowVerticalGridLines;

    property ResultDisplayLayout: TResultDisplayLayout
      read GetResultDisplayLayout write SetResultDisplayLayout;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;
  end;

  IConnectionView = interface
  ['{52F9CB1D-68C8-4E74-B3B7-0A9DDF93A818}']
    {$REGION 'property access methods'}
    function GetForm: TForm;
    function GetActiveConnectionProfile: TConnectionProfile;
    function GetActiveDataView: IDataView;
    function GetEditorView: IEditorView;
    function GetData: IData;
    {$ENDREGION}

    procedure Copy;
    procedure ApplySettings;

    function ExportAsWiki: string;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property Data: IData
      read GetData;

    property EditorView: IEditorView
      read GetEditorView;

    property Form: TForm
      read GetForm;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;
  end;

  IConnectionViewManager = interface
  ['{E71C1D68-F201-4A95-9802-1D5B77445FEC}']
    {$REGION 'property access methods'}
    function GetActiveConnectionView: IConnectionView;
    procedure SetActiveConnectionView(const Value: IConnectionView);
    function GetSettings: ISettings;
    function GetActiveDataView: IDataView;
    function GetActiveData: IData;
    function GetActionList: TActionList;
    function GetAction(AName: string): TCustomAction;
    function GetConnectionViewPopupMenu: TPopupMenu;
    function GetDefaultConnectionProfile: TConnectionProfile;
    function GetItem(AIndex: Integer): IConnectionView;
    function GetCount: Integer;
    {$ENDREGION}

    function AddConnectionView: IConnectionView;
    function DeleteConnectionView(AIndex: Integer): Boolean; overload;
    function DeleteConnectionView(AConnectionView: IConnectionView): Boolean; overload;

    procedure UpdateActions;

    property ActiveConnectionView: IConnectionView
      read GetActiveConnectionView write SetActiveConnectionView;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

    property Settings: ISettings
      read GetSettings;

    property ActionList: TActionList
      read GetActionList;

    property DefaultConnectionProfile: TConnectionProfile
      read GetDefaultConnectionProfile;

    property Actions[AName: string]: TCustomAction
      read GetAction;

    property Items[AIndex: Integer]: IConnectionView
      read GetItem; default;

    property ConnectionViewPopupMenu: TPopupMenu
      read GetConnectionViewPopupMenu;

    property Count: Integer
      read GetCount;
  end;

  IEditorView = interface
  ['{9E365515-2202-458B-98C2-6995344E81DD}']
    {$REGION 'property access methods'}
    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEditorFocused: Boolean;
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    {$ENDREGION}

    procedure AssignParent(AParent: TWinControl);
    procedure FillCompletionLists(ATables, AAttributes : TStrings);
    procedure CopyToClipboard;
    procedure SetFocus;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property EditorFocused: Boolean
      read GetEditorFocused;

    property Color: TColor
      read GetColor write SetColor;

    property Text: string
      read GetText write SetText;
  end;

  IGroupable = interface
  ['{C3E37BA9-5FCF-4AC5-A747-495F0B13E8E4}']
    procedure GroupBySelectedColumns;
    procedure ClearGrouping;
    procedure ExpandAll;
    procedure CollapseAll;
  end;

  IMergable = interface
  ['{68285D0A-5DE8-4782-BC1F-EF9D4A7A0C7E}']
    {$REGION 'property access methods'}
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    procedure MergeAllColumnCells(AActive: Boolean);
    {$ENDREGION}

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;
  end;

implementation

end.

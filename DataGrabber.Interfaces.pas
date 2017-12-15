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
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Menus, Vcl.Forms, Vcl.ActnList,
  Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Intf,

  Spring, Spring.Collections,

  BCEditor.Editor.Base,

  DDuce.DynamicRecord,

  DataGrabber.ConnectionProfiles, DataGrabber.FormSettings,
  DataGrabber.ConnectionSettings;

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
  IEditorView = interface;

  IData = interface
  ['{0E8958C3-CECD-4E3F-A990-B73635E50F26}']
    {$REGION 'property access methods'}
    function GetDataSet : TDataSet;
    function GetRecordCount : Integer;
    function GetExecuted: Boolean;
    function GetActive: Boolean;
    procedure SetExecuted(const Value: Boolean);
    function GetSQL: string;
    procedure SetSQL(const Value: string);
    function GetCanModify: Boolean;
    function GetConnectionSettings: TConnectionSettings;
    function GetConnection: TFDConnection;
    function GetItem(AIndex: Integer): TFDMemTable;
    function GetDataSetCount: Integer;
    function GetOnAfterExecute: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure Execute;

    property SQL: string
      read GetSQL write SetSQL;

    property DataSet: TDataSet
      read GetDataSet;

    property Connection: TFDConnection
      read GetConnection;

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

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property DataSetCount: Integer
      read GetDataSetCount;

    property OnAfterExecute: IEvent<TNotifyEvent>
      read GetOnAfterExecute;
  end;

  IDataPersistable = interface
  ['{18B39929-7F40-4ED0-BF93-C6191CCCFF1E}']
    procedure SaveToFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    );

    procedure LoadFromFile(
      const AFileName : string = '';
      AFormat         : TFDStorageFormat = sfAuto
    );
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
    {$ENDREGION}

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property GridCellColoring: Boolean
      read GetGridCellColoring write SetGridCellColoring;

    property ShowHorizontalGridLines: Boolean
      read GetShowHorizontalGridLines write SetShowHorizontalGridLines;

    property ShowVerticalGridLines: Boolean
      read GetShowVerticalGridLines write SetShowVerticalGridLines;
  end;

  IDataView = interface
  ['{50D61670-EBD1-4A52-8915-C8006053E2B2}']
    {$REGION 'property access methods'}
    function GetName: string;
    function GetGridType: string;
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetRecordCount: Integer;
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetDataSet: TDataSet;
    procedure SetDataSet(const Value: TDataSet);
    {$ENDREGION}

    procedure UpdateView;

    property Name: string
      read GetName;

    property GridType: string
      read GetGridType;

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

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Inspect;

    procedure AssignParent(AParent: TWinControl);

    property DataSet: TDataSet
      read GetDataSet write SetDataSet;

    property Data: IData
      read GetData write SetData;

    property RecordCount: Integer
      read GetRecordCount;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

  IDisplayData = interface
  ['{8BC40D7C-00EC-469D-B8A1-675A52A8F2BF}']
    {$REGION 'property access methods'}
    function GetDisplayValues : IDynamicRecord;
    function GetDisplayLabels : IDynamicRecord;
    {$ENDREGION}

    function IsLookupField(const AFieldName: string): Boolean;
    function IsCheckBoxField(const AFieldName: string): Boolean;
    function IsRequiredField(const AFieldName: string): Boolean;

    property DisplayLabels: IDynamicRecord
      read GetDisplayLabels;

    property DisplayValues: IDynamicRecord
      read GetDisplayValues;
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
    function GetRepositoryVisible: Boolean;
    procedure SetConnectionSettings(const Value: TConnectionSettings);
    procedure SetDataInspectorVisible(const Value: Boolean);
    procedure SetDefaultConnectionProfile(const Value: string);
    procedure SetRepositoryVisible(const Value: Boolean);
    function GetGridType: string;
    procedure SetGridType(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetShowHorizontalGridLines: Boolean;
    function GetShowVerticalGridLines: Boolean;
    procedure SetShowHorizontalGridLines(const Value: Boolean);
    procedure SetShowVerticalGridLines(const Value: Boolean);
    {$ENDREGION}

    procedure Load;
    procedure Save;

    property FileName: string
      read GetFileName write SetFileName;

    property ConnectionProfiles: TConnectionProfiles
      read GetConnectionProfiles write SetConnectionProfiles;

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

    property RepositoryVisible: Boolean
      read GetRepositoryVisible write SetRepositoryVisible;

    property DataInspectorVisible: Boolean
      read GetDataInspectorVisible write SetDataInspectorVisible;

    property FieldTypeColors[Index: TFieldType]: TColor
      read GetFieldTypeColor;

    property DataTypeColors[Index: TDataType]: TColor
      read GetDataTypeColor write SetDataTypeColor;

    property ShowHorizontalGridLines: Boolean
      read GetShowHorizontalGridLines write SetShowHorizontalGridLines;

    property ShowVerticalGridLines: Boolean
      read GetShowVerticalGridLines write SetShowVerticalGridLines;
  end;

  IConnectionView = interface
  ['{52F9CB1D-68C8-4E74-B3B7-0A9DDF93A818}']
    procedure Copy;
    procedure ApplySettings;

    function GetForm: TForm;
    function GetActiveConnectionProfile: TConnectionProfile;
    function GetActiveDataView: IDataView;
    function GetEditorView: IEditorView;
    function GetActiveData: IData;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

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
    {$ENDREGION}

    procedure FillCompletionLists(ATables, AAttributes : TStrings);
    procedure CopyToClipboard;
    procedure SetFocus;

    property EditorFocused: Boolean
      read GetEditorFocused;

    property Color: TColor
      read GetColor write SetColor;

    property Text: string
      read GetText write SetText;
  end;

  ISelection = interface
  ['{121A5480-7187-4A0C-BCDD-019C6FC635A4}']
  end;

  IGroupable = interface
  ['{C3E37BA9-5FCF-4AC5-A747-495F0B13E8E4}']
    procedure GroupBySelectedColumns;
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

  IFieldLists = interface
  ['{0AC7FBDA-CAF3-49B2-990D-7053D3188D51}']
    {$REGION 'property access methods'}
    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;
    {$ENDREGION}

    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;
  end;

  IFieldVisiblity = interface
  ['{DEAE8EF5-FE14-4743-8FC7-1A12A41303E2}']
    {$REGION 'property access methods'}
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFieldsVisible: Boolean;
    function GetShowFavoriteFieldsOnly: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    procedure SetShowFavoriteFieldsOnly(const Value: Boolean);
    {$ENDREGION}

    function ShowAllFields: Boolean;

    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;
  end;

implementation

end.

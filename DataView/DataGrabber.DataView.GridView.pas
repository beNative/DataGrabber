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

unit DataGrabber.DataView.GridView;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  Data.DB,

  Spring.Collections,

  DDuce.Components.GridView, DDuce.Components.DBGridView,

  ts.Interfaces, ts.Components.DBGridViewSort,

  DataGrabber.Interfaces;

type
  TfrmGridView = class(TForm, IDataView, IDGDataView)
    dscMain: TDataSource;

    procedure dscMainStateChange(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);

  strict private
    FSettings               : IDataViewSettings;
    FData                   : IData;
    FGrid                   : TDBGridView;
    FGridSort               : TtsDBGridViewSort;

    {$REGION 'property access methods'}
    function GetName: string;
    function GetDataSet: TDataSet;
    function GetRecordCount: Integer;
    function GetConstantColumnsVisible: Boolean;
    procedure SetConstantColumnsVisible(const Value: Boolean);
    function GetEmptyColumnsVisible: Boolean;
    procedure SetEmptyColumnsVisible(const Value: Boolean);
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);
    function GetPopupMenu: TPopupMenu; reintroduce;
    procedure SetPopupMenu(const Value: TPopupMenu);
    {$ENDREGION}

    procedure InitializeGridColumns;
    procedure InitializeGridColumn(AGridColumn: TDBGridColumn);

    procedure ApplyGridSettings;

    // shortcut methods
    function IsLookupField(const AFieldName: string) : Boolean;
    function IsCheckBoxField(const AFieldName: string) : Boolean;
    function IsCellReadOnly(const ACell: TGridCell) : Boolean;

    procedure FGridKeyPress(
      Sender  : TObject;
      var Key : Char
    );
    procedure FGridChanging(
      Sender       : TObject;
      var Cell     : TGridCell;
      var Selected : Boolean
    );
    procedure FGridRowMultiSelect(
      Sender     : TObject;
      Row        : Integer;
      var Select : Boolean
    );
    procedure FGridClearMultiSelect(Sender : TObject);
    procedure FGridCheckClick(
      Sender : TObject;
      Cell   : TGridCell
    );
    procedure FGridGetCheckState(
      Sender         : TObject;
      Cell           : TGridCell;
      var CheckState : TCheckBoxState
    );
    procedure FGridEditCanModify(
      Sender        : TObject;
      Cell          : TGridCell;
      var CanModify : Boolean
    );
    procedure FGridCellAcceptCursor(
      Sender     : TObject;
      Cell       : TGridCell;
      var Accept : Boolean
    );
    procedure FGridGetCellReadOnly(
      Sender           : TObject;
      Cell             : TGridCell;
      var CellReadOnly : Boolean
    );
    procedure FGridGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FGridGetCellColors(
      Sender : TObject;
      Cell   : TGridCell;
      Canvas : TCanvas
    );
    procedure FGridMouseWheelUp(
      Sender      : TObject;
      Shift       : TShiftState;
      MousePos    : TPoint;
      var Handled : Boolean
    );
    procedure FGridMouseWheelDown(
      Sender      : TObject;
      Shift       : TShiftState;
      MousePos    : TPoint;
      var Handled : Boolean
    );

  private
    function GetGridType: string;

    procedure UpdateMultiSelection(
      const AFieldName  : string;
      const AFieldValue : Variant
    );
  public
    constructor Create; reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AssignParent(AParent: TWinControl);
    procedure HideSelectedColumns;
    procedure AutoSizeColumns;
    procedure Copy;
    procedure Inspect;

    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
    function SelectionToDelimitedTable(ADelimiter : string = #9;
      AIncludeHeader: Boolean = True): string;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;
    function SelectionToFields(AQuoteItems: Boolean = True): string;

    procedure UpdateView;

    property DataSet: TDataSet
       read GetDataSet;

    property Data: IData
      read GetData write SetData;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property RecordCount: Integer
       read GetRecordCount;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property GridType: string
      read GetGridType;
  end;

implementation

{$R *.dfm}

uses
  System.Math,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Logger;

{$REGION 'construction and destruction'}
procedure TfrmGridView.AfterConstruction;
begin
  inherited AfterConstruction;
//  FConstCols := TCollections.CreateObjectList<TDBGridColumn>(False);
//  FEmptyCols := TCollections.CreateObjectList<TDBGridColumn>(False);
//  FConstantColumnsVisible := True;
//  FEmptyColumnsVisible    := True;

  FGrid                          := TDBGridView.Create(Self);
  FGrid.Align                    := alClient;
  FGrid.Parent                   := Self;
  FGrid.Visible                  := True;
  FGrid.CheckBoxes               := True;
  FGrid.DataSource               := dscMain;
  FGrid.DefaultLayout            := True;
  FGrid.DoubleBuffered           := True;
  FGrid.ColumnClick              := True;
  FGrid.EndEllipsis              := True;
  FGrid.Font.Name                := 'Callibri';
  FGrid.ShowCellTips             := True;
  FGrid.CheckStyle               := cs3D;
  FGrid.ColumnsFullDrag          := True;
  FGrid.GridLines                := True;
  FGrid.GridColor                := clSilver;
  FGrid.MultiSelect              := True;

  FGrid.Header.FullSynchronizing := True;
  FGrid.Header.Synchronized      := True;
  FGrid.Header.Font.Style        := [fsBold];
  FGrid.Header.GridColor         := True;
  FGrid.Header.AutoHeight        := True;
  FGrid.Header.Flat              := True;

  FGrid.Rows.AutoHeight          := True;
  FGrid.GridStyle                := [gsVertLine, gsHorzLine];
  FGrid.CursorKeys := [gkArrows, gkTabs, gkReturn, gkMouse, gkMouseWheel];

  FGrid.OnCellAcceptCursor  := FGridCellAcceptCursor;
  FGrid.OnGetCellReadOnly   := FGridGetCellReadOnly;
  FGrid.OnGetCellColors     := FGridGetCellColors;
  FGrid.OnGetCellText       := FGridGetCellText;

  FGrid.OnEditCanModify     := FGridEditCanModify;

  FGrid.OnKeyPress          := FGridKeyPress;
  FGrid.OnChanging          := FGridChanging;

  FGrid.OnCheckClick        := FGridCheckClick;
  FGrid.OnGetCheckState     := FGridGetCheckState;

  FGrid.OnMouseWheelUp      := FGridMouseWheelUp;
  FGrid.OnMouseWheelDown    := FGridMouseWheelDown;

  FGrid.OnRowMultiSelect    := FGridRowMultiSelect;
  FGrid.OnClearMultiSelect  := FGridClearMultiSelect;

  // the TtsDBGridViewSort component has to be created AFTER the event handler
  // assignments because the component remaps the event handlers, which is not
  // very clean...
  FGridSort            := TtsDBGridViewSort.Create(Self);
  FGridSort.DBGridView := FGrid;
  FGridSort.SortedColumnColorEnabled := True;
end;

constructor TfrmGridView.Create;
begin
  inherited Create(Application);
end;

procedure TfrmGridView.BeforeDestruction;
begin
  if Assigned(FData) then
    FData.UnRegisterDataView(Self);
//  FConstCols := nil;
//  FEmptyCols := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmGridView.GetEmptyColumnsVisible: Boolean;
begin
  //Result := FEmptyColumnsVisible;
end;

function TfrmGridView.GetGridType: string;
begin
  Result := 'GridView';
end;

procedure TfrmGridView.SetEmptyColumnsVisible(const Value: Boolean);
var
  C : TDBGridColumn;
begin
//  if Value <> EmptyColumnsVisible then
//  begin
//    FEmptyColumnsVisible := Value;
//    for C in FEmptyCols do
//      C.Visible := Value;
//  end;
end;

function TfrmGridView.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmGridView.GetPopupMenu: TPopupMenu;
begin
  Result := FGrid.PopupMenu;
end;

procedure TfrmGridView.SetPopupMenu(const Value: TPopupMenu);
begin
  FGrid.PopupMenu := Value;
end;

function TfrmGridView.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmGridView.SetSettings(const Value: IDataViewSettings);
begin
  FSettings := Value;
end;

function TfrmGridView.GetConstantColumnsVisible: Boolean;
begin
  //Result := FConstantColumnsVisible;
end;

procedure TfrmGridView.SetConstantColumnsVisible(const Value: Boolean);
var
  C : TDBGridColumn;
begin
//  if Value <> ConstantColumnsVisible then
//  begin
//    FConstantColumnsVisible := Value;
//    for C in FConstCols do
//      C.Visible := Value and C.Field.Visible;
//  end
end;

function TfrmGridView.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

function TfrmGridView.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmGridView.SetData(const Value: IData);
begin
  if Value <> Data then
  begin
    FData := Value;
    FData.RegisterDataView(Self);
    dscMain.DataSet := Data.DataSet;
    UpdateView;
  end;
end;

function TfrmGridView.GetRecordCount: Integer;
begin
  Result := Data.RecordCount;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmGridView.dscMainDataChange(Sender: TObject; Field: TField);
begin
//  if Assigned(dscMain.DataSet) and Assigned(Field) and
//     (dscMain.DataSet.State in dsEditModes) and
//     FGrid.MultiSelect and (FGrid.SelectedRows.Count > 0) then
//  begin
//    UpdateMultiSelection(Field.FieldName, Field.Value);
//  end;
//  FGrid.Refresh;
end;

procedure TfrmGridView.dscMainStateChange(Sender: TObject);
begin
  FGridSort.SortedFieldName := '';
  FGridSort.SortDirection := gsNone;
end;

procedure TfrmGridView.FGridCellAcceptCursor(Sender: TObject; Cell: TGridCell;
  var Accept: Boolean);
begin
//  Accept := Accept and (not FGrid.Columns[Cell.Col].ReadOnly) and
//    (not IsCellReadOnly(Cell));
end;

procedure TfrmGridView.FGridChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
begin
  if Assigned(DataSet) then
  begin
    if DataSet.State = dsEdit then
    begin
      DataSet.Post;
      AutoSizeColumns;
    end;
  end;
end;

procedure TfrmGridView.FGridCheckClick(Sender: TObject; Cell: TGridCell);
var
  S  : string;
  C  : TDBGridColumn;
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  C  := GV.Columns[Cell.Col];
  S  := C.FieldName;
  if C.AllowClick and IsCheckBoxField(S) then
  begin
    DataSet.Edit;
    if C.Field.IsNull then
       C.Field.Value := 1
    else
    begin
      if C.Field.Value = 0 then
        C.Field.Value := 1
      else
        C.Field.Value := 0
    end;
    if DataSet.State in dsEditModes then
      DataSet.Post;
  end;
end;

procedure TfrmGridView.FGridClearMultiSelect(Sender: TObject);
begin
  (Data as IDataSelection).SelectedRecords.Clear;
end;

procedure TfrmGridView.FGridEditCanModify(Sender: TObject; Cell: TGridCell;
  var CanModify: Boolean);
begin
  CanModify := CanModify and not IsCellReadOnly(Cell);
end;

procedure TfrmGridView.FGridGetCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
 var
  GV : TDBGridView;
  F  : TField;
  D  : Double;
begin
  if Settings.GridCellColoring then
  begin
    GV := Sender as TDBGridView;
    F  := GV.Columns[Cell.Col].Field;
    if Assigned(F) then
    begin
      if not GV.IsCellHighlighted(Cell) and (F is TNumericField) then
      begin
        D := F.AsFloat;
        if IsZero(D) then
          Canvas.Font.Color := clBlue
        else if D < 0 then
          Canvas.Font.Color := clRed;
      end;
      if not GV.IsCellHighlighted(Cell) then
      begin
        if F.AsString = '' then
        begin
          Canvas.Brush.Color := $00EFEFEF;
        end
        else
        begin
          Canvas.Brush.Color := Settings.FieldTypeColors[F.DataType];
        end;
      end;
    end;
  end;
end;

procedure TfrmGridView.FGridGetCellReadOnly(Sender: TObject; Cell: TGridCell;
  var CellReadOnly: Boolean);
begin
  CellReadOnly := CellReadOnly and IsCellReadOnly(Cell);
end;

procedure TfrmGridView.FGridGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if IsCheckBoxField(GV.Columns[Cell.Col].FieldName) then
    Value := '';
end;

procedure TfrmGridView.FGridGetCheckState(Sender: TObject; Cell: TGridCell;
  var CheckState: TCheckBoxState);
var
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if Assigned(GV.Columns) and
    IsCheckBoxField(GV.Columns[Cell.Col].FieldName) and
    not GV.Columns[Cell.Col].Field.IsNull and
    (GV.Columns[Cell.Col].Field.AsBoolean) then
    CheckState := cbChecked
  else
    CheckState := cbUnchecked;
end;

procedure TfrmGridView.FGridKeyPress(Sender: TObject; var Key: Char);
var
  GV   : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if GV.Editing and (Key = '.') and (GV.SelectedField is TFloatField) then
    Key := ',';
end;

procedure TfrmGridView.FGridMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if ssCtrl in Shift then
  begin
    GV.Font.Size := GV.Font.Size - 1;
    GV.AutoSizeCols;
    Handled := True;
  end
end;

procedure TfrmGridView.FGridMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if ssCtrl in Shift then
  begin
    GV.Font.Size := GV.Font.Size + 1;
    GV.AutoSizeCols;
    Handled := True;
  end
end;

procedure TfrmGridView.FGridRowMultiSelect(Sender: TObject; Row: Integer;
  var Select: Boolean);
//var
//  KV : TtsKeyValues;
//  N  : Integer;
begin
//  KV := (Data as IDataSelection).SelectedRecords;
//  if Select then
//    KV[IntToStr(DataSet.RecNo)] := DataSet[(Data as IUpdatable).KeyName]
//  else
//  begin
//    N := KV.IndexOf(IntToStr(DataSet.RecNo));
//    KV.Delete(N);
//  end;

//  if (FGrid.SelectedRows.Count > Pred(MaxSelectionCount)) and Select then
//  begin
//    ShowWarning(SSelectionIsLimited, [MaxSelectionCount]);
//    Abort;
//  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
function TfrmGridView.IsCellReadOnly(const ACell: TGridCell): Boolean;
begin
  Result := False;
end;

function TfrmGridView.IsCheckBoxField(const AFieldName: string): Boolean;
begin
  Result := Data.IsCheckBoxField(AFieldName);
end;

function TfrmGridView.IsLookupField(const AFieldName: string): Boolean;
begin
  Result := Data.IsLookupField(AFieldName);
end;

procedure TfrmGridView.InitializeGridColumn(AGridColumn: TDBGridColumn);
var
  S : string;
begin
  S := AGridColumn.FieldName;
  if IsLookupField(S) then
    AGridColumn.EditStyle := geEllipsis
  else if IsCheckBoxField(S) then
  begin
    AGridColumn.CheckKind      := gcCheckBox;
    AGridColumn.AllowEdit      := False;
    AGridColumn.CheckAlignment := taCenter;
    AGridColumn.MaxWidth       := 100;
  end
  else
  begin
    AGridColumn.MaxWidth := 800;
  end;
  AGridColumn.Visible := Assigned(AGridColumn.Field) and AGridColumn.Field.Visible;
end;

procedure TfrmGridView.InitializeGridColumns;
var
  I : Integer;
begin
  for I := 0 to FGrid.Columns.Count - 1 do
    InitializeGridColumn(FGrid.Columns[I]);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmGridView.ApplyGridSettings;
begin
  FGrid.GridLines := True;
  if FSettings.ShowHorizontalGridLines then
    FGrid.GridStyle := FGrid.GridStyle + [gsHorzLine]
  else
    FGrid.GridStyle := FGrid.GridStyle - [gsHorzLine];

  if FSettings.ShowVerticalGridLines then
    FGrid.GridStyle := FGrid.GridStyle + [gsVertLine]
  else
    FGrid.GridStyle := FGrid.GridStyle - [gsVertLine];
end;

procedure TfrmGridView.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

procedure TfrmGridView.AutoSizeColumns;
begin
  FGrid.AutoSizeCols;
end;

procedure TfrmGridView.Copy;
begin
// TODO
end;

procedure TfrmGridView.BeginUpdate;
begin
  FGrid.LockLayout;
end;

procedure TfrmGridView.EndUpdate;
begin
  FGrid.UnLockLayout(False);
end;

procedure TfrmGridView.HideSelectedColumns;
begin
// TODO
end;

procedure TfrmGridView.Inspect;
begin
  InspectComponent(FGrid);
end;

function TfrmGridView.SelectionToCommaText(AQuoteItems: Boolean): string;
begin
// TODO
end;

function TfrmGridView.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin
// TODO
end;

function TfrmGridView.SelectionToFields(AQuoteItems: Boolean): string;
begin
// TODO
end;

function TfrmGridView.SelectionToTextTable(AIncludeHeader: Boolean): string;
begin
// TODO
end;

function TfrmGridView.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin
// TODO
end;

procedure TfrmGridView.UpdateMultiSelection(const AFieldName: string;
  const AFieldValue: Variant);
begin
  BeginUpdate;
  try
    (Data as IDataSelection).Update(AFieldName, AFieldValue);
  finally
    EndUpdate;
  end;
end;

procedure TfrmGridView.UpdateView;
begin
  BeginUpdate;
  try
    dscMain.DataSet := Data.DataSet;
    if Assigned(DataSet) and DataSet.Active then
    begin
      ApplyGridSettings;
      InitializeGridColumns;
    end;
  finally
    EndUpdate;
  end;
  AutoSizeColumns; // needs to be done after all FGrid.UnLockLayout
end;
{$ENDREGION}

end.

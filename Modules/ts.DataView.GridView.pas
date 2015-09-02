unit ts.DataView.GridView;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Generics.Collections, StdCtrls,

  Ex_Grid, Ex_DBGrid,

  ts.Components.DBGridViewSort, ts.Interfaces, ts.DataView;

type
  TfrmGridView = class(TfrmDataView, IDataView)

    procedure dscMainStateChange(Sender: TObject);

  strict private
    FSettings               : IDataViewSettings;
    FData                   : IData;
    FGrid                   : TDBGridView;
    FGridSort               : TtsDBGridViewSort;
    FEmptyCols              : TObjectList<TDBGridColumn>;
    FConstCols              : TObjectList<TDBGridColumn>;
    FConstantColumnsVisible : Boolean;
    FEmptyColumnsVisible    : Boolean;

    function GetRecordCount: Integer;

    procedure InitializeGridColumns;
    procedure InitializeGridColumn(AGridColumn: TDBGridColumn);

    // shortcut methods
    function IsLookupField(const AFieldName : string) : Boolean;
    function IsCheckBoxField(const AFieldName : string) : Boolean;
    function IsCellReadOnly(const ACell : TGridCell) : Boolean;

    procedure grdKeyPress(Sender: TObject; var Key: Char);
    procedure grdChanging(Sender: TObject; var Cell: TGridCell;
      var Selected: Boolean);

    procedure grdRowMultiSelect(Sender: TObject; Row: Integer;
      var Select: Boolean);
    procedure grdClearMultiSelect(Sender: TObject);

    procedure grdCheckClick(Sender: TObject; Cell: TGridCell);
    procedure grdGetCheckState(Sender: TObject; Cell: TGridCell;
      var CheckState: TCheckBoxState);

    procedure grdEditCanModify(Sender: TObject; Cell: TGridCell;
      var CanModify: Boolean);

    procedure grdCellAcceptCursor(Sender: TObject; Cell: TGridCell;
      var Accept: Boolean);
    procedure grdGetCellReadOnly(Sender: TObject; Cell: TGridCell;
      var CellReadOnly: Boolean);
    procedure grdGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);

    procedure grdGetCellColors(Sender: TObject; Cell: TGridCell;
      Canvas: TCanvas);

    procedure grdMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure grdMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure UpdateColumnLists;

    procedure AssignParent(AParent: TWinControl);
    procedure HideSelectedColumns;
    procedure ShowAllColumns;
    procedure AutoSizeColumns;
    procedure Copy;
    procedure Inspect;

    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
    function SelectionToDelimitedTable(ADelimiter : string = #9;
      AIncludeHeader: Boolean = True): string;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;

    //procedure UpdateView; override;

    property RecordCount: Integer
       read GetRecordCount;
  end;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  Math,

  ts.Modules.ComponentInspector;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmGridView.AfterConstruction;
begin
  inherited;
  FConstCols := TObjectList<TDBGridColumn>.Create(False);
  FEmptyCols := TObjectList<TDBGridColumn>.Create(False);
  FConstantColumnsVisible := True;
  FEmptyColumnsVisible    := True;

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
  FGrid.Header.FullSynchronizing := True;
  FGrid.Header.Synchronized      := True;
  FGrid.Header.AutoHeight        := True;
  FGrid.Rows.AutoHeight          := True;
  FGrid.GridStyle                := [gsVertLine, gsHorzLine];
  FGrid.CursorKeys := [gkArrows, gkTabs, gkReturn, gkMouse, gkMouseWheel];

  FGrid.OnCellAcceptCursor  := grdCellAcceptCursor;
  FGrid.OnGetCellReadOnly   := grdGetCellReadOnly;
  FGrid.OnGetCellColors     := grdGetCellColors;
  FGrid.OnGetCellText       := grdGetCellText;

  FGrid.OnEditCanModify     := grdEditCanModify;

  FGrid.OnKeyPress          := grdKeyPress;
  FGrid.OnChanging          := grdChanging;

  FGrid.OnCheckClick        := grdCheckClick;
  FGrid.OnGetCheckState     := grdGetCheckState;

  FGrid.OnMouseWheelUp      := grdMouseWheelUp;
  FGrid.OnMouseWheelDown    := grdMouseWheelDown;

  FGrid.OnRowMultiSelect    := grdRowMultiSelect;
  FGrid.OnClearMultiSelect  := grdClearMultiSelect;

  // the TtsDBGridViewSort component has to be created AFTER the event handler
  // assignments because the component remaps the event handlers, which is not
  // very clean...
  FGridSort            := TtsDBGridViewSort.Create(Self);
  FGridSort.DBGridView := FGrid;
  FGridSort.SortedColumnColorEnabled := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.BeforeDestruction;
begin
  if Assigned(FData) then
    FData.UnRegisterDataView(Self);
  FreeAndNil(FConstCols);
  FreeAndNil(FEmptyCols);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TfrmGridView.GetEmptyColumnsVisible: Boolean;
begin
  Result := FEmptyColumnsVisible;
end;

procedure TfrmGridView.SetEmptyColumnsVisible(const Value: Boolean);
var
  C : TDBGridColumn;
begin
  if Value <> EmptyColumnsVisible then
  begin
    FEmptyColumnsVisible := Value;
    for C in FEmptyCols do
      C.Visible := Value;
  end;
end;

//-----------------------------------------------------------------------------

function TfrmGridView.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmGridView.SetSettings(const Value: IDataViewSettings);
begin
  FSettings := Value;
end;

//-----------------------------------------------------------------------------

function TfrmGridView.GetConstantColumnsVisible: Boolean;
begin
  Result := FConstantColumnsVisible;
end;

procedure TfrmGridView.SetConstantColumnsVisible(const Value: Boolean);
var
  C : TDBGridColumn;
begin
  if Value <> ConstantColumnsVisible then
  begin
    FConstantColumnsVisible := Value;
    for C in FConstCols do
      C.Visible := Value;
  end
end;

//-----------------------------------------------------------------------------

function TfrmGridView.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------

function TfrmGridView.GetRecordCount: Integer;
begin
  Result := Data.RecordCount;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmGridView.dscMainStateChange(Sender: TObject);
begin
  FGridSort.SortedFieldName := '';
  FGridSort.SortDirection := gsNone;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdCellAcceptCursor(Sender: TObject; Cell: TGridCell;
  var Accept: Boolean);
begin
  Accept := Accept and (not FGrid.Columns[Cell.Col].ReadOnly) and
    (not IsCellReadOnly(Cell));
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdChanging(Sender: TObject; var Cell: TGridCell;
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdCheckClick(Sender: TObject; Cell: TGridCell);
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdClearMultiSelect(Sender: TObject);
begin

end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdEditCanModify(Sender: TObject; Cell: TGridCell;
  var CanModify: Boolean);
begin
  CanModify := CanModify and not IsCellReadOnly(Cell);
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdGetCellColors(Sender: TObject; Cell: TGridCell;
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdGetCellReadOnly(Sender: TObject; Cell: TGridCell;
  var CellReadOnly: Boolean);
begin
  CellReadOnly := CellReadOnly and IsCellReadOnly(Cell);
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  GV : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if IsCheckBoxField(GV.Columns[Cell.Col].FieldName) then
    Value := '';
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdGetCheckState(Sender: TObject; Cell: TGridCell;
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdKeyPress(Sender: TObject; var Key: Char);
var
  GV   : TDBGridView;
begin
  GV := Sender as TDBGridView;
  if GV.Editing and (Key = '.') and (GV.SelectedField is TFloatField) then
    Key := ',';
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdMouseWheelDown(Sender: TObject; Shift: TShiftState;
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdMouseWheelUp(Sender: TObject; Shift: TShiftState;
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

//-----------------------------------------------------------------------------

procedure TfrmGridView.grdRowMultiSelect(Sender: TObject; Row: Integer;
  var Select: Boolean);
begin

end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

procedure TfrmGridView.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.AutoSizeColumns;
begin
  FGrid.AutoSizeCols;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.Copy;
begin

end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.EndUpdate;
begin
  FGrid.UnLockLayout(False);
  FGrid.UnLockUpdate(True);
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.HideSelectedColumns;
begin

end;

//-----------------------------------------------------------------------------

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
    AGridColumn.MaxWidth := 200;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.InitializeGridColumns;
var
  I : Integer;
begin
  BeginUpdate;
  try
    FGrid.Columns.BeginUpdate;
    for I := 0 to FGrid.Columns.Count - 1 do
      InitializeGridColumn(FGrid.Columns[I]);
    FGrid.Columns.EndUpdate;
  finally
    EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.Inspect;
begin
  InspectComponent(FGridSort);
end;

//-----------------------------------------------------------------------------

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

function TfrmGridView.SelectionToCommaText(AQuoteItems: Boolean): string;
begin

end;

function TfrmGridView.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin

end;

function TfrmGridView.SelectionToTextTable(AIncludeHeader: Boolean): string;
begin

end;

function TfrmGridView.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin

end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.ShowAllColumns;
var
  I: Integer;
begin
  for I := 0 to FGrid.Columns.Count - 1 do
  begin
    FGrid.Columns[I].Visible := True;
  end;
  FEmptyColumnsVisible    := True;
  FConstantColumnsVisible := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.UpdateColumnLists;
var
  X : Integer;
  Y : Integer;
  B : Boolean;
  S : string;
  T : string;
begin
  FConstCols.Clear;
  FEmptyCols.Clear;

  for X := 0 to FGrid.Columns.Count - 1 do
  begin
    // constant columns
    Y := 0;
    B := True;
    S := FGrid.Cells[X, Y];
    while B and (Y < DataSet.RecordCount) do
    begin
      T := FGrid.Cells[X, Y];
      B := S = T;
      Inc(Y);
    end;
    if B then
      FConstCols.Add(FGrid.Columns[X]);
    // empty columns
    Y := 0;
    B := True;
    while B and (Y < DataSet.RecordCount) do
    begin
      S := FGrid.Cells[X, Y];
      B := (S = '') or (S = '0') or (S = 'False');
      Inc(Y);
    end;
    if B then
      FEmptyCols.Add(FGrid.Columns[X]);;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.UpdateView;
begin
  BeginUpdate;
  try
    dscMain.DataSet := Data.DataSet;
    if Assigned(DataSet) and DataSet.Active then
    begin
      InitializeGridColumns;
      UpdateColumnLists;
      AutoSizeColumns;
    end;
  finally
    EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmGridView.BeginUpdate;
begin
  FGrid.LockLayout;
  FGrid.LockUpdate;
end;

end.

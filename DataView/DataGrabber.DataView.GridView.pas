{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

  DDuce.Components.GridView, DDuce.Components.DBGridView,

  DataGrabber.Interfaces, DataGrabber.DataView.Base;

type
  TfrmGridView = class(TBaseDataView, IDataView)
  private
    FGrid            : TDBGridView;
    FSortedFieldName : string;
    FSortDirection   : TGridSortDirection;
    FUpSortImage     : TBitmap;
    FDownSortImage   : TBitmap;

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
    procedure FGridClearMultiSelect(Sender: TObject);
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
    procedure FGridHeaderClick(
      Sender  : TObject;
      Section : TGridHeaderSection
    );
    procedure FGridGetSortDirection(
      Sender            : TObject;
      Section           : TGridHeaderSection;
      var SortDirection : TGridSortDirection
    );
    procedure FGridGetSortImage(
      Sender    : TObject;
      Section   : TGridHeaderSection;
      SortImage : TBitmap
    );

  protected
    {$REGION 'property access methods'}
    function GetPopupMenu: TPopupMenu; reintroduce; override;
    procedure SetPopupMenu(const Value: TPopupMenu); override;
    function GetGridType: string; override;
    {$ENDREGION}

    procedure InitializeGridColumns;
    procedure InitializeGridColumn(AGridColumn: TDBGridColumn);

    procedure ApplyGridSettings; override;

    // shortcut methods
    function IsLookupField(const AFieldName: string) : Boolean;
    function IsCheckBoxField(const AFieldName: string) : Boolean;
    function IsCellReadOnly(const ACell: TGridCell) : Boolean;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure HideSelectedColumns; override;
    procedure AutoSizeColumns; override;
    procedure Copy; override;
    procedure Inspect; override;

    function SelectionToCommaText(
      AQuoteItems: Boolean = True
    ): string; override;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string; override;
    function SelectionToTextTable(
      AIncludeHeader: Boolean = False
    ): string; override;
    function SelectionToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; override;
    function SelectionToFields(
      AQuoteItems : Boolean = True;
      ABreakItems : Boolean = False
    ): string; override;

    procedure UpdateView; override;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

uses
  System.Math, System.UITypes,
  Vcl.Clipbrd,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Logger,

  DataGrabber.Utils;

{$REGION 'construction and destruction'}
procedure TfrmGridView.AfterConstruction;
begin
  inherited AfterConstruction;
  FGrid                          := TDBGridView.Create(Self);
  FGrid.Align                    := alClient;
  FGrid.BorderStyle              := bsNone;
  FGrid.Parent                   := Self;
  FGrid.Visible                  := True;
  FGrid.CheckBoxes               := True;
  FGrid.DataSource               := dscMain;
  FGrid.DefaultLayout            := True;
  FGrid.AllowEdit                := True;
  FGrid.DoubleBuffered           := True;
  FGrid.ColumnClick              := True;
  FGrid.EndEllipsis              := True;
  FGrid.Font.Name                := 'Callibri';
  FGrid.Font.Size                := 12;
  FGrid.ShowCellTips             := True;
  FGrid.CheckStyle               := csFlat;
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
  FGrid.ThemingEnabled           := False;
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
  FGrid.OnHeaderClick       := FGridHeaderClick;
  FGrid.OnGetSortDirection  := FGridGetSortDirection;
  FGrid.OnGetSortImage      := FGridGetSortImage;

  FDownSortImage := TBitmap.Create;
  FDownSortImage.SetSize(16, 16);
  FDownSortImage.Transparent := True;
  FDownSortImage.Canvas.Pen.Color := clGray;
  FDownSortImage.Canvas.Pen.Width := 2;
  FDownSortImage.Canvas.MoveTo(2, 4);
  FDownSortImage.Canvas.LineTo(8, 10);
  FDownSortImage.Canvas.LineTo(14, 4);

  FUpSortImage := TBitmap.Create;
  FUpSortImage.SetSize(16, 16);
  FUpSortImage.Transparent := True;
  FUpSortImage.Canvas.Pen.Color := clGray;
  FUpSortImage.Canvas.Pen.Width := 2;
  FUpSortImage.Canvas.MoveTo(2, 10);
  FUpSortImage.Canvas.LineTo(8, 4);
  FUpSortImage.Canvas.LineTo(14, 10);

  FSortDirection := gsNone;
  UpdateView;
end;

destructor TfrmGridView.Destroy;
begin
  FUpSortImage.Free;
  FDownSortImage.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmGridView.GetGridType: string;
begin
  Result := 'GridView';
end;

function TfrmGridView.GetPopupMenu: TPopupMenu;
begin
  Result := FGrid.PopupMenu;
end;

procedure TfrmGridView.SetPopupMenu(const Value: TPopupMenu);
begin
  FGrid.PopupMenu := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FGrid event handlers'}
procedure TfrmGridView.FGridCellAcceptCursor(Sender: TObject; Cell: TGridCell;
  var Accept: Boolean);
begin
  Accept := Accept and (not FGrid.Columns[Cell.Col].ReadOnly) and
    (not IsCellReadOnly(Cell));
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
  //(Data as IDataSelection).SelectedRecords.Clear;
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
  //CellReadOnly := CellReadOnly and IsCellReadOnly(Cell);
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

procedure TfrmGridView.FGridGetSortDirection(Sender: TObject;
  Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
var
  Field : TField;
begin
  if Section.ColumnIndex < FGrid.Columns.Count then
  begin
    Field := FGrid.Columns[Section.ColumnIndex].Field;
    if Assigned(Field) then
    begin
      if Field.FieldName = FSortedFieldName then
        SortDirection := FSortDirection;
    end;
  end;
end;

procedure TfrmGridView.FGridGetSortImage(Sender: TObject;
  Section: TGridHeaderSection; SortImage: TBitmap);
begin
  if FSortDirection = gsAscending then
    SortImage.Assign(FUpSortImage)
  else
  begin
    SortImage.Assign(FDownSortImage)
  end;
end;

procedure TfrmGridView.FGridHeaderClick(Sender: TObject;
  Section: TGridHeaderSection);
var
  LField : TField;
begin
  LField := FGrid.Columns[Section.ColumnIndex].Field;
  if Assigned(LField) and (LField.FieldKind = fkData) then
  begin
    case FSortDirection of
      gsNone, gsDescending:
        FSortDirection := gsAscending;
      gsAscending:
      begin
        if LField.FieldName = FSortedFieldName then
          FSortDirection := gsDescending
      end;
    end;
    FSortedFieldName := LField.FieldName;
    Data.Sort(DataSet, LField.FieldName, FSortDirection = gsDescending);
    DataSet.First;
  end
  else
  begin
    FSortDirection   := gsNone;
    FSortedFieldName := '';
  end;
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
{$ENDREGION}

{$REGION 'private methods'}
function TfrmGridView.IsCellReadOnly(const ACell: TGridCell): Boolean;
begin
  Result := False;
end;

function TfrmGridView.IsCheckBoxField(const AFieldName: string): Boolean;
begin
  //Result := (Data as IDisplayData).IsCheckBoxField(AFieldName);
  Result := False;
end;

function TfrmGridView.IsLookupField(const AFieldName: string): Boolean;
begin
  //Result := (Data as IDisplayData).IsLookupField(AFieldName);
  Result := False;
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
  AGridColumn.TabStop := True;
  AGridColumn.AllowClick := True;
  AGridColumn.ReadOnly := False;
  AGridColumn.Field.ReadOnly := False;
end;

procedure TfrmGridView.InitializeGridColumns;
var
  I : Integer;
begin
  for I := 0 to FGrid.Columns.Count - 1 do
    InitializeGridColumn(FGrid.Columns[I]);
end;

procedure TfrmGridView.ApplyGridSettings;
begin
  FGrid.GridLines := True;
  if Settings.ShowHorizontalGridLines then
    FGrid.GridStyle := FGrid.GridStyle + [gsHorzLine]
  else
    FGrid.GridStyle := FGrid.GridStyle - [gsHorzLine];

  if Settings.ShowVerticalGridLines then
    FGrid.GridStyle := FGrid.GridStyle + [gsVertLine]
  else
    FGrid.GridStyle := FGrid.GridStyle - [gsVertLine];
  FGrid.Font.Assign(Settings.GridFont);
  FGrid.Font.Size := 10;
  FGrid.Header.Font.Assign(Settings.GridFont);
  FGrid.Header.Font.Size := 10;
  FGrid.Header.Font.Style := FGrid.Header.Font.Style + [fsBold];
end;

procedure TfrmGridView.AutoSizeColumns;
begin
  FGrid.AutoSizeCols;
  FGrid.Invalidate;
end;

procedure TfrmGridView.Copy;
begin
  if Assigned(FGrid.SelectedField) then
    Clipboard.AsText := FGrid.SelectedField.AsString;
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

function TfrmGridView.SelectionToFields(AQuoteItems: Boolean;
  ABreakItems: Boolean): string;
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

procedure TfrmGridView.UpdateView;
begin
  BeginUpdate;
  try
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

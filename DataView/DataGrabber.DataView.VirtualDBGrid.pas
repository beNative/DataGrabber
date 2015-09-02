{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.DataView.VirtualDBGrid;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Data.DB,

  VirtualTrees,

  ts.Interfaces,

  DDuce.Components.VirtualDBGrid,

  DataGrabber.Interfaces;

type
  TfrmVirtualDBGrid = class(TForm, IDataView, IDGDataView)
    dscMain: TDataSource;

    procedure grdMainBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure grdMainKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdMainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdMainFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure grdMainPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure grdMainAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure grdMainStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
  private
    grdMain                 : TVirtualDBGrid;
    FSettings               : IDataViewSettings;
    FData                   : IData;
    FEmptyCols              : TObjectList<TVirtualDBTreeColumn>;
    FConstCols              : TObjectList<TVirtualDBTreeColumn>;
    FConstantColumnsVisible : Boolean;
    FEmptyColumnsVisible    : Boolean;

//    StartSelectedColumn: integer;
//    FirstSelectedColumn: integer;
//    LastSelectedColumn: integer;
//    Selecting: boolean;

    function GetName: string;
    function GetConstantColumnsVisible: Boolean;
    function GetData: IData;
    function GetDataSet: TDataSet;
    function GetEmptyColumnsVisible: Boolean;
    function GetRecordCount: Integer;
    function GetSettings: IDataViewSettings;
    procedure SetConstantColumnsVisible(const Value: Boolean);
    procedure SetData(const Value: IData);
    procedure SetEmptyColumnsVisible(const Value: Boolean);
    procedure SetSettings(const Value: IDataViewSettings);
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);

  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

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

    property ConstantColumnsVisible: Boolean
      read GetConstantColumnsVisible write SetConstantColumnsVisible;

    property EmptyColumnsVisible: Boolean
      read GetEmptyColumnsVisible write SetEmptyColumnsVisible;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;

implementation

{$R *.dfm}

uses
  System.Math,

  ts.Utils, ts.Modules.ComponentInspector;

{$REGION 'construction and destruction'}
procedure TfrmVirtualDBGrid.AfterConstruction;
begin
  inherited;
  FEmptyCols := TObjectList<TVirtualDBTreeColumn>.Create(False);
  FConstCols := TObjectList<TVirtualDBTreeColumn>.Create(False);
  FConstantColumnsVisible := True;
  FEmptyColumnsVisible    := True;
  grdMain := TVirtualDBGrid.Create(Self);
  grdMain.Parent := Self;
  grdMain.Align  := alClient;
  grdMain.DBOptions.DataSource := dscMain;
  grdMain.OnBeforeCellPaint := grdMainBeforeCellPaint;
  grdMain.OnKeyDown         := grdMainKeyDown;
  grdMain.OnKeyUp           := grdMainKeyUp;
  grdMain.OnFocusChanged    := grdMainFocusChanged;
  grdMain.OnPaintText       := grdMainPaintText;
  grdMain.OnAddToSelection  := grdMainAddToSelection;
  grdMain.OnStateChange     := grdMainStateChange;
end;

procedure TfrmVirtualDBGrid.BeforeDestruction;
begin
  if Assigned(FData) then
    FData.UnRegisterDataView(Self);
  FreeAndNil(FEmptyCols);
  FreeAndNil(FConstCols);
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmVirtualDBGrid.GetConstantColumnsVisible: Boolean;
begin
  Result := FConstantColumnsVisible;
end;

procedure TfrmVirtualDBGrid.SetConstantColumnsVisible(const Value: Boolean);
var
  C : TVirtualDBTreeColumn;
begin
  if Value <> ConstantColumnsVisible then
  begin
    FConstantColumnsVisible := Value;
    for C in FConstCols do
    begin
      if Value then
        C.Options := C.Options + [coVisible]
      else
        C.Options := C.Options - [coVisible];
    end;
  end;
end;

function TfrmVirtualDBGrid.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmVirtualDBGrid.SetData(const Value: IData);
begin
  if Value <> Data then
  begin
    FData := Value;
    FData.RegisterDataView(Self);
    dscMain.DataSet := Data.DataSet;
    UpdateView;
  end;
end;

function TfrmVirtualDBGrid.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

function TfrmVirtualDBGrid.GetEmptyColumnsVisible: Boolean;
begin
  Result := FEmptyColumnsVisible;
end;

procedure TfrmVirtualDBGrid.SetEmptyColumnsVisible(const Value: Boolean);
var
  C : TVirtualDBTreeColumn;
begin
  if Value <> EmptyColumnsVisible then
  begin
    FEmptyColumnsVisible := Value;
    for C in FEmptyCols do
    begin
      if Value then
        C.Options := C.Options + [coVisible]
      else
        C.Options := C.Options - [coVisible];
    end;
  end;
end;

function TfrmVirtualDBGrid.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmVirtualDBGrid.GetPopupMenu: TPopupMenu;
begin
  Result := grdMain.PopupMenu;
end;

procedure TfrmVirtualDBGrid.SetPopupMenu(const Value: TPopupMenu);
begin
  grdMain.PopupMenu := Value;
end;

function TfrmVirtualDBGrid.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TfrmVirtualDBGrid.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmVirtualDBGrid.SetSettings(const Value: IDataViewSettings);
begin
  FSettings := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVirtualDBGrid.grdMainAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  //Node. States := Node.States + [vsSelected]
end;

procedure TfrmVirtualDBGrid.grdMainBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  G : TVirtualDBGrid;
  F : TField;
  D : Double;
begin
  if Settings.GridCellColoring then
  begin
    G := Sender as TVirtualDBGrid;
    F := DataSet.FindField(TVirtualDBTreeColumn(G.Header.Columns[Column]).FieldName);
    if Assigned(F) then
    begin
      if F is TNumericField then
      begin
        D := F.AsFloat;
        if IsZero(D) then
          TargetCanvas.Font.Color := clBlue
        else if D < 0 then
          TargetCanvas.Font.Color := clRed;
      end;
//      if F.AsString = '' then
//      begin
//        TargetCanvas.Brush.Color := Settings.DataTypeColors[dtNULL];
//      end
//      else
      begin
        TargetCanvas.Brush.Color := Settings.FieldTypeColors[F.DataType];
      end;
      TargetCanvas.FillRect(CellRect);
    end;
  end;

//  if grdMain.Selected[node] and InRange(column, FirstSelectedColumn, LastSelectedColumn) then
//  begin
//    if grdMain.Focused then
//      TargetCanvas.Brush.Color := grdMain.Colors.FocusedSelectionColor
//    else
//      TargetCanvas.Brush.Color := grdMain.Colors.UnfocusedSelectionColor;
//    TargetCanvas.Brush.Style := bsSolid;
//    TargetCanvas.FillRect(CellRect);
//  end;

end;

procedure TfrmVirtualDBGrid.grdMainFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
//  if Selecting then
//  begin
//    if column < StartSelectedColumn then
//    begin
//      FirstSelectedColumn := column;
//      LastSelectedColumn := StartSelectedColumn;
//    end
//    else if column > StartSelectedColumn then
//    begin
//      FirstSelectedColumn := StartSelectedColumn;
//      LastSelectedColumn := column
//    end
//    else
//    begin
//      FirstSelectedColumn := column;
//      LastSelectedColumn := column;
//    end;
//  end
//  else
//  begin
//    StartSelectedColumn := column;
//    FirstSelectedColumn := column;
//    LastSelectedColumn := column;
//  end;

end;

procedure TfrmVirtualDBGrid.grdMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  if (not Selecting) and (Key = VK_SHIFT) then
//  begin
//    StartSelectedColumn := grdMain.FocusedColumn;
//    FirstSelectedColumn := StartSelectedColumn;
//    LastSelectedColumn := StartSelectedColumn;
//    Selecting := true;
//  end;
end;

procedure TfrmVirtualDBGrid.grdMainKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  if Key = VK_SHIFT then
//    Selecting := false;
end;

procedure TfrmVirtualDBGrid.grdMainPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
//  if grdMain.Selected[node] and InRange(column, FirstSelectedColumn, LastSelectedColumn) then
//  begin
//    if grdMain.Focused then
//      TargetCanvas.Font.Color := clHighlightText
//    else
//      TargetCanvas.Font.Color := grdMain.Font.Color;
//  end;
end;

procedure TfrmVirtualDBGrid.grdMainStateChange(Sender: TBaseVirtualTree; Enter,
  Leave: TVirtualTreeStates);
begin

end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmVirtualDBGrid.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

procedure TfrmVirtualDBGrid.AutoSizeColumns;
var
  C: TVirtualDBTreeColumn;
  I: Integer;
begin
  BeginUpdate;
  try
    grdMain.Header.Columns.BeginUpdate;
    try
      for I := 0 to grdMain.Header.Columns.Count - 1 do
      begin
        C := TVirtualDBTreeColumn(grdMain.Header.Columns[I]);
        C.MinWidth := GetTextWidth(C.Text, grdMain.Font) + 20;
        C.MaxWidth := 600;
      end;
      grdMain.Header.AutoFitColumns(False, smaAllColumns, 1);
    finally
      grdMain.Header.Columns.EndUpdate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmVirtualDBGrid.BeginUpdate;
begin
  grdMain.BeginUpdate;
end;

procedure TfrmVirtualDBGrid.Copy;
begin

end;

constructor TfrmVirtualDBGrid.Create;
begin
  inherited Create(Application);
end;

procedure TfrmVirtualDBGrid.EndUpdate;
begin
  grdMain.EndUpdate;
end;

procedure TfrmVirtualDBGrid.HideSelectedColumns;
begin

end;

procedure TfrmVirtualDBGrid.Inspect;
begin
  InspectComponent(grdMain);
end;

function TfrmVirtualDBGrid.SelectionToCommaText(AQuoteItems: Boolean): string;
begin

end;

function TfrmVirtualDBGrid.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin

end;

function TfrmVirtualDBGrid.SelectionToFields(AQuoteItems: Boolean): string;
begin

end;

function TfrmVirtualDBGrid.SelectionToTextTable(
  AIncludeHeader: Boolean): string;
begin

end;

function TfrmVirtualDBGrid.SelectionToWikiTable(
  AIncludeHeader: Boolean): string;
begin

end;

procedure TfrmVirtualDBGrid.ShowAllColumns;
var
  I: Integer;
  C: TVirtualTreeColumn;
begin
  for I := 0 to grdMain.Header.Columns.Count - 1 do
  begin
    C := grdMain.Header.Columns[I];
    C.Options := C.Options + [coVisible];
  end;
  FEmptyColumnsVisible    := True;
  FConstantColumnsVisible := True;
end;

procedure TfrmVirtualDBGrid.UpdateColumnLists;
var
  X : Integer;
  Y : Integer;
  B : Boolean;
  S : string;
  T : string;
  N : PVirtualNode;
  C : TVirtualDBTreeColumn;
begin
  FConstCols.Clear;
  FEmptyCols.Clear;

  for X := 1 to grdMain.Header.Columns.Count - 1 do
  begin
    C := TVirtualDBTreeColumn(grdMain.Header.Columns[X]);
    // constant columns
    Y := 0;
    N := grdMain.GetFirst;
    B := True;
    S := grdMain.Text[N, X];
    while B and (Y < DataSet.RecordCount) do
    begin
      T := grdMain.Text[N, X];
      B := S = T;
      Inc(Y);
      N := N.NextSibling;
    end;
    if B then
      FConstCols.Add(C);
    // empty columns
    Y := 0;
    N := grdMain.GetFirst;
    B := True;
    while B and (Y < DataSet.RecordCount) do
    begin
      S := grdMain.Text[N, X];
      B := (S = '') or (S = '0') or (S = 'False');
      Inc(Y);
      N := N.NextSibling;
    end;
    if B then
      FEmptyCols.Add(C);
  end;
end;

procedure TfrmVirtualDBGrid.UpdateView;
begin
  BeginUpdate;
  try
    dscMain.DataSet := Data.DataSet;
    UpdateColumnLists;
    AutoSizeColumns;
  finally
    EndUpdate;
  end;
end;
{$ENDREGION}

end.

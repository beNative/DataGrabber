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

unit DataGrabber.DataView.cxGrid;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ActnList,
  Data.DB,

  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxDBData, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGrid,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxInplaceContainer,
  cxVGrid, cxOI, cxGridExportLink, cxGridCustomPopupMenu, cxGridPopupMenu,
  cxLookAndFeels, cxLookAndFeelPainters, cxGridCardView, cxGridBandedTableView,
  cxGridDBCardView, cxGridDBBandedTableView, cxNavigator,

  ts.Interfaces,

  DataGrabber.Interfaces;

{
  TODO: DataShaper

  - filter out selection from cells
  - quote and delimit

}

type
  TfrmcxGrid = class(TFrame, IDataView, IDGDataView, IGroupable, IMergable)
    dscMain        : TDataSource;
    grdMain        : TcxGrid;
    grlGrid1Level1 : TcxGridLevel;
    ppmMain        : TcxGridPopupMenu;
    tvwMain        : TcxGridDBTableView;

    procedure tvwMainCustomDrawGroupSummaryCell(
      Sender      : TObject;
      ACanvas     : TcxCanvas;
      ARow        : TcxGridGroupRow;
      AColumn     : TcxGridColumn;
      ASummaryItem: TcxDataSummaryItem;
      AViewInfo   : TcxCustomGridViewCellViewInfo;
      var ADone   : Boolean
    );
    procedure tvwMainCustomDrawCell(
      Sender   : TcxCustomGridTableView;
      ACanvas  : TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean
    );

  private
    FSettings               : IDataViewSettings;
    FMergeColumnCells       : Boolean;
    FAutoSizeCols           : Boolean;
    FEmptyCols              : TObjectList<TcxGridDBColumn>;
    FConstCols              : TObjectList<TcxGridDBColumn>;
    FEmptyColumnsVisible    : Boolean;
    FConstantColumnsVisible : Boolean;
    FData                   : IData;

    function GetName: string;
    function GetDataSet: TDataSet;
    function GetRecordCount: Integer;
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    function GetGridMode: Boolean;
    procedure SetGridMode(const Value: Boolean);
    function GetAutoSizeCols: Boolean;
    procedure SetAutoSizeCols(const Value: Boolean);
    function GetConstantColumnsVisible: Boolean;
    procedure SetConstantColumnsVisible(const Value: Boolean);
    function GetEmptyColumnsVisible: Boolean;
    procedure SetEmptyColumnsVisible(const Value: Boolean);
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetPopupMenu: TPopupMenu; reintroduce;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetGridType: string;

  protected
    procedure CopySelectionToClipboard(
      AController   : TcxGridTableController;
      AIncludeHeader: Boolean = False
    );
    function SelectionToDelimitedTable(
      AController   : TcxGridTableController;
      ADelimiter    : string = #9; // TAB
      AIncludeHeader: Boolean = True
    ): string; overload;
    function SelectionToCommaText(
      AController: TcxGridTableController;
      AQuoteItems: Boolean = True
    ): string; overload;
    function SelectionToFields(
      AController: TcxGridTableController;
      AQuoteItems: Boolean = True
    ): string; overload;
    function SelectionToTextTable(
      AController   : TcxGridTableController;
      AIncludeHeader: Boolean = False
    ): string; overload;
    function SelectionToWikiTable(
      AController   : TcxGridTableController;
      AIncludeHeader: Boolean = False
    ): string; overload;

    procedure UpdateColumnLists;

  public
    constructor Create; reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignParent(AParent: TWinControl);
    procedure HideSelectedColumns;
    procedure MergeAllColumnCells(AActive: Boolean);
    procedure ShowAllColumns;
    procedure AutoSizeColumns;
    procedure GroupBySelectedColumns;
    procedure Copy;
    procedure Inspect;
    procedure UpdateView;

    procedure BeginUpdate;
    procedure EndUpdate;

    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;
      overload;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
      overload;
    function SelectionToDelimitedTable(
      ADelimiter    : string = #9; // TAB
      AIncludeHeader: Boolean = True
    ): string; overload;
    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
      overload;
    function SelectionToFields(AQuoteItems: Boolean = True): string; overload;

    property DataSet: TDataSet
      read GetDataSet;

    property Data: IData
      read GetData write SetData;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property GridMode: Boolean
      read GetGridMode write SetGridMode;

    property RecordCount: Integer
      read GetRecordCount;

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;

    property AutoSizeCols: Boolean
      read GetAutoSizeCols write SetAutoSizeCols;

    property ConstantColumnsVisible: Boolean
      read GetConstantColumnsVisible write SetConstantColumnsVisible;

    property EmptyColumnsVisible: Boolean
      read GetEmptyColumnsVisible write SetEmptyColumnsVisible;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property GridType: string
      read GetGridType;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  Vcl.Clipbrd,

  cxGridDBDataDefinitions, cxGridCommon,

  DDuce.ObjectInspector;

{$REGION 'non-interfaced routines'}
function GetTextWidth(const AText: string): Integer;
var
  SL      : TStringList;
  I, W, R : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AText;
    R := 0;
    for I := 0 to SL.Count - 1 do
    begin
      W := Length(SL[I]);
      if W > R then
        R := W;
    end;
    Result := R;
  finally
    SL.Free;
  end;
end;

function GetMaxTextWidth(AStrings: TStrings): Integer;
var
  I : Integer;
  N : Integer;
begin
  Result := 0;
  if Assigned(AStrings) then
  begin
    for I := 0 to AStrings.Count - 1 do
    begin
      N := GetTextWidth(AStrings[I]);
      if N > Result then
        Result := N;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TfrmcxGrid.Create;
begin
  inherited Create(Application);
  Name := '';
end;

procedure TfrmcxGrid.AfterConstruction;
begin
  inherited;
  FEmptyCols := TObjectList<TcxGridDBColumn>.Create(False);
  FConstCols := TObjectList<TcxGridDBColumn>.Create(False);
  FConstantColumnsVisible := True;
  FEmptyColumnsVisible    := True;
  FAutoSizeCols := True;
  GridMode      := False;
end;

procedure TfrmcxGrid.BeforeDestruction;
begin
  inherited;
  if Assigned(FData) then
    FData.UnRegisterDataView(Self);
  FreeAndNil(FEmptyCols);
  FreeAndNil(FConstCols);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmcxGrid.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmcxGrid.SetData(const Value: IData);
begin
  if Value <> Data then
  begin
    if Data <> nil then
      FData.UnregisterDataView(Self);
    FData := Value;
    FData.RegisterDataView(Self);
    dscMain.DataSet := DataSet;
    UpdateView;
  end;
end;

function TfrmcxGrid.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

function TfrmcxGrid.GetRecordCount: Integer;
begin
  Result := tvwMain.DataController.RecordCount;
end;

function TfrmcxGrid.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmcxGrid.SetSettings(const Value: IDataViewSettings);
begin
  FSettings := Value;
end;

function TfrmcxGrid.GetMergeColumnCells: Boolean;
begin
  Result := FMergeColumnCells;
end;

procedure TfrmcxGrid.SetMergeColumnCells(const Value: Boolean);
begin
  if Value <> MergeColumnCells then
  begin
    FMergeColumnCells := Value;
    MergeAllColumnCells(Value);
  end;
end;

function TfrmcxGrid.GetName: string;
begin
  Result := inherited Name;
end;

function TfrmcxGrid.GetPopupMenu: TPopupMenu;
begin
  Result := TPopupMenu(grdMain.PopupMenu);
end;

procedure TfrmcxGrid.SetPopupMenu(const Value: TPopupMenu);
begin
  grdMain.PopupMenu := Value;
end;

function TfrmcxGrid.GetGridMode: Boolean;
begin
  Result := tvwMain.DataController.DataModeController.GridMode;
end;

function TfrmcxGrid.GetGridType: string;
begin
  Result := 'cxGrid';
end;

procedure TfrmcxGrid.SetGridMode(const Value: Boolean);
begin
  if Value <> GridMode then
  begin
    tvwMain.DataController.DataModeController.GridMode := Value;
    tvwMain.DataController.DataModeController.GridModeBufferCount := 0;
  end;
end;

function TfrmcxGrid.GetAutoSizeCols: Boolean;
begin
  Result := FAutoSizeCols;
end;

procedure TfrmcxGrid.SetAutoSizeCols(const Value: Boolean);
begin
  if Value <> AutoSizeCols then
  begin
    FAutoSizeCols := Value;
    if Value then
    begin
      tvwMain.OptionsBehavior.BestFitMaxRecordCount := 100;
      AutoSizeColumns;
    end;
  end;
end;

function TfrmcxGrid.GetConstantColumnsVisible: Boolean;
begin
  Result := FConstantColumnsVisible;
end;

procedure TfrmcxGrid.SetConstantColumnsVisible(const Value: Boolean);
begin
  if Value <> ConstantColumnsVisible then
  begin
    FConstantColumnsVisible := Value;
    UpdateColumnLists;
  end;
end;

function TfrmcxGrid.GetEmptyColumnsVisible: Boolean;
begin
  Result := FEmptyColumnsVisible;
end;

procedure TfrmcxGrid.SetEmptyColumnsVisible(const Value: Boolean);
begin
  if Value <> EmptyColumnsVisible then
  begin
    FEmptyColumnsVisible := Value;
    UpdateColumnLists;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmcxGrid.tvwMainCustomDrawCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
  var ADone: Boolean);
var
  VTC: TcxValueTypeClass;
begin
  if Assigned(Settings) and Settings.GridCellColoring then
  begin
    if AViewInfo.Text = '0' then
      ACanvas.Font.Color := clBlue;

    if AViewInfo.Text = '' then
    begin
      ACanvas.Brush.Color := $00EFEFEF;
    end
    else
    begin
      VTC := AViewInfo.Item.DataBinding.ValueTypeClass;
      if (VTC = TcxDateTimeValueType) or (VTC = TcxSQLTimeStampValueType) then
      begin
        ACanvas.Brush.Color := Settings.DataTypeColors[dtDateTime];
      end
      else if (VTC = TcxStringValueType) or (VTC = TcxWideStringValueType) then
      begin
        ACanvas.Brush.Color := Settings.DataTypeColors[dtString];
      end
      else if (VTC = TcxIntegerValueType) or (VTC = TcxWordValueType)
       or (VTC = TcxSmallintValueType) or (VTC = TcxLargeIntValueType) then
      begin
        ACanvas.Brush.Color := Settings.DataTypeColors[dtInteger];
      end
      else if (VTC = TcxFloatValueType) or (VTC = TcxCurrencyValueType)
        or (VTC = TcxFMTBcdValueType) then
      begin
        ACanvas.Brush.Color := Settings.DataTypeColors[dtFloat];
      end
      else if VTC = TcxBooleanValueType then
      begin
        ACanvas.Brush.Color := Settings.DataTypeColors[dtBoolean];
      end;
    end;
    if AViewInfo.Selected then
    begin
      ACanvas.Brush.Color := clGray;
      ACanvas.Font.Color := clWhite;
    end;
  end;
end;

procedure TfrmcxGrid.tvwMainCustomDrawGroupSummaryCell(Sender: TObject;
  ACanvas: TcxCanvas; ARow: TcxGridGroupRow; AColumn: TcxGridColumn;
  ASummaryItem: TcxDataSummaryItem;
  AViewInfo: TcxCustomGridViewCellViewInfo; var ADone: Boolean);
begin
  if not ARow.Selected then
    ACanvas.Font.Color := clBlue
  else
    ACanvas.Font.Color := clWhite;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmcxGrid.Copy;
begin
  Clipboard.AsText := Trim(SelectionToDelimitedTable(#9, False));
end;

procedure TfrmcxGrid.CopySelectionToClipboard(AController: TcxGridTableController;
  AIncludeHeader: Boolean);
var
  X, Y : Integer;
  S, T : string;
  V    : Variant;
  SL   : TStringList;
begin
  SL := TStringList.Create;
  try
    if AIncludeHeader then
    begin
      S := ',';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption + ',';
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := ',';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        V := AController.SelectedRows[Y].Values[AController.SelectedColumns[X].Index];
        T := VarToStr(V);
        if T = '' then
          T := ' ';
        S := S + T + ',';
      end;
      SL.Add(S);
    end;
    Clipboard.AsText := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmcxGrid.EndUpdate;
begin
  tvwMain.EndUpdate;
end;

function TfrmcxGrid.SelectionToDelimitedTable(
  AController: TcxGridTableController; ADelimiter: string;
  AIncludeHeader: Boolean): string;
var
  X, Y : Integer;
  S, T : string;
  V    : Variant;
  SL   : TStringList;
begin
  SL := TStringList.Create;
  try
    S := '';
    if AIncludeHeader then
    begin
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption ;
        if X < AController.SelectedColumnCount -1 then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := '';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        V := AController.SelectedRows[Y].Values[AController.SelectedColumns[X].Index];
        T := VarToStr(V);
        S := S + T;
        if X < AController.SelectedColumnCount -1 then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TfrmcxGrid.SelectionToWikiTable(AController: TcxGridTableController;
  AIncludeHeader: Boolean): string;
var
  X, Y : Integer;
  S, T : string;
  V    : Variant;
  SL   : TStringList;
begin
  SL := TStringList.Create;
  try
    if AIncludeHeader then
    begin
      S := '||';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption + '||';
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := '|';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        V := AController.SelectedRows[Y].Values[AController.SelectedColumns[X].Index];
        T := VarToStr(V);
        if T = '' then
          T := ' ';
        S := S + T + '|';
      end;
      SL.Add(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TfrmcxGrid.SelectionToCommaText(AController: TcxGridTableController;
  AQuoteItems: Boolean): string;
var
  X, Y   : Integer;
  S, T   : string;
  V      : Variant;
  CCount : Integer;
  RCount : Integer;
begin
  S := '';
  CCount := AController.SelectedColumnCount;
  RCount := AController.SelectedRowCount;
  for Y := 0 to RCount - 1 do
  begin
    for X := 0 to CCount - 1 do
    begin
      V := AController.SelectedRows[Y].Values[AController.SelectedColumns[X].Index];
      T := VarToStr(V);
      if AQuoteItems then
        T := QuotedStr(T);
      S := S + T;
      if X < CCount -1 then
        S := S + ', ';
    end;
    if (CCount = 1) and (Y < RCount - 1) then
      S := S + ', '
    else if Y < RCount - 1 then
      S := S + #13#10
  end;
  Result := S;
end;

function TfrmcxGrid.SelectionToTextTable(AController: TcxGridTableController;
  AIncludeHeader: Boolean): string;
var
  X, Y   : Integer;
  S      : string;
  V      : Variant;
  F      : TField;
  I      : Integer;
  N      : Integer;
  sTxt   : string;
  sLine  : string;
  sFmt   : string;
  Widths : array of Integer;
  SL     : TStringList;
begin
  SetLength(Widths, AController.SelectedColumnCount);
  try
    SL := TStringList.Create;
    try
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        I := AController.SelectedColumns[X].Index;
        SL.Clear;
        if AIncludeHeader then
        begin
          F := tvwMain.Columns[I].DataBinding.Field;
          SL.Add(F.FieldName);
        end;

        for Y := 0 to AController.SelectedRowCount - 1 do
        begin
          V := AController.SelectedRows[Y].Values[I];
          S := VarToStr(V);
          SL.Add(S);
        end;
        Widths[X] := GetMaxTextWidth(SL);
      end;
    finally
      FreeAndNil(SL);
    end;

    if AIncludeHeader then
    begin
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        I := AController.SelectedColumns[X].Index;
        F := tvwMain.Columns[I].DataBinding.Field;
        N := Widths[X];
        sFmt := '%-' + IntToStr(N) + 's';
        sLine := sLine + '+' + Format(sFmt, [DupeString('-', N)]);
        sTxt := sTxt + '|' + Format(sFmt, [F.FieldName]);
      end;
      sTxt := sTxt + '|';
      sLine := sLine + '+';
      Result := sLine + #13#10 + sTxt + #13#10 + sLine;
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      sTxt := '';
      for X := 0 to AController.SelectedColumnCount -1 do
      begin
        I := AController.SelectedColumns[X].Index;
        V := AController.SelectedRows[Y].Values[I];
        S := VarToStr(V);
        F := tvwMain.Columns[I].DataBinding.Field;
        if Assigned(F) then
        begin
          N := Widths[X];
          sFmt := '%-' + IntToStr(N) + 's';
          sTxt := sTxt + '|' + Format(sFmt, [S]);
        end;
      end;
      sTxt := sTxt + '|';
      Result := Result + #13#10 + sTxt;
      sTxt := '';
    end;
    Result := Result + #13#10 + sLine;
  finally
    Finalize(Widths);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmcxGrid.UpdateColumnLists;
var
  X       : Integer;
  Y       : Integer;
  IsEmpty : Boolean;
  IsConst : Boolean;
  DC      : TcxGridDBDataController;
  S       : string;
  T       : string;
  C       : TcxGridDBColumn;
begin
  FConstCols.Clear;
  FEmptyCols.Clear;
  DC := tvwMain.DataController;
  if DC.RecordCount > 0 then
  begin
    BeginUpdate;
    try
      for X := 0 to tvwMain.ColumnCount - 1 do
      begin
        C := tvwMain.Columns[X];
        Y := 0;
        IsEmpty := True;
        while IsEmpty and (Y < DC.RecordCount) do
        begin
          S := VarToStrDef(DC.GetValue(Y, X), '');
          IsEmpty := (S = '') or (S = '0') or (S = 'False');
          Inc(Y);
        end;
        if IsEmpty then
          FEmptyCols.Add(C);
        Y := 0;
        IsConst := True;
        S := VarToStrDef(DC.GetValue(Y, X), '');
        while IsConst and (Y < DC.RecordCount) do
        begin
          T := VarToStrDef(DC.GetValue(Y, X), '');
          IsConst := S = T;
          Inc(Y);
        end;
        if IsConst then
          FConstCols.Add(C);
        C.Visible := (DC.RecordCount <= 1) or
          ((not (not EmptyColumnsVisible and IsEmpty)) and
          (not (not ConstantColumnsVisible and IsConst)));
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmcxGrid.UpdateView;
begin
  dscMain.DataSet := DataSet;
  if Assigned(DataSet) and DataSet.Active then
  begin
    tvwMain.ClearItems;
    tvwMain.DataController.CreateAllItems;
    BeginUpdate;
    try
      UpdateColumnLists;
    finally
      EndUpdate;
    end;
    AutoSizeColumns;
  end;
end;

function TfrmcxGrid.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin
  Result := SelectionToWikiTable(tvwMain.Controller, AIncludeHeader);
end;

function TfrmcxGrid.SelectionToCommaText(AQuoteItems: Boolean): string;
begin
  Result := SelectionToCommaText(tvwMain.Controller, AQuoteItems);
end;

function TfrmcxGrid.SelectionToTextTable(AIncludeHeader: Boolean): string;
begin
  Result := SelectionToTextTable(tvwMain.Controller, AIncludeHeader);
end;

procedure TfrmcxGrid.ShowAllColumns;
var
  I: Integer;
begin
  BeginUpdate;
  try
   for I := 0 to tvwMain.ColumnCount - 1 do
   begin
     tvwMain.Columns[I].Visible := True;
   end;
  FEmptyColumnsVisible    := True;
  FConstantColumnsVisible := True;
  finally
    EndUpdate;
  end
end;

procedure TfrmcxGrid.HideSelectedColumns;
var
  C : TcxGridTableController;
  I : Integer;
begin
  BeginUpdate;
  try
    C := tvwMain.Controller;
    for I := 0 to C.SelectedColumnCount - 1 do
    begin
      tvwMain.Columns[C.SelectedColumns[I].Index].Visible := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmcxGrid.Inspect;
begin
  InspectComponent(tvwMain);
end;

procedure TfrmcxGrid.MergeAllColumnCells(AActive: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to tvwMain.ColumnCount - 1 do
    begin
      tvwMain.Columns[I].Options.CellMerging  := AActive;
      tvwMain.Columns[I].Options.GroupFooters := True;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmcxGrid.AssignParent(AParent: TWinControl);
begin
  Parent := AParent;
  Align  := alClient;
end;

procedure TfrmcxGrid.AutoSizeColumns;
begin
  tvwMain.BeginBestFitUpdate;
  try
    tvwMain.ApplyBestFit;
  finally
    tvwMain.EndBestFitUpdate;
  end;
end;

procedure TfrmcxGrid.BeginUpdate;
begin
  tvwMain.BeginUpdate;
end;

procedure TfrmcxGrid.GroupBySelectedColumns;
var
  C          : TcxGridTableController;
  I          : Integer;
  Col        : TcxGridDBColumn;
  GroupIndex : Integer;
begin
  BeginUpdate;
  try
    C := tvwMain.Controller;
    for I := 0 to C.SelectedColumnCount - 1 do
    begin
      Col := tvwMain.Columns[C.SelectedColumns[I].Index];
      if Col.GroupIndex <> -1 then
        GroupIndex := -1
      else
        GroupIndex := tvwMain.GroupedColumnCount;
      Col.GroupBy(GroupIndex);
    end;
  finally
    EndUpdate;
  end;
end;

function TfrmcxGrid.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin
  Result :=
    SelectionToDelimitedTable(tvwMain.Controller, ADelimiter, AIncludeHeader);
end;

function TfrmcxGrid.SelectionToFields(AController: TcxGridTableController;
  AQuoteItems: Boolean): string;
var
  X    : Integer;
  S, T : string;
  SL   : TStringList;
begin
  SL := TStringList.Create;
  try
    S := '';
    for X := 0 to AController.SelectedColumnCount -1 do
    begin
      T := tvwMain.Columns[AController.SelectedColumns[X].Index].Caption;
      if AQuoteItems then
        T := QuotedStr(T);
      S := S + T;
      if X < AController.SelectedColumnCount -1 then
        S := S + ',';
    end;
    SL.Add(S);
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TfrmcxGrid.SelectionToFields(AQuoteItems: Boolean): string;
begin
  Result := SelectionToFields(tvwMain.Controller, AQuoteItems);
end;
{$ENDREGION}

end.

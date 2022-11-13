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

unit DataGrabber.DataView.cxGrid;

interface

{$I DataGrabber.inc}

//{$IFDEF DEVEXPRESS}
uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ActnList, Vcl.ComCtrls,
  Data.DB,

  cxCustomData, cxGraphics, cxDataStorage, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGrid, cxGridCustomTableView, cxGridTableView, cxFilter,
  cxGridDBTableView, cxGridCustomPopupMenu, cxGridPopupMenu, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxEdit, cxNavigator, cxDBData, cxData,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxBarBuiltInMenu,
  dxDateRanges,

  DataGrabber.Interfaces, DataGrabber.DataView.Base, dxScrollbarAnnotations;

type
  TfrmcxGrid = class(TBaseDataView, IDataView, IGroupable, IMergable)
    ppmMain : TcxGridPopupMenu;
    grdMain : TcxGrid;
    tvwMain : TcxGridDBTableView;
    grlMain : TcxGridLevel;

    procedure tvwMainCustomDrawGroupSummaryCell(
      Sender       : TObject;
      ACanvas      : TcxCanvas;
      ARow         : TcxGridGroupRow;
      AColumn      : TcxGridColumn;
      ASummaryItem : TcxDataSummaryItem;
      AViewInfo    : TcxCustomGridViewCellViewInfo;
      var ADone    : Boolean
    );
    procedure tvwMainCustomDrawCell(
      Sender    : TcxCustomGridTableView;
      ACanvas   : TcxCanvas;
      AViewInfo : TcxGridTableDataCellViewInfo;
      var ADone : Boolean
    );
    procedure tvwMainCustomDrawColumnHeader(
      Sender    : TcxGridTableView;
      ACanvas   : TcxCanvas;
      AViewInfo : TcxGridColumnHeaderViewInfo;
      var ADone : Boolean
    );

  private
    FMergeColumnCells : Boolean;
    FAutoSizeCols     : Boolean;

    procedure CopySelectionToClipboard(
      AController    : TcxGridTableController;
      AIncludeHeader : Boolean = False
    );
    function SelectionToDelimitedTable(
      AController    : TcxGridTableController;
      ADelimiter     : string = #9; // TAB
      AIncludeHeader : Boolean = True
    ): string; reintroduce; overload;
    function SelectionToCommaText(
      AController : TcxGridTableController;
      AQuoteItems : Boolean = True
    ): string; reintroduce; overload;
    function SelectionToFields(
      AController : TcxGridTableController;
      AQuoteItems : Boolean = True
    ): string; reintroduce; overload;
    function SelectionToTextTable(
      AController    : TcxGridTableController;
      AIncludeHeader : Boolean = False
    ): string; reintroduce; overload;
    function SelectionToWikiTable(
      AController    : TcxGridTableController;
      AIncludeHeader : Boolean = False
    ): string; reintroduce; overload;

  protected
    {$REGION 'property access methods'}
    function GetRecordCount: Integer; override;
    function GetMergeColumnCells: Boolean;
    procedure SetMergeColumnCells(const Value: Boolean);
    function GetAutoSizeCols: Boolean;
    procedure SetAutoSizeCols(const Value: Boolean);
    function GetPopupMenu: TPopupMenu; reintroduce;
    procedure SetPopupMenu(const Value: TPopupMenu); override;
    function GetGridType: string; override;
    {$ENDREGION}

    procedure ApplyGridSettings; override;

    function ResultsToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; override;
    function ResultsToTextTable(
      AIncludeHeader: Boolean = False
    ): string; override;

    procedure AutoSizeColumns; override;
    procedure HideSelectedColumns; override;
    procedure Copy; override;
    procedure Inspect; override;
    procedure UpdateView; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    function SelectionToWikiTable(
      AIncludeHeader : Boolean = False
    ): string; overload; override;
    function SelectionToTextTable(
      AIncludeHeader : Boolean = False
    ): string; overload; override;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9; // TAB
      AIncludeHeader : Boolean = True
    ): string; overload; override;
    function SelectionToCommaText(
      AQuoteItems : Boolean = True
    ): string; overload; override;
    function SelectionToFields(
      AQuoteItems : Boolean = True
    ): string; overload; override;

    procedure MergeAllColumnCells(AActive: Boolean);
    procedure GroupBySelectedColumns;
    procedure ExpandAll;
    procedure CollapseAll;
    procedure ClearGrouping;

  public
    procedure AfterConstruction; override;

    property MergeColumnCells: Boolean
      read GetMergeColumnCells write SetMergeColumnCells;

    property AutoSizeCols: Boolean
      read GetAutoSizeCols write SetAutoSizeCols;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
  end;
//{$ENDIF}

implementation

//{$IFDEF DEVEXPRESS}
{$R *.dfm}

uses
  System.StrUtils, System.UITypes,
  Vcl.Clipbrd,

  cxGridDBDataDefinitions,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Logger, DDuce.Utils;

{$REGION 'construction and destruction'}
procedure TfrmcxGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  FAutoSizeCols := True;
  UpdateView;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmcxGrid.GetRecordCount: Integer;
begin
  Result := tvwMain.DataController.RecordCount;
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

function TfrmcxGrid.GetPopupMenu: TPopupMenu;
begin
  Result := TPopupMenu(grdMain.PopupMenu);
end;

procedure TfrmcxGrid.SetPopupMenu(const Value: TPopupMenu);
begin
  grdMain.PopupMenu := Value;
end;

function TfrmcxGrid.GetGridType: string;
begin
  Result := 'cxGrid';
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

procedure TfrmcxGrid.tvwMainCustomDrawColumnHeader(Sender: TcxGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
  var ADone: Boolean);
begin
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
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
      for X := 0 to AController.SelectedColumnCount - 1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption + ',';
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := ',';
      for X := 0 to AController.SelectedColumnCount - 1 do
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
      for X := 0 to AController.SelectedColumnCount - 1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption;
        if X < AController.SelectedColumnCount - 1 then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := '';
      for X := 0 to AController.SelectedColumnCount - 1 do
      begin
        V := AController.SelectedRows[Y].Values[AController.SelectedColumns[X].Index];
        T := VarToStr(V);
        S := S + T;
        if X < AController.SelectedColumnCount - 1 then
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
      for X := 0 to AController.SelectedColumnCount - 1 do
      begin
        S := S + tvwMain.Columns[AController.SelectedColumns[X].Index].Caption + '||';
      end;
      SL.Add(S);
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      S := '|';
      for X := 0 to AController.SelectedColumnCount - 1 do
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
      if X < CCount - 1 then
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
  X, Y    : Integer;
  S       : string;
  V       : Variant;
  F       : TField;
  I       : Integer;
  N       : Integer;
  LTxt    : string;
  LLine   : string;
  LFmt    : string;
  LWidths : array of Integer;
  SL      : TStringList;
begin
  SetLength(LWidths, AController.SelectedColumnCount);
  try
    SL := TStringList.Create;
    try
      for X := 0 to AController.SelectedColumnCount - 1 do
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
        LWidths[X] := GetMaxTextWidth(SL);
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
        N := LWidths[X];
        LFmt := '%-' + IntToStr(N) + 's';
        LLine := LLine + '+' + Format(LFmt, [DupeString('-', N)]);
        LTxt := LTxt + '|' + Format(LFmt, [F.FieldName]);
      end;
      LTxt := LTxt + '|';
      LLine := LLine + '+';
      Result := LLine + #13#10 + LTxt + #13#10 + LLine;
    end;
    for Y := 0 to AController.SelectedRowCount - 1 do
    begin
      LTxt := '';
      for X := 0 to AController.SelectedColumnCount - 1 do
      begin
        I := AController.SelectedColumns[X].Index;
        V := AController.SelectedRows[Y].Values[I];
        S := VarToStr(V);
        F := tvwMain.Columns[I].DataBinding.Field;
        if Assigned(F) then
        begin
          N := LWidths[X];
          LFmt := '%-' + IntToStr(N) + 's';
          LTxt := LTxt + '|' + Format(LFmt, [S]);
        end;
      end;
      LTxt := LTxt + '|';
      Result := Result + #13#10 + LTxt;
      LTxt := '';
    end;
    Result := Result + #13#10 + LLine;
  finally
    Finalize(LWidths);
  end;
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
    for X := 0 to AController.SelectedColumnCount - 1 do
    begin
      T := tvwMain.Columns[AController.SelectedColumns[X].Index].Caption;
      if AQuoteItems then
        T := QuotedStr(T);
      S := S + T;
      if X < AController.SelectedColumnCount - 1 then
        S := S + ',' + #13#10;
    end;
    SL.Add(S);
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmcxGrid.Copy;
begin
  Clipboard.AsText := Trim(SelectionToDelimitedTable(#9, False));
end;

procedure TfrmcxGrid.ClearGrouping;
var
  I : Integer;
begin
  for I := 0 to tvwMain.GroupedColumnCount - 1 do
  begin
    tvwMain.GroupedColumns[I].GroupIndex := -1;
  end;
end;

procedure TfrmcxGrid.CollapseAll;
begin
  tvwMain.ViewData.Collapse(True);
end;

procedure TfrmcxGrid.ExpandAll;
begin
  tvwMain.ViewData.Expand(True);
end;

procedure TfrmcxGrid.ApplyGridSettings;
var
  GL : TcxGridLines;
begin
  if Assigned(Settings) then
  begin
    GL := glNone;
    if Settings.ShowHorizontalGridLines then
    begin
      if Settings.ShowVerticalGridLines then
        GL := glBoth
      else
        GL := glHorizontal;
    end
    else if Settings.ShowVerticalGridLines then
    begin
      GL := glVertical;
    end;
    tvwMain.OptionsView.GridLines := GL;
    tvwMain.OptionsView.GroupByBox := Settings.GroupByBoxVisible;
    MergeColumnCells := Settings.MergeColumnCells;
    grdMain.Font.Assign(Settings.GridFont);
  end;
end;

procedure TfrmcxGrid.AutoSizeColumns;
var
  I : Integer;
begin
  BeginUpdate;
  try
    tvwMain.ApplyBestFit;
    for I := 0 to tvwMain.ColumnCount - 1 do
    begin
      tvwMain.Columns[I].Width := tvwMain.Columns[I].Width + 10;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmcxGrid.BeginUpdate;
begin
  tvwMain.BeginUpdate;
end;

procedure TfrmcxGrid.EndUpdate;
begin
  tvwMain.EndUpdate;
end;

procedure TfrmcxGrid.GroupBySelectedColumns;
var
  C           : TcxGridTableController;
  I           : Integer;
  LCol        : TcxGridDBColumn;
  LGroupIndex : Integer;
begin
  BeginUpdate;
  try
    C := tvwMain.Controller;
    for I := 0 to C.SelectedColumnCount - 1 do
    begin
      LCol := tvwMain.Columns[C.SelectedColumns[I].Index];
      if LCol.GroupIndex <> -1 then
        LGroupIndex := -1
      else
        LGroupIndex := tvwMain.GroupedColumnCount;
      LCol.GroupBy(LGroupIndex);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmcxGrid.HideSelectedColumns;
var
  C : TcxGridTableController;
  I : Integer;
  J : Integer;
  F : TField;
begin
  BeginUpdate;
  try
    C := tvwMain.Controller;
    for I := 0 to C.SelectedColumnCount - 1 do
    begin
      J := C.SelectedColumns[I].Index;
      tvwMain.Columns[J].Visible := False;
      F := tvwMain.Columns[J].DataBinding.Field;
      Data.HideField(F.DataSet, F.FieldName);
      if not ResultSet.HiddenFields.Contains(F) then
        ResultSet.HiddenFields.Add(F);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmcxGrid.Inspect;
begin
  InspectObject(tvwMain);
end;

procedure TfrmcxGrid.MergeAllColumnCells(AActive: Boolean);
var
  I : Integer;
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

function TfrmcxGrid.ResultsToTextTable(AIncludeHeader: Boolean): string;
begin
//
end;

function TfrmcxGrid.ResultsToWikiTable(AIncludeHeader: Boolean): string;
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
      for X := 0 to tvwMain.VisibleColumnCount - 1 do
      begin
        S := S + tvwMain.VisibleColumns[X].Caption + '||';
      end;
      SL.Add(S);
    end;
    for Y := 0 to tvwMain.ViewData.RowCount - 1 do
    begin
      S := '|';
      for X := 0 to tvwMain.VisibleColumnCount - 1 do
      begin
        V := tvwMain.ViewData.Rows[Y].Values[tvwMain.VisibleColumns[X].Index];
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

function TfrmcxGrid.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin
  Result :=
    SelectionToDelimitedTable(tvwMain.Controller, ADelimiter, AIncludeHeader);
end;

function TfrmcxGrid.SelectionToFields(AQuoteItems: Boolean): string;
begin
  Result := SelectionToFields(tvwMain.Controller, AQuoteItems);
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

procedure TfrmcxGrid.UpdateView;
begin
  if Assigned(DataSet) and DataSet.Active then
  begin
    tvwMain.ClearItems;
    tvwMain.DataController.CreateAllItems;
    BeginUpdate;
    try
      ApplyGridSettings;
      AutoSizeColumns;
    finally
      EndUpdate;
    end;
  end;
end;
{$ENDREGION}
//{$ENDIF}
end.

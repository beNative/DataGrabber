{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.DataView.KGrid;

{
  Issues:
    - column moving does not move its content, so it is disabled and not
      supported.
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Dialogs,
  Data.DB,

  KControls, KGrids, KDBGrids,

  DataGrabber.Interfaces, DataGrabber.DataView.Base;

type
  TfrmKGrid = class(TBaseDataView, IDataView)
    grdMain : TKDBGrid;

    procedure grdMainDrawCell(
      Sender : TObject;
      ACol   : Integer;
      ARow   : Integer;
      R      : TRect;
      State  : TKGridDrawState
    );
    procedure grdMainCustomSortRows(
      Sender     : TObject;
      ByIndex    : Integer;
      SortMode   : TKGridSortMode;
      var Sorted : Boolean
    );

  protected
    {$REGION 'property access methods'}
    function GetGridType: string; override;
    procedure SetPopupMenu(const Value: TPopupMenu); override;
    {$ENDREGION}
    procedure NormalizeRect(var SR: TKGridRect);
    procedure UpdateColumns;

  public
    procedure AfterConstruction; override;

    function SelectionToCommaText(
      AQuoteItems: Boolean = True
    ): string; override;
    function SelectionToTextTable(
      AIncludeHeader: Boolean = False
    ): string; override;
    function SelectionToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; override;
    function SelectionToFields(
      AQuoteItems: Boolean = True
    ): string; override;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string; override;

    procedure ApplyGridSettings; override;
    procedure AutoSizeColumns; override;

    procedure Copy; override;
    procedure HideSelectedColumns; override;
    procedure MergeAllColumnCells(AActive: Boolean);

    procedure UpdateView; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure Inspect; override;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils, System.Math, System.UITypes,
  Vcl.Clipbrd,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Utils,

  KFunctions,

  DataGrabber.Utils;

{$REGION 'construction and destruction'}
procedure TfrmKGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  grdMain.MinRowHeight             := 17;
  grdMain.DefaultRowHeight         := 17;
  grdMain.Colors.FocusedRangeBkGnd := clGray;
  UpdateView;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmKGrid.GetGridType: string;
begin
  Result := 'KGrid';
end;

procedure TfrmKGrid.SetPopupMenu(const Value: TPopupMenu);
begin
  grdMain.PopupMenu := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'grdMain'}
procedure TfrmKGrid.grdMainCustomSortRows(Sender: TObject; ByIndex: Integer;
  SortMode: TKGridSortMode; var Sorted: Boolean);
var
  Field : TField;
begin
  Field := DataSet.FieldByName(TKDBGridCol(grdMain.Columns[ByIndex]).FieldName);
  if Assigned(Field) and (Field.FieldKind = fkData) then
  begin
    case SortMode of
      smNone, smDown:
        Data.Sort(DataSet, Field.FieldName, False);
      smUp:
      begin
        Data.Sort(DataSet, Field.FieldName, True);
      end;
    end;
    Sorted := True;
    DataSet.First;
  end
end;

procedure TfrmKGrid.grdMainDrawCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
var
  C : TCanvas;
  F : TField;
  D : Double;
begin
  C := grdMain.CellPainter.Canvas;
  if ARow = 0 then
  begin
    C.Font.Style := C.Font.Style + [fsBold];
  end;
  grdMain.Cell[ACol, ARow].ApplyDrawProperties;
  if Settings.GridCellColoring and
    (State * [gdSelected, gdFixed, gdMouseOver] = []) then
  begin
    F := DataSet.FindField(TKDBGridCol(grdMain.Cols[ACol]).FieldName);
    if Assigned(F) then
    begin
      if F is TNumericField then
      begin
        D := F.AsFloat;
        if IsZero(D) then
          C.Font.Color := clBlue
        else if D < 0 then
          C.Font.Color := clRed;
      end;
      if grdMain.CellPainter.Text = '' then
      begin
        C.Brush.Color := $00EFEFEF;
      end
      else
      begin
        C.Brush.Color := Settings.FieldTypeColors[F.DataType];
      end;
    end;
  end
  else if gdSelected in State then
  begin
    C.Brush.Color := clGray;
    C.Font.Color  := clWhite;
  end;

  C.FillRect(R);
  grdMain.CellPainter.DefaultDraw;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmKGrid.NormalizeRect(var SR: TKGridRect);
var
  T: Integer;
begin
  if SR.Row1 > SR.Row2 then
  begin
    T := SR.Row1;
    SR.Row1 := SR.Row2;
    SR.Row2 := T;
  end;
  if SR.Col1 > SR.Col2 then
  begin
    T := SR.Col1;
    SR.Col1 := SR.Col2;
    SR.Col2 := T;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmKGrid.ApplyGridSettings;
begin
  if Settings.ShowHorizontalGridLines then
    grdMain.Options := grdMain.Options + [goHorzLine]
  else
    grdMain.Options := grdMain.Options - [goHorzLine];

  if Settings.ShowVerticalGridLines then
    grdMain.Options := grdMain.Options + [goVertLine]
  else
    grdMain.Options := grdMain.Options - [goVertLine];
  grdMain.Font.Assign(Settings.GridFont);
end;

procedure TfrmKGrid.AutoSizeColumns;
var
  C : TKDBGridCol;
  I : Integer;
  CF: TKCurrencyFormat;
begin
  BeginUpdate;
  try
    CF.CurrencyFormat   := FormatSettings.CurrencyFormat;
    CF.CurrencyDecimals := FormatSettings.CurrencyDecimals;
    CF.CurrencyString   := '';
    CF.DecimalSep       := FormatSettings.DecimalSeparator;
    CF.ThousandSep      := FormatSettings.ThousandSeparator;
    CF.UseThousandSep   := True;
    grdMain.AutoSizeGrid(mpColWidth);
    for I := 0 to grdMain.ColCount - 1 do
    begin
      grdMain.ColWidths[I] := grdMain.ColWidths[I] + 12;
      C := TKDBGridCol(grdMain.Cols[I]);
      C.CurrencyFormat := CF;
      C.Font.Assign(grdMain.Font);
      C.TitleFont.Assign(grdMain.Font);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmKGrid.Copy;
begin
  Clipboard.AsText := Trim(SelectionToDelimitedTable(#9, False));
end;

{ A unique feature of this grid is that it allows to have multiple distinct
  selections. }

procedure TfrmKGrid.HideSelectedColumns;
var
  I  : Integer;
  N  : Integer;
  SR : TKGridRect;
  F  : TField;
begin
  SR := grdMain.Selection;
  BeginUpdate;
  try
    for N := 0 to grdMain.SelectionCount - 1 do
    begin
      SR := grdMain.Selections[N];
      NormalizeRect(SR);
      for I := SR.Col1 to SR.Col2 do
      begin
        F := DataSet.FieldByName(TKDBGridCol(grdMain.Cols[I]).FieldName);
        Data.HideField(DataSet, F.FieldName);
        if not ResultSet.HiddenFields.Contains(F) then
          ResultSet.HiddenFields.Add(F);
        grdMain.Cols[I].Visible := False;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmKGrid.Inspect;
begin
  InspectComponent(grdMain);
end;

procedure TfrmKGrid.MergeAllColumnCells(AActive: Boolean);
begin
  // TODO
end;

function TfrmKGrid.SelectionToCommaText(AQuoteItems: Boolean): string;
var
  X, Y   : Integer;
  S, T   : string;
  SR     : TKGridRect;
  CCount : Integer;
begin
  S := '';
  SR := grdMain.Selection;
  NormalizeRect(SR);
  CCount := SR.Col2 - SR.Col1;
  for Y := SR.Row1 to SR.Row2 do
  begin
    for X := SR.Col1 to SR.Col2 do
    begin
      T := grdMain.Cells[X, Y];
      if AQuoteItems then
        T := QuotedStr(T);
      S := S + T;
      if X < SR.Col2 then
        S := S + ', ';
    end;
    if (CCount = 1) and (Y < SR.Row2) then
      S := S + ', '
    else if Y < SR.Row2 then
      S := S + #13#10
  end;
  Result := S;
end;

function TfrmKGrid.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
var
  X, Y : Integer;
  S, T : string;
  SL   : TStringList;
  SR   : TKGridRect;
begin
  SL := TStringList.Create;
  try
    S := '';
    SR := grdMain.Selection;
    NormalizeRect(SR);
    if AIncludeHeader then
    begin
      for X := SR.Col1 to SR.Col2 do
      begin
        S := S + grdMain.Cells[X, 0];
        if X < SR.Col2 then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    for Y := SR.Row1 to SR.Row2 do
    begin
      S := '';
      for X := SR.Col1 to SR.Col2 do
      begin
        T := grdMain.Cells[X, Y];
        S := S + T;
        if X < SR.Col2 then
          S := S + ADelimiter;
      end;
      SL.Add(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TfrmKGrid.SelectionToFields(AQuoteItems: Boolean): string;
begin
// TODO
end;

{ does not take hidden columns into account! }

function TfrmKGrid.SelectionToTextTable(AIncludeHeader: Boolean): string;
var
  X, Y   : Integer;
  S      : string;
  N      : Integer;
  sTxt   : string;
  sLine  : string;
  sFmt   : string;
  Widths : array of Integer;
  SL     : TStringList;
  SR     : TKGridRect;
begin
  BeginUpdate;
  try
    sLine := '';
    sTxt := '';
    SR := grdMain.Selection;
    NormalizeRect(SR);
    SetLength(Widths, SR.Col2 - SR.Col1);
    try
      SL := TStringList.Create;
      try
        for X := SR.Col1 to SR.Col2 do
        begin
          SL.Clear;
          if AIncludeHeader then
          begin
            SL.Add(TKDBGridCol(grdMain.Cols[X]).FieldName);
          end;
          for Y := SR.Row1 to SR.Row2 do
          begin
            S := grdMain.Cells[X, Y];
            SL.Add(S);
          end;
          Widths[X] := GetMaxTextWidth(SL);
        end;
      finally
        FreeAndNil(SL);
      end;

      if AIncludeHeader then
      begin
        for X := SR.Col1 to SR.Col2 do
        begin
          N := Widths[X];
          sFmt := '%-' + IntToStr(N) + 's';
          sLine := sLine + '+' + Format(sFmt, [DupeString('-', N)]);
          sTxt := sTxt + '|' + Format(sFmt, [TKDBGridCol(grdMain.Cols[X]).FieldName]);
        end;
        sTxt := sTxt + '|';
        sLine := sLine + '+';
        Result := sLine + #13#10 + sTxt + #13#10 + sLine;
      end;
      for Y := SR.Row1 to SR.Row2 do
      begin
        sTxt := '';
        for X := SR.Col1 to SR.Col2 do
        begin
          S := grdMain.Cells[X, Y];
          N := Widths[X];
          sFmt := '%-' + IntToStr(N) + 's';
          sTxt := sTxt + '|' + Format(sFmt, [S]);
        end;
        sTxt := sTxt + '|';
        Result := Result + #13#10 + sTxt;
        sTxt := '';
      end;
      Result := Result + #13#10 + sLine;
    finally
      Finalize(Widths);
    end;
  finally
    EndUpdate;
  end;
end;

function TfrmKGrid.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin
// TODO
end;

procedure TfrmKGrid.UpdateColumns;
var
  I : Integer;
  F : TField;
begin
  for I := 0 to grdMain.ColCount - 1 do
  begin
    F := DataSet.FindField(TKDBGridCol(grdMain.Cols[I]).FieldName);
    if Assigned(F) then
      grdMain.Cols[I].Visible := not ResultSet.HiddenFields.Contains(F);
  end;
end;

procedure TfrmKGrid.UpdateView;
begin
  BeginUpdate;
  try
    if Assigned(DataSet) and DataSet.Active then
    begin
      ApplyGridSettings;
      UpdateColumns;
      AutoSizeColumns;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmKGrid.EndUpdate;
begin
  grdMain.UnlockSortMode;
  grdMain.UnlockUpdate;
end;

procedure TfrmKGrid.BeginUpdate;
begin
  grdMain.LockSortMode;
  grdMain.LockUpdate;
end;
{$ENDREGION}

end.

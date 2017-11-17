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

unit DataGrabber.DataView.KGrid;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Dialogs,
  Data.DB,

  KControls, KGrids, KDBGrids,

  ts.Interfaces,

  DataGrabber.Interfaces;

type
  TfrmKGrid = class(TForm, IDataView, IDGDataView)
    grdMain : TKDBGrid;
    dscMain : TDataSource;

    procedure grdMainDrawCell(
      Sender     : TObject;
      ACol, ARow : Integer;
      R          : TRect;
      State      : TKGridDrawState
    );

  private
    FData     : IData;
    FSettings : IDataViewSettings;

    function GetDataSet: TDataSet;
    function GetRecordCount: Integer;
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);

    procedure NormalizeRect(var SR: TKGridRect);
    function GetData: IData;
    procedure SetData(const Value: IData);

    function GetGridType: string;
    function GetName: string;

  protected
    procedure SetPopupMenu(const Value: TPopupMenu);

  public
    function SelectionToCommaText(AQuoteItems: Boolean = True): string;
    function SelectionToDelimitedTable(ADelimiter : string = #9;
      AIncludeHeader: Boolean = True): string;
    function SelectionToTextTable(AIncludeHeader: Boolean = False): string;
    function SelectionToWikiTable(AIncludeHeader: Boolean = False): string;
    function SelectionToFields(AQuoteItems: Boolean = True): string;
    procedure ApplyGridSettings;
    procedure AutoSizeColumns;
    procedure Copy;
    procedure HideSelectedColumns;
    procedure MergeAllColumnCells(AActive: Boolean);
    procedure UpdateView;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AssignParent(AParent: TWinControl);
    procedure AfterConstruction; override;

    procedure Inspect;
    procedure BeforeDestruction; override;

    property DataSet: TDataSet
      read GetDataSet;

    property Data: IData
      read GetData write SetData;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property RecordCount: Integer
      read GetRecordCount;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils, System.Math,
  Vcl.Clipbrd,

  DDuce.ObjectInspector.zObjectInspector,

  KFunctions,

  DataGrabber.Utils;

{$REGION 'construction and destruction'}
procedure TfrmKGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  grdMain.Font.Name                := 'Callibri';
  grdMain.Colors.FocusedRangeBkGnd := clGray;
end;

procedure TfrmKGrid.BeforeDestruction;
begin
  if Assigned(FData) then
    (FData as IDataViews).UnRegisterDataView(Self);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmKGrid.GetGridType: string;
begin
  Result := 'GridView';
end;

function TfrmKGrid.GetName: string;
begin
  Result := inherited Name;
end;

procedure TfrmKGrid.SetPopupMenu(const Value: TPopupMenu);
begin
  grdMain.PopupMenu := Value;
end;

function TfrmKGrid.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmKGrid.SetData(const Value: IData);
begin
  if Value <> Data then
  begin
    FData := Value;
    (FData as IDataViews).RegisterDataView(Self);
    dscMain.DataSet := Data.DataSet;
    UpdateView;
  end;
end;

function TfrmKGrid.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

function TfrmKGrid.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TfrmKGrid.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmKGrid.SetSettings(const Value: IDataViewSettings);
begin
  FSettings := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmKGrid.grdMainDrawCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
var
  C: TCanvas;
  F : TField;
  D : Double;
begin
  C := grdMain.CellPainter.Canvas;
  if ARow = 0 then
  begin
    C.Font.Style := C.Font.Style + [fsBold];
  end;

  grdMain.Cell[ACol, ARow].ApplyDrawProperties;
  if Settings.GridCellColoring and (State = []) then
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
  end;
  C.FillRect(R);
  grdMain.CellPainter.DefaultDraw;
end;
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
  if FSettings.ShowHorizontalGridLines then
    grdMain.Options := grdMain.Options + [goHorzLine]
  else
    grdMain.Options := grdMain.Options - [goHorzLine];

  if FSettings.ShowVerticalGridLines then
    grdMain.Options := grdMain.Options + [goVertLine]
  else
    grdMain.Options := grdMain.Options - [goVertLine];
end;

procedure TfrmKGrid.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
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
      grdMain.ColWidths[I] := grdMain.ColWidths[I] + 4;
      C := TKDBGridCol(grdMain.Cols[I]);
      C.CurrencyFormat := CF;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmKGrid.Copy;
begin
  Clipboard.AsText := Trim(SelectionToDelimitedTable(#9, False));
end;

procedure TfrmKGrid.HideSelectedColumns;
var
  I  : Integer;
  SR : TKGridRect;
begin
  SR := grdMain.Selection;
  NormalizeRect(SR);
  BeginUpdate;
  try
    for I := SR.Col1 to SR.Col2 do
    begin
      grdMain.Cols[I].Visible := False;
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

procedure TfrmKGrid.UpdateView;
begin
  BeginUpdate;
  try
    dscMain.DataSet := Data.DataSet;
    if Assigned(DataSet) and DataSet.Active then
    begin
      ApplyGridSettings;
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

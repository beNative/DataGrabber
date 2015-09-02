unit ts.Modules.List.Settings;

{ Persistence support for IList instances. }

//*****************************************************************************

interface

uses
  Classes,

  DB,

  Ex_DBGrid,

  ts.Modules.List.Columns, tsXMLSettings, ts.Classes.ListReport,
  ts.Classes.ListReport.Columns,

  ts.Modules.Interfaces;

//=============================================================================

const
  DEFAULT_GRID_FONTSIZE = 8;

//=============================================================================

type
  TtsListSettings = class(TComponent, IListSettings)
  private
    FSettings        : TtsXMLSettings;
    FColumns         : TtsListColumns;
    FAutoSizeColumns : Boolean;
    FGridFontSize    : Integer;
    FDBGridView      : TDBGridView;
    FModified        : Boolean;

    //function GetTextWidth : Integer
  protected
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    function GetGridFontSize: Integer;
    procedure SetGridFontSize(const Value: Integer);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetDBGridView: TDBGridView;
    procedure SetDBGridView(const Value: TDBGridView);
    function GetAutoSizeColumns: Boolean;
    procedure SetAutoSizeColumns(const Value: Boolean);
    function GetColumns : TtsListColumns;

    procedure AssignFromField(AField: TField; AColumn: TtsListColumn);
    function AssignToField(AField: TField) : Boolean;
    procedure AssignFromGridColumn(AGridColumn : TDBGridColumn;
                                   AColumn     : TtsListColumn;
                                   AGrid       : TDBGridView);
    procedure AssignToGridColumn(AColumn     : TtsListColumn;
                                 AGridColumn : TDBGridColumn;
                                 AGrid       : TDBGridView);
    procedure AssignFromReportColumn(AReportColumn : TtsListReportColumn;
                                     AColumn       : TtsListColumn;
                                     AReport       : TtsFRListReport);
    procedure AssignToReportColumn(AColumn       : TtsListColumn;
                                   AReportColumn : TtsListReportColumn;
                                   AReport       : TtsFRListReport);

    procedure OnColumsUpdate(ASender : TObject; AColumn : TtsListColumn);

    procedure Changed;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Load: Boolean;
    procedure Save;

    function IsEmpty : Boolean;

    procedure AssignFromDataSet(ADataSet : TDataSet);
    procedure AssignToDataSet(ADataSet : TDataSet);
    procedure AssignFromGridView(AGridView : TDBGridView);
    procedure AssignToGridView(AGridView : TDBGridView);
    procedure AssignFromReport(AReport : TtsFRListReport);
    procedure AssignToReport(AReport : TtsFRListReport);

    { Optional TDBGridView reference to preview the current settings. }
    property DBGridView : TDBGridView
      read GetDBGridView write SetDBGridView;

    property FileName : string
      read GetFileName write SetFileName;

    property Modified: Boolean
      read GetModified write SetModified;

  published
    property Columns : TtsListColumns
      read GetColumns;

    property GridFontSize : Integer
      read GetGridFontSize write SetGridFontSize default DEFAULT_GRID_FONTSIZE;

    property AutoSizeColumns : Boolean
      read GetAutoSizeColumns write SetAutoSizeColumns default True;
  end;

//*****************************************************************************

implementation

uses
  SysUtils,

  ts.Classes.Alignment, ts.Utils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TtsListSettings.Create(AOwner: TComponent);
begin
  inherited;
  FSettings         := TtsXMLSettings.Create(Self);
  FColumns          := TtsListColumns.Create(Self);
  FColumns.OnUpdate := OnColumsUpdate;
  FGridFontSize     := DEFAULT_GRID_FONTSIZE;
  FAutoSizeColumns  := True;
  FModified         := False;
end;

//-----------------------------------------------------------------------------

destructor TtsListSettings.Destroy;
begin
  FColumns.Free;
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|AutoSizeColumns|---------------------------------------------------------

function TtsListSettings.GetAutoSizeColumns: Boolean;
begin
  Result := FAutoSizeColumns;
end;

procedure TtsListSettings.SetAutoSizeColumns(const Value: Boolean);
begin
  if Value <> AutoSizeColumns then
  begin
    FAutoSizeColumns := Value;
    if Value and Assigned(FDBGridView) then
      FDBGridView.AutoSizeCols;
    Changed;
  end;
end;

//-----------------------------------------------------------------------------

function TtsListSettings.GetDBGridView: TDBGridView;
begin
  Result := FDBGridView;
end;

procedure TtsListSettings.SetDBGridView(const Value: TDBGridView);
begin
  FDBGridView := Value;
end;

//---|Columns|-----------------------------------------------------------------

function TtsListSettings.GetColumns: TtsListColumns;
begin
  Result := FColumns;
end;

//---|FileName|----------------------------------------------------------------

function TtsListSettings.GetFileName: string;
begin
  Result := FSettings.FileName;
end;

procedure TtsListSettings.SetFileName(const Value: string);
begin
  if Value <> FileName then
  begin
    FSettings.FileName := Value;
  end;
end;

//---|GridFontSize|------------------------------------------------------------

function TtsListSettings.GetGridFontSize: Integer;
begin
  Result := FGridFontSize;
end;

procedure TtsListSettings.SetGridFontSize(const Value: Integer);
begin
  if Value <> GridFontSize then
  begin
    FGridFontSize := Value;
    if Assigned(FDBGridView) then
      FDBGridView.Font.Size := Value;
    Changed;
  end;
end;

//---|Modified|----------------------------------------------------------------

function TtsListSettings.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TtsListSettings.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TtsListSettings.Changed;
begin
  FModified := True;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignFromField(AField: TField; AColumn:
  TtsListColumn);
begin
  AColumn.Caption   := AField.DisplayLabel;
  AColumn.Alignment := AField.Alignment;
  AColumn.Visible   := AField.Visible;
  AColumn.Name      := AField.FieldName;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignToDataSet(ADataSet: TDataSet);
begin
//
end;

{ Assigns column settings to the given field object. Returns False if no
  settings for the field were found. }

function TtsListSettings.AssignToField(AField: TField): Boolean;
var
  C : TtsListColumn;
begin
  if not IsEmpty then
  begin
    C := Columns.Find(AField.FieldName);
    if Assigned(C) then
    begin
      AField.Alignment    := C.Alignment;
      AField.DisplayLabel := C.Caption;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignFromGridColumn(AGridColumn: TDBGridColumn;
  AColumn: TtsListColumn; AGrid: TDBGridView);
var
  N : Integer;
begin
  N := GetTextWidth('O', AGrid.Font);
  AColumn.Caption   := AGridColumn.Caption;
  AColumn.Alignment := AGridColumn.Alignment;
  AColumn.Width     := AGridColumn.DefWidth div N;
  AColumn.MinWidth  := AGridColumn.MinWidth div N;
  AColumn.MaxWidth  := AGridColumn.MaxWidth div N;
  AColumn.Visible   := AGridColumn.Visible;
  AColumn.Name      := AGridColumn.FieldName;
  AColumn.FixedSize := AGridColumn.FixedSize;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignToGridColumn(AColumn: TtsListColumn;
  AGridColumn: TDBGridColumn; AGrid: TDBGridView);
var
  N : Integer;
begin
  N := GetTextWidth('O', AGrid.Font);
  AGridColumn.Caption   := AColumn.Caption;
  AGridColumn.Alignment := AColumn.Alignment;
  AGridColumn.DefWidth  := AColumn.Width * N;
  AGridColumn.MinWidth  := AColumn.MinWidth * N;
  AGridColumn.MaxWidth  := AColumn.MaxWidth * N;
  AGridColumn.Visible   := AColumn.Visible;
  AGridColumn.Index     := AColumn.Index;
  AGridColumn.FixedSize := AColumn.FixedSize;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignFromReportColumn(
  AReportColumn: TtsListReportColumn; AColumn: TtsListColumn;
    AReport: TtsFRListReport);
var
  N : Integer;
begin
  N := GetTextWidth('O', AReport.DefaultFont);
  AColumn.Caption   := AReportColumn.Title;
  AColumn.Alignment := TAlignment(AReportColumn.DataAlignment.Horizontal);
  AColumn.Width     := Round(AReportColumn.Width) div N;
  AColumn.MinWidth  := Round(AReportColumn.MinWidth) div N;
  AColumn.MaxWidth  := Round(AReportColumn.MaxWidth) div N;
  AColumn.Visible   := AReportColumn.Visible;
  AColumn.Index     := AReportColumn.Index;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignToReportColumn(AColumn: TtsListColumn;
  AReportColumn: TtsListReportColumn; AReport: TtsFRListReport);
var
  A : THorizontalAlignment;
  N : Integer;
begin
  N := GetTextWidth('O', AReport.DefaultFont);
  AReportColumn.Title    := AColumn.Caption;
  AReportColumn.Width    := AColumn.Width * N;
  AReportColumn.MinWidth := AColumn.MinWidth * N;
  AReportColumn.MaxWidth := AColumn.MaxWidth * N;
  AReportColumn.Visible  := AColumn.Visible;
  AReportColumn.Index    := AColumn.Index;
  A := THorizontalAlignment(AColumn.Alignment);
  AReportColumn.DataAlignment.Horizontal   := A;
  AReportColumn.TitleAlignment.Horizontal  := A;
  AReportColumn.FooterAlignment.Horizontal := A;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TtsListSettings.AssignFromGridView(AGridView: TDBGridView);
var
  I : Integer;
  C : TtsListColumn;
begin
  Columns.BeginUpdate;
  try
    for I := 0 to AGridView.Columns.Count - 1 do
    begin
      C := Columns.Find(AGridView.Columns[I].FieldName);
      if not Assigned(C) then
        C := Columns.Add;
      AssignFromGridColumn(AGridView.Columns[I], C, AGridView)
    end;
  finally
    Columns.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignToGridView(AGridView: TDBGridView);
var
  I : Integer;
  C : TDBGridColumn;
begin
  AGridView.Columns.BeginUpdate;
  try
    for I := 0 to Columns.Count - 1 do
    begin
      C := AGridView.Columns.Find(Columns[I].Name);
      if Assigned(C) then
        AssignToGridColumn(Columns[I], C, AGridView);
    end;
    AGridView.Font.Size := GridFontSize;
    if AutoSizeColumns then
      AGridView.AutoSizeCols;
  finally
    AGridView.Columns.EndUpdate;
    AGridView.Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignFromDataSet(ADataSet: TDataSet);
var
  I : Integer;
  C : TtsListColumn;
begin
  Columns.BeginUpdate;
  try
    for I := 0 to ADataSet.Fields.Count - 1 do
    begin
      C := Columns.Find(ADataSet.Fields[I].FieldName);
      if not Assigned(C) then
        C := Columns.Add;
      AssignFromField(ADataSet.Fields[I], C);
    end;
  finally
    Columns.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignFromReport(AReport: TtsFRListReport);
var
  I : Integer;
  C : TtsListColumn;
begin
  Columns.BeginUpdate;
  try
    for I := 0 to AReport.Columns.Count - 1 do
    begin
      C := Columns.Find(AReport.Columns[I].FieldName);
      if not Assigned(C) then
        C := Columns.Add;
      AssignFromReportColumn(AReport.Columns[I], C, AReport)
    end;
  finally
    Columns.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.AssignToReport(AReport: TtsFRListReport);
var
  I : Integer;
  C : TtsListReportColumn;
begin
  AReport.Columns.BeginUpdate;
  try
    for I := 0 to Columns.Count - 1 do
    begin
      C := AReport.Columns.FindField(Columns[I].Name);
      if Assigned(C) then
      begin
        AssignToReportColumn(Columns[I], C, AReport);
      end;
    end;
  finally
    AReport.Columns.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------

function TtsListSettings.IsEmpty: Boolean;
begin
  Result := Columns.Count = 0;
end;

//-----------------------------------------------------------------------------

{ Load from file. }

function TtsListSettings.Load: Boolean;
begin
  Result := FSettings.LoadComponent(Self);
  FModified := False;
end;

//-----------------------------------------------------------------------------

procedure TtsListSettings.OnColumsUpdate(ASender: TObject;
  AColumn: TtsListColumn);
var
  C : TDBGridColumn;
begin
  if Assigned(AColumn) and Assigned(FDBGridView) then
  begin
    C := FDBGridView.Columns.Find(AColumn.Name);
    if Assigned(C) then
    begin
      AssignToGridColumn(AColumn, C, FDBGridView);
      Changed;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{ Save to file. }

procedure TtsListSettings.Save;
begin
  FSettings.SaveComponent(Self);
  FModified := False;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.

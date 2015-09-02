unit ts.Modules.List.Report;

// REMARK: Not in use yet!

//*****************************************************************************

interface

uses
  DB,

  ts.Interfaces, ts.Classes.ListReport, ts.Classes.ListReport.Columns;

//=============================================================================

type
  TListReport = class(TAggregatedObject, IListReport)
  private
    FData        : IData;
    FPrepared    : Boolean;   // indicates if the report has been prepared
    FReportTitle : string;
    FReport      : TtsFRListReport;

  protected
    function GetData: IData;
    function GetDataSet: TDataSet;
    function GetReportTitle: string;
    procedure SetReportTitle(const Value: string);
    function GetPrepared: Boolean;
    procedure SetPrepared(const Value: Boolean);

    procedure ReportGetColumnProperties(Sender: TObject;
      AColumn: TtsListReportColumn);

    procedure InitReportColumn(AColumn : TtsListReportColumn); virtual;

    procedure PrepareReport;
    procedure PrintReport;
    procedure PreviewReport;
    procedure DesignReport;
    procedure LayoutReport;
    procedure ExportReport(      AExportFilter : TExportFilter;
                           const AFileName     : string = '');
    procedure EditProperties;

    property ReportTitle : string
      read GetReportTitle write SetReportTitle;

    property Prepared : Boolean
      read GetPrepared write SetPrepared;

    property DataSet : TDataSet
      read GetDataSet;

    property Data : IData
      read GetData;

    public
      constructor Create(const AData : IData);
      destructor Destroy; override;
  end;

//*****************************************************************************

implementation

uses
  SysUtils, Forms, Controls;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TListReport.Create(const AData: IData);
begin
  inherited Create(AData);
  FData := AData;
  FReport := TtsFRListReport.Create(nil);
  FReport.OnGetColumnProperties := ReportGetColumnProperties;
end;

//-----------------------------------------------------------------------------

destructor TListReport.Destroy;
begin
  FreeAndNil(FReport);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Data|--------------------------------------------------------------------

function TListReport.GetData: IData;
begin
  Result := Controller as IData;
end;

//---|DataSet|-----------------------------------------------------------------

function TListReport.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

//---|Prepared|----------------------------------------------------------------

function TListReport.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TListReport.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;

//---|ReportTitle|-------------------------------------------------------------

function TListReport.GetReportTitle: string;
begin
  Result := FReportTitle;
end;

procedure TListReport.SetReportTitle(const Value: string);
begin
  FReportTitle := Value;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TListReport.DesignReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    FReport.DesignReport;
  finally
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TListReport.EditProperties;
begin

end;

//-----------------------------------------------------------------------------

procedure TListReport.ExportReport(AExportFilter: TExportFilter;
  const AFileName: string);
var
  S : string;
begin
  Screen.Cursor := crSQLWait;
  DataSet.DisableControls;
  try
    PrepareReport;
    if AFileName = '' then
      S := 'Export'
    else
      S := AFileName;
    FReport.ExportReport(AExportFilter, S, True);
  finally
    Screen.Cursor := crDefault;
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TListReport.InitReportColumn(AColumn: TtsListReportColumn);
begin
  if AColumn.DataType = dtNumeric then
  begin
    AColumn.MinWidth := 40;
    AColumn.FooterVisible := True;
    AColumn.FooterExpression :=
      Format('[SUM(<frxDataSet."%s">,DataBand)]', [AColumn.FieldName]);
  end
  else
    AColumn.FooterVisible := False;

  if AColumn.DataType = dtString then
  begin
    AColumn.MinWidth := 64;
    AColumn.MaxWidth := 300;
    AColumn.DataAlignment.WordWrap := True;
  end
  else if AColumn.DataType = dtDateTime then
    AColumn.MinWidth := 64;
end;

//-----------------------------------------------------------------------------

procedure TListReport.LayoutReport;
begin
  PrepareReport;
end;

//-----------------------------------------------------------------------------

procedure TListReport.PrepareReport;
begin
  if not Data.Executed then
    Data.Execute;

  if not Prepared then
  begin
    FReport.ClearReport;
    FReport.AssignColumnsFromDataSet;
    FReport.ReportTitle    := ReportTitle;
//    FReport.ReportSubtitle := GenerateReportSubtitle + #13#10 + ' ';
//    FReport.CompanyName    := (Connection as ICompanyData).CompanyName;
//    FReport.CompanyLogo.Bitmap.Assign((Connection as ICompanyData).CompanyLogo);
    FReport.BuildReport;
    Prepared := True;
  end;
end;

//-----------------------------------------------------------------------------

procedure TListReport.PreviewReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    FReport.PreviewReport(False);
  finally
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TListReport.PrintReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
//    if Data.Params['prmPrinter'] <> Unassigned then
//      FReport.Report.PrintOptions.Printer := Data.Params['prmPrinter'];
    FReport.PrintReport;
  finally
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TListReport.ReportGetColumnProperties(Sender: TObject;
  AColumn: TtsListReportColumn);
begin
  InitReportColumn(AColumn);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.


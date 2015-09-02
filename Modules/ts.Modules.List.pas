{ $Id: DCustomList.pas,v 1.2 2009/01/08 08:08:37 Tim Sinaeve Exp $ }

unit ts.Modules.List;

//*****************************************************************************

{
  TODO
    - implement IListReport, IListExport in a dedicated class and implement the
      interface through delegation.
    -> implementation can be done in TdmCustomModule, where the object
       FListSelection is only created on demand.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  DB, DBClient, Provider, ADODB,

  ts.Classes.ListReport, ts.Classes.ListReport.Columns, ts.Classes.KeyValues,
  ts.Classes.ModuleManager,

  ts.Modules.Data, ts.Modules.Interfaces, ts.Modules.List.Selection;

//=============================================================================

type
  TdmCustomList = class (TdmCustomModule, IListData,
                                          IListSelection,
                                          IListReport,
                                          IListExport)

  private
    FListSelection     : TListSelection;
    FPrepared          : Boolean;   // indicates if the report has been prepared
    FReportTitle       : string;
    FMaxSelectionCount : Integer;
    rptMain            : TtsFRListReport;

  protected
    // protected property access methods
    function GetReportTitle: string;
    procedure SetReportTitle(const Value: string);
    function GetPrepared: Boolean;
    procedure SetPrepared(const Value: Boolean);

   procedure rptMainGetColumnProperties(Sender: TObject;
      AColumn: TtsListReportColumn);

    // protected property access methods
    function GetMaxSelectionCount: Integer;
    procedure SetMaxSelectionCount(const Value: Integer);
    procedure SetExecuted(const Value: Boolean); override;

    // protected virtual methods
    procedure Initialize; override;
    function IsActiveCriteriumField(const AFieldName : string) : Boolean;
      virtual;

    function GetRowID : Variant; override;

    function IsLookupField(const AFieldName: string): Boolean; override;
    procedure InternalExecute(const ACommandText : string); override;

    procedure InitReportColumn(AColumn : TtsListReportColumn); virtual;
    function GenerateReportSubtitle : string; virtual;

    // protected methods
    { IListReport }
    procedure PrepareReport; virtual;

  public
    // construction and destruction
    destructor Destroy; override;

    // public methods
    procedure Execute(AShowSQL : Boolean = False); override;

    { IListReport }
    procedure PrintReport;
    procedure PreviewReport;
    procedure DesignReport;
    procedure LayoutReport;
    procedure ExportReport(      AExportFilter : TExportFilter;
                           const AFileName     : string = '');

    { IListExport }
    procedure ExportExcel;
    procedure ExportText;

    procedure EditProperties;

    // public properties
    property Prepared : Boolean
      read GetPrepared write SetPrepared;

    { IListReport }
    property ReportTitle : string
      read GetReportTitle write SetReportTitle;

    { IListSelection }
    property ListSelection: TListSelection
      read FListSelection implements IListSelection;
  end;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  JclStrings,

  ts.DBUtils, ts.ExportUtils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

destructor TdmCustomList.Destroy;
begin
  FreeAndNil(rptMain);
  FreeAndNIl(FListSelection);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Executed|----------------------------------------------------------------

procedure TdmCustomList.SetExecuted(const Value: Boolean);
begin
  inherited SetExecuted(Value);
  if Value <> Executed then
  begin
    if Executed then
      Prepared := False;
  end;
end;

//---|MaxSelectionCount|-------------------------------------------------------

function TdmCustomList.GetMaxSelectionCount: Integer;
begin
  Result := FMaxSelectionCount;
end;

procedure TdmCustomList.SetMaxSelectionCount(const Value: Integer);
begin
  FMaxSelectionCount := Value;
end;

//---|Prepared|----------------------------------------------------------------

function TdmCustomList.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TdmCustomList.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;

//---|ReportTitle|-------------------------------------------------------------

function TdmCustomList.GetReportTitle: string;
begin
  Result := FReportTitle;
end;

procedure TdmCustomList.SetReportTitle(const Value: string);
begin
  FReportTitle := Value;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

//---|rptMain|-----------------------------------------------------------------

procedure TdmCustomList.rptMainGetColumnProperties(Sender: TObject;
  AColumn: TtsListReportColumn);
begin
  InitReportColumn(AColumn);
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

//-----------------------------------------------------------------------------

{ TODO: needs to be rewritten; DisplayValues.Items[I].DisplayName can be used
        in a loop that calls a procedure for every item
}

function TdmCustomList.GenerateReportSubtitle: string;
begin
  UpdateDisplayValues;
end;

//-----------------------------------------------------------------------------

{ Returns True if a selection criterium is specified for the given field. }

function TdmCustomList.IsActiveCriteriumField(const AFieldName: string): Boolean;
begin
  Result := False;
end;

//-----------------------------------------------------------------------------

{ Returns True if the current field represents a lookup value. }

function TdmCustomList.IsLookupField(const AFieldName: string): Boolean;
begin
  Result := StrHasPrefix(AFieldName, ['lkp']);
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.Initialize;
begin
  inherited;
  FListSelection := TListSelection.Create(Self);
  FPrepared := False;
  NullStrictConvert := False;
  rptMain := TtsFRListReport.Create(Self);
  rptMain.OnGetColumnProperties := rptMainGetColumnProperties;
  if (Params['prmReportTitle'] = Unassigned) and Assigned(ModuleItem) then
    FReportTitle := ModuleItem.DisplayName
  else
    FReportTitle := Params['prmReportTitle'];
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.InitReportColumn(AColumn: TtsListReportColumn);
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

procedure TdmCustomList.InternalExecute(const ACommandText: string);
begin
  rptMain.DataSet := DataSet;
  inherited InternalExecute(ACommandText);
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.PrepareReport;
begin
  if not Executed then
    Execute;

  if not Prepared then
  begin
    rptMain.ClearReport;
    rptMain.AssignColumnsFromDataSet;
    rptMain.ReportTitle    := ReportTitle;
    rptMain.ReportSubtitle := GenerateReportSubtitle + #13#10 + ' ';
    rptMain.BuildReport;
    Prepared := True;
  end;
end;

//-----------------------------------------------------------------------------

function TdmCustomList.GetRowID: Variant;
const
  QUERY = 'SELECT TOP 1 Fn_RowID() FROM %s';
begin
  Result := QueryLookup((Connection as IADOConnection).ADOConnection, QUERY, [FromClause]);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TdmCustomList.DesignReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    rptMain.DesignReport;
  finally
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.LayoutReport;
begin
  PrepareReport;
end;

//-----------------------------------------------------------------------------

{ Shows a dialogbox with properties that can be configured by the end user.

  TODO: this still needs to be implemented. }

procedure TdmCustomList.EditProperties;
begin
  if not Executed then
    Execute;

  rptMain.ClearReport;
  rptMain.AssignColumnsFromDataSet;
  rptMain.ReportTitle := ReportTitle;
  rptMain.ReportSubtitle := GenerateReportSubtitle + #13#10 + ' ';
  rptMain.BuildReport;
end;

//-----------------------------------------------------------------------------

{ Executes the list query and resets the Prepared property.}

procedure TdmCustomList.Execute(AShowSQL : Boolean);
begin
  inherited Execute(AShowSQL);
  Prepared := False;
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.ExportExcel;
begin
  ts.ExportUtils.ExportExcel(DataSet, ReportTitle);
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.ExportReport(AExportFilter: TExportFilter;
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
    rptMain.ExportReport(AExportFilter, S, True);
  finally
    Screen.Cursor := crDefault;
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.ExportText;
begin
  ts.ExportUtils.ExportText(DataSet, ReportTitle);
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.PreviewReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    rptMain.PreviewReport(False);
  finally
    DataSet.EnableControls;
  end;
end;

//-----------------------------------------------------------------------------

procedure TdmCustomList.PrintReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    if Params['prmPrinter'] <> Unassigned then
      rptMain.Report.PrintOptions.Printer := Params['prmPrinter'];
    rptMain.PrintReport;
  finally
    DataSet.EnableControls;
  end;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.

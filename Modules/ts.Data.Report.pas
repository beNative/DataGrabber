{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Data.Report;

interface

uses
  Data.DB,

  ts.Interfaces, ts.Classes.ListReport, ts.Classes.ListReport.Columns;

type
  TDataReport = class(TAggregatedObject, IDataReport)
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

implementation

uses
  System.SysUtils, System.Variants,
  Vcl.Forms, Vcl.Controls,

  DDuce.ObjectInspector;

{$REGION 'construction and destruction'}
constructor TDataReport.Create(const AData: IData);
begin
  inherited Create(AData);
  FData := AData;
  FReport := TtsFRListReport.Create(Application.Mainform);
  FReport.OnGetColumnProperties := ReportGetColumnProperties;
end;

destructor TDataReport.Destroy;
begin
  FreeAndNil(FReport);
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Data|--------------------------------------------------------------------

function TDataReport.GetData: IData;
begin
  Result := Controller as IData;
end;

//---|DataSet|-----------------------------------------------------------------

function TDataReport.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

//---|Prepared|----------------------------------------------------------------

function TDataReport.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TDataReport.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;

//---|ReportTitle|-------------------------------------------------------------

function TDataReport.GetReportTitle: string;
begin
  Result := FReportTitle;
end;

procedure TDataReport.SetReportTitle(const Value: string);
begin
  FReportTitle := Value;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TDataReport.DesignReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    FReport.DesignReport;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TDataReport.EditProperties;
begin
  LayoutReport;
  InspectComponent(FReport);
end;

procedure TDataReport.ExportReport(AExportFilter: TExportFilter;
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

procedure TDataReport.InitReportColumn(AColumn: TtsListReportColumn);
begin
//  if AColumn.DataType = dtNumeric then
//  begin
//    AColumn.MinWidth := 40;
//    AColumn.FooterVisible := True;
//    AColumn.FooterExpression :=
//      Format('[SUM(<frxDataSet."%s">,DataBand)]', [AColumn.FieldName]);
//  end
//  else
//    AColumn.FooterVisible := False;

  if AColumn.DataType = dtString then
  begin
    AColumn.MinWidth := 64;
    AColumn.MaxWidth := 300;
    AColumn.DataAlignment.WordWrap := True;
  end
  else if AColumn.DataType = dtDateTime then
    AColumn.MinWidth := 64;
end;

procedure TDataReport.LayoutReport;
begin
  PrepareReport;
end;

procedure TDataReport.PrepareReport;
begin
  if not Data.Executed then
    Data.Execute;

  if not Prepared then
  begin
    FReport.DataSet := Data.DataSet;
    FReport.ClearReport;
    FReport.AssignColumnsFromDataSet;
    FReport.ReportTitle    := ReportTitle;
    //FReport.ReportSubtitle := GenerateReportSubtitle + #13#10 + ' ';
//    FReport.CompanyName    := (Connection as ICompanyData).CompanyName;
//    FReport.CompanyLogo.Bitmap.Assign((Connection as ICompanyData).CompanyLogo);
    FReport.BuildReport;
    Prepared := True;
  end;
end;

procedure TDataReport.PreviewReport;
begin
  DataSet.DisableControls;
  try
    PrepareReport;
    FReport.PreviewReport(False);
  finally
    DataSet.EnableControls;
  end;
end;

procedure TDataReport.PrintReport;
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

procedure TDataReport.ReportGetColumnProperties(Sender: TObject;
  AColumn: TtsListReportColumn);
begin
  InitReportColumn(AColumn);
end;
{$ENDREGION}

end.


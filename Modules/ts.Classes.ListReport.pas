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

unit ts.Classes.ListReport;

{
  Customizable component for automatic generation of FastReport layouts for a
  given DataSet (any TDataSet descendant).

  Supported FastReport versions:
    3.x
    4.x
    4.x

  Author: Tim Sinaeve
}

{
 BuildReportLayout
   -> CreatePage
   -> CreateHeader -> CreateHeaderBody
                   -> CreateHeaderChildBody
   -> CreateColumnHeader
   -> CreateGroupMemoBand
   -> CreateDataBand
   -> CreateColumnFooter
   -> CreateDataBandFooter
   -> CreateFooter -> CreateFooterBody

 End user methods:
   - DesignReport
   - PreviewReport
   - ClearReport
   - PrintReport
   - ExportReport
}

{
  KNOWN ISSUES - TODO
    - The column sequence needs to be automatically updated when columns are
      deleted at design time. Now we get an assertion failure when we try to
      open the designer after columns are deleted because there are sequence
      numbers for columns that are deleted.
      Maybe the Sequence property is not needed as the Index property can be
      used for this purpose. This needs to be checked.
    - A warning should be issued when SizeColumnsToPaper is enabled and one or
      columns are getting too small compared to their contents.
}

{
  The generated report layout looks as follows.

  REMARKS:
    - Names of the protected properties referencing the FastReport objects
      (bands, memo's, lines, pictures, ...) are shown between rectangular
      brackets ('[]').
    - Angular brackets ('<>') are used to denote a group of memo objects that
      will be created according to the column settings defined by the published
      Columns collection property.

+---|[ReportPage]|------------------------------------------------------------+
|                                                                             |
| +---|[Header]|------------------------------------------------------------+ |
| | [CompanyNameMemo]                                   [CompanyLogoPicture]| |
| |                            [ReportTitleMemo]                            | |
| |                          [ReportSubtitleMemo]                           | |
| |                              [TitleLine]                                | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[HeaderChild]|-------------------------------------------------------+ |
| |                                                                         | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[ColumnHeader]|------------------------------------------------------+ |
| |                             <column titles>                             | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[DataBand]|----------------------------------------------------------+ |
| |                              <column data>                              | |
| +------------------------------------------------------------------------ + |
|                                                                             |
| +---|[GroupMemoBand]|-----------------------------------------------------+ |
| |                               [GroupMemo]                               | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[ColumnFooter]|------------------------------------------------------+ |
| |                             <column totals>                             | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[DataBandFooter]|----------------------------------------------------+ |
| |                           [DataBandFooterMemo]                          | |
| +-------------------------------------------------------------------------+ |
|                                                                             |
| +---|[Footer]|------------------------------------------------------------+ |
| |                               [FooterLine]                              | |
| | [UserIDMemo]                 [DateTimeMemo]              [PageIndexMemo]| |
| +------------------------------------------------------------------------ + |
|                                                                             |
+-----------------------------------------------------------------------------+

The values shown in following memo-objects can be manipulated through the
following published properties of the component:

        +----------------------+--------------------------------------+
        | FastReport object(s) |           published property         |
        +----------------------+--------------------------------------+
        | CompanyNameMemo      | CompanyName                          |
        | ReportTitleMemo      | ReportTitle                          |
        | ReportSubtitleMemo   | ReportSubtitle                       |
        | CompanyLogoPicture   | CompanyLogo                          |
        | <column titles>      | Columns                              |
        | <column data>        | DataSet/Columns                      |
        | <column totals>      | DataSet/Columns                      |
        | Header               | Options (lroHeaderOnEveryPage)       |
        | ColumnHeader         | Options (lroColumnHeaderOnEveryPage) |
        | Footer               | Options (lroFooterOnEveryPage)       |
        +----------------------+--------------------------------------+
}

{
TODO:
  - Full design time support for report generation (implementation of custom
    property- and component-editors).
  - More layout settings for CompanyName, ReportTitle, ...
  - SizeColumnsToPaper needs to be rewritten (smarter resizing algorithm).
  - Prepared should be set to False if any of the report settings changes,
    to indicate that the report layout should be regenerated with the new
    settings => needs more testing.
  - Work with update locks.
  - DataSet's position should be restored after generating the report (use
    bookmarks). => almost done, needs more testing.
  - Support for running totals (shown in seperate band).
  - Move tsAlignment, tsListReportColumns to this unit, as they are not (
    and probably never will be) shared by any other unit.
}

interface

uses
  System.Classes, System.SysUtils, System.Contnrs,
  Vcl.Graphics, Vcl.DockTabSet,
  Data.DB,

  ts.Classes.Alignment, ts.Classes.ListReport.Columns,

  frxClass, frxExportCSV, frxExportHTML, frxExportImage,

  frxExportPDF, frxExportRTF, frxExportText,

  //frxExportXLS, frxExportXML,  frxExportODF, //frxExportMail, {frxExportTXT, }

  frxExportMatrix, frxDBSet;

{$INCLUDE frx.inc}

type
  TtsCellBorder        = ts.Classes.ListReport.Columns.TtsCellBorder;
  TtsCellBorders       = ts.Classes.ListReport.Columns.TtsCellBorders;
  TtsDataType          = ts.Classes.ListReport.Columns.TtsDataType;
  TtsListReportColumn  = ts.Classes.ListReport.Columns.TtsListReportColumn;
  TtsListReportColumns = ts.Classes.ListReport.Columns.TtsListReportColumns;

type
  EtsFRListReport = class(Exception);

  // TODO: Should be extended with FastReports native XML format to store the
  // previews (*.fp3).
  TExportFilter = (
    efCSV,
//    efMail,
    efJPEG,
    efGIF,
    efBMP,
    efTIFF,
    efHTML,
//    efTXT,
    efSimpleText,
//    efXML,
//    efXLS,
    efPDF,
    efRTF
//    efODS,
//    efODT
  );

  TPageOrientation = (pgoPortrait, pgoLandscape, pgoAuto);

  TfrxCustomExportFilterClass = class of TfrxCustomExportFilter;

  TExportFilterClasses = array [TExportFilter] of TfrxCustomExportFilterClass;

  TtsListReportOption = (
    lroHeaderOnEveryPage,
    lroColumnHeaderOnEveryPage,
    lroFooterOnEveryPage,
    lroAutoSizeColumns,
    lroSizeColumnsToPaper
  );

  TtsListReportOptions = set of TtsListReportOption;

  TGetColumnPropertiesEvent = procedure(
    Sender  : TObject;
    AColumn : TtsListReportColumn
  ) of object;

var
  ExportFilterClasses : TExportFilterClasses = (
    TfrxCSVExport,
//    TfrxMailExport,
    TfrxJPEGExport,
    TfrxGIFExport,
    TfrxBMPExport,
    TfrxTIFFExport,
    TfrxHTMLExport,
    //TfrxTXTExport,
    TfrxSimpleTextExport,
//    TfrxXMLExport,
//    TfrxXLSExport,
    TfrxPDFExport,
    TfrxRTFExport
//    TfrxODSExport,
//    TfrxODTExport
  );

const
  DEFAULT_OPTIONS = [
    lroHeaderOnEveryPage,
    lroColumnHeaderOnEveryPage,
    lroFooterOnEveryPage,
    lroSizeColumnsToPaper,
    lroAutoSizeColumns
  ];

type
  TtsFRListReport = class(TComponent)
  private
    FColumnCells           : array of string;
    FColumns               : TtsListReportColumns;
    FCompanyLogo           : TPicture;
    FCompanyName           : WideString;
    FDataSet               : TDataSet;
    FDataSetBookmark       : TBookmark;
    FDefaultFont           : TFont;
    FDefaultFooterFont     : TFont;
    FDefaultTitleFont      : TFont;
    FEnabled               : Boolean;
    FExportFilter          : TExportFilter;
    FExportFilters         : TObjectList;
    FfrxDBDataSet          : TfrxDBDataset;
    FOnGetColumnProperties : TGetColumnPropertiesEvent;
    FOptions               : TtsListReportOptions;
    FPageOrientation       : TPageOrientation;
    FPrepared              : Boolean;
    FPreview               : TfrxCustomPreview;
    FReport                : TfrxReport;
    FReportSubtitle        : WideString;
    FReportTitle           : WideString;
    FTableWidth            : Extended;
    FUpdateLock            : Integer;

    procedure OnFontChange(Sender : TObject);

    // private property access methods
    function GetColumnCells(Index: Integer): string;
    function GetColumnFooter: TfrxColumnFooter;
    function GetColumnHeader: TfrxBand;
    function GetCompanyLogoPicture: TfrxPictureView;
    function GetCompanyName: WideString;
    function GetCompanyNameMemo: TfrxMemoView;
    function GetDataBand: TfrxDataBand;
    function GetDataBandFooter: TfrxFooter;
    function GetDataBandFooterMemo: TfrxMemoView;
    function GetDataSet: TDataSet;
    function GetDateTimeMemo: TfrxMemoView;
    function GetEnabled: Boolean;
    function GetFooter: TfrxBand;
    function GetFooterLine: TfrxLineView;
    function GetGroupMemo: TfrxMemoView;
    function GetGroupMemoBand: TfrxBand;
    function GetHeader: TfrxBand;
    function GetOnProgress: TfrxProgressEvent;
    function GetOnProgressStart: TfrxProgressEvent;
    function GetOnProgressStop: TfrxProgressEvent;
    function GetPageClientWidth: Extended;
    function GetPageIndexMemo: TfrxMemoView;
    function GetReportPage: TfrxReportPage;
    function GetReportSubtitle: WideString;
    function GetReportSubtitleMemo: TfrxMemoView;
    function GetReportTitle: WideString;
    function GetReportTitleMemo: TfrxMemoView;
    function GetTextWidth(const AText: string; AFont: TFont): Integer;
    function GetTitleLine: TfrxLineView;
    function GetUserIDMemo: TfrxMemoView;
    procedure RegisterExportFilters;
    procedure SetColumnCells(Index: Integer; const Value: string);
    procedure SetColumns(const Value: TtsListReportColumns);
    procedure SetCompanyLogo(const Value: TPicture);
    procedure SetCompanyName(const Value: WideString);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetDefaultFont(const Value: TFont);
    procedure SetDefaultFooterFont(const Value: TFont);
    procedure SetDefaultTitleFont(const Value: TFont);
    procedure SetfrxDBDataSet(const Value: TfrxDBDataset);
    procedure SetOnProgress(const Value: TfrxProgressEvent);
    procedure SetOnProgressStart(const Value: TfrxProgressEvent);
    procedure SetOnProgressStop(const Value: TfrxProgressEvent);
    procedure SetOptions(const Value: TtsListReportOptions);
    procedure SetPageOrientation(const Value: TPageOrientation);
    procedure SetPrepared(const Value: Boolean);
    procedure SetPreview(const Value: TfrxCustomPreview);
    procedure SetReport(const Value: TfrxReport);
    procedure SetReportSubtitle(const Value: WideString);
    procedure SetReportTitle(const Value: WideString);
    procedure SetTableWidth(const Value: Extended);

  protected
    // protected property access methods
    procedure SetEnabled(const Value: Boolean); virtual;

    // protected methods
    procedure AutoSizeCols;
    procedure LockUpdate;
    procedure UnlockUpdate;
    procedure SizeColumnsToPaper;

    // protected virtual methods
    procedure CreatePage; virtual;
    procedure CreateHeader; virtual;
    procedure CreateFooter; virtual;
    procedure CreateHeaderBody(ABand: TfrxBand); virtual;
    procedure CreateHeaderChildBody(AChild : TfrxChild); virtual;
    procedure CreateFooterBody(ABand: TfrxBand); virtual;
    procedure CreateColumnHeader; virtual;
    procedure CreateColumnFooter; virtual;
    procedure CreateDataBand; virtual;
    procedure CreateDataBandFooter; virtual;
    procedure CreateGroupMemoBand; virtual;
    procedure AutoSizeCol(ACol: Integer); virtual;
    procedure AssignColumnProperties; virtual;

    // event dispatching methods
    procedure DoGetColumnProperties(AColumn : TtsListReportColumn);

    // overridden protected methods
    procedure Notification(AComponent : TComponent;
                           Operation  : TOperation); override;

    // protected properties
    property ExportFilter: TExportFilter
      read FExportFilter;

    property frxDBDataSet: TfrxDBDataset
      read FfrxDBDataSet write SetfrxDBDataSet;

    // These properties hold references to the main buildingblocks of the
    // report design. These references are only available after the report has
    // been prepared.
    property ReportPage: TfrxReportPage
      read GetReportPage;

    property Header: TfrxBand
      read GetHeader;

    property Footer: TfrxBand
      read GetFooter;

    property DataBand: TfrxDataBand
      read GetDataBand;

    property DataBandFooter: TfrxFooter
      read GetDataBandFooter;

    property GroupMemoBand: TfrxBand
      read GetGroupMemoBand;

    property ColumnHeader: TfrxBand
      read GetColumnHeader;

    property ColumnFooter: TfrxColumnFooter
      read GetColumnFooter;

    property TitleLine: TfrxLineView
      read GetTitleLine;

    property FooterLine: TfrxLineView
      read GetFooterLine;

    property CompanyNameMemo: TfrxMemoView
      read GetCompanyNameMemo;

    property ReportTitleMemo: TfrxMemoView
      read GetReportTitleMemo;

    property ReportSubtitleMemo: TfrxMemoView
      read GetReportSubtitleMemo;

    property PageIndexMemo: TfrxMemoView
      read GetPageIndexMemo;

    property DateTimeMemo: TfrxMemoView
      read GetDateTimeMemo;

    property UserIDMemo: TfrxMemoView
      read GetUserIDMemo;

    property CompanyLogoPicture : TfrxPictureView
      read GetCompanyLogoPicture;

    property ColumnCells[Index: Integer]: string
      read GetColumnCells write SetColumnCells;
  public
    // construction and destruction
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // public methods
    procedure BuildReportLayout; virtual;
    procedure BuildDataLayout; virtual;
    procedure DesignReport; virtual;
    procedure PrepareReport(AClearLastReport: Boolean); virtual;
    function PrintReport(AShowDialog : Boolean = False) : Boolean; virtual;
    procedure PreviewReport(AShowPrepared : Boolean = True); virtual;
    procedure BuildReport(
      AClearLastReport          : Boolean;
      AAssignColumnsFromDataSet : Boolean
    ); overload; virtual;
    procedure BuildReport; overload; virtual;
    procedure ClearReport; virtual;
    function ExportReport(
            AExportFilter : TExportFilter;
      const AFileName     : TFileName = '';
            AShowDialog   : Boolean = False
    ) : Boolean; virtual;
    procedure AssignColumnsFromDataSet; virtual;

    // public properties
    { Width of the printable area of the report in pixels. }
    property PageClientWidth: Extended
      read GetPageClientWidth;

    { MemoView component that can be used for arbitrary text between the rows
      of the listing. }
    property GroupMemo : TfrxMemoView
      read GetGroupMemo;

    { MemoView component that can be used to show arbitrary text on the bottom
      of the generated report. }
    property DataBandFooterMemo: TfrxMemoView
      read GetDataBandFooterMemo;

    { Width of the table in pixels. }
    property TableWidth: Extended
      read FTableWidth write SetTableWidth;

    { True when a prepared report exists for the current DataSet's data. }
    property Prepared: Boolean
      read FPrepared write SetPrepared;

    { TODO When Enabled is set to True, the report layout of the underlying
      report will be created dynamically. }
    property Enabled : Boolean
      read GetEnabled write SetEnabled;

    { The default font used for the column footers. This font can be overruled
      in the column properties. }
    property DefaultFooterFont : TFont
      read FDefaultFooterFont write SetDefaultFooterFont;

  published
    { Embedded report component which will hold the generated report. }
    property Report: TfrxReport
      read FReport write SetReport;

    { Collection of all column properties. }
    property Columns : TtsListReportColumns
      read FColumns write SetColumns;

    { Title string shown in the report header band. }
    property ReportTitle: WideString
      read GetReportTitle write SetReportTitle;

    { Subtitle string shown in the report header band. }
    property ReportSubtitle: WideString
      read GetReportSubtitle write SetReportSubtitle;

    { Company name shown in the upper left corner of the report. }
    property CompanyName: WideString
      read GetCompanyName write SetCompanyName;

    { Optional picture to be included in the generated report design. }
    property CompanyLogo: TPicture
      read FCompanyLogo write SetCompanyLogo;

    { DataSet holding the data to be printed on the report. }
    property DataSet: TDataSet
      read GetDataSet write SetDataSet;

    { The default font used for the column titles. This font can be overruled
      in the column properties. }
    property DefaultTitleFont : TFont
      read FDefaultTitleFont write SetDefaultTitleFont;

    { The default font used for the column data. This font can be overruled
      in the column properties. }
    property DefaultFont : TFont
      read FDefaultFont write SetDefaultFont;

    property Preview : TfrxCustomPreview
      read FPreview write SetPreview;

    property Options : TtsListReportOptions
      read FOptions write SetOptions default DEFAULT_OPTIONS;

    property PageOrientation : TPageOrientation
      read FPageOrientation write SetPageOrientation default pgoAuto;

    property OnProgress : TfrxProgressEvent
      read GetOnProgress write SetOnProgress;

    property OnProgressStart : TfrxProgressEvent
      read GetOnProgressStart write SetOnProgressStart;

    property OnProgressStop : TfrxProgressEvent
      read GetOnProgressStop write SetOnProgressStop;

    property OnGetColumnProperties : TGetColumnPropertiesEvent
      read FOnGetColumnProperties write FOnGetColumnProperties;
  end;

const
{ TODO: It might be useful to implement some of these constants as properties
  in TtsFRListReport. }

  // Units in pixels.
  // There are 96 / 25.4 = 3.77953 = fr01cm pixels per milimeter assumed by
  // FastReport.
  HEIGHT_REPORT_TITLE       = 40;
  HEIGHT_COMPANY_NAME       = 20;

  // REMARK: When bands are stretched, these settings will only have a meaning
  // in the report designer.
  HEIGHT_COLUMN_HEADER      = 20;
  HEIGHT_COLUMN_FOOTER      = 0;
  HEIGHT_DATA_BAND          = 16;
  HEIGHT_MEMO_BAND          = 20;

  FRAME_WIDTH               = 1.5;

  PAGE_LEFT_MARGIN          = 10.0;
  PAGE_RIGHT_MARGIN         = 10.0;
  PAGE_BOTTOM_MARGIN        = 10.0;
  PAGE_TOP_MARGIN           = 10.0;

  FOOTER_TOP_MARGIN         = 10;
  FOOTER_HEIGHT             = 40;

  HEADER_LINE_WIDTH         = 0;       // hide header line
  FOOTER_LINE_WIDTH         = 3;

  NORMAL_LINE_WIDTH         = 1.5;
  THICK_LINE_WIDTH          = 3;

  DATA_GAP_X                = 4;

  FONTSIZE_REPORT_TITLE     = 10;
  FONTSIZE_REPORT_SUBTITLE  = 8;
  MAX_FIELD_SIZE            = 200;

implementation

uses
  System.Math, System.Variants, System.StrUtils,
  Vcl.Forms, Vcl.Controls, Vcl.Printers,

  { Comment this if you do not want to have the FastReport designer compiled
    into your application. }
  frxDesgn, frxDsgnIntf,

  ts.Utils;

const
  DATETIME_EXPRESSION   = '[<Date>],[<Time>  #hh:MM:SS]';
  PAGE_INDEX_EXPRESSION = '[Page#]/[TotalPages#]';
  LOCAL_ID_EXPRESSION   = '%s >>> %s';

{$REGION 'construction and destruction'}
constructor TtsFRListReport.Create(AOwner: TComponent);
begin
  FExportFilters := TObjectList.Create(True);
  RegisterExportFilters;

  FOptions         := DEFAULT_OPTIONS;
  FPageOrientation := pgoAuto;

  FReport                         := TfrxReport.Create(Self);
  FReport.StoreInDFM              := False;
  FReport.PreviewOptions.ZoomMode := zmPageWidth;
  FReport.Name                    := 'Report';

  FCompanyLogo            := TPicture.Create;
  FDefaultTitleFont       := TFont.Create;
  FDefaultFont            := TFont.Create;
  FDefaultFooterFont      := TFont.Create;
  FDefaultFont.Name       := 'Arial';
  FDefaultTitleFont.Name  := 'Arial';
  FDefaultFooterFont.Name := 'Arial';
  FDefaultFont.OnChange       := OnFontChange;
  FDefaultTitleFont.OnChange  := OnFontChange;
  FDefaultFooterFont.OnChange := OnFontChange;

  // needs to be created here!
  FfrxDBDataSet := TfrxDBDataset.Create(Self);

  inherited;

  FColumns  := TtsListReportColumns.Create(Self);
  FPrepared := False;
end;

destructor TtsFRListReport.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FExportFilters);
  FreeAndNil(FCompanyLogo);
  FreeAndNil(FDefaultTitleFont);
  FreeAndNil(FDefaultFooterFont);
  FreeAndNil(FDefaultFont);
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Columns|-----------------------------------------------------------------

procedure TtsFRListReport.SetColumns(const Value: TtsListReportColumns);
begin
  FColumns := Value;
end;

//---|ReportTitle|-------------------------------------------------------------

function TtsFRListReport.GetReportTitle: WideString;
begin
  Result := FReportTitle;
end;

procedure TtsFRListReport.SetReportTitle(const Value: WideString);
begin
  if Value <> ReportTitle then
  begin
    FReportTitle := Value;
    if Assigned(ReportTitleMemo) then
      ReportTitleMemo.Text := Value;
    Prepared := False;
  end;
end;

//---|ReportSubtitle|----------------------------------------------------------

function TtsFRListReport.GetReportSubtitle: WideString;
begin
  Result := FReportSubtitle;
end;

procedure TtsFRListReport.SetReportSubtitle(const Value: WideString);
begin
  if Value <> ReportSubtitle then
  begin
    FReportSubtitle := Value;
    if Assigned(ReportSubtitleMemo) then
      ReportSubtitleMemo.Text := Value;
    Prepared := False;
  end;
end;

//---|CompanyName|-------------------------------------------------------------

function TtsFRListReport.GetCompanyName: WideString;
begin
  Result := FCompanyName;
end;

procedure TtsFRListReport.SetCompanyName(const Value: WideString);
begin
  if Value <> CompanyName then
  begin
    FCompanyName := Value;
    if Assigned(CompanyNameMemo) then
      CompanyNameMemo.Text := Value;
    Prepared := False;
  end;
end;

//---|PageClientWidth|---------------------------------------------------------

function TtsFRListReport.GetPageClientWidth: Extended;
begin
  // PaperWidth, LeftMargin and RightMargin are in milimeters.
  Result := (ReportPage.PaperWidth - ReportPage.LeftMargin -
             ReportPage.RightMargin) * 96 / 25.4
end;

//---|Enabled|-----------------------------------------------------------------

// REMARK: This property is for the moment NOT used! When full design time
// support is implemented this property can be used to test the report layout
// in design time.

function TtsFRListReport.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TtsFRListReport.SetEnabled(const Value: Boolean);
begin
  // TODO : Creation of the actual report layout.
  FEnabled := Value;
end;

//---|Report|------------------------------------------------------------------

procedure TtsFRListReport.SetReport(const Value: TfrxReport);
begin
  FReport := Value;
end;

//---|DataSet|-----------------------------------------------------------------

function TtsFRListReport.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TtsFRListReport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

//---|DefaultFont|-------------------------------------------------------------

procedure TtsFRListReport.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
  Prepared := False;
end;

//---|DefaultFooterFont|-------------------------------------------------------

procedure TtsFRListReport.SetDefaultFooterFont(const Value: TFont);
begin
  FDefaultFooterFont.Assign(Value);
  Prepared := False;
end;

//---|DefaultTitleFont|--------------------------------------------------------

procedure TtsFRListReport.SetDefaultTitleFont(const Value: TFont);
begin
  FDefaultTitleFont.Assign(Value);
  Prepared := False;
end;

//---|DataBand|----------------------------------------------------------------

function TtsFRListReport.GetDataBand: TfrxDataBand;
begin
  Result := Report.FindObject('DataBand') as TfrxDataBand;
end;

//---|DataBandFooter|----------------------------------------------------------

function TtsFRListReport.GetDataBandFooter: TfrxFooter;
begin
  Result := Report.FindObject('DataBandFooter') as TfrxFooter;
end;

//---|GroupMemoBand|-----------------------------------------------------------

function TtsFRListReport.GetGroupMemoBand: TfrxBand;
begin
  Result := Report.FindObject('GroupMemoBand') as TfrxGroupHeader;
end;

//---|Footer|------------------------------------------------------------------

function TtsFRListReport.GetFooter: TfrxBand;
begin
  Result := Report.FindObject('Footer') as TfrxBand;
end;

//---|Header|------------------------------------------------------------------

function TtsFRListReport.GetHeader: TfrxBand;
begin
  Result := Report.FindObject('Header') as TfrxBand;
end;

//---|ReportPage|--------------------------------------------------------------

function TtsFRListReport.GetReportPage: TfrxReportPage;
begin
  Result := Report.FindObject('ReportPage') as TfrxReportPage;
end;

//---|ReportSubtitleMemo|------------------------------------------------------

function TtsFRListReport.GetReportSubtitleMemo: TfrxMemoView;
begin
  Result := Report.FindObject('ReportSubtitleMemo') as TfrxMemoView;
end;

//---|ColumnHeader|------------------------------------------------------------

function TtsFRListReport.GetColumnHeader: TfrxBand;
begin
  Result := Report.FindObject('ColumnHeader') as TfrxBand;
end;

//---|ColumnFooter|------------------------------------------------------------

function TtsFRListReport.GetColumnFooter: TfrxColumnFooter;
begin
  Result := Report.FindObject('ColumnFooter') as TfrxColumnFooter;
end;

//---|TitleLine|---------------------------------------------------------------

function TtsFRListReport.GetTitleLine: TfrxLineView;
begin
  Result := Report.FindObject('TitleLine') as TfrxLineView;
end;

//---|FooterLine|--------------------------------------------------------------

function TtsFRListReport.GetFooterLine: TfrxLineView;
begin
  Result := Report.FindObject('FooterLine') as TfrxLineView;
end;

//---|CompanyNameMemo|---------------------------------------------------------

function TtsFRListReport.GetCompanyNameMemo: TfrxMemoView;
begin
  Result := Report.FindObject('CompanyNameMemo') as TfrxMemoView;
end;

//---|DateTimeMemo|------------------------------------------------------------

function TtsFRListReport.GetDateTimeMemo: TfrxMemoView;
begin
  Result := Report.FindObject('DateTimeMemo') as TfrxMemoView;
end;

//---|PageIndexMemo|-----------------------------------------------------------

function TtsFRListReport.GetPageIndexMemo: TfrxMemoView;
begin
  Result := Report.FindObject('PageIndexMemo') as TfrxMemoView;
end;

//---|ReportTitleMemo|---------------------------------------------------------

function TtsFRListReport.GetReportTitleMemo: TfrxMemoView;
begin
  Result := Report.FindObject('ReportTitleMemo') as TfrxMemoView;
end;

//---|UserIDMemo|--------------------------------------------------------------

function TtsFRListReport.GetUserIDMemo: TfrxMemoView;
begin
  Result := Report.FindObject('UserIDMemo') as TfrxMemoView;
end;

//---|GroupMemo|---------------------------------------------------------------

function TtsFRListReport.GetGroupMemo: TfrxMemoView;
begin
  Result := Report.FindObject('GroupMemo') as TfrxMemoView;
end;

//---|DataBandFooterMemo|------------------------------------------------------

function TtsFRListReport.GetDataBandFooterMemo: TfrxMemoView;
begin
  Result := Report.FindObject('DataBandFooterMemo') as TfrxMemoView;
end;

//---|CompanyLogoPicture|------------------------------------------------------

function TtsFRListReport.GetCompanyLogoPicture: TfrxPictureView;
begin
  Result := Report.FindObject('CompanyLogo') as TfrxPictureView;
end;

//---|ColumnCells|-------------------------------------------------------------

function TtsFRListReport.GetColumnCells(Index: Integer): string;
begin
  Result := FColumnCells[Index];
end;

procedure TtsFRListReport.SetColumnCells(Index: Integer; const Value: string);
begin
  FColumnCells[Index] := Value;
end;

//---|CompanyLogo|-------------------------------------------------------------

procedure TtsFRListReport.SetCompanyLogo(const Value: TPicture);
var
  PV : TfrxPictureView;
begin
  FCompanyLogo.Assign(Value);
  PV := CompanyLogoPicture;
  if Assigned(PV) then
  begin
    PV.Picture.Assign(Value);
    PV.Height := HEIGHT_COMPANY_NAME + HEIGHT_REPORT_TITLE - 10;
    if Assigned(Value) and (Value.Height > 0) then
      PV.Width  := (PV.Height / Value.Height) * Value.Width;
  end;
  Prepared := False;
end;

//---|Preview|-----------------------------------------------------------------

procedure TtsFRListReport.SetPreview(const Value: TfrxCustomPreview);
begin
  FPreview := Value;
end;

//---|Options|-----------------------------------------------------------------

procedure TtsFRListReport.SetOptions(const Value: TtsListReportOptions);
begin
  if Value <> Options then
  begin
    FOptions := Value;
    Prepared := False;
  end;
end;

//---|TableWidth|--------------------------------------------------------------

procedure TtsFRListReport.SetTableWidth(const Value: Extended);
begin
  FTableWidth := Value;
end;

//---|OnProgress|--------------------------------------------------------------

function TtsFRListReport.GetOnProgress: TfrxProgressEvent;
begin
  Result := FReport.OnProgress;
end;

procedure TtsFRListReport.SetOnProgress(const Value: TfrxProgressEvent);
begin
  FReport.OnProgress := Value;
end;

//---|OnProgressStart|---------------------------------------------------------

function TtsFRListReport.GetOnProgressStart: TfrxProgressEvent;
begin
  Result := FReport.OnProgressStart;
end;

procedure TtsFRListReport.SetOnProgressStart(const Value: TfrxProgressEvent);
begin
  FReport.OnProgressStart := Value;
end;

//---|OnProgressStop|----------------------------------------------------------

function TtsFRListReport.GetOnProgressStop: TfrxProgressEvent;
begin
  Result := FReport.OnProgressStop;
end;

procedure TtsFRListReport.SetOnProgressStop(const Value: TfrxProgressEvent);
begin
  FReport.OnProgressStop := Value;
end;

//---|frxDBDataSet|------------------------------------------------------------

procedure TtsFRListReport.SetfrxDBDataSet(const Value: TfrxDBDataset);
begin
  FfrxDBDataSet := Value;
end;

//---|PageOrientation|---------------------------------------------------------

procedure TtsFRListReport.SetPageOrientation(const Value: TPageOrientation);
begin
  if Value <> PageOrientation then
  begin
    FPageOrientation := Value;
    Prepared := False;
  end;
end;

//---|Prepared|----------------------------------------------------------------

procedure TtsFRListReport.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TtsFRListReport.DoGetColumnProperties(
  AColumn: TtsListReportColumn);
begin
  if Assigned(OnGetColumnProperties) then
    OnGetColumnProperties(Self, AColumn);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TtsFRListReport.OnFontChange(Sender: TObject);
begin
  Prepared := False;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TtsFRListReport.CreatePage;
var
  RP: TfrxReportPage;
begin
  RP              := TfrxReportPage.Create(FReport);
  RP.Name         := 'ReportPage';
  RP.SetDefaults;
  RP.LeftMargin   := PAGE_LEFT_MARGIN;
  RP.RightMargin  := PAGE_RIGHT_MARGIN;
  RP.BottomMargin := PAGE_BOTTOM_MARGIN;
  RP.TopMargin    := PAGE_TOP_MARGIN;
  if PageOrientation <> pgoAuto then
    RP.Orientation  := TPrinterOrientation(Ord(PageOrientation));
end;

procedure TtsFRListReport.CreateFooter;
var
  F : TfrxBand;
begin
  if Assigned(ReportPage) then
  begin
    if lroFooterOnEveryPage in Options then
      F := TfrxPageFooter.Create(ReportPage)
    else
      F := TfrxReportSummary.Create(ReportPage);
    F.Name := 'Footer';
    F.Top  := 100;
    CreateFooterBody(F);
  end; // if Assigned(FReportPage) then
end;

procedure TtsFRListReport.CreateHeader;
var
  H  : TfrxBand;
  HC : TfrxChild;
begin
  if Assigned(ReportPage) then
  begin
    if lroHeaderOnEveryPage in Options then
      H := TfrxPageHeader.Create(ReportPage)
    else
      H := TfrxReportTitle.Create(ReportPage);
    H.Name      := 'Header';
    H.Stretched := True;
    CreateHeaderBody(H);

    HC           := TfrxChild.Create(ReportPage);
    HC.Name      := 'HeaderChild';
    HC.Stretched := True;
    CreateHeaderChildBody(HC);
    H.Child := HC;
  end;
end;

procedure TtsFRListReport.CreateColumnFooter;
var
  CF  : TfrxColumnFooter;
  CFM : TfrxMemoView;
begin
  if Assigned(ReportPage) then
  begin
    CF := TfrxColumnFooter.Create(ReportPage);
    CF.Name      := 'ColumnFooter';
    CF.Height    := HEIGHT_COLUMN_FOOTER;
    CF.Stretched := True;

    CFM               := TfrxMemoView.Create(CF);
    CFM.Name          := 'ColumnFooterMemo';
    CFM.Top           := 0;
    CFM.Left          := 0;
    CFM.Height        := 0;
    CFM.AllowHTMLTags := True;
    CFM.Align         := baClient;
    CFM.StretchMode   := smMaxHeight;
  end;
end;

procedure TtsFRListReport.CreateColumnHeader;
var
  CH : TfrxBand;
begin
  if Assigned(ReportPage) then
  begin
    if lroColumnHeaderOnEveryPage in Options then
      CH := TfrxColumnHeader.Create(ReportPage)
    else
      CH := TfrxHeader.Create(ReportPage);

    CH.Name      := 'ColumnHeader';
    CH.Height    := HEIGHT_COLUMN_HEADER;
    CH.Stretched := True;
  end;
end;

procedure TtsFRListReport.CreateFooterBody(ABand: TfrxBand);
var
  Width : Extended;
  PI    : TfrxMemoView;
  DT    : TfrxMemoView;
  UI    : TfrxMemoView;
  FL    : TfrxLineView;
begin
  FL             := TfrxLineView.Create(ABand);
  FL.Name        := 'FooterLine';
  FL.Top         := FOOTER_TOP_MARGIN;
  FL.Align       := baWidth;
  FL.Width       := PageClientWidth ;
  FL.Frame.Width := FOOTER_LINE_WIDTH;
  FL.Frame.Color := clDefault;

  Width := PageClientWidth / 3;

  UI        := TfrxMemoView.Create(ABand);
  UI.Name   := 'UserIDMemo';
  UI.Text   := Format(LOCAL_ID_EXPRESSION,
    [GetLocalUserName, GetLocalComputerName]);
  UI.Top    := FL.Top + FOOTER_LINE_WIDTH;
  UI.Height := FOOTER_HEIGHT - FOOTER_TOP_MARGIN - FOOTER_LINE_WIDTH;
  UI.Width  := Width;
  UI.Align  := baLeft;
  UI.HAlign := haLeft;
  UI.VAlign := vaCenter;
  UI.Left   := 0;

  DT        := TfrxMemoView.Create(ABand);
  DT.Name   := 'DateTimeMemo';
  DT.Text   := DATETIME_EXPRESSION;
  DT.Top    := FL.Top + FOOTER_LINE_WIDTH;
  DT.Height := FOOTER_HEIGHT - FOOTER_TOP_MARGIN - FOOTER_LINE_WIDTH;
  DT.Width  := Width;
  DT.Align  := baCenter;
  DT.HAlign := haCenter;
  DT.VAlign := vaCenter;
  DT.Left   := Width;

  PI        := TfrxMemoView.Create(ABand);
  PI.Name   := 'PageIndexMemo';
  PI.Text   := PAGE_INDEX_EXPRESSION;
  PI.Top    := FL.Top + FOOTER_LINE_WIDTH;
  PI.Height := FOOTER_HEIGHT - FOOTER_TOP_MARGIN - FOOTER_LINE_WIDTH;
  PI.Width  := Width;
  PI.Align  := baRight;
  PI.HAlign := haRight;
  PI.VAlign := vaCenter;
  PI.Left   := Width * 2;;

  ABand.Height := FOOTER_HEIGHT;
end;

procedure TtsFRListReport.CreateHeaderBody(ABand: TfrxBand);
var
  CN  : TfrxMemoView;
  RT  : TfrxMemoView;
  TL  : TfrxLineView;
  PV  : TfrxPictureView;
begin
  PV := TfrxPictureView.Create(ABand);
  PV.Picture.Assign(CompanyLogo);
  PV.Height := HEIGHT_COMPANY_NAME + HEIGHT_REPORT_TITLE - 10;
  if Assigned(CompanyLogo) and (CompanyLogo.Height > 0) then
    PV.Width  := (PV.Height / CompanyLogo.Height) * CompanyLogo.Width;
  PV.Name   := 'CompanyLogo';
  PV.Align  := baRight;

  CN            := TfrxMemoView.Create(ABand);
  CN.Name       := 'CompanyNameMemo';
  CN.Font.Style := [fsBold];
  CN.Height     := HEIGHT_COMPANY_NAME;
  CN.Align      := baLeft;
  // Width to be shown in designer
  CN.Width      := 150;
  CN.AutoWidth  := True;
  CN.Text       := CompanyName;

  RT             := TfrxMemoView.Create(ABand);
  RT.Name        := 'ReportTitleMemo';
  RT.Font.Size   := FONTSIZE_REPORT_TITLE;
  RT.Font.Style  := [fsBold];
  RT.Top         := CN.Height;
  RT.Height      := HEIGHT_REPORT_TITLE;
  RT.HAlign      := haCenter;
  RT.VAlign      := vaTop;
  RT.Width       := PageClientWidth;
  RT.Align       := baCenter;
  RT.Text        := ReportTitle;
  RT.StretchMode := smActualHeight;

  TL             := TfrxLineView.Create(ABand);
  TL.Name        := 'TitleLine';
  TL.Top         := CN.Height + RT.Height;
  TL.Align       := baWidth;
  TL.Width       := PageClientWidth;
  TL.Frame.Width := HEADER_LINE_WIDTH;
  TL.Frame.Color := clDefault;
  TL.Visible     := TL.Frame.Width <> 0;

  // needed to show correctly in designer!!! In runtime the band's height will
  // depend on the contents of RT
  ABand.Height := CN.Height + RT.Height;
end;

procedure TtsFRListReport.CreateHeaderChildBody(AChild: TfrxChild);
var
  RST : TfrxMemoView;
begin
  RST             := TfrxMemoView.Create(AChild);
  RST.Name        := 'ReportSubtitleMemo';
  RST.Font.Size   := FONTSIZE_REPORT_SUBTITLE;
  RST.Font.Style  := [fsBold];
  RST.Top         := 0;
  RST.HAlign      := haLeft;
  RST.VAlign      := vaCenter;
  RST.Width       := PageClientWidth;
  RST.Align       := baClient;
  RST.Text        := ReportSubtitle;
  RST.StretchMode := smActualHeight;
  AChild.Height   := 5;
end;

procedure TtsFRListReport.CreateDataBand;
var
  MD : TfrxMasterData;
begin
  if Assigned(ReportPage) then
  begin
    MD           := TfrxMasterData.Create(ReportPage);
    MD.Name      := 'DataBand';
    MD.Height    := HEIGHT_DATA_BAND;
    MD.Top       := 30;
    MD.DataSet   := FfrxDBDataSet;
    MD.Stretched := True;
  end;
end;

procedure TtsFRListReport.CreateDataBandFooter;
var
  DBF  : TfrxFooter;
  DBFM : TfrxMemoView;
begin
  if Assigned(ReportPage) then
  begin
    DBF           := TfrxFooter.Create(ReportPage);
    DBF.Name      := 'DataBandFooter';
    DBF.Top       := 40;   // ???? needed to suppress warning
    DBF.Height    := 0;
    DBF.Stretched := True;

    DBFM               := TfrxMemoView.Create(DBF);
    DBFM.Name          := 'DataBandFooterMemo';
    DBFM.Height        := 0;
    DBFM.Top           := 0;
    DBFM.Left          := 0;
    DBFM.Frame.Typ     := [ftTop];
    DBFM.Frame.Width   := FRAME_WIDTH;
    DBFM.Align         := baLeft;
    DBFM.AllowHTMLTags := True;
    DBFM.StretchMode   := smMaxHeight;
  end;
end;

procedure TtsFRListReport.CreateGroupMemoBand;
var
  MB : TfrxGroupHeader;
  GM : TfrxMemoView;
begin
  if Assigned(ReportPage) then
  begin
    MB           := TfrxGroupHeader.Create(ReportPage);
    MB.Name      := 'GroupMemoBand';
    MB.Height    := HEIGHT_MEMO_BAND;
    MB.Top       := 20;
    MB.Visible   := False;
    MB.Condition := '0';
    MB.Stretched := True;

    GM               := TfrxMemoView.Create(MB);
    GM.StretchMode   := smActualHeight;
    GM.AllowHTMLTags := True;
    GM.Name          := 'GroupMemo';
    GM.VAlign        := vaCenter;
    GM.DataSet       := FfrxDBDataSet;
    GM.Height        := HEIGHT_MEMO_BAND;
    GM.Frame.Width   := FRAME_WIDTH;
  end;
end;

procedure TtsFRListReport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataSet) then
    FDataSet := nil;
end;

procedure TtsFRListReport.AutoSizeCol(ACol: Integer);
var
  fTxtW : Extended;
  fMaxW : Extended;
begin
  fMaxW := 0;
  if (ACol >= 0) and (ACol < Columns.Count) and Columns[ACol].Visible then
  begin
    if Columns[ACol].Title <> '' then
      fMaxW := GetTextWidth(Columns[ACol].Title, Columns[ACol].TitleFont)
      + DATA_GAP_X * 2 + NORMAL_LINE_WIDTH * 2 + 5;

    DataSet.FindFirst;
    while not DataSet.Eof do
    begin
      if DataSet.Fields[ACol].AsString <> '' then
        fTxtW := GetTextWidth(DataSet.Fields[ACol].AsString, Columns[ACol].Font)
          + DATA_GAP_X * 2 + NORMAL_LINE_WIDTH * 2 + 5
      else
        fTxtW := fMaxW;
      fMaxW := Max(fMaxW, fTxtW);
      DataSet.Next;
    end;
    Columns[ACol].Width := fMaxW;
  end
end;

function TtsFRListReport.GetTextWidth(const AText: string;
  AFont: TFont): Integer;
var
  Bitmap  : TBitmap;
  SL      : TStringList;
  I, W, R : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AText;
    Bitmap := TBitmap.Create;
    try
      Bitmap.Canvas.Font.Assign(AFont);
      R := 0;
      for I := 0 to SL.Count - 1 do
      begin
        W := Bitmap.Canvas.TextWidth(SL[I]);
        if W > R then
          R := W;
      end;
      Result := R;
    finally
      Bitmap.Free;
    end;
  finally
    SL.Free;
  end;
end;

{
  TODO : Propotional scaling needs to be properly implemented. For now lineair
         scaling is applied, which is not very efficient.
}

procedure TtsFRListReport.SizeColumnsToPaper;
var
  iVisColCount : Integer;  // visible column count
  fTotColW     : Extended; // total column width
  fClientW     : Extended; // clientwidth of the report
  fOldW        : Extended; // old column width
  fFiller      : Extended; // redistributable space
  I            : Integer;  // loop variable
  K            : Integer;  // resizing cycle counter
  bCanResize   : Boolean;
begin
  Columns.BeginUpdate;
  try
    // should not be done if we want to take GUI-resizings into account
    AutoSizeCols;

    // determine iVisColCount and iTotColW
    iVisColCount := 0;
    fTotColW     := 0;

    for I := 0 to Pred(Columns.Count) do
    begin
      if Columns[I].Visible then
      begin
        fTotColW := fTotColW + Columns[I].Width;
        Inc(iVisColCount);
      end;
    end;

    if iVisColCount = 0 then
      Exit;

    if (fTotColW > GetPageClientWidth) and (PageOrientation = pgoAuto) and
       (ReportPage.Orientation = poPortrait) then
      ReportPage.Orientation := poLandscape;

    fClientW   := PageClientWidth;
    bCanResize := fClientW > 0;
    K := 0;
    while bCanResize and (fTotColW < fClientW) and (K < 100000) do // stretch columns
    begin
      fFiller := fClientW - fTotColW;
      fTotColW := 0; // needed for second pass

      for I := 0 to Pred(Columns.Count) do
      begin
        fOldW := Columns[I].Width;
        Columns[I].Width := Columns[I].Width + 0.01;
        fFiller := fFiller - (Columns[I].Width - fOldW);
        if Columns[I].Visible then
          fTotColW := fTotColW + Columns[I].Width;
      end; // for I := 0 to Pred(Columns.Count) do

      Inc(K);
    end; // while bCanResize and (iTotColW < iClientW) do

    K := 0;
    while bCanResize and (fTotColW > fClientW) and (K < 100000) do // shrink columns
    begin
      fFiller := fTotColW - fClientW;
      fTotColW := 0;

      for I := 0 to Pred(Columns.Count) do
      begin
        fOldW := Columns[I].Width;
        Columns[I].Width := Columns[I].Width - 0.01;

        if Columns[I].Width < 0 then
          Columns[I].Width := 0;

        fFiller := fFiller - (fOldW - Columns[I].Width);

      if Columns[I].Visible then
        fTotColW := fTotColW + Columns[I].Width;
      end; // for I := 0 to Pred(Columns.Count) do

      Inc(K);
    end; // while bCanResize and (iTotColW > iClientW) do

  finally
    Columns.EndUpdate;
  end;
end;

procedure TtsFRListReport.AutoSizeCols;
var
 I : Integer;
begin
  for I := 0 to Pred(Columns.Count) do
    AutoSizeCol(I);
end;

procedure TtsFRListReport.RegisterExportFilters;
var
  I: TExportFilter;
begin
  frxExportFilters.Clear;
  FExportFilters.Clear;

  for I := Low(ExportFilterClasses) to High(ExportFilterClasses) do
    FExportFilters.Add(ExportFilterClasses[I].Create(nil));
end;

procedure TtsFRListReport.LockUpdate;
begin
  Inc(FUpdateLock);
  if FUpdateLock = 1 then
  begin
    Screen.Cursor := crHourGlass;
    FDataSetBookmark := DataSet.Bookmark;
    DataSet.DisableControls;
  end;
end;

procedure TtsFRListReport.UnlockUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    DataSet.Bookmark := FDataSetBookmark;
    DataSet.EnableControls;
    Screen.Cursor := crDefault;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
{
  Generates the reportlayout and prepares the report for preview, printing or
  export.

  AClearLastReport          : Clears the field definitions and the report buffer

  AAssignColumnsFromDataSet : Clears any existing report columns and creates
                              report columns according to the dataset field
                              settings.

}

procedure TtsFRListReport.BuildReport(AClearLastReport,
  AAssignColumnsFromDataSet: Boolean);
begin
  LockUpdate;
  try
    if not Prepared then
    begin
      if AClearLastReport then
        ClearReport;
      if AAssignColumnsFromDataSet then
        AssignColumnsFromDataSet;
      AssignColumnProperties;
      PrepareReport(AClearLastReport);
    end;
    Prepared := True;
  finally
    UnlockUpdate;
  end;
end;

procedure TtsFRListReport.BuildReport;
begin
  BuildReport(True, True);
end;

procedure TtsFRListReport.DesignReport;
begin
  Report.DesignReport;
end;

procedure TtsFRListReport.PrepareReport(AClearLastReport: Boolean);
begin
  LockUpdate;
  try
    RegisterExportFilters;
    BuildReportLayout;
    BuildDataLayout;
    Report.PrepareReport(AClearLastReport);
  finally
    UnlockUpdate;
  end;
end;

{ Shows the reportpreview. If AShowPrepared is True (default), then the
  preview is shown after the complete report is prepared. If False, the report
  is prepared in background while the preview is shown. }

procedure TtsFRListReport.PreviewReport(AShowPrepared: Boolean);
begin
  LockUpdate;
  try
    // TEST TS BEGIN
    //BuildReport(True, True);
    // TEST TS END
    Report.Preview := Preview;
    if AShowPrepared then
      Report.ShowPreparedReport
    else
      Report.ShowReport;
  finally
    UnlockUpdate;
  end;
end;

function TtsFRListReport.PrintReport(AShowDialog: Boolean) : Boolean;
begin
  LockUpdate;
  try
    Report.PrintOptions.ShowDialog := AShowDialog;
    Result := Report.Print;
  finally
    UnlockUpdate;
  end;
end;

function TtsFRListReport.ExportReport(AExportFilter: TExportFilter;
  const AFileName: TFileName; AShowDialog: Boolean): Boolean;
var
  ExportFilter : TfrxCustomExportFilter;
begin
  Result := True;
  FExportFilter := AExportFilter;
  ExportFilter :=
    FExportFilters[Integer(AExportFilter)] as TfrxCustomExportFilter;
  ExportFilter.FileName := ChangeFileExt(AFileName, ExportFilter.DefaultExt);
  ExportFilter.ShowDialog   := AShowDialog;
  ExportFilter.ShowProgress := True;
//  if ExportFilter is TfrxXLSExport then
//  begin
//    with TfrxXLSExport(ExportFilter) do
//    begin
//      EmptyLines                 := False;
//      PageBreaks                 := False;
//      SuppressPageHeadersFooters := True;
//    end;
//  end
//  else if ExportFilter is TfrxXMLExport then
//  begin
//    with TfrxXMLExport(ExportFilter) do
//    begin
//      EmptyLines                 := False;
//      SuppressPageHeadersFooters := True;
//      Creator                    := ExtractFileName(Application.ExeName);
//    end;
//  end
//  else
  if (ExportFilter is TfrxPDFExport) then
  begin
    with TfrxPDFExport(ExportFilter) do
    begin
      PrintOptimized := True;
      Title          := ReportTitle;
      Author         := CompanyName;
      Creator        := ExtractFileName(Application.ExeName);
      Producer       := Application.Title;
      Subject        := '';
    end;
  end;
  Report.Export(ExportFilter);
end;

procedure TtsFRListReport.ClearReport;
begin
  Report.Clear;
  if Assigned(DataSet) then
    DataSet.FieldDefs.Clear;
  Prepared := False;
end;

procedure TtsFRListReport.AssignColumnProperties;
var
  I : Integer;
begin
  for I := 0 to Columns.Count - 1 do
    DoGetColumnProperties(Columns[I]);
end;

procedure TtsFRListReport.AssignColumnsFromDataSet;
var
  I      : Integer;
  Field  : TField;
  Column : TtsListReportColumn;
begin
  Columns.Clear;
  if Assigned(DataSet) then
  begin
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[I];
      Column := Columns.Add;
      if Field is TNumericField then
      // TODO (Field as TNumericField).DisplayFormat; -> not compatible with
      // FastReport's format string.
      begin
        Column.DataType := dtNumeric;
        if Field is TFloatField then
          Column.DisplayFormat := '%2.2n'
        else
          Column.DisplayFormat := '%g'
      end
      else if Field is TDateTimeField then
      begin
        Column.DataType := dtDateTime;
      end;

      Column.Sequence         := I;
      Column.Visible          := Field.Visible;
      Column.FieldName        := Field.FieldName;
      Column.Title            := Field.DisplayLabel;
      Column.TitleFont.Name   := DefaultTitleFont.Name;
      Column.TitleFont.Size   := DefaultTitleFont.Size;
      Column.TitleFont.Style  := [fsBold];
      Column.CellBorders      := [cbLeft, cbRight];
      Column.TitleBorders     := [cbLeft, cbRight, cbTop, cbBottom];
      Column.FooterFont.Style := [fsBold];
      Column.Font.Name        := DefaultFont.Name;
      Column.Font.Size        := DefaultFont.Size;
      Column.Width            :=
        GetTextWidth(DupeString('W', Field.DisplayWidth), Column.Font);
      Column.DataAlignment.Horizontal :=
        THorizontalAlignment(Ord(Field.Alignment));
      Column.FooterAlignment.Horizontal := Column.DataAlignment.Horizontal;
    end;
  end;
end;

procedure TtsFRListReport.BuildReportLayout;
begin
  FfrxDBDataSet.DataSet := DataSet;
  FfrxDBDataSet.Name    := 'frxDataSet';
  Report.DataSets.Add(FfrxDBDataSet);
  CreatePage;
  CreateHeader;
  CreateColumnHeader;
  CreateGroupMemoBand;
  CreateDataBand;
  CreateColumnFooter;
  CreateDataBandFooter;
  CreateFooter;
end;

{ Fills the DataBand with the MemoViews that are coupled to the dataset using
  the column definitions of the Columns collection property }

procedure TtsFRListReport.BuildDataLayout;
var
  mvTitle  : TfrxMemoView;
  mvData   : TfrxMemoView;
  mvFooter : TfrxMemoView;
  I        : Integer;
  fOffset  : Double;
  Column   : TtsListReportColumn;
begin
  LockUpdate;
  try
    fOffSet := 0;
    SetLength(FColumnCells, Columns.Count);

    if DataSet.Active then
    begin
      if lroAutoSizeColumns in Options then
        AutoSizeCols;
      if lroSizeColumnsToPaper in Options then
        SizeColumnsToPaper;
    end;

    FDataSet.FieldDefs.Clear;
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns.FindSequence(I);

      Assert(Assigned(Column));

      if not Assigned(Column) then
        raise Exception.CreateFmt('No data assigned to column %d', [I]);

      if Assigned(Column) and Column.Visible then
      begin
        mvTitle := TfrxMemoView.Create(ColumnHeader);
        mvTitle.Name        := Format('lbl%sTitle', [Column.Name]);
        mvTitle.Text        := Column.Title;
        mvTitle.Font        := Column.TitleFont;
        mvTitle.Color       := Column.TitleCellColor;
        mvTitle.HAlign      := TfrxHAlign(Ord(Column.TitleAlignment.Horizontal));
        mvTitle.VAlign      := TfrxVAlign(Ord(Column.TitleAlignment.Vertical));
        mvTitle.WordWrap    := Column.TitleAlignment.WordWrap;
        mvTitle.Height      := ColumnHeader.Height;
        mvTitle.Width       := Column.Width;
        mvTitle.Visible     := Column.Visible;
        mvTitle.Frame.Typ   := TfrxFrameTypes(Column.TitleBorders);
        mvTitle.Frame.Width := FRAME_WIDTH;
        mvTitle.StretchMode := smMaxHeight;

        mvData                  := TfrxMemoView.Create(DataBand);
        mvData.Name             := Format('lbl%sValue', [Column.Name]);
        mvData.Font             := Column.Font;
        mvData.Height           := mvData.Parent.Height;
        mvData.Width            := Column.Width;
        mvData.ShiftMode        := smDontShift;
        mvData.Color            := Column.CellColor;
        mvData.GapX             := DATA_GAP_X;
        mvData.WordWrap         := False;
        mvData.Frame.Typ        := TfrxFrameTypes(Column.CellBorders);
        mvData.Frame.Width      := NORMAL_LINE_WIDTH;
        mvData.StretchMode      := smMaxHeight;
        mvData.HideZeros        := Column.HideZeros;
        mvData.SuppressRepeated := Column.SuppressRepeated;
        mvData.HAlign   := TfrxHAlign(Ord(Column.DataAlignment.Horizontal));
        mvData.VAlign   := TfrxVAlign(Ord(Column.DataAlignment.Vertical));
        mvData.WordWrap := Column.DataAlignment.WordWrap;
        FColumnCells[I] := mvData.Name;
        DataBand.Height := Max(Column.Height, DataBand.Height);

        mvFooter             := TfrxMemoView.Create(DataBandFooter);
        mvFooter.Name        := Format('lbl%sFooterValue', [Column.Name]);
        mvFooter.Font        := Column.FooterFont;
        mvFooter.Width       := Column.Width;
        mvFooter.ShiftMode   := smDontShift;
        mvFooter.Color       := Column.FooterCellColor;
        mvFooter.GapX        := DATA_GAP_X;
        mvFooter.WordWrap    := False;
        mvFooter.Frame.Typ   := TfrxFrameTypes(Column.FooterBorders);
        mvFooter.Frame.Width := NORMAL_LINE_WIDTH;
        mvFooter.StretchMode := smMaxHeight;
        mvFooter.Text        := Column.FooterExpression;
        mvFooter.HAlign  := TfrxHAlign(Ord(Column.FooterAlignment.Horizontal));
        mvFooter.VAlign  := TfrxVAlign(Ord(Column.FooterAlignment.Vertical));
        mvFooter.Visible := Column.FooterVisible;

        case Column.DataType of
          dtNumeric :
          begin
            mvData.DisplayFormat.Kind      := fkNumeric;
            mvData.DisplayFormat.FormatStr := Column.DisplayFormat;
            if Column.FooterVisible then
            begin
              mvFooter.DisplayFormat.Kind      := fkNumeric;
              mvFooter.DisplayFormat.FormatStr := Column.DisplayFormat;
            end;
          end;
          dtString   : mvData.DisplayFormat.Kind := fkText;
          dtDateTime : mvData.DisplayFormat.Kind := fkDateTime;
        end;
        mvData.DataSet   := FfrxDBDataSet;
        mvData.DataField := Column.FieldName;

        with FDataSet.FieldDefs.AddFieldDef do
        begin
          Name        := Column.FieldName;
          case Column.DataType of
            dtNumeric: DataType := ftFloat;
            dtString :
            begin
              DataType := ftString;
              Size     := MAX_FIELD_SIZE;
            end
            else
              DataType := ftDateTime;
          end;
        end;

        mvTitle.Left := fOffset;
        mvData.Left  := fOffset;
        if Column.FooterVisible then
          mvFooter.Left := fOffset;

        fOffset := fOffset + mvData.Width;
      end; // if Assigned(Column) and Column.Visible then
    end; // for I := 0 to Columns.Count - 1 do

    TableWidth := fOffset;

    if Assigned(GroupMemo) then
    begin
      GroupMemo.Width := TableWidth;
      GroupMemo.Frame.Typ := [ftLeft, ftRight, ftBottom];
    end;

    if Assigned(DataBandFooterMemo) then
    begin
      DataBandFooterMemo.Width := TableWidth;
    end;

    // Needed to reference the memo fields in the OnBefore print event handler
    // of the report
    SetLength(FColumnCells, DataSet.FieldDefs.Count);
  finally
    UnlockUpdate;
  end;
end;
{$ENDREGION}

end.

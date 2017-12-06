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

unit DataGrabber.ConnectionViewManager;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Actions,
  System.ImageList,
  Vcl.ActnList, Vcl.Menus, Vcl.ImgList, Vcl.Controls,

  DataGrabber.DataInspector, DataGrabber.FieldInspector,

  DataGrabber.Interfaces, DataGrabber.ConnectionProfiles,

  DDuce.Logger;

{$REGION 'documentation'}
{
  The ConnectionViewManager is a singleton instance which manages:
    - the application settings (TDGSettings)
    - the ConnectionView instances (ConnectionViews)
    - the active connection view (ActiveConnectionView)
    - all actions that can be executed on the active connectionview

    TODO:
      react on changes in manager
}
{$ENDREGION}

type
  TdmConnectionViewManager = class(TDataModule, IConnectionViewManager)
    {$REGION 'designer controls'}
    aclActions                    : TActionList;
    actExecute                    : TAction;
    actHideEmptyColumns           : TAction;
    actShowAllColumns             : TAction;
    actToggleStayOnTop            : TAction;
    actHideSelectedColumns        : TAction;
    actHideConstantColumns        : TAction;
    actSelectionAsWiki            : TAction;
    actSelectionAsText            : TAction;
    actMergeAllColumnCells        : TAction;
    actAutoSizeCols               : TAction;
    actFormatSQL                  : TAction;
    actToggleFullScreen           : TAction;
    actCopy                       : TAction;
    actGroupBySelection           : TAction;
    actSelectionAsCommaText       : TAction;
    actSelectionAsQuotedCommaText : TAction;
    actMergeCells                 : TAction;
    actSelectionAsTextTable       : TAction;
    actInspect                    : TAction;
    actDataInspector              : TAction;
    actSettings                   : TAction;
    actInspectDataSet             : TAction;
    actInspectConnection          : TAction;
    actInspectGrid                : TAction;
    actInspectFields              : TAction;
    actPreview                    : TAction;
    actDesigner                   : TAction;
    actPrint                      : TAction;
    actDebug                      : TAction;
    actSelectionAsFields          : TAction;
    actSelectionAsQuotedFields    : TAction;
    actFavoriteFieldsOnly         : TAction;
    actRtti                       : TAction;
    actSelectionAsWhereIn         : TAction;
    ppmConnectionView             : TPopupMenu;
    mniGroupBySelection           : TMenuItem;
    mniHideSelectedColumns        : TMenuItem;
    mniHideEmptyColumns           : TMenuItem;
    mniHideConstantColumns        : TMenuItem;
    mniShowAllColumns             : TMenuItem;
    mniN1                         : TMenuItem;
    mniMergeColumns               : TMenuItem;
    mniCopy                       : TMenuItem;
    mniCopyWikiTable              : TMenuItem;
    mniCopyTextTable              : TMenuItem;
    mniSelectionAsTextTable       : TMenuItem;
    mniSelectionAsCommaText       : TMenuItem;
    mniSelectionAsQuotedCommaText : TMenuItem;
    mniSelectionAsFields          : TMenuItem;
    mniSelectionAsQuotedFields    : TMenuItem;
    mniInspectConnection          : TMenuItem;
    mniInspectDataSet             : TMenuItem;
    mniInspectFields              : TMenuItem;
    mniInspectGrid                : TMenuItem;
    mniN3                         : TMenuItem;
    mniSettings                   : TMenuItem;
    ppmEditorView                 : TPopupMenu;
    mniFormatSQL1                 : TMenuItem;
    mniAutoSizeCols               : TMenuItem;
    imlMain                       : TImageList;
    Selection1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Inspect1: TMenuItem;
    N4: TMenuItem;
    actAddConnectionView: TAction;
    actInspectFDManager: TAction;
    Inspectconnectionmanager1: TMenuItem;
    actChinookExampleQuery: TAction;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddConnectionViewExecute(Sender: TObject);
    procedure actAutoSizeColsExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actDataInspectorExecute(Sender: TObject);
    procedure actDesignerExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actFavoriteFieldsOnlyExecute(Sender: TObject);
    procedure actGroupBySelectionExecute(Sender: TObject);
    procedure actHideConstantColumnsExecute(Sender: TObject);
    procedure actHideEmptyColumnsExecute(Sender: TObject);
    procedure actHideSelectedColumnsExecute(Sender: TObject);
    procedure actInspectConnectionExecute(Sender: TObject);
    procedure actInspectDataSetExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actInspectFDManagerExecute(Sender: TObject);
    procedure actInspectFieldsExecute(Sender: TObject);
    procedure actInspectGridExecute(Sender: TObject);
    procedure actMergeAllColumnCellsExecute(Sender: TObject);
    procedure actMergeCellsExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actSelectionAsCommaTextExecute(Sender: TObject);
    procedure actSelectionAsFieldsExecute(Sender: TObject);
    procedure actSelectionAsQuotedCommaTextExecute(Sender: TObject);
    procedure actSelectionAsQuotedFieldsExecute(Sender: TObject);
    procedure actSelectionAsTextExecute(Sender: TObject);
    procedure actSelectionAsTextTableExecute(Sender: TObject);
    procedure actSelectionAsWhereInExecute(Sender: TObject);
    procedure actSelectionAsWikiExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowAllColumnsExecute(Sender: TObject);
    procedure actChinookExampleQueryExecute(Sender: TObject);
    {$ENDREGION}

  private
    FSettings             : ISettings;
    FConnectionViewList   : TConnectionViewList;
    FActiveConnectionView : IConnectionView;
    FStopWatch            : TStopwatch;
    FDataInspector        : TfrmDataInspector;
    FFieldInspector       : TfrmFieldInspector;

    {$REGION 'property access methods'}
    function GetSettings: ISettings;
    function GetActiveConnectionView: IConnectionView;
    procedure SetActiveConnectionView(const Value: IConnectionView);
    function GetActiveDataView: IDataView;
    function GetActiveData: IData;
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;
    function GetConnectionViewPopupMenu: TPopupMenu;
    function GetDefaultConnectionProfile: TConnectionProfile;
    {$ENDREGION}

  protected
    procedure Execute(const ASQL: string);
    procedure ApplySettings;
    procedure UpdateActions;
    procedure UpdateConnectionViewCaptions;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(ASettings: ISettings); reintroduce; virtual;

    function AddConnectionView: IConnectionView;

    property ActiveConnectionView: IConnectionView
      read GetActiveConnectionView write SetActiveConnectionView;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

    property Settings: ISettings
      read GetSettings;

    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ConnectionViewPopupMenu: TPopupMenu
      read GetConnectionViewPopupMenu;

    property DefaultConnectionProfile: TConnectionProfile
      read GetDefaultConnectionProfile;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Forms, Vcl.Clipbrd, Vcl.Dialogs,

  DDuce.ObjectInspector.zObjectInspector,

  DataGrabber.Settings.Dialog, DataGrabber.ConnectionView,
  DataGrabber.Data, DataGrabber.Resources,

  FireDAC.Comp.Client,

  Spring.Container;

{$REGION 'construction and destruction'}
constructor TdmConnectionViewManager.Create(ASettings: ISettings);
begin
  inherited Create(Application);
  FSettings := ASettings;
end;

procedure TdmConnectionViewManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings.Load;
  FConnectionViewList := TConnectionViewList.Create;
  FDataInspector      := TfrmDataInspector.Create(Self);
  FFieldInspector     := TfrmFieldInspector.Create(Self);

  // disable actions that are not fully implemented yet
  actPreview.Visible            := False;
  actPrint.Visible              := False;
  actDesigner.Visible           := False;
  actFavoriteFieldsOnly.Visible := False;
  actRtti.Visible               := False;
  actToggleFullScreen.Visible   := False;
  actDataInspector.Visible      := False;
end;

procedure TdmConnectionViewManager.BeforeDestruction;
begin
  FSettings.FormSettings.Assign(Application.MainForm);
  FSettings.Save;
  FreeAndNil(FDataInspector);
  FreeAndNil(FConnectionViewList);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
// editor
procedure TdmConnectionViewManager.actSettingsExecute(Sender: TObject);
begin
  ExecuteSettingsDialog(Settings,
    procedure
    begin
      ApplySettings;
      UpdateActions;
    end
  );
end;

procedure TdmConnectionViewManager.actCopyExecute(Sender: TObject);
begin
  ActiveConnectionView.Copy;
end;

// grid
{$REGION 'DataView actions'}
procedure TdmConnectionViewManager.actFavoriteFieldsOnlyExecute(
  Sender: TObject);
begin
  (ActiveData as IFieldVisiblity).ShowFavoriteFieldsOnly :=
    not actFavoriteFieldsOnly.Checked;
  ActiveDataView.UpdateView;
end;

procedure TdmConnectionViewManager.actGroupBySelectionExecute(Sender: TObject);
begin
  (ActiveDataView as IGroupable).GroupBySelectedColumns;
end;

procedure TdmConnectionViewManager.actShowAllColumnsExecute(Sender: TObject);
begin
  if (ActiveData as IFieldVisiblity).ShowAllFields then
    ActiveDataView.UpdateView;
end;

procedure TdmConnectionViewManager.actSelectionAsCommaTextExecute(
  Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToCommaText(False);
end;

procedure TdmConnectionViewManager.actSelectionAsFieldsExecute(Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToFields(False);
end;

procedure TdmConnectionViewManager.actSelectionAsQuotedCommaTextExecute(
  Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToCommaText(True);
end;

procedure TdmConnectionViewManager.actSelectionAsQuotedFieldsExecute(
  Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToFields;
end;

procedure TdmConnectionViewManager.actSelectionAsTextExecute(Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToDelimitedTable;
end;

procedure TdmConnectionViewManager.actSelectionAsTextTableExecute(
  Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToTextTable(True);
end;

procedure TdmConnectionViewManager.actSelectionAsWhereInExecute(
  Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actSelectionAsWikiExecute(Sender: TObject);
begin
  Clipboard.AsText := ActiveDataView.SelectionToWikiTable(True);
end;

procedure TdmConnectionViewManager.actHideConstantColumnsExecute(
  Sender: TObject);
begin
  (ActiveData as IFieldVisiblity).ConstantFieldsVisible := not
  (ActiveData as IFieldVisiblity).ConstantFieldsVisible;
  ActiveDataView.UpdateView;
end;

procedure TdmConnectionViewManager.actHideEmptyColumnsExecute(Sender: TObject);
begin
  (ActiveData as IFieldVisiblity).EmptyFieldsVisible :=
    not (ActiveData as IFieldVisiblity).EmptyFieldsVisible;
  ActiveDataView.UpdateView;
end;

procedure TdmConnectionViewManager.actHideSelectedColumnsExecute(
  Sender: TObject);
begin
  ActiveDataView.HideSelectedColumns;
end;

procedure TdmConnectionViewManager.actInspectConnectionExecute(Sender: TObject);
begin
  InspectComponent(ActiveData.Connection);
end;

procedure TdmConnectionViewManager.actInspectDataSetExecute(Sender: TObject);
begin
  InspectComponent(ActiveData.DataSet);
end;

procedure TdmConnectionViewManager.actInspectExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actInspectFDManagerExecute(Sender: TObject);
begin
  InspectComponent(FDManager);
end;

procedure TdmConnectionViewManager.actInspectFieldsExecute(Sender: TObject);
begin
  FFieldInspector.Data := ActiveData;
  FFieldInspector.Show;
end;

procedure TdmConnectionViewManager.actInspectGridExecute(Sender: TObject);
begin
  ActiveDataView.Inspect;
end;

procedure TdmConnectionViewManager.actMergeAllColumnCellsExecute(
  Sender: TObject);
begin
  (ActiveDataView as IMergable).MergeColumnCells := actMergeAllColumnCells.Checked;
end;

procedure TdmConnectionViewManager.actMergeCellsExecute(Sender: TObject);
begin
  (ActiveDataView as IMergable).MergeColumnCells :=
    not (ActiveDataView as IMergable).MergeColumnCells;
end;

procedure TdmConnectionViewManager.actAddConnectionViewExecute(Sender: TObject);
begin
  AddConnectionView;
end;

procedure TdmConnectionViewManager.actAutoSizeColsExecute(Sender: TObject);
begin
  ActiveDataView.AutoSizeColumns;
end;
{$ENDREGION}

// data
{$REGION 'ActiveData actions'}
procedure TdmConnectionViewManager.actDataInspectorExecute(Sender: TObject);
begin
  FSettings.DataInspectorVisible := actDataInspector.Checked;
  FDataInspector.Data := ActiveDataView.Data;
  if actDataInspector.Checked then
    FDataInspector.Show;
end;

procedure TdmConnectionViewManager.actChinookExampleQueryExecute(Sender: TObject);
begin
  ActiveConnectionView.EditorView.Text := CHINOOK_EXAMPLE_QUERY;
end;

procedure TdmConnectionViewManager.actExecuteExecute(Sender: TObject);
begin
  Execute(ActiveConnectionView.EditorView.Text);
end;

procedure TdmConnectionViewManager.actPrintExecute(Sender: TObject);
begin
  //(ActiveData as IDataReport).PrintReport;
end;

procedure TdmConnectionViewManager.actDesignerExecute(Sender: TObject);
begin
//  (ActiveData as IDataReport).DesignReport;
//  (ActiveData as IDataReport).EditProperties;
end;

procedure TdmConnectionViewManager.actPreviewExecute(Sender: TObject);
begin
//  (ActiveData as IDataReport).ReportTitle := 'DataGrabber';
//  (ActiveData as IDataReport).PreviewReport;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'property access methods'}
function TdmConnectionViewManager.GetActionList: TActionList;
begin
  Result := aclActions;
end;

function TdmConnectionViewManager.GetActiveConnectionView: IConnectionView;
begin
  Result := FActiveConnectionView;
end;

procedure TdmConnectionViewManager.SetActiveConnectionView(
  const Value: IConnectionView);
begin
  FActiveConnectionView := Value;
end;

function TdmConnectionViewManager.GetActiveData: IData;
begin
  if Assigned(FActiveConnectionView) then
    Result := FActiveConnectionView.ActiveData
  else
    Result := nil;
end;

function TdmConnectionViewManager.GetActiveDataView: IDataView;
begin
  if Assigned(FActiveConnectionView) then
    Result := FActiveConnectionView.ActiveDataView
  else
    Result := nil;
end;

function TdmConnectionViewManager.GetConnectionViewPopupMenu: TPopupMenu;
begin
  Result := ppmConnectionView;
end;

function TdmConnectionViewManager.GetDefaultConnectionProfile: TConnectionProfile;
begin
  Result := FSettings.ConnectionProfiles.Find(FSettings.DefaultConnectionProfile);
end;

function TdmConnectionViewManager.GetItem(AName: string): TCustomAction;
var
  I: Integer;
begin
  I := ActionList.ActionCount - 1;
  while (I >= 0) and (CompareText(TAction(ActionList[I]).Name, AName) <> 0) do
    Dec(I);
  if I >= 0 then
    Result := ActionList[I] as TCustomAction
  else
    Result := nil;
end;

function TdmConnectionViewManager.GetSettings: ISettings;
begin
  Result := FSettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmConnectionViewManager.ApplySettings;
begin
  ActiveConnectionView.ApplySettings;
end;

procedure TdmConnectionViewManager.Execute(const ASQL: string);
begin
  FStopWatch.Reset;
  Screen.Cursor := crSQLWait;
  try
    ActiveData.SQL := ASQL;
    FStopWatch.Start;
    ActiveData.Execute;
    FStopWatch.Stop;
    if FDataInspector.Visible then
    begin
      FDataInspector.Data := ActiveData;
    end;
    if FFieldInspector.Visible then
    begin
      FFieldInspector.Data := ActiveData;
    end;
  finally
    Screen.Cursor := crDefault;
//    pnlElapsedTime.Caption := Format('%d ms', [FStopWatch.ElapsedMilliseconds]);
  end;
end;

procedure TdmConnectionViewManager.UpdateActions;
var
  B: Boolean;
begin
  if Assigned(FSettings) then
  begin
    actToggleStayOnTop.Checked := FSettings.FormSettings.FormStyle = fsStayOnTop;
    actDataInspector.Checked   := FSettings.DataInspectorVisible;
  end;

  if Assigned(ActiveData) and Assigned(ActiveDataView) then
  begin
    B := ActiveData.Active;
    actMergeCells.Visible          := Supports(ActiveDataView, IMergable);
    actMergeCells.Enabled          := B and Supports(ActiveDataView, IMergable);
    actGroupBySelection.Visible    := Supports(ActiveDataView, IGroupable);
    actGroupBySelection.Enabled    := B and Supports(ActiveDataView, IGroupable);
    actMergeAllColumnCells.Visible := actMergeCells.Visible;
    actMergeAllColumnCells.Enabled := actMergeCells.Enabled;
    actFavoriteFieldsOnly.Enabled  := B;
    actHideEmptyColumns.Enabled    := B;
    actHideConstantColumns.Enabled := B;
    actHideSelectedColumns.Enabled := B;
    actShowAllColumns.Enabled      := B;
    actPreview.Enabled             := B;
    actPrint.Enabled               := B;
    actDesigner.Enabled            := B;
    actHideEmptyColumns.Checked    :=
      not (ActiveData as IFieldVisiblity).EmptyFieldsVisible;
    actHideConstantColumns.Checked :=
      not (ActiveData as IFieldVisiblity).ConstantFieldsVisible;
    actFavoriteFieldsOnly.Checked  :=
      (ActiveData as IFieldVisiblity).ShowFavoriteFieldsOnly;

    Logger.Watch('RecordCount', ActiveDataView.RecordCount);
  end;
end;

procedure TdmConnectionViewManager.UpdateConnectionViewCaptions;
var
  CV : IConnectionView;
  I  : Integer;
begin
  for I := 0 to FConnectionViewList.Count - 1 do
  begin
    CV := FConnectionViewList[I] as IConnectionView;
    if Assigned(CV.ActiveConnectionProfile) then
      CV.Form.Caption := Format('(%d) %s', [I + 1, CV.ActiveConnectionProfile.Name]);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TdmConnectionViewManager.AddConnectionView: IConnectionView;
var
  CV : IConnectionView;
  EV : IEditorView;
  DV : IDataView;
  D  : IData;
  CP : TConnectionProfile;
begin
  EV := GlobalContainer.Resolve<IEditorView>;
  DV := GlobalContainer.Resolve<IDataView>(Settings.GridType);
  DV.Settings := FSettings as IDataViewSettings;
  DV.PopupMenu := ConnectionViewPopupMenu;
  if Assigned(FActiveConnectionView) then
  begin
    CP := FActiveConnectionView.ActiveConnectionProfile;
  end
  else
  begin
    CP := DefaultConnectionProfile;
  end;
  D := TdmDataFireDAC.Create(Self, CP.ConnectionSettings);
  DV.Data := D;
  CV := TfrmConnectionView.Create(Self, EV, DV, D);
  FConnectionViewList.Add(CV);
  ActiveConnectionView := CV;
  UpdateConnectionViewCaptions;
  Result := CV;
end;
{$ENDREGION}

initialization
  TdmConnectionViewManager.ClassName;

end.

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

  ts.Interfaces, ts.Modules.DataInspector, ts.Modules.FieldInspector,

  DataGrabber.Interfaces, DataGrabber.Data;

{
  The ConnectionViewManager is a singleton instance which manages:
    - the application settings (TDGSettings)
    - the ConnectionView instances (ConnectionViews)
    - the active connection view
    - all actions that can be executed on the active connectionview
}

type
  TdmConnectionViewManager = class(TDataModule, IConnectionViewManager)
    {$REGION 'designer controls'}
    aclActions                    : TActionList;
    actExecute                    : TAction;
    actHideEmptyColumns           : TAction;
    actShowAllColumns             : TAction;
    actToggleStayOnTop            : TAction;
    actExecuteLimited             : TAction;
    actHideSelectedColumns        : TAction;
    actHideConstantColumns        : TAction;
    actSelectionAsWiki            : TAction;
    actSelectionAsText            : TAction;
    actMergeAllColumnCells        : TAction;
    actGridMode                   : TAction;
    actAutoSizeCols               : TAction;
    actFormatSQL                  : TAction;
    actToggleFullScreen           : TAction;
    actCopy                       : TAction;
    actGroupBySelection           : TAction;
    actSelectionAsCommaText       : TAction;
    actSelectionAsQuotedCommaText : TAction;
    actMergeCells                 : TAction;
    actSelectionAsTextTable       : TAction;
    actStartTransaction           : TAction;
    actCommitTransaction          : TAction;
    actRollbackTransaction        : TAction;
    actProviderMode               : TAction;
    actInspect                    : TAction;
    actDataInspector              : TAction;
    actcxGrid                     : TAction;
    actGridView                   : TAction;
    actKGrid                      : TAction;
    actADO                        : TAction;
    actZEOS                       : TAction;
    actDBX                        : TAction;
    actSettings                   : TAction;
    actInspectDataSet             : TAction;
    actInspectConnection          : TAction;
    actInspectGrid                : TAction;
    actUNI                        : TAction;
    actInspectFields              : TAction;
    actToggleRepositoryTree       : TAction;
    actSyncEditorWithRepository   : TAction;
    actPreview                    : TAction;
    actDesigner                   : TAction;
    actPrint                      : TAction;
    actDebug                      : TAction;
    actSelectionAsFields          : TAction;
    actSelectionAsQuotedFields    : TAction;
    actFavoriteFieldsOnly         : TAction;
    actRtti                       : TAction;
    actSelectionAsWhereIn         : TAction;
    actCreateModel                : TAction;
    ppmConnectionView             : TPopupMenu;
    mniGroupBySelection           : TMenuItem;
    mniHideSelectedColumns        : TMenuItem;
    mniHideEmptyColumns           : TMenuItem;
    mniHideConstantColumns        : TMenuItem;
    mniShowAllColumns             : TMenuItem;
    mniN1                         : TMenuItem;
    mniMergeColumns               : TMenuItem;
    mniN2                         : TMenuItem;
    mniCopy                       : TMenuItem;
    mniCopyWikiTable              : TMenuItem;
    mniCopyTextTable              : TMenuItem;
    mniSelectionAsTextTable       : TMenuItem;
    mniSelectionAsCommaText       : TMenuItem;
    mniSelectionAsQuotedCommaText : TMenuItem;
    mniSelectionAsFields          : TMenuItem;
    mniSelectionAsQuotedFields    : TMenuItem;
    mniN11                        : TMenuItem;
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
    {$ENDREGION}

    procedure actExecuteExecute(Sender: TObject);
    procedure actExecuteLimitedExecute(Sender: TObject);
    procedure actStartTransactionExecute(Sender: TObject);
    procedure actCommitTransactionExecute(Sender: TObject);
    procedure actRollbackTransactionExecute(Sender: TObject);
    procedure actProviderModeExecute(Sender: TObject);
    procedure actAutoSizeColsExecute(Sender: TObject);
    procedure actShowAllColumnsExecute(Sender: TObject);
    procedure actHideSelectedColumnsExecute(Sender: TObject);
    procedure actHideConstantColumnsExecute(Sender: TObject);
    procedure actSelectionAsWikiExecute(Sender: TObject);
    procedure actSelectionAsTextExecute(Sender: TObject);
    procedure actMergeAllColumnCellsExecute(Sender: TObject);
    procedure actGridModeExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actGroupBySelectionExecute(Sender: TObject);
    procedure actSelectionAsCommaTextExecute(Sender: TObject);
    procedure actSelectionAsQuotedCommaTextExecute(Sender: TObject);
    procedure actMergeCellsExecute(Sender: TObject);
    procedure actSelectionAsTextTableExecute(Sender: TObject);
    procedure actcxGridExecute(Sender: TObject);
    procedure actGridViewExecute(Sender: TObject);
    procedure actKGridExecute(Sender: TObject);
    procedure actSelectionAsFieldsExecute(Sender: TObject);
    procedure actFavoriteFieldsOnlyExecute(Sender: TObject);
    procedure actSelectionAsWhereInExecute(Sender: TObject);
    procedure actHideEmptyColumnsExecute(Sender: TObject);
    procedure actSelectionAsQuotedFieldsExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actDataInspectorExecute(Sender: TObject);
    procedure actInspectDataSetExecute(Sender: TObject);
    procedure actInspectConnectionExecute(Sender: TObject);
    procedure actInspectGridExecute(Sender: TObject);
    procedure actInspectFieldsExecute(Sender: TObject);
    procedure actUNIExecute(Sender: TObject);
    procedure actDBXExecute(Sender: TObject);
    procedure actZEOSExecute(Sender: TObject);
    procedure actADOExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actDesignerExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);

  private
    FSettings             : IDGSettings;
    FConnectionViewList   : TConnectionViewList;
    FActiveConnectionView : IConnectionView;
    FActiveDataView       : IDGDataView;
    FActiveData           : IData;
    FStopWatch            : TStopwatch;
    FDataInspector        : TfrmDataInspector;
    FFieldInspector       : TfrmFieldInspector;

    function GetSettings: IDGSettings;
    function GetActiveConnectionView: IConnectionView;
    procedure SetActiveConnectionView(const Value: IConnectionView);
    function GetActiveDataView: IDGDataView;
    function GetActiveData: IData;
    function GetActionList: TActionList;
    function GetItem(AName: string): TCustomAction;
    function GetConnectionViewPopupMenu: TPopupMenu;

  protected
    procedure Execute(const ASQL: string);
    procedure ApplySettings;
    procedure UpdateActions;
    procedure UpdateConnectionViewCaptions;

  public
    constructor Create(ASettings: IDGSettings); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddConnectionView: IConnectionView;

    property ActiveConnectionView: IConnectionView
      read GetActiveConnectionView write SetActiveConnectionView;

    property ActiveDataView: IDGDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

    property Settings: IDGSettings
      read GetSettings;

    property ActionList: TActionList
      read GetActionList;

    property Items[AName: string]: TCustomAction
      read GetItem; default;

    property ConnectionViewPopupMenu: TPopupMenu
      read GetConnectionViewPopupMenu;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Forms, Vcl.Clipbrd, Vcl.Dialogs,

  DDuce.ObjectInspector,

  DataGrabber.SettingsDialog, DataGrabber.ConnectionView,

  Spring.Container;

{$REGION 'construction and destruction'}
constructor TdmConnectionViewManager.Create(ASettings: IDGSettings);
begin
  inherited Create(Application);
  FSettings := ASettings;
end;

procedure TdmConnectionViewManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FSettings.Load;
  FConnectionViewList := TConnectionViewList.Create;
//  FDataInspector := TfrmDataInspector.Create(Self);
//  FFieldInspector := TfrmFieldInspector.Create(Self);
//  FDataInspector.HideEmptyFields := True;

  // disable actions that are not fully implemented yet
  actSyncEditorWithRepository.Visible := False;
  actToggleRepositoryTree.Visible     := False;
  actCreateModel.Visible              := False;
  actPreview.Visible                  := False;
  actPrint.Visible                    := False;
  actDesigner.Visible                 := False;
  actFavoriteFieldsOnly.Visible       := False;
  actRtti.Visible                     := False;
  actToggleFullScreen.Visible         := False;
end;

procedure TdmConnectionViewManager.BeforeDestruction;
begin
//  FreeAndNil(FDataInspector);

//  FSettings.FormSettings.Assign(Self);
//  FSettings.FormSettings.VSplitterPos := pnlConnectionProfiles.Width;
//  FSettings.FormSettings.HSplitterPos := pnlTop.Height;
//  FSettings.Save;

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

procedure TdmConnectionViewManager.actGridModeExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actGridViewExecute(Sender: TObject);
begin
  FSettings.GridType := 'GridView';
  ApplySettings;
end;

procedure TdmConnectionViewManager.actGroupBySelectionExecute(Sender: TObject);
begin
  (ActiveDataView as IGroupable).GroupBySelectedColumns;
end;

procedure TdmConnectionViewManager.actShowAllColumnsExecute(Sender: TObject);
begin
  ActiveDataView.ShowAllColumns;
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
  ActiveDataView.ConstantColumnsVisible := not actHideConstantColumns.Checked;
end;

procedure TdmConnectionViewManager.actHideEmptyColumnsExecute(Sender: TObject);
begin
  ActiveDataView.EmptyColumnsVisible := not actHideEmptyColumns.Checked;
end;

procedure TdmConnectionViewManager.actHideSelectedColumnsExecute(
  Sender: TObject);
begin
  ActiveDataView.HideSelectedColumns;
end;

procedure TdmConnectionViewManager.actInspectConnectionExecute(Sender: TObject);
begin
  InspectComponent(ActiveData.Connection.Connection);
end;

procedure TdmConnectionViewManager.actInspectDataSetExecute(Sender: TObject);
begin
  InspectComponent(ActiveData.DataSet);
end;

procedure TdmConnectionViewManager.actInspectExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actInspectFieldsExecute(Sender: TObject);
begin
  FFieldInspector.Show;
end;

procedure TdmConnectionViewManager.actInspectGridExecute(Sender: TObject);
begin
  ActiveDataView.Inspect;
end;

procedure TdmConnectionViewManager.actKGridExecute(Sender: TObject);
begin
  FSettings.GridType := 'KGrid';
  ApplySettings;
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

procedure TdmConnectionViewManager.actZEOSExecute(Sender: TObject);
begin
  FSettings.ConnectionType := 'ZEOS';
  ApplySettings;
end;

procedure TdmConnectionViewManager.actADOExecute(Sender: TObject);
begin
  FSettings.ConnectionType := 'ADO';
  ApplySettings;
end;

procedure TdmConnectionViewManager.actAutoSizeColsExecute(Sender: TObject);
begin
  ActiveDataView.AutoSizeColumns;
end;

procedure TdmConnectionViewManager.actcxGridExecute(Sender: TObject);
begin
  FSettings.GridType := 'cxGrid';
  ApplySettings;
end;
{$ENDREGION}

// data
{$REGION 'ActiveData actions'}
procedure TdmConnectionViewManager.actDataInspectorExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
//  FSettings.DataInspectorVisible := actDataInspector.Checked;
//  if actDataInspector.Checked then
//    ShowToolWindow(FDataInspector)
//  else
//    HideToolWindow(FDataInspector);
end;

procedure TdmConnectionViewManager.actDBXExecute(Sender: TObject);
begin
  FSettings.ConnectionType := 'DBX';
  ApplySettings;
end;

procedure TdmConnectionViewManager.actCommitTransactionExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
//  ActiveDataView
//
end;

procedure TdmConnectionViewManager.actExecuteExecute(Sender: TObject);
begin
  ActiveData.MaxRecords := 0;
  Execute(ActiveConnectionView.EditorView.Text);
end;

procedure TdmConnectionViewManager.actProviderModeExecute(Sender: TObject);
begin
  FSettings.ProviderMode  := actProviderMode.Checked;
  ActiveData.ProviderMode := FSettings.ProviderMode;
end;

procedure TdmConnectionViewManager.actRollbackTransactionExecute(
  Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actStartTransactionExecute(Sender: TObject);
begin
  ShowMessage('Not supported yet.');
end;

procedure TdmConnectionViewManager.actUNIExecute(Sender: TObject);
begin
  FSettings.ConnectionType := 'UNI';
  ApplySettings;
end;

procedure TdmConnectionViewManager.actExecuteLimitedExecute(Sender: TObject);
begin
  ActiveData.MaxRecords := 100;
  Execute(ActiveConnectionView.EditorView.Text);
end;

procedure TdmConnectionViewManager.actPrintExecute(Sender: TObject);
begin
//  (ActiveData as IDataReport).PrintReport;
end;

procedure TdmConnectionViewManager.actDesignerExecute(Sender: TObject);
begin
//  (ActiveData as IDataReport).EditProperties;
//  (ActiveData as IDataReport).DesignReport;
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
  Result := FActiveData;
end;

function TdmConnectionViewManager.GetActiveDataView: IDGDataView;
begin
  Result := FActiveDataView;
end;

function TdmConnectionViewManager.GetConnectionViewPopupMenu: TPopupMenu;
begin
  Result := ppmConnectionView;
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

function TdmConnectionViewManager.GetSettings: IDGSettings;
begin
  Result := FSettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TdmConnectionViewManager.ApplySettings;
//var
//  S  : string;
//  CP : TConnectionProfile;
begin
//  ActiveConnectionView.ApplySettings;
//  vstProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
//  if Assigned(vstProfiles.FocusedNode) then
//  begin
//    CP := FSettings.ConnectionProfiles.Items[vstProfiles.FocusedNode.Index];
//    FSettings.DefaultConnectionProfile := CP.Name;
//    FSettings.ConnectionSettings.Assign(CP.ConnectionSettings);
//    if CP.ConnectionType <> '' then
//      FSettings.ConnectionType := CP.ConnectionType;
//    FSettings.ProviderMode := CP.ProviderMode;
//    FSettings.PacketRecords := CP.PacketRecords;
//    FEditor.Color := CP.ProfileColor;
//    Application.Title := CP.Name;
//  end
//  else
//    FSettings.DefaultConnectionProfile := '';
////
//  CreateData(FSettings.ConnectionType);
//  FData.Connection.ConnectionSettings.Assign(FSettings.ConnectionSettings);
//  FData.PacketRecords := FSettings.PacketRecords;
//  FData.ProviderMode  := FSettings.ProviderMode;
//  FData.FetchOnDemand := FSettings.FetchOnDemand;
////
//  CreateView(FSettings.GridType);
////
//  S := FSettings.ConnectionType;
//  if S = 'ADO' then
//    actADO.Checked := True
//  else if S = 'DBX' then
//    actDBX.Checked := True
//  else if S = 'ZEOS' then
//    actZEOS.Checked := True
//  else if S = 'UNI' then
//    actUNI.Checked := True;
//
//  S := FSettings.GridType;
//  if S = 'cxGrid' then
//    actcxGrid.Checked := True
//  else if S = 'GridView' then
//    actGridView.Checked := True
//  else if S = 'KGrid' then
//    actKGrid.Checked := True
//  else if S = 'VirtualDBGrid' then
//    actVirtualDBGrid.Checked := True;
//
//  if FSettings.RepositoryVisible then
//    ShowToolWindow(FTree);
//
//  if FSettings.DataInspectorVisible then
//    ShowToolWindow(FDataInspector);
end;

procedure TdmConnectionViewManager.Execute(const ASQL: string);
begin
  FStopWatch.Reset;
  Screen.Cursor := crSQLWait;
  try
    //pnlStatus.Caption := SFetchingData;
    //OptimizeWidth(pnlStatus);
    //Application.ProcessMessages;
    //FScriptParser.ParseText(ASQL);
    ActiveData.SQL := ASQL;
//    if FScriptParser.StatementCount = 0 then
//    begin
//      ActiveData.SQL := ASQL;
//    end
//    else
//    begin
//      ActiveData.SQL := FScriptParser.Statements[0];
//    end;
    FStopWatch.Start;
    ActiveData.Execute;
    FStopWatch.Stop;
    //OptimizeWidth(pnlStatus);
    //Application.ProcessMessages;
//    if FDataInspector.Visible then
//    begin
//      FDataInspector.Data := ActiveData;
//    end;
//    if FFieldInspector.Visible then
//    begin
//      FFieldInspector.Data := ActiveData;
//    end;
  finally
    Screen.Cursor := crDefault;
//    pnlStatus.Caption := 'Ready';
//    pnlElapsedTime.Caption := Format('%d ms', [FStopWatch.ElapsedMilliseconds]);
  end;
end;

procedure TdmConnectionViewManager.UpdateActions;
var
  B: Boolean;
begin
  if Assigned(FSettings) then
  begin
    actSyncEditorWithRepository.Visible := actToggleRepositoryTree.Checked;
    actSyncEditorWithRepository.Enabled := actToggleRepositoryTree.Checked;

    actToggleStayOnTop.Checked := FSettings.FormSettings.FormStyle = fsStayOnTop;
    actToggleRepositoryTree.Checked := FSettings.RepositoryVisible;
    actDataInspector.Checked        := FSettings.DataInspectorVisible;
  end;

  if Assigned(ActiveData) and Assigned(ActiveDataView) then
  begin
    B := ActiveData.Active;
    actMergeCells.Visible          := B and Supports(ActiveDataView, IMergable);
    actMergeCells.Enabled          := B and Supports(ActiveDataView, IMergable);
    actGroupBySelection.Visible    := B and Supports(ActiveDataView, IGroupable);
    actGroupBySelection.Enabled    := B and Supports(ActiveDataView, IGroupable);
    actMergeAllColumnCells.Visible := actMergeCells.Visible;
    actMergeAllColumnCells.Enabled := actMergeCells.Enabled;
//    actFavoriteFieldsOnly.Visible  := B;
//    actHideEmptyColumns.Visible    := B;
//    actHideConstantColumns.Visible := B;
//    actHideSelectedColumns.Visible := B;
//    actShowAllColumns.Visible      := B;
//    actPreview.Visible             := B;
//    actPrint.Visible               := B;
//    actDesigner.Visible            := B;
    actFavoriteFieldsOnly.Enabled  := B;
    actHideEmptyColumns.Enabled    := B;
    actHideConstantColumns.Enabled := B;
    actHideSelectedColumns.Enabled := B;
    actShowAllColumns.Enabled      := B;
    actPreview.Enabled             := B;
    actPrint.Enabled               := B;
    actDesigner.Enabled            := B;
    actHideEmptyColumns.Checked    := not ActiveDataView.EmptyColumnsVisible;
    actHideConstantColumns.Checked := not ActiveDataView.ConstantColumnsVisible;
    actFavoriteFieldsOnly.Checked  :=
      (ActiveData as IFieldVisiblity).ShowFavoriteFieldsOnly;
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
  DV : IDGDataView;
  D  : IData;
  C  : IConnection;
begin
  EV := GlobalContainer.Resolve<IEditorView>;
  DV := GlobalContainer.Resolve<IDGDataView>(Settings.GridType);
  DV.Settings := FSettings as IDataViewSettings;
  DV.PopupMenu := ConnectionViewPopupMenu;
  FActiveDataView := DV;
  C := GlobalContainer.Resolve<IConnection>(Settings.ConnectionType);
  D           := TdmData.Create(Self, C);
  DV.Data     := D;
  FActiveData := D;
  FActiveDataView := DV;
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

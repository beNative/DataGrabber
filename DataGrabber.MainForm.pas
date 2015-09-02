{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.Diagnostics,
  Vcl.Menus, Vcl.ActnList, Vcl.Controls, Vcl.Forms, Vcl.ToolWin, Vcl.ExtCtrls,
  Vcl.Graphics, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls,
  Data.DB, Data.Win.ADODB,

  MidasLib,

  cxGraphics, cxControls, cxButtons, cxLookAndFeels, cxLookAndFeelPainters,
  dxDockPanel, cxPC, cxClasses, dxDockControl,

  SynEdit,

  DDuce.Components.PropertyInspector,

  VirtualTrees,

  ts.Interfaces, ts.Classes.ConnectionSettings, ts.Data, DataGrabber.Data,

  ts.Modules.DataInspector, ts.Modules.FieldInspector,

  DataGrabber.EditorView, DataGrabber.Interfaces, DataGrabber.Settings;

{
  TODO:
    - registration mechanism for dataviews (maybe we can use a modulemanager).
    - autosize form (to data)

    - testing !!!
    - query tree
    - multiple statements => multiple resultsets
    - log executed statements to database
    -  Enkel ‘populaire’ velden tonen (instelbaar via stringlist) :=> eventueel
       meerdere sets (per profile?)
    -  Auto hide empty/constant rows
    -  Versie info (delphi compiler + build date + platform)
    -  Bindings in settings
    -  Select <selected fields?> Where in
    -  Presenter voor vertical grid
    -  Meervoudige selectie in vertical grid? => selection delimited quoted text fieldnames
    -  Docking
    -  Datainspector -> groepering per tabel?
    -  Presenters als algemene basis
    -  Store executed sql’s
    -  Multiple sessions
    -  Smart grouping (rekening houden met gemeenschappelijke prefixes van velden (vb. Datum…, Coupe…., Last…)
    -  Working set van tables (meta info en links cachen voor deze tabellen profile->VisibleItems)
    -  Statement builder
    - introduce DDuce
}

type
  TModuleClass = class of TdmCustomModule;

type
  TfrmMain = class(TForm)
    {$region 'designer controls'}
    aclMain                       : TActionList;
    actAddConnectionView          : TAction;
    dcmMain                       : TdxDockingManager;
    dsConnectionViews             : TdxDockSite;
    dscRepository                 : TDataSource;
    mniADO                        : TMenuItem;
    mniCopy                       : TMenuItem;
    mniCopyTextTable              : TMenuItem;
    mniCopyWikiTable              : TMenuItem;
    mnicxGrid                     : TMenuItem;
    mniDBX                        : TMenuItem;
    mniFormatSQL                  : TMenuItem;
    mniGridView                   : TMenuItem;
    mniGroupBySelection           : TMenuItem;
    mniHideConstantColumns        : TMenuItem;
    mniHideEmptyColumns           : TMenuItem;
    mniHideSelectedColumns        : TMenuItem;
    mniInspectConnection          : TMenuItem;
    mniInspectDataSet             : TMenuItem;
    mniInspectFields              : TMenuItem;
    mniInspectGrid                : TMenuItem;
    mniKGrid                      : TMenuItem;
    mniMergeColumns               : TMenuItem;
    mniN1                         : TMenuItem;
    mniN2                         : TMenuItem;
    mniN3                         : TMenuItem;
    mniN4                         : TMenuItem;
    mniSelectionAsCommaText       : TMenuItem;
    mniSelectionAsFields          : TMenuItem;
    mniSelectionAsQuotedCommaText : TMenuItem;
    mniSelectionAsQuotedFields    : TMenuItem;
    mniSelectionAsTextTable       : TMenuItem;
    mniSettings                   : TMenuItem;
    mniShowAllColumns             : TMenuItem;
    mniUNI                        : TMenuItem;
    mniVirtualDBGrid              : TMenuItem;
    mniZEOS                       : TMenuItem;
    N1                            : TMenuItem;
    pnlConnectionStatus           : TPanel;
    pnlConnectionType             : TPanel;
    pnlConnectionViews            : TPanel;
    pnlConstantFieldsCount        : TPanel;
    pnlEditMode                   : TPanel;
    pnlElapsedTime                : TPanel;
    pnlEmptyFieldsCount           : TPanel;
    pnlFieldCount                 : TPanel;
    pnlGridType                   : TPanel;
    pnlProviderMode               : TPanel;
    pnlRecordCount                : TPanel;
    pnlStatus                     : TPanel;
    pnlStatusBar                  : TPanel;
    ppm                           : TPopupMenu;
    ppmConnectionTypes            : TPopupMenu;
    ppmGridTypes                  : TPopupMenu;
    tlbMain                       : TToolBar;
    {$endregion}

    {$REGION 'action handlers'}
    procedure actCopyExecute(Sender: TObject);
    procedure actToggleFullScreenExecute(Sender: TObject);
    procedure actToggleRepositoryTreeExecute(Sender: TObject);
    procedure actSyncEditorWithRepositoryExecute(Sender: TObject);
    procedure actRttiExecute(Sender: TObject);
    procedure actCreateModelExecute(Sender: TObject);
    procedure actAddConnectionViewExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure dscRepositoryDataChange(Sender: TObject; Field: TField);
    procedure tlbMainCustomDraw(Sender: TToolBar; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure dcmMainCreateFloatSite(Sender: TdxCustomDockControl;
      AFloatSite: TdxFloatDockSite);
    procedure dcmMainCreateTabContainer(Sender: TdxCustomDockControl;
      ATabContainer: TdxTabContainerDockSite);
    procedure dcmMainCreateLayoutSite(Sender: TdxCustomDockControl;
      ALayoutSite: TdxLayoutDockSite);
    procedure dcmMainCreateSideContainer(Sender: TdxCustomDockControl;
      ASideContainer: TdxSideContainerDockSite);
    {$ENDREGION}

  private
    //FTree           : TfrmVirtualDBTree;
    //FScriptParser   : TZSQLScriptParser;
    //FTokenizer      : IZTokenizer;
    //FRepositoryData : TdmRepositoryData;
    //FSyncEditor     : Boolean;
    FManager  : IConnectionViewManager;
    FSettings : IDGSettings;

    FTables        : TStringList;
    FFields        : TStringList;
    FLastPanel     : TdxDockPanel;

    procedure WMMove(var AMessage: TWMMove); message WM_MOVE;
    procedure WMSize(var AMessage: TWMSize); message WM_SIZING;

    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);

  protected
    function GetData: IData;
    function GetSettings: IDGSettings;
    function GetManager: IConnectionViewManager;

    procedure UpdateStatusBar;
    procedure OptimizeWidth(APanel: TPanel);
    procedure InitializeActions;

    procedure UpdateActions; override;

    procedure ShowToolWindow(AForm: TForm);
    procedure HideToolWindow(AForm: TForm);
    procedure RepositionToolWindows;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddConnectionView: IConnectionView;

    property Manager: IConnectionViewManager
      read GetManager;

    property Data: IData
      read GetData;

    property Settings: IDGSettings
      read GetSettings;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Vcl.Clipbrd,

  Spring.Services,

  ts.Utils,
  ts.Modules.ComponentInspector,  ts.Modules.RTTEye,

  DataGrabber.Utils, DataGrabber.Resources, DataGrabber.SettingsDialog,
  DataGrabber.ModelData,

  DataGrabber.ConnectionProfiles, DataGrabber.Factories,
  DataGrabber.RegisterServices;

resourcestring
  SFetchingData = 'Fetching data...';
  SUpdatingView = 'Updating view...';

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FManager := ServiceLocator.GetService<IConnectionViewManager>;
  AddConnectionView;
  tlbMain.DrawingStyle := dsNormal;
  InitializeActions;

  // TODO: not used for the moment
//  FTokenizer    := TZTokenizer.Create;
//  FScriptParser := TZSQLScriptParser.CreateWithTokenizer(FTokenizer);
//  FScriptParser.DelimiterType := dtEmptyLine;
//  FScriptParser.Delimiter := '';
//  FScriptParser.CleanupStatements := False;

// TODO : trace memory leaks in repository view
//  FRepositoryData := TdmRepositoryData.Create(Self);
//  FTree := TfrmVirtualDBTree.Create(Self);
//  FTree.DataSet := FRepositoryData.DataSet;
//  FTree.DataSet.Active := True;
//  dscRepository.DataSet := FTree.DataSet;

//  FSettings.FormSettings.AssignTo(Self);
//  pnlConnectionProfiles.Width := FSettings.FormSettings.VSplitterPos;
//  pnlTop.Height               := FSettings.FormSettings.HSplitterPos;
  pnlStatus.Caption := 'Ready';

  FTables := TStringList.Create;
  FFields := TStringList.Create;

  if FileExists('tablenames.txt') then
    FTables.LoadFromFile('tablenames.txt');
//
//  Editor.FillCompletionLists(FTables, FFields);

  SetWindowSizeGrip(pnlStatusBar.Handle, True);
  // TSI
  //actToggleRepositoryTree.Visible := False;
end;

procedure TfrmMain.BeforeDestruction;
begin
//  FSettings.FormSettings.Assign(Self);
//  FSettings.FormSettings.VSplitterPos := pnlConnectionProfiles.Width;
//  FSettings.FormSettings.HSplitterPos := pnlTop.Height;
//  FSettings.Save;
//  if Assigned(FView) then
//  begin
//    (FView as TComponent).Free;
//  end;
//  FView := nil;
//  if Assigned(FData) then
//  begin
//    (FData as TComponent).Free;
//  end;
//  FData := nil;
  FreeAndNil(FTables);
  FreeAndNil(FFields);
  //FreeAndNil(FScriptParser);
  FManager := nil;
  inherited BeforeDestruction;

end;
{$ENDREGION}

{$REGION 'message handlers'}
procedure TfrmMain.WMMove(var AMessage: TWMMove);
begin
  AMessage.Result := 0;
  RepositionToolWindows;
end;

procedure TfrmMain.WMSize(var AMessage: TWMSize);
begin
  AMessage.Result := 0;
  RepositionToolWindows;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
//  if not FEditor.EditorFocused then
//    View.Copy
//  else
//    Editor.CopyToClipboard;
end;

procedure TfrmMain.actCreateModelExecute(Sender: TObject);
//var
//  Tables: TStringList;
//  Fields: TStringList;
//  S      : string;
begin
//  Tables := TStringList.Create;
//  try
//    Fields := TStringList.Create;
//    try
//      if Supports(Data.Connection, IMetaData) then
//      begin
//        (Data.Connection as IMetaData).GetTableNames(Tables);
//        ModelData.AddTables(Tables);
//        for S in Tables do
//        begin
//          (Data.Connection as IMetaData).GetFieldNames(Fields, S);
//          ModelData.AddFields(S, Fields);
//        end;
//      end;
//    finally
//      Fields.Free;
//    end;
//  finally
//    Tables.Free;
//  end;

end;

procedure TfrmMain.actSyncEditorWithRepositoryExecute(Sender: TObject);
begin
//  FSyncEditor := actSyncEditorWithRepository.Checked;
//  if FSyncEditor then
//    FEditor.Text := dscRepository.DataSet.FieldByName('Text').AsString;
end;

procedure TfrmMain.actAddConnectionViewExecute(Sender: TObject);
begin
  AddConnectionView;
end;

procedure TfrmMain.actToggleFullScreenExecute(Sender: TObject);
begin
//  if actToggleFullScreen.Checked then
//    WindowState := wsMaximized
//  else
//    WindowState := wsNormal;
end;

procedure TfrmMain.actToggleRepositoryTreeExecute(Sender: TObject);
begin
//  FSettings.RepositoryVisible := actToggleRepositoryTree.Checked;
//  if actToggleRepositoryTree.Checked then
//    ShowToolWindow(FTree)
//  else
//    HideToolWindow(FTree);
end;

procedure TfrmMain.actRttiExecute(Sender: TObject);
begin
  ShowRTTEye;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.tlbMainCustomDraw(Sender: TToolBar; const ARect: TRect;
  var DefaultDraw: Boolean);
begin
  Sender.Canvas.FillRect(ARect);
end;

procedure TfrmMain.dcmMainCreateFloatSite(Sender: TdxCustomDockControl;
  AFloatSite: TdxFloatDockSite);
begin
  AFloatSite.FloatWidth := (Width div 2);
  AFloatSite.FloatHeight := (Height div 3) * 2;
end;

procedure TfrmMain.dcmMainCreateLayoutSite(Sender: TdxCustomDockControl;
  ALayoutSite: TdxLayoutDockSite);
begin
  ALayoutSite.CaptionButtons := [cbClose];
end;

procedure TfrmMain.dcmMainCreateSideContainer(Sender: TdxCustomDockControl;
  ASideContainer: TdxSideContainerDockSite);
begin
  ASideContainer.CaptionButtons := [];
  ASideContainer.ShowCaption := False;
end;

procedure TfrmMain.dcmMainCreateTabContainer(Sender: TdxCustomDockControl;
  ATabContainer: TdxTabContainerDockSite);
begin
  ATabContainer.CaptionButtons := [cbClose];
end;

procedure TfrmMain.dscRepositoryDataChange(Sender: TObject; Field: TField);
begin
//  if (Field = nil) and Assigned(FEditor) and FTree.Visible and FSyncEditor then
//  begin
//    FEditor.Text := dscRepository.DataSet.FieldByName('Text').AsString;
//  end;
end;

procedure TfrmMain.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
//  if FTree.Visible and FSyncEditor and (FEditor.Text <> '') then
//  begin
//    if not (dscRepository.DataSet.State in dsEditModes) then
//      dscRepository.DataSet.Edit;
//    dscRepository.DataSet.FieldByName('Text').AsString := FEditor.Text;
//  end;
end;

{ Needed to route shortcuts to the actionlist in the data module. }

procedure TfrmMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := Manager.ActionList.IsShortCut(Msg);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMain.GetData: IData;
begin
  Result := Manager.ActiveData;
end;

function TfrmMain.GetSettings: IDGSettings;
begin
  Result := FSettings;
end;

function TfrmMain.GetManager: IConnectionViewManager;
begin
  Result := FManager;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmMain.OptimizeWidth(APanel: TPanel);
var
  S: string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
  begin
    APanel.Width := ts.Utils.GetTextWidth(APanel.Caption, APanel.Font) + 10;
    APanel.AlignWithMargins := True;
  end
  else
  begin
    APanel.Width := 0;
    APanel.AlignWithMargins := False;
  end;
end;

function TfrmMain.AddConnectionView: IConnectionView;
var
  DP  : TdxDockPanel;
  CV  : IConnectionView;
  I   : Integer;
  N   : Integer;
begin
  LockPaint(Self);
  dxDockingController.BeginUpdate;
  DP := TdxDockPanel.Create(Self);
  DP.ShowSingleTab := False;
  DP.CaptionButtons := [cbClose];
  for I := 0 to dxDockingController.DockControlCount - 1 do
  begin
    if dxDockingController.DockControls[I] is TdxDockSite then
    begin
      if Assigned(FLastPanel) then
      begin
        DP.DockTo(FLastPanel, dtClient, -1);
      end
      else
      begin
        DP.DockTo(
          TdxCustomDockControl(dxDockingController.DockControls[I]),
          dtClient,
          -1
        );
      end;
      Break;
    end;
  end;
  CV := Manager.AddConnectionView;
  CV.Form.Parent := DP;
  DP.ShowCaption := True;
  CV.Form.BorderStyle := bsNone;
  CV.Form.Align := alClient;
  CV.Form.Visible := True;
  FLastPanel := DP;
  UnlockPaint(Self);
  dxDockingController.EndUpdate;
end;

procedure TfrmMain.InitializeActions;
begin
  AddToolbarButtons(tlbMain, Manager);
//  btnSyncEditorWithRepository.Action :=
//    ConnectionViewManager['actSyncEditorWithRepository'];
//  btnToggleRepositoryTree.Action :=
//    ConnectionViewManager['actToggleRepositoryTree'];
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.ShowToolWindow(AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    LockPaint(AForm);
    try
      RepositionToolWindows;
      AForm.BorderStyle := bsSizeToolWin;
      AForm.Visible := True;
      ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
      SetWindowPos(AForm.Handle, GetNextWindow(Self.Handle, GW_HWNDNEXT),
        0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    finally
      UnLockPaint(AForm);
    end;
    AForm.Invalidate;
  end;
end;

procedure TfrmMain.HideToolWindow(AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    ShowWindow(AForm.Handle, SW_HIDE);
  end;
end;

procedure TfrmMain.RepositionToolWindows;
begin
//  if Assigned(FDataInspector) then
//  begin
//    with FDataInspector do
//    begin
//      Left   := Self.Left + Self.Width;
//      Top    := Self.Top;
//      Height := Self.Height;
//    end;
//  end;
//  if Assigned(FTree) then
//  begin
//    with FTree do
//    begin
//      Left   := Self.Left - Width;
//      Top    := Self.Top;
//      Height := Self.Height;
//    end;
//  end;
end;

procedure TfrmMain.UpdateActions;
var
  V: IConnectionView;
  D: TdxCustomDockControl;
begin
  inherited UpdateActions;
  UpdateStatusBar;
  if Assigned(Manager.ActiveConnectionView) then
  begin
    V := Manager.ActiveConnectionView;
    D := dxDockingController.GetDockControlForWindow(V.Form.Handle);

    if Assigned(D) then
    begin
      D.Caption := V.Form.Caption;
      if Assigned(D.TabContainer) then
      begin
        D.ManagerColor := True;
        D.TabContainer.TabsProperties.Options :=
            D.TabContainer.TabsProperties.Options + [pcoUsePageColorForTab];
      end;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMain.UpdateStatusBar;
var
  S: string;
begin
  if Assigned(Data) and  Assigned(Data.DataSet) and Data.DataSet.Active then
  begin
    pnlRecordCount.Caption := Format('%d records', [Data.RecordCount]);
    pnlFieldCount.Caption  := Format('%d fields', [Data.DataSet.FieldCount]);
    pnlConstantFieldsCount.Caption :=
      Format('%d constant fields', [(Data as IFieldLists).ConstantFields.Count]);
    pnlEmptyFieldsCount.Caption :=
      Format('%d empty fields', [(Data as IFieldLists).EmptyFields.Count]);
    if Data.CanModify then
      S := 'Updateable'
    else
      S := 'ReadOnly';
    if Data.ProviderMode then
    begin
      pnlProviderMode.Caption := 'Provider mode';
    end
    else
    begin
      pnlProviderMode.Caption :=  'Native mode';
    end;
    //actProviderMode.Checked := Data.ProviderMode;
  end
  else
  begin
    S                              := '';
    pnlRecordCount.Caption         := '';
    pnlFieldCount.Caption          := '';
    pnlElapsedTime.Caption         := '';
    pnlConstantFieldsCount.Caption := '';
    pnlEmptyFieldsCount.Caption    := '';
    if Assigned(Settings) and Settings.ProviderMode then
    begin
      pnlProviderMode.Caption := 'Provider mode';
    end
    else
    begin
      pnlProviderMode.Caption :=  'Native mode';
    end;
    //actProviderMode.Checked := Settings.ProviderMode;
  end;

  if Assigned(Settings) then
  begin
    pnlConnectionType.Caption := Settings.ConnectionType;
    pnlGridType.Caption := Settings.GridType;
  end;
  if Assigned(Data) and Assigned(Data.Connection) and Data.Connection.Connected then
    pnlConnectionStatus.Caption := 'Connected'
  else
    pnlConnectionStatus.Caption := 'Disconnected';

  pnlEditMode.Caption := S;
  OptimizeWidth(pnlStatus);
  OptimizeWidth(pnlConnectionStatus);
  OptimizeWidth(pnlRecordCount);
  OptimizeWidth(pnlFieldCount);
  OptimizeWidth(pnlEmptyFieldsCount);
  OptimizeWidth(pnlConstantFieldsCount);
  OptimizeWidth(pnlElapsedTime);
  OptimizeWidth(pnlConnectionType);
  OptimizeWidth(pnlProviderMode);
  OptimizeWidth(pnlEditMode);
  OptimizeWidth(pnlGridType);
end;
{$ENDREGION}

end.

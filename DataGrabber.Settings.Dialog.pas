{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.Settings.Dialog;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.ImageList, System.Actions, System.SysUtils,
  System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.ToolWin,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,

  VirtualTrees,

  zObjInspector, zObjInspTypes,

  kcontrols, kbuttons, kedits,

  SynEditHighlighter, SynHighlighterJScript, SynEdit,
  SynHighlighterJSON, SynEditCodeFolding, SynHighlighterIni,

  DataGrabber.Interfaces,

  DataGrabber.Settings, DataGrabber.ConnectionProfiles,
  DataGrabber.ConnectionProfileValueManager;

type
  TApplySettingsMethod = reference to procedure;

type
  TfrmSettingsDialog = class(TForm)
    {$REGION 'designer controls'}
    aclMain                      : TActionList;
    actAdd                       : TAction;
    actApply                     : TAction;
    actCancel                    : TAction;
    actClose                     : TAction;
    actDelete                    : TAction;
    actDuplicate                 : TAction;
    actEditConnectionDef         : TAction;
    actGridlinesBoth             : TAction;
    actGridlinesHorizontal       : TAction;
    actGridlinesNone             : TAction;
    actGridlinesVertical         : TAction;
    actMoveDown                  : TAction;
    actMoveUp                    : TAction;
    actMRSAsMultipleTabs         : TAction;
    actMRSHorizontally           : TAction;
    actMRSVertically             : TAction;
    actOpenSettingsFileLocation  : TAction;
    actRefreshFile               : TAction;
    actSaveFile                  : TAction;
    actTestConnection            : TAction;
    btnAdd                       : TToolButton;
    btnApply                     : TButton;
    btnBooleanColor              : TKColorButton;
    btnCancel                    : TButton;
    btnClose                     : TButton;
    btnDateColor                 : TKColorButton;
    btnDateTimeColor             : TKColorButton;
    btnDelete                    : TToolButton;
    btnDuplicate                 : TToolButton;
    btnFloatColor                : TKColorButton;
    btnGridlinesAll              : TToolButton;
    btnGridlinesHorizontal       : TToolButton;
    btnGridlinesNone             : TToolButton;
    btnGridlinesVertical         : TToolButton;
    btnIntegerColor              : TKColorButton;
    btnMemoColor                 : TKColorButton;
    btnMoveDown                  : TToolButton;
    btnMoveUp                    : TToolButton;
    btnMRSAsMultipleTabs         : TToolButton;
    btnMRSHorizontally           : TToolButton;
    btnMRSVertically             : TToolButton;
    btnNullColor                 : TKColorButton;
    btnProfileColor              : TKColorButton;
    btnRefreshFile               : TToolButton;
    btnSaveFile                  : TToolButton;
    btnSpacer1                   : TToolButton;
    btnSpacer2                   : TToolButton;
    btnStringColor               : TKColorButton;
    btnTestConnection            : TButton;
    btnTimeColor                 : TKColorButton;
    cbxConnectionDefs            : TComboBox;
    cbxDrivers                   : TComboBox;
    chkAutoReconnect             : TCheckBox;
    chkDisconnectedMode          : TCheckBox;
    chkFetchOnDemand             : TCheckBox;
    chkGridCellColoringEnabled   : TCheckBox;
    chkMultipleResultSets        : TCheckBox;
    chkOSAuthent                 : TCheckBox;
    chkReadOnlyResultSets        : TCheckBox;
    chkSetAsDefault              : TCheckBox;
    conTest                      : TFDConnection;
    dlgFont                      : TFontDialog;
    dlgOpenFile                  : TOpenDialog;
    edtCatalog                   : TButtonedEdit;
    edtDatabase                  : TButtonedEdit;
    edtEditorFont                : TButtonedEdit;
    edtGridFont                  : TButtonedEdit;
    edtPacketRecords             : TEdit;
    edtPassword                  : TEdit;
    edtProfileName               : TLabeledEdit;
    edtUserName                  : TEdit;
    grpCellBackgroundColoring    : TGroupBox;
    grpClientSettings            : TGroupBox;
    grpConnectionSettings        : TGroupBox;
    grpDBMSUserLogin             : TGroupBox;
    grpEditorSettings            : TGroupBox;
    grpGridFont                  : TGroupBox;
    grpGridLines                 : TGroupBox;
    grpProfileSettings           : TGroupBox;
    grpResultSetDisplay          : TGroupBox;
    imlMain                      : TImageList;
    lblBoolean                   : TLabel;
    lblCatalog                   : TLabel;
    lblConnectionDefinitionName  : TLabel;
    lblDatabase                  : TLabel;
    lblDates                     : TLabel;
    lblDateTimes                 : TLabel;
    lblDriverID                  : TLabel;
    lblEditorFont                : TLabel;
    lblFloats                    : TLabel;
    lblGridFont                  : TLabel;
    lblIntegers                  : TLabel;
    lblMemo                      : TLabel;
    lblNULL                      : TLabel;
    lblPacketrecords             : TLabel;
    lblPassword                  : TLabel;
    lblProfileColor              : TLabel;
    lblString                    : TLabel;
    lblTimes                     : TLabel;
    lblUserName                  : TLabel;
    pgcConnectionProfile         : TPageControl;
    pgcMain                      : TPageControl;
    pgcSettingsFiles             : TPageControl;
    pnlConnectionProfileDetail   : TPanel;
    pnlConnectionProfilesList    : TPanel;
    pnlGridTypeColoring          : TGridPanel;
    pnlLogin                     : TGridPanel;
    rgpGridTypes                 : TRadioGroup;
    seFDConnectionDefs           : TSynEdit;
    seSettings                   : TSynEdit;
    splVertical                  : TSplitter;
    synIni                       : TSynIniSyn;
    synJScript                   : TSynJScriptSyn;
    tlbConnectionProfiles        : TToolBar;
    tlbDisplayMultipleResultSets : TToolBar;
    tlbGridlines                 : TToolBar;
    tlbSettingsFiles             : TToolBar;
    tsAdvanced                   : TTabSheet;
    tsBasic                      : TTabSheet;
    tsConnectionProfiles         : TTabSheet;
    tsDataGrabberSettings        : TTabSheet;
    tsDisplay                    : TTabSheet;
    tsFDConnectionDefs           : TTabSheet;
    tsSettings                   : TTabSheet;
    btnEditConnectionDef: TToolButton;
    tsConnectionDefinitions: TTabSheet;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actEditConnectionDefExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDuplicateExecute(Sender: TObject);
    procedure actGridlinesBothExecute(Sender: TObject);
    procedure actGridlinesHorizontalExecute(Sender: TObject);
    procedure actGridlinesNoneExecute(Sender: TObject);
    procedure actGridlinesVerticalExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actOpenSettingsFileLocationExecute(Sender: TObject);
    procedure actTestConnectionExecute(Sender: TObject);
    procedure actMRSAsMultipleTabsExecute(Sender: TObject);
    procedure actMRSHorizontallyExecute(Sender: TObject);
    procedure actMRSVerticallyExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure FVSTProfilesGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FVSTProfilesFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure FVSTProfilesBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
    procedure FVSTProfilesPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );
    procedure btnProfileColorClick(Sender: TObject);
    procedure cbxDriversChange(Sender: TObject);
    procedure chkAutoReconnectClick(Sender: TObject);
    procedure chkDisconnectedModeClick(Sender: TObject);
    procedure chkFetchOnDemandClick(Sender: TObject);
    procedure chkGridCellColoringEnabledClick(Sender: TObject);
    procedure chkMultipleResultSetsClick(Sender: TObject);
    procedure chkProviderModeClick(Sender: TObject);
    procedure chkSetAsDefaultClick(Sender: TObject);
    procedure edtCatalogChange(Sender: TObject);
    procedure edtDatabaseChange(Sender: TObject);
    procedure edtDatabaseRightButtonClick(Sender: TObject);
    procedure edtPacketRecordsChange(Sender: TObject);
    procedure edtPasswordChange(Sender: TObject);
    procedure edtProfileNameChange(Sender: TObject);
    procedure edtUserNameChange(Sender: TObject);
    procedure tsSettingsEnter(Sender: TObject);
    procedure seFDConnectionDefsExit(Sender: TObject);
    procedure cbxConnectionDefsDropDown(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure seSettingsChange(Sender: TObject);
    procedure seFDConnectionDefsChange(Sender: TObject);
    procedure actRefreshFileExecute(Sender: TObject);
    procedure cbxConnectionDefsChange(Sender: TObject);
    procedure edtGridFontRightButtonClick(Sender: TObject);
    procedure conTestError(ASender, AInitiator: TObject;
      var AException: Exception);
    procedure lblConnectionDefinitionNameDblClick(Sender: TObject);
    procedure tsAdvancedExit(Sender: TObject);
    procedure edtEditorFontRightButtonClick(Sender: TObject);
    {$ENDREGION}

  private
    FSettings            : ISettings;
    FApplySettingsMethod : TApplySettingsMethod;
    FObjectInspector     : TzObjectInspector;
    FVSTProfiles         : TVirtualStringTree;
    FModified            : Boolean;
    FValueManager        : TConnectionProfileValueManager;

    function GetSelectedProfile: TConnectionProfile;

  protected
    procedure Apply;
    procedure Save;
    procedure InitializeControls;
    procedure UpdateActions; override;
    procedure Changed;

    function AskSaveFileChanges: Boolean;
    function ConfirmDeleteProfile: Boolean;
    procedure SaveConnectionDefinitionsFile;
    procedure LoadConnectionDefinitionsFile;
    procedure LoadApplicationSettingsFile;
    procedure SaveApplicationSettingsFile;

    procedure InspectConnectionProfile(ACP: TConnectionProfile);
    procedure UpdateConnectionProfileControls(ACP: TConnectionProfile);
    procedure SaveConnectionProfileChanges(ACP: TConnectionProfile);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : ISettings
    ); reintroduce; virtual;

    property ApplySettingsMethod: TApplySettingsMethod
      read FApplySettingsMethod write FApplySettingsMethod;

    property SelectedProfile: TConnectionProfile
      read GetSelectedProfile;
  end;

procedure ExecuteSettingsDialog(
  ASettings            : ISettings;
  AApplySettingsMethod : TApplySettingsMethod = nil
);

implementation

{$R *.dfm}

uses
  System.Rtti, System.UITypes,

  FireDAC.VCLUI.ConnEdit, FireDAC.Stan.Consts,

  Spring,

  DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector, DDuce.Logger,

  DataGrabber.Utils, DataGrabber.ConnectionSettings, DataGrabber.Resources;

{$REGION 'non-interfaced routines'}
function ExecuteFDConnectionDialog(AConnDef: IFDStanConnectionDef;
  const ACaption: string): Boolean;
var
  LConn : TFDCustomConnection;
  LName : string;
begin
  LConn := TFDCustomConnection.Create(nil);
  try
    LConn.Temporary := True;
    LConn.Params.SetStrings(AConnDef.Params);
    LConn.ConnectionDefName := AConnDef.Name;
    AConnDef.ReadOptions(LConn.FormatOptions, LConn.UpdateOptions,
      LConn.FetchOptions, LConn.ResourceOptions);
    LName := AConnDef.Name;
    Result := TfrmFDGUIxFormsConnEdit.Execute(LConn, ACaption, nil);
    if Result then
    begin
      AConnDef.Params.SetStrings(LConn.Params);
      AConnDef.WriteOptions(LConn.FormatOptions, LConn.UpdateOptions,
        LConn.FetchOptions, LConn.ResourceOptions);
      AConnDef.Name := LName;
    end;
  finally
    LConn.Free;
  end;
end;
{$ENDREGION}

{$REGION 'interfaced routines'}
procedure ExecuteSettingsDialog(ASettings: ISettings;
  AApplySettingsMethod: TApplySettingsMethod);
var
  Form : TfrmSettingsDialog;
begin
  Form := TfrmSettingsDialog.Create(nil, ASettings);
  try
    Form.ApplySettingsMethod := AApplySettingsMethod;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
constructor TfrmSettingsDialog.Create(AOwner: TComponent; ASettings: ISettings);
begin
  Guard.CheckNotNull(ASettings, 'ASettings');
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmSettingsDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueManager := TConnectionProfileValueManager.Create;
  FObjectInspector :=
    TzObjectInspectorFactory.Create(Self, tsAdvanced, nil, FValueManager);
  FObjectInspector.SplitterPos     := FObjectInspector.Width div 4;
  FObjectInspector.SortByCategory  := False;
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FDManager.RefreshConnectionDefFile;
  LoadApplicationSettingsFile;
  LoadConnectionDefinitionsFile;
  InitializeControls;
end;

procedure TfrmSettingsDialog.BeforeDestruction;
begin
  FValueManager.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSettingsDialog.actAddExecute(Sender: TObject);
var
  CP: TConnectionProfile;
begin
  CP := FSettings.ConnectionProfiles.Add;
  CP.ConnectionSettings.Assign(FSettings.ConnectionSettings);
  FVSTProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  SelectNode(FVSTProfiles, FVSTProfiles.RootNodeCount - 1);
end;

procedure TfrmSettingsDialog.actApplyExecute(Sender: TObject);
begin
  Apply;
end;

procedure TfrmSettingsDialog.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettingsDialog.actCloseExecute(Sender: TObject);
begin
  Apply;
  Close;
end;

procedure TfrmSettingsDialog.actEditConnectionDefExecute(Sender: TObject);
var
  CD : IFDStanConnectionDef;
  //OC : IFDStanOptions;
  CS : TConnectionSettings;
  S  : string;
begin
  CS := SelectedProfile.ConnectionSettings;
  Logger.SendObject('CS', CS);
  S  := Trim(cbxConnectionDefs.Text);
  CD := FDManager.ConnectionDefs.FindConnectionDef(S);
  if not Assigned(CD) then
  begin
    CD := FDManager.ConnectionDefs.AddConnectionDef;
    CD.Name            := S;
    CD.Params.DriverID := CS.DriverName;
    CD.Params.Database := CS.Database;
    CD.Params.UserName := CS.UserName;
    CD.Params.Password := CS.Password;
    Logger.SendObject('CD', TObject(CD));
    CD.MarkPersistent; // required to add it to the connection definition file.
  end;

  TfrmFDGUIxFormsConnEdit.Execute(S, '');




  Logger.Info(CD.Params.Text);

//  OC := TFDOptionsContainer.Create(
//    FDManager, // inherit standard options from global manager instance
//    TFDFetchOptions,
//    TFDUpdateOptions,
//    TFDResourceOptions,
//    nil
//  );
//  OC.FetchOptions.RecsMax := CS.MaxRecords;
//  if CS .FetchOnDemand then
//    OC.FetchOptions.Mode := fmOnDemand
//  else
//    OC.FetchOptions.Mode := fmAll;
//  OC.FetchOptions.RowsetSize := CS .PacketRecords;
//  CD.WriteOptions(
//    OC.FormatOptions, OC.UpdateOptions, OC.FetchOptions, OC.ResourceOptions
//  );
//  CD.ReadOptions(
//    OC.FormatOptions, OC.UpdateOptions, OC.FetchOptions, OC.ResourceOptions
//  );
//  CS.MaxRecords    := OC.FetchOptions.RecsMax;
//  CS.PacketRecords := OC.FetchOptions.RowsetSize;
//  CS.FetchOnDemand := OC.FetchOptions.Mode = fmOnDemand;

  CS.Database   := CD.Params.Database;
  CS.DriverName := CD.Params.DriverID;
  CS.UserName   := CD.Params.UserName;
  CS.Password   := CD.Params.Password;
end;

procedure TfrmSettingsDialog.actDeleteExecute(Sender: TObject);
begin
  if ConfirmDeleteProfile then
  begin
    FVSTProfiles.BeginUpdate;
    try
      FObjectInspector.Component := nil;
      FSettings.ConnectionProfiles.Delete(FVSTProfiles.FocusedNode.Index);
      SelectNode(FVSTProfiles, FSettings.ConnectionProfiles.Count - 1);
    finally
      FVSTProfiles.EndUpdate;
    end;
    FVSTProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
    FVSTProfiles.Refresh;
  end;
end;

procedure TfrmSettingsDialog.actDuplicateExecute(Sender: TObject);
var
  N: Integer;
begin
  N := FVSTProfiles.FocusedNode.Index;
  with FSettings.ConnectionProfiles.Add do
  begin
    Assign(FSettings.ConnectionProfiles[N]);
    N := Index;
  end;
  FVSTProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  SelectNode(FVSTProfiles, N);
end;

procedure TfrmSettingsDialog.actGridlinesBothExecute(Sender: TObject);
begin
  FSettings.ShowHorizontalGridLines := True;
  FSettings.ShowVerticalGridLines   := True;
end;

procedure TfrmSettingsDialog.actGridlinesHorizontalExecute(Sender: TObject);
begin
  FSettings.ShowHorizontalGridLines := True;
  FSettings.ShowVerticalGridLines   := False;
end;

procedure TfrmSettingsDialog.actGridlinesNoneExecute(Sender: TObject);
begin
  FSettings.ShowHorizontalGridLines := False;
  FSettings.ShowVerticalGridLines   := False;
end;

procedure TfrmSettingsDialog.actGridlinesVerticalExecute(Sender: TObject);
begin
  FSettings.ShowHorizontalGridLines := False;
  FSettings.ShowVerticalGridLines   := True;
end;

procedure TfrmSettingsDialog.actMoveDownExecute(Sender: TObject);
var
  N: Integer;
begin
  N := FVSTProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N + 1;
  FVSTProfiles.Refresh;
  SelectNode(FVSTProfiles, N + 1);
end;

procedure TfrmSettingsDialog.actMoveUpExecute(Sender: TObject);
var
  N: Integer;
begin
  N := FVSTProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N - 1;
  FVSTProfiles.Refresh;
  SelectNode(FVSTProfiles, N - 1);
end;

procedure TfrmSettingsDialog.actMRSAsMultipleTabsExecute(Sender: TObject);
begin
  FSettings.ResultDisplayLayout := TResultDisplayLayout.Tabbed;
end;

procedure TfrmSettingsDialog.actMRSHorizontallyExecute(Sender: TObject);
begin
  FSettings.ResultDisplayLayout := TResultDisplayLayout.Horizontal;
end;

procedure TfrmSettingsDialog.actMRSVerticallyExecute(Sender: TObject);
begin
  FSettings.ResultDisplayLayout := TResultDisplayLayout.Vertical;
end;

procedure TfrmSettingsDialog.actOpenSettingsFileLocationExecute(
  Sender: TObject);
begin
  ShowMessage('Not implemented yet.');
end;

procedure TfrmSettingsDialog.actRefreshFileExecute(Sender: TObject);
begin
  LoadConnectionDefinitionsFile;
  LoadApplicationSettingsFile;
end;

procedure TfrmSettingsDialog.actSaveFileExecute(Sender: TObject);
begin
  SaveConnectionDefinitionsFile;
end;

procedure TfrmSettingsDialog.actTestConnectionExecute(Sender: TObject);
begin
  conTest.ConnectionDefName := cbxConnectionDefs.Text;
  conTest.LoginPrompt := False;
  conTest.Connected := True;
  conTest.Connected := False;
  ShowMessage('Connection successful!');
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmSettingsDialog.GetSelectedProfile: TConnectionProfile;
begin
  if Assigned(FVSTProfiles.FocusedNode) then
    Result := FSettings.ConnectionProfiles[FVSTProfiles.FocusedNode.Index]
  else
  begin
    Result := nil;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FVSTProfiles'}
procedure TfrmSettingsDialog.FVSTProfilesBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if Node.Index < Cardinal(FSettings.ConnectionProfiles.Count) then
  begin
    TargetCanvas.Brush.Color :=
      FSettings.ConnectionProfiles[Node.Index].ProfileColor;
    TargetCanvas.FillRect(CellRect);
    if Sender.FocusedNode = Node then
    begin
      TargetCanvas.Pen.Color := clBlue;
      TargetCanvas.Rectangle(CellRect);
    end;
  end;
end;

procedure TfrmSettingsDialog.FVSTProfilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  CP : TConnectionProfile;
begin
  if Node.Index >= Cardinal(FSettings.ConnectionProfiles.Count) then
    Node.Index := FSettings.ConnectionProfiles.Count - 1;
  CP := FSettings.ConnectionProfiles[Node.Index];
  InspectConnectionProfile(CP);
  UpdateConnectionProfileControls(CP);
end;

procedure TfrmSettingsDialog.FVSTProfilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Node.Index < Cardinal(FSettings.ConnectionProfiles.Count) then
    CellText := FSettings.ConnectionProfiles[Node.Index].DisplayName;
end;

procedure TfrmSettingsDialog.FVSTProfilesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.FocusedNode = Node then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end;
end;
{$ENDREGION}

procedure TfrmSettingsDialog.btnProfileColorClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.cbxConnectionDefsChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.cbxConnectionDefsDropDown(Sender: TObject);
begin
  FDManager.GetConnectionDefNames(cbxConnectionDefs.Items);
end;

procedure TfrmSettingsDialog.cbxDriversChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.chkAutoReconnectClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.chkDisconnectedModeClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.chkFetchOnDemandClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.chkGridCellColoringEnabledClick(Sender: TObject);
var
  CI : TCollectionItem;
begin
  for CI in pnlGridTypeColoring.ControlCollection do
  begin
    TControlItem(CI).Control.Enabled := (Sender as TCheckBox).Checked;
  end;
end;

procedure TfrmSettingsDialog.chkMultipleResultSetsClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    chkReadOnlyResultSets.Checked := True;
  Changed;
end;

procedure TfrmSettingsDialog.chkProviderModeClick(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.chkSetAsDefaultClick(Sender: TObject);
begin
  if chkSetAsDefault.Checked and Assigned(SelectedProfile) then
  begin
    FSettings.DefaultConnectionProfile := SelectedProfile.Name;
    Changed;
  end;
end;

procedure TfrmSettingsDialog.conTestError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
  ShowMessageFmt('Connection failed! %s', [AException.Message]);
end;

function TfrmSettingsDialog.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := PItem.Prop.IsWritable;
end;

procedure TfrmSettingsDialog.edtCatalogChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.edtDatabaseChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.edtDatabaseRightButtonClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edtDatabase.Text;
  if dlgOpenFile.Execute then
  begin
    edtDatabase.Text := dlgOpenFile.FileName;
    Changed;
  end;
end;

procedure TfrmSettingsDialog.edtEditorFontRightButtonClick(Sender: TObject);
begin
  dlgFont.Font.Assign(FSettings.EditorFont);
  dlgFont.Execute;
  FSettings.EditorFont := dlgFont.Font;
  edtEditorFont.Font.Assign(FSettings.EditorFont);
  edtEditorFont.Text := FSettings.EditorFont.Name;
  Changed;
end;

procedure TfrmSettingsDialog.edtGridFontRightButtonClick(Sender: TObject);
begin
  dlgFont.Font.Assign(FSettings.GridFont);
  dlgFont.Execute;
  FSettings.GridFont := dlgFont.Font;
  edtGridFont.Font.Assign(FSettings.GridFont);
  edtGridFont.Text := FSettings.GridFont.Name;
  Changed;
end;

procedure TfrmSettingsDialog.edtPacketRecordsChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.edtPasswordChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.edtProfileNameChange(Sender: TObject);
begin
  if Assigned(SelectedProfile) and (SelectedProfile.Name <> edtProfileName.Text) then
  begin
    SelectedProfile.Name := edtProfileName.Text;
    Changed;
  end;
end;

procedure TfrmSettingsDialog.edtUserNameChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.tsAdvancedExit(Sender: TObject);
var
  CP : TConnectionProfile;
begin
  CP := FSettings.ConnectionProfiles[FVSTProfiles.FocusedNode.Index];
  UpdateConnectionProfileControls(CP);
end;

procedure TfrmSettingsDialog.tsSettingsEnter(Sender: TObject);
begin
  LoadApplicationSettingsFile;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSettingsDialog.Save;
begin
  FSettings.Save;
end;

procedure TfrmSettingsDialog.LoadApplicationSettingsFile;
begin
  seSettings.Lines.LoadFromFile(FSettings.FileName);
end;

procedure TfrmSettingsDialog.LoadConnectionDefinitionsFile;
begin
  seFDConnectionDefs.Lines.LoadFromFile(S_FD_DefCfgFileName);
  seFDConnectionDefs.ResetModificationIndicator;
  seFDConnectionDefs.Modified := False;
end;

procedure TfrmSettingsDialog.SaveApplicationSettingsFile;
begin
  seSettings.Lines.SaveToFile(FSettings.FileName);
end;

procedure TfrmSettingsDialog.SaveConnectionDefinitionsFile;
begin
  seFDConnectionDefs.Lines.SaveToFile(S_FD_DefCfgFileName);
  seFDConnectionDefs.MarkModifiedLinesAsSaved;
  seFDConnectionDefs.Modified := False;
end;

procedure TfrmSettingsDialog.SaveConnectionProfileChanges(
  ACP: TConnectionProfile);
begin
  ACP.Name         := edtProfileName.Text;
  ACP.ProfileColor := btnProfileColor.DlgColor;
  ACP.ConnectionSettings.DriverName    := cbxDrivers.Text;
  ACP.ConnectionSettings.Database      := edtDatabase.Text;
  ACP.ConnectionSettings.Catalog       := edtCatalog.Text;
  ACP.ConnectionSettings.FetchOnDemand := chkFetchOnDemand.Checked;
  ACP.ConnectionSettings.PacketRecords := StrToIntDef(edtPacketRecords.Text, 0);
  ACP.ConnectionSettings.OSAuthent     := chkOSAuthent.Checked;
  ACP.ConnectionSettings.UserName      := edtUserName.Text;
  ACP.ConnectionSettings.Password      := edtPassword.Text;
  ACP.ConnectionSettings.AutoReconnect := chkAutoReconnect.Checked;
  ACP.ConnectionSettings.ReadOnly      := chkReadOnlyResultSets.Checked;
  ACP.ConnectionSettings.MultipleResultSets := chkMultipleResultSets.Checked;
  ACP.ConnectionSettings.DisconnectedMode   := chkDisconnectedMode.Checked;
  ACP.ConnectionSettings.ConnectionDefName  := cbxConnectionDefs.Text;
  FModified := False;
end;

procedure TfrmSettingsDialog.seFDConnectionDefsChange(Sender: TObject);
begin
  if seFDConnectionDefs.Modified then
    actSaveFile.Enabled := True;
end;

procedure TfrmSettingsDialog.seFDConnectionDefsExit(Sender: TObject);
begin
  if seFDConnectionDefs.Modified then
  begin
    if AskSaveFileChanges then
      SaveConnectionDefinitionsFile
    else
      LoadConnectionDefinitionsFile;
  end;
end;

procedure TfrmSettingsDialog.seSettingsChange(Sender: TObject);
begin
  if seSettings.Modified then
    actSaveFile.Enabled := True;
end;

procedure TfrmSettingsDialog.Changed;
begin
  FModified := True;
end;

procedure TfrmSettingsDialog.Apply;
var
  CP : TConnectionProfile;
begin
  CP := FSettings.ConnectionProfiles[FVSTProfiles.FocusedNode.Index];
  if edtProfileName.Text = '' then
  begin
    MessageDlg(SConnectionProfileNameCannotBeEmpty, mtError, [mbOK], 0);
    pgcMain.ActivePage := tsConnectionProfiles;
    edtProfileName.SetFocus;
  end
  else
  begin
    SaveConnectionProfileChanges(CP);
    FSettings.GridCellColoring           := chkGridCellColoringEnabled.Checked;
    FSettings.DataTypeColors[dtBoolean]  := btnBooleanColor.DlgColor;
    FSettings.DataTypeColors[dtDate]     := btnDateColor.DlgColor;
    FSettings.DataTypeColors[dtDateTime] := btnDateTimeColor.DlgColor;
    FSettings.DataTypeColors[dtFloat]    := btnFloatColor.DlgColor;
    FSettings.DataTypeColors[dtInteger]  := btnIntegerColor.DlgColor;
    FSettings.DataTypeColors[dtString]   := btnStringColor.DlgColor;
    FSettings.DataTypeColors[dtNULL]     := btnNULLColor.DlgColor;
    FSettings.DataTypeColors[dtTime]     := btnTimeColor.DlgColor;
    FSettings.GridType := rgpGridTypes.Items[rgpGridTypes.ItemIndex];
    if Assigned(ApplySettingsMethod) then
      ApplySettingsMethod;
    Save;
    LoadApplicationSettingsFile;
    FVSTProfiles.Invalidate;
  end;
end;

function TfrmSettingsDialog.AskSaveFileChanges: Boolean;
begin
  Result := MessageDlg(
    SAskSaveChanges,
    mtConfirmation,
    [mbYes, mbNo],
    0,
    mbYes
  ) = mrYes;
end;

function TfrmSettingsDialog.ConfirmDeleteProfile: Boolean;
begin
  Result := MessageDlg(
    SConfirmDeleteProfile,
    mtWarning,
    [mbYes, mbNo],
    0,
    mbYes
  ) = mrYes;
end;

procedure TfrmSettingsDialog.InitializeControls;
var
  I : Integer;
  S : string;
begin
  actOpenSettingsFileLocation.Caption :=
    Format(SOpenSettingsFileLocation, [
    ExpandFileName(
      ExtractFileDir(Application.ExeName)
        + '\'
        + FSettings.FileName
      )
    ]);
  btnBooleanColor.DlgColor  := FSettings.DataTypeColors[dtBoolean];
  btnDateColor.DlgColor     := FSettings.DataTypeColors[dtDate];
  btnDateTimeColor.DlgColor := FSettings.DataTypeColors[dtDateTime];
  btnFloatColor.DlgColor    := FSettings.DataTypeColors[dtFloat];
  btnIntegerColor.DlgColor  := FSettings.DataTypeColors[dtInteger];
  btnStringColor.DlgColor   := FSettings.DataTypeColors[dtString];
  btnNULLColor.DlgColor     := FSettings.DataTypeColors[dtNULL];
  btnTimeColor.DlgColor     := FSettings.DataTypeColors[dtTime];

  if FSettings.ShowHorizontalGridLines then
  begin
    if FSettings.ShowVerticalGridLines then
      actGridlinesBoth.Checked := True
    else
      actGridlinesHorizontal.Checked := True;
  end
  else
  begin
    if FSettings.ShowVerticalGridLines then
      actGridlinesVertical.Checked := True
    else
      actGridlinesNone.Checked := True;
  end;

  case FSettings.ResultDisplayLayout of
    TResultDisplayLayout.Tabbed:
      actMRSAsMultipleTabs.Checked := True;
    TResultDisplayLayout.Horizontal:
      actMRSHorizontally.Checked   := True;
    TResultDisplayLayout.Vertical:
      actMRSVertically.Checked     := True;
  end;
  chkGridCellColoringEnabled.Checked := FSettings.GridCellColoring;
  edtEditorFont.Font.Assign(FSettings.EditorFont);
  edtEditorFont.Text := FSettings.EditorFont.Name;
  edtGridFont.Font.Assign(FSettings.GridFont);
  edtGridFont.Text   := FSettings.GridFont.Name;

  FVSTProfiles := TVirtualStringTreeFactory.CreateGrid(
    Self,
    pnlConnectionProfilesList
  );
  FVSTProfiles.AlignWithMargins  := False;
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;
  FVSTProfiles.OnPaintText       := FVSTProfilesPaintText;
  FVSTProfiles.Header.Options    := FVSTProfiles.Header.Options - [hoVisible];
  FVSTProfiles.TreeOptions.PaintOptions := FVSTProfiles.TreeOptions.PaintOptions
    - [toHideSelection, toUseExplorerTheme, toHotTrack];
  FVSTProfiles.Colors.FocusedSelectionColor := clBtnHighlight;
  FVSTProfiles.Indent    := 0;

  S := 'GridView';
  I := rgpGridTypes.Items.Add(S);
  if SameText(S, FSettings.GridType) then
    rgpGridTypes.ItemIndex := I;
  S := 'cxGrid';
  I := rgpGridTypes.Items.Add(S);
  if SameText(S, FSettings.GridType) then
    rgpGridTypes.ItemIndex := I;
  S := 'KGrid';
  I := rgpGridTypes.Items.Add(S);
  if SameText(S, FSettings.GridType) then
    rgpGridTypes.ItemIndex := I;

  FVSTProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  FVSTProfiles.FocusedNode   := FVSTProfiles.GetFirstVisible;
  FVSTProfiles.Selected[FVSTProfiles.FocusedNode] := True;
  pgcMain.ActivePage := tsConnectionProfiles;
end;

procedure TfrmSettingsDialog.InspectConnectionProfile(ACP: TConnectionProfile);
begin
  FObjectInspector.BeginUpdate;
  try
    FObjectInspector.Component := ACP;
    FObjectInspector.ExpandAll;
    FObjectInspector.SelectItem(-1);
  finally
    FObjectInspector.EndUpdate;
  end;
end;

procedure TfrmSettingsDialog.lblConnectionDefinitionNameDblClick(
  Sender: TObject);
begin
  actTestConnection.Execute;
end;

procedure TfrmSettingsDialog.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  actApply.Enabled        := FModified;
  edtPacketRecords.Enabled := chkFetchOnDemand.Checked;
  lblPacketrecords.Enabled := chkFetchOnDemand.Checked;
  B := Assigned(FVSTProfiles.FocusedNode)
    and (pgcMain.ActivePage = tsConnectionProfiles);
  actMoveUp.Enabled   := B and (FVSTProfiles.FocusedNode.Index > 0);
  actMoveDown.Enabled := B
    and (FVSTProfiles.FocusedNode.Index < FVSTProfiles.RootNodeCount - 1);
  actDelete.Enabled    := B;
  actDuplicate.Enabled := B;
  actAdd.Enabled       := pgcMain.ActivePage = tsConnectionProfiles;

  chkReadOnlyResultSets.Enabled := not chkMultipleResultSets.Checked;
//  actEditConnectionDef.Enabled := Trim(cbxConnectionDefs.Text) <> '';

  actEditConnectionDef.Enabled := True;

  B := chkOSAuthent.Checked;
  edtUserName.Enabled := not B;
  lblUserName.Enabled := not B;
  edtPassword.Enabled := not B;
  lblPassword.Enabled := not B;

  actSaveFile.Enabled := (pgcMain.ActivePage = tsSettings) and
    (pgcSettingsFiles.ActivePage = tsFDConnectionDefs)
    and seFDConnectionDefs.Modified;
end;

procedure TfrmSettingsDialog.UpdateConnectionProfileControls(
  ACP: TConnectionProfile);
begin
  edtProfileName.Text      := ACP.Name;
  btnProfileColor.DlgColor := ACP.ProfileColor;
  chkSetAsDefault.Checked  := ACP.Name = FSettings.DefaultConnectionProfile;
  edtPacketRecords.Text    := ACP.ConnectionSettings.PacketRecords.ToString;
  chkFetchOnDemand.Checked := ACP.ConnectionSettings.FetchOnDemand;
  cbxDrivers.Text          := ACP.ConnectionSettings.DriverName;
  edtDatabase.Text         := ACP.ConnectionSettings.Database;
  edtCatalog.Text          := ACP.ConnectionSettings.Catalog;
  edtUserName.Text         := ACP.ConnectionSettings.UserName;
  edtPassword.Text         := ACP.ConnectionSettings.Password;
  chkOSAuthent.Checked     := ACP.ConnectionSettings.OSAuthent;
  chkAutoReconnect.Checked := ACP.ConnectionSettings.AutoReconnect;
  chkMultipleResultSets.Checked := ACP.ConnectionSettings.MultipleResultSets;
  chkReadOnlyResultSets.Checked := ACP.ConnectionSettings.ReadOnly;
  chkDisconnectedMode.Checked   := ACP.ConnectionSettings.DisconnectedMode;
  cbxConnectionDefs.Text        := ACP.ConnectionSettings.ConnectionDefName;
  FDManager.GetDriverNames(cbxDrivers.Items);
  FDManager.GetConnectionDefNames(cbxConnectionDefs.Items);
end;
{$ENDREGION}

end.

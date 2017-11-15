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

unit DataGrabber.Settings.Dialog;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.ImageList, System.Actions, System.SysUtils, System.Variants,
  System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.Buttons, Vcl.ImgList,

  VirtualTrees,

  zObjInspector, zObjInspTypes,

  kcontrols, kbuttons, kedits,

  DataGrabber.Interfaces,

  DDuce.Editor.Factories, DDuce.Editor.Interfaces,

  DataGrabber.Settings, DataGrabber.ConnectionProfiles,
  DataGrabber.ConnectionProfileValueManager;

type
  TApplySettingsMethod = reference to procedure;

type
  TfrmSettingsDialog = class(TForm)
    {$REGION 'designer controls'}
    aclMain                        : TActionList;
    actAdd                         : TAction;
    actApply                       : TAction;
    actCancel                      : TAction;
    actClose                       : TAction;
    actConnectionString            : TAction;
    actDelete                      : TAction;
    actDuplicate                   : TAction;
    actMoveDown                    : TAction;
    actMoveUp                      : TAction;
    btnAdd                         : TToolButton;
    btnApply                       : TButton;
    btnBooleanColor                : TKColorButton;
    btnCancel                      : TButton;
    btnClose                       : TButton;
    btnConnectionString            : TButton;
    btnDateColor                   : TKColorButton;
    btnDateTimeColor               : TKColorButton;
    btnDelete                      : TToolButton;
    btnDuplicate                   : TToolButton;
    btnFloatColor                  : TKColorButton;
    btnIntegerColor                : TKColorButton;
    btnMemoColor                   : TKColorButton;
    btnMoveDown                    : TToolButton;
    btnMoveUp                      : TToolButton;
    btnNullColor                   : TKColorButton;
    btnProfileColor                : TKColorButton;
    btnStringColor                 : TKColorButton;
    btnTimeColor                   : TKColorButton;
    cbxProtocols                   : TComboBox;
    chkAllowMultipleInstances      : TCheckBox;
    chkFetchOnDemand               : TCheckBox;
    chkGridCellColoringEnabled     : TCheckBox;
    chkProviderMode                : TCheckBox;
    chkSeperateThreads             : TCheckBox;
    chkUseIDInUpdatableQueries     : TCheckBox;
    edtCatalog                     : TButtonedEdit;
    edtDatabase                    : TButtonedEdit;
    edtPacketRecords               : TEdit;
    edtProfileName                 : TLabeledEdit;
    grpCellBackgroundColoring      : TGroupBox;
    grpClientSettings              : TGroupBox;
    grpConnectionSettings          : TGroupBox;
    grpProfileSettings             : TGroupBox;
    imlMain                        : TImageList;
    lblBoolean                     : TLabel;
    lblCatalog                     : TLabel;
    lblDatabase                    : TLabel;
    lblDates                       : TLabel;
    lblDateTimes                   : TLabel;
    lblFloats                      : TLabel;
    lblIntegers                    : TLabel;
    lblMemo                        : TLabel;
    lblNULL                        : TLabel;
    lblPacketrecords               : TLabel;
    lblProfileColor                : TLabel;
    lblProtocols                   : TLabel;
    lblString                      : TLabel;
    lblTimes                       : TLabel;
    pgcConnectionProfile           : TPageControl;
    pgcMain                        : TPageControl;
    pnlConnectionProfileDetail     : TPanel;
    pnlConnectionProfilesList      : TPanel;
    pnlGridTypeColoring            : TGridPanel;
    rgpConnectionType              : TRadioGroup;
    rgpGridTypes                   : TRadioGroup;
    splVertical                    : TSplitter;
    tlbConnectionProfiles          : TToolBar;
    tsAdvanced                     : TTabSheet;
    tsBasic                        : TTabSheet;
    tsConnectionProfiles           : TTabSheet;
    tsDisplay                      : TTabSheet;
    tsSettings                     : TTabSheet;
    chkSetAsDefault: TCheckBox;
    btn1: TToolButton;
    btn2: TToolButton;
    grpGridLines: TGroupBox;
    actGridlinesBoth: TAction;
    actGridlinesHorizontal: TAction;
    actGridlinesVertical: TAction;
    actGridlinesNone: TAction;
    tlb1: TToolBar;
    btnGridlinesNone: TToolButton;
    btnGridlinesVertical: TToolButton;
    btnGridlinesHorizontal: TToolButton;
    btnGridlinesAll: TToolButton;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actApplyExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actDuplicateExecute(Sender: TObject);
    procedure actConnectionStringExecute(Sender: TObject);
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
    procedure cbxProtocolsChange(Sender: TObject);
    procedure chkFetchOnDemandClick(Sender: TObject);
    procedure chkProviderModeClick(Sender: TObject);
    procedure edtCatalogChange(Sender: TObject);
    procedure edtDatabaseChange(Sender: TObject);
    procedure edtDatabaseRightButtonClick(Sender: TObject);
    procedure edtProfileNameChange(Sender: TObject);
    procedure rgpConnectionTypeClick(Sender: TObject);
    procedure tsSettingsEnter(Sender: TObject);
    procedure chkSetAsDefaultClick(Sender: TObject);
    procedure actGridlinesHorizontalExecute(Sender: TObject);
    procedure actGridlinesVerticalExecute(Sender: TObject);
    procedure actGridlinesNoneExecute(Sender: TObject);
    procedure chkGridCellColoringEnabledClick(Sender: TObject);
    procedure actGridlinesBothExecute(Sender: TObject);
    {$ENDREGION}

  private
    FEditorSettings      : IEditorSettings;
    FEditor              : IEditorView;
    FManager             : IEditorManager;
    FSettings            : IDGSettings;
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

    procedure InspectConnectionProfile(ACP: TConnectionProfile);
    procedure InitializeConnectionProfileControls;
    procedure UpdateConnectionProfileControls(ACP: TConnectionProfile);
    procedure SaveConnectionProfileChanges(ACP: TConnectionProfile);
    procedure UpdateProtocols(AConnectionType: string);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner    : TComponent;
      ASettings : IDGSettings
    ); reintroduce; virtual;

    property ApplySettingsMethod: TApplySettingsMethod
      read FApplySettingsMethod write FApplySettingsMethod;

    property SelectedProfile: TConnectionProfile
      read GetSelectedProfile;
  end;

procedure ExecuteSettingsDialog(
  ASettings            : IDGSettings;
  AApplySettingsMethod : TApplySettingsMethod = nil
);

implementation

{$R *.dfm}

uses
  System.Rtti, System.UITypes,
  Data.DBConnAdmin, Data.Win.ADOConEd, Data.Win.ADODB,

  FireDAC.VCLUI.ConnEdit,

  Spring.Container,

  DDuce.Factories,

  ts.Interfaces,

  DataGrabber.Utils;

{$REGION 'interfaced routines'}
procedure ExecuteSettingsDialog(ASettings: IDGSettings;
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
constructor TfrmSettingsDialog.Create(AOwner: TComponent;
  ASettings: IDGSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TfrmSettingsDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  FValueManager := TConnectionProfileValueManager.Create;
  FObjectInspector :=
    TFactories.CreatezObjectInspector(Self, tsAdvanced, nil, FValueManager);
  FObjectInspector.SplitterPos     := FObjectInspector.Width div 2;
  FObjectInspector.SortByCategory  := False;
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;

  FEditorSettings := TEditorFactories.CreateSettings(Self);
  FManager        := TEditorFactories.CreateManager(Self, FEditorSettings);
  FEditor         := TEditorFactories.CreateView(tsSettings, FManager);
  FEditor.Form.BorderStyle := bsNone;
  FEditor.Form.AlignWithMargins := True;
  FEditor.HighlighterName := 'JSON';
  FEditor.Load(FSettings.FileName);
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

procedure TfrmSettingsDialog.actConnectionStringExecute(Sender: TObject);
var
  AC : TADOConnection;
  CP : TConnectionProfile;
  S  : string;
begin
  CP := FSettings.ConnectionProfiles[FVSTProfiles.FocusedNode.Index];
  if CP.ConnectionType = 'ADO' then
  begin
    AC := TADOConnection.Create(Self);
    try
      AC.ConnectionString := CP.ConnectionString;
      EditConnectionString(AC);
      CP.ConnectionString := AC.ConnectionString;
    finally
      AC.Free;
    end;
  end
  else if CP.ConnectionType = 'FireDAC' then
  begin
    S := CP.ConnectionString;
    TfrmFDGUIxFormsConnEdit.Execute(S, '');
    CP.ConnectionString := S;
  end;

end;

procedure TfrmSettingsDialog.actDeleteExecute(Sender: TObject);
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

procedure TfrmSettingsDialog.cbxProtocolsChange(Sender: TObject);
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
    with TControlItem(CI) do
      Control.Enabled := (Sender as TCheckBox).Checked;
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

function TfrmSettingsDialog.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not (PItem.Prop.PropertyType is TRttiMethodType);
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
// TODO open file dialog?
end;

procedure TfrmSettingsDialog.edtProfileNameChange(Sender: TObject);
begin
  Changed;
end;

procedure TfrmSettingsDialog.tsSettingsEnter(Sender: TObject);
begin
  FEditor.Load(FSettings.FileName);
end;

procedure TfrmSettingsDialog.rgpConnectionTypeClick(Sender: TObject);
begin
  if rgpConnectionType.ItemIndex >= 0 then
  begin
    Changed;
    UpdateProtocols(rgpConnectionType.Items[rgpConnectionType.ItemIndex]);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSettingsDialog.Save;
begin
  FSettings.Save;
end;

procedure TfrmSettingsDialog.SaveConnectionProfileChanges(
  ACP: TConnectionProfile);
begin
  ACP.Name           := edtProfileName.Text;
  ACP.ProfileColor   := btnProfileColor.DlgColor;
  ACP.ConnectionType :=
    rgpConnectionType.Items[rgpConnectionType.ItemIndex];
  ACP.ProviderMode   := chkProviderMode.Checked;
  ACP.PacketRecords  := StrToIntDef(edtPacketRecords.Text, 0);

  ACP.ConnectionSettings.Protocol := cbxProtocols.Text;
  ACP.ConnectionSettings.Database := edtDatabase.Text;
  ACP.ConnectionSettings.Catalog  := edtCatalog.Text;
  FModified := False;
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
  FEditor.Load(FSettings.FileName);
end;

procedure TfrmSettingsDialog.InitializeConnectionProfileControls;
var
  C : IConnection;
begin
  for C in GlobalContainer.ResolveAll<IConnection> do
    rgpConnectionType.Items.Add(C.ConnectionType);
end;

procedure TfrmSettingsDialog.InitializeControls;
var
  I : Integer;
  S : string;
begin
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

  chkGridCellColoringEnabled.Checked := FSettings.GridCellColoring;
  rgpConnectionType.Items.Clear;

  InitializeConnectionProfileControls;

  FVSTProfiles := TFactories.CreateVirtualStringTree(
    Self,
    pnlConnectionProfilesList
  );
  FVSTProfiles.AlignWithMargins  := False;
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;
  FVSTProfiles.OnPaintText       := FVSTProfilesPaintText;
  FVSTProfiles.Header.Options    := FVSTProfiles.Header.Options - [hoVisible];
  FVSTProfiles.TreeOptions.PaintOptions :=
    FVSTProfiles.TreeOptions.PaintOptions - [toHideSelection];
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

procedure TfrmSettingsDialog.UpdateActions;
var
  B: Boolean;
begin
  inherited UpdateActions;
  B := chkProviderMode.Checked;
  chkFetchOnDemand.Enabled := B;
  edtPacketRecords.Enabled := B and chkFetchOnDemand.Checked;
  lblPacketrecords.Enabled := B and chkFetchOnDemand.Checked;
  B := Assigned(FVSTProfiles.FocusedNode);
  actMoveUp.Enabled := B and (FVSTProfiles.FocusedNode.Index > 0);
  actMoveDown.Enabled := B
    and (FVSTProfiles.FocusedNode.Index < FVSTProfiles.RootNodeCount - 1);
  actDelete.Enabled := B;
end;

procedure TfrmSettingsDialog.UpdateConnectionProfileControls(
  ACP: TConnectionProfile);
begin
  edtProfileName.Text      := ACP.Name;
  btnProfileColor.DlgColor := ACP.ProfileColor;

  rgpConnectionType.ItemIndex :=
    rgpConnectionType.Items.IndexOf(ACP.ConnectionType);

  chkSetAsDefault.Checked := ACP.Name = FSettings.DefaultConnectionProfile;

  chkProviderMode.Checked := ACP.ProviderMode;
  edtPacketRecords.Text   := ACP.PacketRecords.ToString;

  UpdateProtocols(ACP.ConnectionType);
  cbxProtocols.Text := ACP.ConnectionSettings.Protocol;
  edtDatabase.Text  := ACP.ConnectionSettings.Database;
  edtCatalog.Text   := ACP.ConnectionSettings.Catalog;
end;

procedure TfrmSettingsDialog.UpdateProtocols(AConnectionType: string);
var
  S : IConnection;
begin
  if AConnectionType <> '' then
  begin
    S := GlobalContainer.Resolve<IConnection>(AConnectionType);
    cbxProtocols.Items.Assign(S.Protocols);
  end;
end;
{$ENDREGION}

end.

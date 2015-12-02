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

unit DataGrabber.SettingsDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.MSXMLIntf,
  System.ImageList, System.Actions, System.SysUtils, System.Variants,
  System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.Grids, Vcl.Buttons, Vcl.ImgList,

  VirtualTrees,

  DDuce.Components.PropertyInspector,
  //DDuce.Components.XMLTree,

  DataGrabber.Interfaces, DataGrabber.Settings, DataGrabber.PropertyEditors;

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
    actDelete                      : TAction;
    actDuplicate                   : TAction;
    actMoveDown                    : TAction;
    actMoveUp                      : TAction;
    actSave                        : TAction;
    btnAdd                         : TToolButton;
    btnApply                       : TButton;
    btnCancel                      : TButton;
    btnClose                       : TButton;
    btnDelete                      : TToolButton;
    btnDuplicate                   : TToolButton;
    btnMoveDown                    : TToolButton;
    btnMoveUp                      : TToolButton;
    btnSave                        : TButton;
    chkAllowMultipleInstances      : TCheckBox;
    chkFetchOnDemand               : TCheckBox;
    chkGridCellColoringEnabled     : TCheckBox;
    chkProviderMode                : TCheckBox;
    chkSeperateThreads             : TCheckBox;
    chkUseIDInUpdatableQueries     : TCheckBox;
    edtPacketRecords               : TEdit;
    grpCellBackgroundColoring      : TGroupBox;
    grpConnectionSettings          : TGroupBox;
    imlMain                        : TImageList;
    lbl1                           : TLabel;
    lblDates                       : TLabel;
    lblDateTimes                   : TLabel;
    lblFloats                      : TLabel;
    lblIntegers                    : TLabel;
    lblMemo                        : TLabel;
    lblNULL                        : TLabel;
    lblPacketrecords               : TLabel;
    lblString                      : TLabel;
    lblTimes                       : TLabel;
    pgcMain                        : TPageControl;
    pnlConnectionProfilesInspector : TPanel;
    pnlConnectionProfilesList      : TPanel;
    rgpConnectionType              : TRadioGroup;
    rgpGridTypes                   : TRadioGroup;
    splVertical                    : TSplitter;
    tlbConnectionProfiles          : TToolBar;
    tsConnection                   : TTabSheet;
    tsConnectionProfiles           : TTabSheet;
    tsDisplay                      : TTabSheet;
    tsXML                          : TTabSheet;
    {$ENDREGION}

    procedure actApplyExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actDuplicateExecute(Sender: TObject);

    procedure OnColorButtonClick(Sender: TObject);
    procedure xtrSettingsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure xtrSettingsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure xtrSettingsColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure xtrSettingsColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
//    procedure xtrSettingsCheckNode(Sender: TXMLTree; Node: PVirtualNode;
//      var NewXmlNode: IXMLDOMNode; var NewNodeType: Integer; var Add: Boolean);
    procedure vstProfilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstProfilesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure piConnectionProfilesGetEditorClass(Sender: TObject;
      AInstance: TObject; APropInfo: PPropInfo;
      var AEditorClass: TPropertyEditorClass);
    procedure vstProfilesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

  private
    FSettings            : IDGSettings;
    FApplySettingsMethod : TApplySettingsMethod;
    piConnectionProfiles : TPropertyInspector;
    vstProfiles          : TVirtualStringTree;
    //xtrSettings          : TXMLTree;

    procedure Apply;
    procedure Save;

    procedure InspectConnectionProfile(AIndex: Integer);

  protected
    procedure Initialize;
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : IDGSettings
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;


    property ApplySettingsMethod: TApplySettingsMethod
      read FApplySettingsMethod write FApplySettingsMethod;
  end;

procedure ExecuteSettingsDialog(
  ASettings            : IDGSettings;
  AApplySettingsMethod : TApplySettingsMethod = nil
);

implementation

{$R *.dfm}

uses
  ts.Utils, ts.Interfaces,

  DataGrabber.ConnectionProfiles, DataGrabber.Utils, DataGrabber.Factories,

  Spring.Services;

{$REGION 'interfaced routines'}
procedure ExecuteSettingsDialog(ASettings: IDGSettings;
  AApplySettingsMethod: TApplySettingsMethod);
var
  Form : TfrmSettingsDialog;
begin
  Form := TfrmSettingsDialog.Create(Application, ASettings);
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
  //xtrSettings := CreateXMLTree(Self, tsXML);
  //xtrSettings.OnCheckNode      := xtrSettingsCheckNode;
//  xtrSettings.OnColumnClick    := xtrSettingsColumnClick;
//  xtrSettings.OnColumnDblClick := xtrSettingsColumnDblClick;
//  xtrSettings.OnEditing        := xtrSettingsEditing;
//  xtrSettings.OnPaintText      := xtrSettingsPaintText;

  piConnectionProfiles := CreateInspector(Self, pnlConnectionProfilesInspector);
  piConnectionProfiles.OnGetEditorClass := piConnectionProfilesGetEditorClass;
  Initialize;
end;

procedure TfrmSettingsDialog.BeforeDestruction;
begin
  piConnectionProfiles.Designer := nil;
  FSettings := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSettingsDialog.actAddExecute(Sender: TObject);
var
  CP: TConnectionProfile;
begin
  CP := FSettings.ConnectionProfiles.Add;
  // copy default settings
  CP.ConnectionType := FSettings.ConnectionType;
  CP.ProviderMode   := FSettings.ProviderMode;
  CP.PacketRecords  := FSettings.PacketRecords;
  CP.ConnectionSettings.Assign(FSettings.ConnectionSettings);
  vstProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  SelectNode(vstProfiles, vstProfiles.RootNodeCount - 1);
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

procedure TfrmSettingsDialog.actDeleteExecute(Sender: TObject);
begin
  vstProfiles.BeginUpdate;
  try
    piConnectionProfiles.Clear;
    FSettings.ConnectionProfiles.Delete(vstProfiles.FocusedNode.Index);
    SelectNode(vstProfiles, FSettings.ConnectionProfiles.Count - 1);
  finally
    vstProfiles.EndUpdate;
  end;
  vstProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  vstProfiles.Refresh;
end;

procedure TfrmSettingsDialog.actDuplicateExecute(Sender: TObject);
var
  N: Integer;
begin
  N := vstProfiles.FocusedNode.Index;
  with FSettings.ConnectionProfiles.Add do
  begin
    Assign(FSettings.ConnectionProfiles[N]);
    N := Index;
  end;
  vstProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  SelectNode(vstProfiles, N);
end;

procedure TfrmSettingsDialog.actMoveDownExecute(Sender: TObject);
var
  N: Integer;
begin
  N := vstProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N + 1;
  SelectNode(vstProfiles, N + 1);
  vstProfiles.Refresh;
end;

procedure TfrmSettingsDialog.actMoveUpExecute(Sender: TObject);
var
  N: Integer;
begin
  N := vstProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N - 1;
  SelectNode(vstProfiles, N - 1);
  vstProfiles.Refresh;
end;

procedure TfrmSettingsDialog.actSaveExecute(Sender: TObject);
begin
  Save;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmSettingsDialog.OnColorButtonClick(Sender: TObject);
begin
  //(Sender as TColorPickerButton).DroppedDown := True;
end;

procedure TfrmSettingsDialog.piConnectionProfilesGetEditorClass(Sender: TObject;
  AInstance: TObject; APropInfo: PPropInfo;
  var AEditorClass: TPropertyEditorClass);
begin
  if APropInfo.Name = 'Protocol' then
    AEditorClass := TProtocolPropertyEditor
  else if APropInfo.Name = 'ConnectionType' then
    AEditorClass := TConnectionTypePropertyEditor;
end;

procedure TfrmSettingsDialog.vstProfilesBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if Node.Index < FSettings.ConnectionProfiles.Count then
  begin
    TargetCanvas.Brush.Color :=
      FSettings.ConnectionProfiles[Node.Index].ProfileColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TfrmSettingsDialog.vstProfilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  InspectConnectionProfile(Node.Index);
end;

procedure TfrmSettingsDialog.vstProfilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Node.Index < FSettings.ConnectionProfiles.Count then
    CellText := FSettings.ConnectionProfiles[Node.Index].DisplayName;
end;

//procedure TfrmSettingsDialog.xtrSettingsCheckNode(Sender: TXMLTree;
//  ANode: PVirtualNode; var ANewXmlNode: TXmlNode; var ANewNodeType: TNodeType;
//  var AAdd: Boolean);
//begin
//  AAdd := True;
//end;

procedure TfrmSettingsDialog.xtrSettingsColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  //Sender.EditNode(Sender.GetFirstSelected, Column);
end;

procedure TfrmSettingsDialog.xtrSettingsColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
//  Sender.EditNode(Sender.GetFirstSelected, Column);
end;

procedure TfrmSettingsDialog.xtrSettingsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmSettingsDialog.xtrSettingsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
  begin
    TargetCanvas.Font.Color := clWhite;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmSettingsDialog.Apply;
begin
//  FSettings.DataTypeColors[dtBoolean] := btnGridBooleanColor.SelectionColor;
//  FSettings.DataTypeColors[dtDate] := btnGridDateColor.SelectionColor;
//  FSettings.DataTypeColors[dtDateTime] := btnGridDateTimeColor.SelectionColor;
//  FSettings.DataTypeColors[dtFloat] := btnGridFloatColor.SelectionColor;
//  FSettings.DataTypeColors[dtInteger] := btnGridIntegerColor.SelectionColor;
//  FSettings.DataTypeColors[dtString] := btnGridStringColor.SelectionColor;
//  FSettings.DataTypeColors[dtNULL] := btnGridNULLColor.SelectionColor;
//  FSettings.DataTypeColors[dtTime] := btnGridTimeColor.SelectionColor;
  FSettings.ProviderMode := chkProviderMode.Checked;
  FSettings.PacketRecords := StrToIntDef(edtPacketRecords.Text, -1);
  FSettings.FetchOnDemand := chkFetchOnDemand.Checked;
  FSettings.GridCellColoring := chkGridCellColoringEnabled.Checked;
  FSettings.ConnectionType := rgpConnectionType.Items
    [rgpConnectionType.ItemIndex];
//  FSettings.GridType := rgpGridTypes.Items[rgpGridTypes.ItemIndex];
  if Assigned(ApplySettingsMethod) then
    ApplySettingsMethod;
end;

procedure TfrmSettingsDialog.Save;
begin
  FSettings.Save;
end;

procedure TfrmSettingsDialog.Initialize;
var
  I  : Integer;
  C  : IConnection;
  DV : IDGDataView;
  S  : string;
begin
//  btnGridBooleanColor.SelectionColor  := FSettings.DataTypeColors[dtBoolean];
//  btnGridDateColor.SelectionColor     := FSettings.DataTypeColors[dtDate];
//  btnGridDateTimeColor.SelectionColor := FSettings.DataTypeColors[dtDateTime];
//  btnGridFloatColor.SelectionColor    := FSettings.DataTypeColors[dtFloat];
//  btnGridIntegerColor.SelectionColor  := FSettings.DataTypeColors[dtInteger];
//  btnGridStringColor.SelectionColor   := FSettings.DataTypeColors[dtString];
//  btnGridNULLColor.SelectionColor     := FSettings.DataTypeColors[dtNULL];
//  btnGridTimeColor.SelectionColor     := FSettings.DataTypeColors[dtTime];

  chkProviderMode.Checked            := FSettings.ProviderMode;
  chkGridCellColoringEnabled.Checked := FSettings.GridCellColoring;

//  xtrSettings                  := TXMLTree.Create(Self);
//  xtrSettings.Parent           := tsXML;
//  xtrSettings.Align            := alClient;
//  xtrSettings.OnEditing        := xtrSettingsEditing;
//  xtrSettings.OnPaintText      := xtrSettingsPaintText;
//  xtrSettings.OnColumnClick    := xtrSettingsColumnClick;
//  xtrSettings.OnColumnDblClick := xtrSettingsColumnDblClick;

 // xtrSettings.OnCheckNode := xtrSettingsCheckNode;
  //xtrSettings.Xml := FSettings.XML;

  piConnectionProfiles        := TPropertyInspector.Create(Self);
  piConnectionProfiles.Parent := pnlConnectionProfilesInspector;
  piConnectionProfiles.Align  := alClient;
  piConnectionProfiles.OnGetEditorClass := piConnectionProfilesGetEditorClass;
  rgpConnectionType.Items.Clear;

  vstProfiles := TVirtualStringTree.Create(Self);
  vstProfiles.Parent := pnlConnectionProfilesList;
  vstProfiles.Align  := alClient;
  vstProfiles.OnGetText := vstProfilesGetText;
  vstProfiles.OnFocusChanged := vstProfilesFocusChanged;
  vstProfiles.OnBeforeCellPaint := vstProfilesBeforeCellPaint;

  for C in ServiceLocator.GetAllServices<IConnection> do
  begin
    I := rgpConnectionType.Items.Add(C.ConnectionType);
    if SameText(C.ConnectionType, FSettings.ConnectionType) then
      rgpConnectionType.ItemIndex := I;
  end;

  rgpGridTypes.Items.Clear;
  for DV in ServiceLocator.GetAllServices<IDGDataView> do
  begin
    S := DV.Name;
    rgpGridTypes.Items.Add(S);
    if SameText(S, FSettings.GridType) then
      rgpGridTypes.ItemIndex := I;
  end;

  vstProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
  SetWindowSizeGrip(Handle, True);
end;

procedure TfrmSettingsDialog.InspectConnectionProfile(AIndex: Integer);
var
  I: Integer;
begin
  if AIndex >= FSettings.ConnectionProfiles.Count then
    AIndex := FSettings.ConnectionProfiles.Count - 1;
  piConnectionProfiles.BeginUpdate;
  try
    piConnectionProfiles.Clear;
    piConnectionProfiles.Add(FSettings.ConnectionProfiles[AIndex]);
    piConnectionProfiles.Designer := FSettings.ConnectionProfiles[AIndex];
    for I := 0 to piConnectionProfiles.Items.Count - 1 do
    begin
      if piConnectionProfiles.Items[I].Expandable = mieYes then
        piConnectionProfiles.Items[I].Expand;
    end;
  finally
    piConnectionProfiles.EndUpdate;
  end;
end;

procedure TfrmSettingsDialog.UpdateActions;
var
  B: Boolean;
begin
  inherited;
  B := chkProviderMode.Checked;
  chkFetchOnDemand.Enabled := B;
  edtPacketRecords.Enabled := B and chkFetchOnDemand.Checked;
  lblPacketrecords.Enabled := B and chkFetchOnDemand.Checked;
  B := Assigned(vstProfiles.FocusedNode);
  actMoveUp.Enabled := B and (vstProfiles.FocusedNode.Index > 0);
  actMoveDown.Enabled := B
    and (vstProfiles.FocusedNode.Index < vstProfiles.RootNodeCount - 1);
  actDelete.Enabled := B;
end;
{$ENDREGION}

end.

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
  Winapi.Windows, Winapi.Messages, Winapi.MSXMLIntf,
  System.ImageList, System.Actions, System.SysUtils, System.Variants,
  System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.Grids, Vcl.Buttons, Vcl.ImgList,

  VirtualTrees, zObjInspector,

  DDuce.Components.PropertyInspector,

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
    actConnectionString            : TAction;
    actDelete                      : TAction;
    actDuplicate                   : TAction;
    actMoveDown                    : TAction;
    actMoveUp                      : TAction;
    btnAdd                         : TToolButton;
    btnApply                       : TButton;
    btnCancel                      : TButton;
    btnClose                       : TButton;
    btnConnectionString            : TButton;
    btnDelete                      : TToolButton;
    btnDuplicate                   : TToolButton;
    btnMoveDown                    : TToolButton;
    btnMoveUp                      : TToolButton;
    chkAllowMultipleInstances      : TCheckBox;
    chkFetchOnDemand               : TCheckBox;
    chkGridCellColoringEnabled     : TCheckBox;
    chkProviderMode                : TCheckBox;
    chkSeperateThreads             : TCheckBox;
    chkUseIDInUpdatableQueries     : TCheckBox;
    dlgColor                       : TColorDialog;
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
    procedure piConnectionProfilesGetEditorClass(
      Sender           : TObject;
      AInstance        : TObject;
      APropInfo        : PPropInfo;
      var AEditorClass : TPropertyEditorClass
    );
    {$ENDREGION}

  private
    FSettings            : IDGSettings;
    FApplySettingsMethod : TApplySettingsMethod;
    FObjectInspector     : TzObjectInspector;
    FVSTProfiles         : TVirtualStringTree;

  protected
    procedure Apply;
    procedure Save;
    procedure InspectConnectionProfile(AIndex: Integer);
    procedure InitializeControls;
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : IDGSettings
    ); reintroduce; virtual;
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
  System.Rtti,
  Data.DBConnAdmin, Data.Win.ADOConEd, Data.Win.ADODB,

  Spring.Container,

  DDuce.Factories,

  ts.Utils, ts.Interfaces,

  DataGrabber.ConnectionProfiles, DataGrabber.Utils, DataGrabber.Factories;

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
  FObjectInspector :=
    TFactories.CreatezObjectInspector(Self, pnlConnectionProfilesInspector);
  FObjectInspector.SplitterPos     := FObjectInspector.Width div 2;
  FObjectInspector.SortByCategory  := False;
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  InitializeControls;
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
begin
  AC := TADOConnection.Create(Self);
  try
    EditConnectionString(AC);
  finally
    AC.Free;
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

procedure TfrmSettingsDialog.actMoveDownExecute(Sender: TObject);
var
  N: Integer;
begin
  N := FVSTProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N + 1;
  SelectNode(FVSTProfiles, N + 1);
  FVSTProfiles.Refresh;
end;

procedure TfrmSettingsDialog.actMoveUpExecute(Sender: TObject);
var
  N: Integer;
begin
  N := FVSTProfiles.FocusedNode.Index;
  FSettings.ConnectionProfiles[N].Index := N - 1;
  SelectNode(FVSTProfiles, N - 1);
  FVSTProfiles.Refresh;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmSettingsDialog.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not (PItem.Prop.PropertyType is TRttiMethodType);
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
  end;
end;

procedure TfrmSettingsDialog.FVSTProfilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  InspectConnectionProfile(Node.Index);
end;

procedure TfrmSettingsDialog.FVSTProfilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Node.Index < Cardinal(FSettings.ConnectionProfiles.Count) then
    CellText := FSettings.ConnectionProfiles[Node.Index].DisplayName;
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
  FSettings.ProviderMode     := chkProviderMode.Checked;
  FSettings.PacketRecords    := StrToIntDef(edtPacketRecords.Text, -1);
  FSettings.FetchOnDemand    := chkFetchOnDemand.Checked;
  FSettings.GridCellColoring := chkGridCellColoringEnabled.Checked;
  FSettings.ConnectionType   := rgpConnectionType.Items
    [rgpConnectionType.ItemIndex];
  FSettings.GridType := rgpGridTypes.Items[rgpGridTypes.ItemIndex];
  if Assigned(ApplySettingsMethod) then
    ApplySettingsMethod;
  Save;
end;

procedure TfrmSettingsDialog.Save;
begin
  FSettings.Save;
end;

procedure TfrmSettingsDialog.InitializeControls;
var
  I  : Integer;
  C  : IConnection;
  //DV : IDGDataView;
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
  rgpConnectionType.Items.Clear;

  FVSTProfiles := TFactories.CreateVirtualStringTree(Self, pnlConnectionProfilesList);
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;

  for C in GlobalContainer.ResolveAll<IConnection> do
  begin
    I := rgpConnectionType.Items.Add(C.ConnectionType);
    if SameText(C.ConnectionType, FSettings.ConnectionType) then
      rgpConnectionType.ItemIndex := I;
  end;

//  rgpGridTypes.Items.Clear;
//  for DV in GlobalContainer.ResolveAll<IDGDataView> do
//  begin
//    S := DV.GridType;
//    I := rgpGridTypes.Items.Add(S);
//    if SameText(S, FSettings.GridType) then
//      rgpGridTypes.ItemIndex := I;
//  end;
  S := 'GridView';
  I := rgpGridTypes.Items.Add(S);
  if SameText(S, FSettings.GridType) then
    rgpGridTypes.ItemIndex := I;
  S := 'cxGrid';
  I := rgpGridTypes.Items.Add(S);
  if SameText(S, FSettings.GridType) then
    rgpGridTypes.ItemIndex := I;

  FVSTProfiles.RootNodeCount := FSettings.ConnectionProfiles.Count;
end;

procedure TfrmSettingsDialog.InspectConnectionProfile(AIndex: Integer);
begin
  if AIndex >= FSettings.ConnectionProfiles.Count then
    AIndex := FSettings.ConnectionProfiles.Count - 1;

  FObjectInspector.BeginUpdate;
  try
    FObjectInspector.Component := FSettings.ConnectionProfiles[AIndex];
    FObjectInspector.ExpandAll;
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
{$ENDREGION}

end.

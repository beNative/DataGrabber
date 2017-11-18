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

unit DataGrabber.ConnectionView;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls,

  VirtualTrees,

  DataGrabber.Interfaces, DataGrabber.ConnectionProfiles;

{
   A IConnectionView instance consists of
     - one editorview
     - one or more dataviews corresponding to the user input in the editor as
       multiple datasets can be returned as a result of one statement.

     - a list of connectionprofiles (those defined in the settings)

     - an active connection profile (of the available profiles in
        FSettings.ConnectionProfiles)

     - The owner of a ConnectionView is always a IConnectionViewManager instance
}

type
  TfrmConnectionView = class(TForm, IConnectionView)
    {$REGION 'designer controls'}
    pnlBottom     : TPanel;
    pnlEditor     : TPanel;
    pnlGrid       : TPanel;
    pnlProfiles   : TPanel;
    pnlTop        : TPanel;
    splHorizontal : TSplitter;
    splVertical   : TSplitter;
    {$ENDREGION}

    procedure FVSTProfilesBeforeCellPaint(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      CellPaintMode   : TVTCellPaintMode;
      CellRect        : TRect;
      var ContentRect : TRect
    );
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
    procedure FVSTProfilesPaintText(
      Sender             : TBaseVirtualTree;
      const TargetCanvas : TCanvas;
      Node               : PVirtualNode;
      Column             : TColumnIndex;
      TextType           : TVSTTextType
    );

    procedure tlbGridCustomDraw(
      Sender          : TToolBar;
      const ARect     : TRect;
      var DefaultDraw : Boolean
    );

    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);

  private
    FEditorView     : IEditorView;
    FActiveDataView : IDataView;
    FActiveData     : IData;
    FVSTProfiles    : TVirtualStringTree;
    FDefaultNode    : PVirtualNode;

    function GetManager: IConnectionViewManager;
    function GetForm: TForm;
    function GetActiveData: IData;
    function GetActiveDataView: IDataView;
    function GetEditorView: IEditorView;
    function GetActiveConnectionProfile: TConnectionProfile;

  protected
    procedure InitializeEditorView;
    procedure InitializeConnectionProfilesView;
    procedure Copy;
    procedure UpdateActions; override;
    procedure ApplySettings;

  public
    procedure AfterConstruction; override;
    constructor Create(
      AOwner      : TComponent;
      AEditorView : IEditorView;
      ADataView   : IDataView;
      AData       : IData
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    property Form: TForm
      read GetForm;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property ActiveData: IData
      read GetActiveData;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;

    property EditorView: IEditorView
      read GetEditorView;

    property Manager: IConnectionViewManager
      read GetManager;
  end;

implementation

uses
  System.UITypes,

  Spring,

  DDuce.Factories,

  DataGrabber.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmConnectionView.Create(AOwner: TComponent;
  AEditorView: IEditorView; ADataView: IDataView; AData: IData);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AEditorView, 'AEditorView');
  Guard.CheckNotNull(ADataView, 'ADataView');
  Guard.CheckNotNull(AData, 'AData');
  FEditorView     := AEditorView;
  FActiveDataView := ADataView;
  FActiveData     := AData;
  ActiveDataView.AssignParent(pnlBottom);
end;

procedure TfrmConnectionView.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeEditorView;
  InitializeConnectionProfilesView;
  ApplySettings;
end;

procedure TfrmConnectionView.BeforeDestruction;
begin
  FEditorView := nil;
  FActiveDataView := nil;
  FActiveData := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'FVSTProfiles'}
procedure TfrmConnectionView.FVSTProfilesBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  VST : TVirtualStringTree absolute Sender;
begin
  TargetCanvas.Brush.Color :=
    Manager.Settings.ConnectionProfiles[Node.Index].ProfileColor;
  TargetCanvas.FillRect(CellRect);
  if Sender.FocusedNode = Node then
  begin
    TargetCanvas.Pen.Width := 1;
    TargetCanvas.Pen.Color := clBlue;
    TargetCanvas.Rectangle(CellRect);
  end;
end;

procedure TfrmConnectionView.FVSTProfilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if ContainsFocus(Self) then
    ApplySettings;
end;

procedure TfrmConnectionView.FVSTProfilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  CellText := Manager.Settings.ConnectionProfiles[Node.Index].DisplayName;
  if Manager.Settings.ConnectionProfiles[Node.Index] =
    (Owner as IConnectionViewManager).DefaultConnectionProfile
  then
    FDefaultNode := Node;
end;

procedure TfrmConnectionView.FVSTProfilesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.FocusedNode = Node then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end;
end;
{$ENDREGION}

{ Needed to allow the shortcuts of the manager's actions to be executed. }

procedure TfrmConnectionView.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfrmConnectionView.FormShow(Sender: TObject);
begin
  FVSTProfiles.SetFocus;
  if Assigned(FDefaultNode) then
  begin
    FVSTProfiles.FocusedNode := FDefaultNode
  end
  else
  begin
    FVSTProfiles.FocusedNode := FVSTProfiles.GetFirstVisible;
  end;
  EditorView.SetFocus;
end;

procedure TfrmConnectionView.tlbGridCustomDraw(Sender: TToolBar;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  Sender.Canvas.FillRect(ARect);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmConnectionView.GetActiveConnectionProfile: TConnectionProfile;
begin
  if Assigned(FVSTProfiles.FocusedNode) then
    Exit(Manager.Settings.ConnectionProfiles.Items[FVSTProfiles.FocusedNode.Index])
  else
    Exit(nil);
end;

function TfrmConnectionView.GetActiveData: IData;
begin
  Result := FActiveData;
end;

function TfrmConnectionView.GetActiveDataView: IDataView;
begin
  Result := FActiveDataView;
end;

function TfrmConnectionView.GetManager: IConnectionViewManager;
begin
  Result := Owner as IConnectionViewManager;
end;

function TfrmConnectionView.GetEditorView: IEditorView;
begin
  Result := FEditorView;
end;

function TfrmConnectionView.GetForm: TForm;
begin
  Result := Self;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmConnectionView.InitializeConnectionProfilesView;
begin
  FVSTProfiles := TFactories.CreateVirtualStringTree(Self, pnlProfiles);
  FVSTProfiles.AlignWithMargins  := False;
  FVSTProfiles.RootNodeCount     := Manager.Settings.ConnectionProfiles.Count;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnPaintText       := FVSTProfilesPaintText;
  FVSTProfiles.Header.Options    := FVSTProfiles.Header.Options - [hoVisible];
  FVSTProfiles.TreeOptions.PaintOptions :=
    FVSTProfiles.TreeOptions.PaintOptions - [toHideSelection];
  FVSTProfiles.Colors.FocusedSelectionColor := clBtnHighlight;
  FVSTProfiles.Margins.Right := 0;
  FVSTProfiles.Indent        := 0;
end;

procedure TfrmConnectionView.InitializeEditorView;
var
  F: TForm;
begin
  F := FEditorView as TForm;
  F.PopupMenu   := Manager.ConnectionViewPopupMenu;
  F.BorderStyle := bsNone;
  F.Parent      := pnlTop;
  F.Align       := alClient;
  F.Visible     := True;
end;

procedure TfrmConnectionView.ApplySettings;
var
  CP: TConnectionProfile;
begin
  if Assigned(FVSTProfiles.FocusedNode) then
  begin
    FVSTProfiles.RootNodeCount := Manager.Settings.ConnectionProfiles.Count;
    FVSTProfiles.Refresh;

    CP := Manager.Settings.ConnectionProfiles.Items[
      FVSTProfiles.FocusedNode.Index
    ];
    Application.Title := CP.Name;
    Caption := CP.Name;
//    FActiveData.ConnectionSettings.Assign(CP.ConnectionSettings);
  end;
end;

procedure TfrmConnectionView.Copy;
begin
  if EditorView.EditorFocused then
    EditorView.CopyToClipboard
  else
    ActiveDataView.Copy;
end;

procedure TfrmConnectionView.UpdateActions;
begin
  inherited UpdateActions;
  if ContainsFocus(Self) then
  begin
    if Manager.ActiveConnectionView <> (Self as IConnectionView) then
    begin
      Manager.ActiveConnectionView := Self;
      EditorView.SetFocus;
    end;
    Manager.UpdateActions;
  end;
end;
{$ENDREGION}

end.

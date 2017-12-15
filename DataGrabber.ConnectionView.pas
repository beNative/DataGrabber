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

  Spring.Collections,

  VirtualTrees,

  DataGrabber.Interfaces, DataGrabber.ConnectionProfiles, OMultiPanel;

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
    pnlMain: TOMultiPanel;
    pnlBottom: TOMultiPanel;
    pnlTop: TOMultiPanel;
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
    procedure FVSTProfilesDrawText(
      Sender          : TBaseVirtualTree;
      TargetCanvas    : TCanvas;
      Node            : PVirtualNode;
      Column          : TColumnIndex;
      const Text      : string;
      const CellRect  : TRect;
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
    FDataViewList   : IList<IDataView>;
    FPageControl    : TPageControl;

    {$REGION 'property access methods'}
    function GetManager: IConnectionViewManager;
    function GetForm: TForm;
    function GetActiveData: IData;
    function GetActiveDataView: IDataView;
    function GetEditorView: IEditorView;
    function GetActiveConnectionProfile: TConnectionProfile;
    {$ENDREGION}

    procedure DataAfterExecute(Sender: TObject);

  protected
    procedure InitializeEditorView;
    procedure InitializeConnectionProfilesView;
    procedure Copy;
    procedure UpdateActions; override;
    procedure ApplySettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner      : TComponent;
      AEditorView : IEditorView;
      AData       : IData
    ); reintroduce; virtual;

    property Form: TForm
      read GetForm;

    property ActiveData: IData
      read GetActiveData;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;

    property EditorView: IEditorView
      read GetEditorView;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property Manager: IConnectionViewManager
      read GetManager;
  end;

implementation

uses
  System.UITypes,
  Data.DB,

  Spring, Spring.Container,

  DDuce.Logger, DDuce.Factories, DDuce.ObjectInspector.zObjectInspector,

  DataGrabber.Utils;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmConnectionView.Create(AOwner: TComponent;
  AEditorView: IEditorView; AData: IData);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AEditorView, 'AEditorView');
  Guard.CheckNotNull(AData, 'AData');
  FEditorView     := AEditorView;
  FActiveData     := AData;
end;

procedure TfrmConnectionView.AfterConstruction;
begin
  inherited AfterConstruction;
  ActiveData.OnAfterExecute.Add(DataAfterExecute);
  FDataViewList := TCollections.CreateInterfaceList<IDataView>;
  InitializeConnectionProfilesView;
  InitializeEditorView;
  pnlTop.PanelCollection[0].Position := 0.2;

  ApplySettings;
//  pnlTop.Align   := alClient;
//  pnlBottom.Align := alBottom;
//    pnlBottom.Height     := 0;
//  splHorizontal.Visible := False;
end;

procedure TfrmConnectionView.BeforeDestruction;
begin
  if Assigned(ActiveData) then
    ActiveData.OnAfterExecute.Remove(DataAfterExecute);
  FEditorView     := nil;
  FActiveDataView := nil;
  FActiveData     := nil;
  FDataViewList   := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'Data'}
procedure TfrmConnectionView.DataAfterExecute(Sender: TObject);
var
  DS : TDataSet;
  I  : Integer;
  TS : TTabSheet;
  DV : IDataView;
begin
  if Assigned(FPageControl) then
    FreeAndNil(FPageControl);

//  FPageControl := TPageControl.Create(Self);
//  FPageControl.Parent := pnlGrid;
//  FPageControl.Align := alClient;

  for I := 0 to ActiveData.DataSetCount - 1 do
  begin
    //TS := TTabSheet.Create(FPageControl);

//    TS.PageControl := FPageControl;


    DV := GlobalContainer.Resolve<IDataView>(Manager.Settings.GridType);
    DV.Settings := Manager.Settings as IDataViewSettings;
  //  DV.AssignParent(TS);
    DV.AssignParent(pnlBottom);
    DV.DataSet := ActiveData.Items[I];
  end;
//  pnlTop.Align := alTop;
//  pnlBottom.Align := alClient;
//  pnlBottom.Height     := Height div 2;
//  splHorizontal.Visible := True;
//  splHorizontal.Align := alBottom;








//  create dataviews here
//  FActiveDataView := ADataView;
//  ActiveDataView.AssignParent(pnlBottom);
  Logger.Track('TfrmConnectionView.DataAfterExecute');
end;
{$ENDREGION}

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

procedure TfrmConnectionView.FVSTProfilesDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
//var
//  S : string;
//  R : TRect;
begin
//  R := CellRect;
//  S := Text;
//  TargetCanvas.TextRect(R, S, [tfCenter, tfVerticalCenter]);
//  DefaultDraw := False;
  DefaultDraw := True;
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
  FVSTProfiles := TFactories.CreateVirtualStringTree(Self, pnlTop);
  FVSTProfiles.AlignWithMargins  := False;
  FVSTProfiles.RootNodeCount     := Manager.Settings.ConnectionProfiles.Count;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnPaintText       := FVSTProfilesPaintText;
  FVSTProfiles.OnDrawText        := FVSTProfilesDrawText;
  FVSTProfiles.Header.Options    := FVSTProfiles.Header.Options - [hoVisible];
  FVSTProfiles.TreeOptions.PaintOptions :=
    FVSTProfiles.TreeOptions.PaintOptions - [toHideSelection];
  FVSTProfiles.Colors.FocusedSelectionColor := clBtnHighlight;
  FVSTProfiles.Margins.Right := 0;
  FVSTProfiles.Indent        := 0;
  FVSTProfiles.Constraints.MinWidth := 150;
  FVSTProfiles.Constraints.MaxWidth := 300;
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
    FActiveData.ConnectionSettings.Assign(CP.ConnectionSettings);
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

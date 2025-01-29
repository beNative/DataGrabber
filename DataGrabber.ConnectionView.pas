{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DataGrabber.inc}

unit DataGrabber.ConnectionView;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ActnList, Vcl.ExtCtrls,

  Spring.Collections,

  VirtualTrees, VirtualTrees.BaseTree,

  OMultiPanel,

  DataGrabber.Interfaces, DataGrabber.ConnectionProfiles;

{$REGION 'documentation'}
{
   A IConnectionView instance consists of
     - one editor view (IEditorView)
     - one or more dataviews corresponding to the user input in the editor as
       multiple datasets can be returned as a result of one statement
       (IDataView).

     - a list of connection profiles (those defined in the settings)

     - an active connection profile (of the available profiles in
       FSettings.ConnectionProfiles)

     - an IData instance created with the connection settings associated with
       the active connection profile.
}
{$ENDREGION}

type
  TfrmConnectionView = class(TForm, IConnectionView)
    pnlMain     : TOMultiPanel;
    pnlTop      : TPanel;
    pnlVST      : TPanel;
    splVertical : TSplitter;
    pnlTopRight : TPanel;
    pnlBottom   : TPanel;

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

    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure splVerticalMoved(Sender: TObject);

  private
    FEditorView              : IEditorView;
    FActiveDataView          : IDataView;
    FData                    : IData;
    FVSTProfiles             : TVirtualStringTree;
    FDataViewList            : IList<IDataView>;
    FPageControl             : TPageControl;
    FMultiPanel              : TOMultiPanel;
    FManager                 : IConnectionViewManager;
    FActiveConnectionProfile : TConnectionProfile;

    {$REGION 'property access methods'}
    function GetManager: IConnectionViewManager;
    function GetForm: TForm;
    function GetData: IData;
    function GetActiveDataView: IDataView;
    function GetEditorView: IEditorView;
    function GetActiveConnectionProfile: TConnectionProfile;
    function GetDataView(AIndex: Integer): IDataView;
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure DataAfterExecute(Sender: TObject);
    procedure DataBeforeExecute(Sender: TObject);
    procedure FPageControlChange(Sender: TObject);
    {$ENDREGION}

  protected
    procedure InitializeConnectionProfilesView;
    procedure Copy;
    procedure UpdateActions; override;
    procedure ApplySettings;
    procedure UpdateActiveDataView;

    function ExportAsWiki: string;

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    constructor Create(
      AOwner             : TComponent;
      AManager           : IConnectionViewManager;
      AConnectionProfile : TConnectionProfile
    ); reintroduce; virtual;

    property Form: TForm
      read GetForm;

    property Data: IData
      read GetData;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;

    property EditorView: IEditorView
      read GetEditorView;

    property ActiveDataView: IDataView
      read GetActiveDataView;

    property DataViews[AIndex: Integer]: IDataView
      read GetDataView;

    property Manager: IConnectionViewManager
      read GetManager;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes,
  Vcl.GraphUtil,
  Data.DB,

  VirtualTrees.Types, VirtualTrees.Header,

  Spring, Spring.Container,

  DDuce.ObjectInspector.zObjectInspector,
  DDuce.Logger, DDuce.Factories.VirtualTrees, DDuce.Utils,

  DataGrabber.Utils, DataGrabber.Factories, DataGrabber.Resources;

{$REGION 'construction and destruction'}
constructor TfrmConnectionView.Create(AOwner: TComponent;
  AManager: IConnectionViewManager; AConnectionProfile: TConnectionProfile);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckNotNull(AConnectionProfile, 'AConnectionProfile');
  FActiveConnectionProfile := AConnectionProfile;
  FManager := AManager;
end;

procedure TfrmConnectionView.AfterConstruction;
begin
  inherited AfterConstruction;
  FManager.ActiveConnectionView := Self;
  FData := TDataGrabberFactories.CreateData(
    Self,
    FActiveConnectionProfile.ConnectionSettings
  );
  Data.OnBeforeExecute.Add(DataBeforeExecute);
  Data.OnAfterExecute.Add(DataAfterExecute);
  FDataViewList := TCollections.CreateInterfaceList<IDataView>;
  FEditorView := TDataGrabberFactories.CreateEditorView(Self, Manager);
  FEditorView.AssignParent(pnlTopRight);
  pnlMain.MinPosition                 := 0;
  pnlMain.PanelCollection[0].Position := 1;
  pnlMain.SplitterSize                := 4;
  InitializeConnectionProfilesView;
  ApplySettings;
end;

destructor TfrmConnectionView.Destroy;
begin
  if Assigned(Data) then
  begin
    Data.OnAfterExecute.Remove(DataAfterExecute);
    Data.OnBeforeExecute.Remove(DataBeforeExecute);
  end;
  FEditorView     := nil;
  FActiveDataView := nil;
  FData           := nil;
  FDataViewList   := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$REGION 'Data'}
procedure TfrmConnectionView.DataAfterExecute(Sender: TObject);
var
  I  : Integer;
  TS : TTabSheet;
  DV : IDataView;
  B  : Boolean;
begin
  Logger.Track(Self, 'DataAfterExecute');
  if Assigned(FMultiPanel) then
  begin
    FMultiPanel.PanelCollection.Clear;
    FreeAndNil(FMultiPanel);
  end;
  FActiveDataView := nil;
  FDataViewList.Clear;
  if Assigned(FPageControl) then
  begin
    FreeAndNil(FPageControl);
  end;
  B := (Manager.Settings.ResultDisplayLayout = TResultDisplayLayout.Tabbed)
    and (not Data.DataEditMode);
  if B then
  begin
    if not Assigned(FPageControl) then
      FPageControl := TPageControl.Create(Self);
    FPageControl.Parent      := pnlBottom;
    FPageControl.Align       := alClient;
    FPageControl.TabPosition := tpBottom;
    FPageControl.OnChange    := FPageControlChange;
  end
  else
  begin
    FMultiPanel := TOMultiPanel.Create(Self);
    FMultiPanel.SplitterSize := 4;
    FMultiPanel.Parent       := pnlBottom;
    FMultiPanel.Align        := alClient;
    if Manager.Settings.ResultDisplayLayout = TResultDisplayLayout.Horizontal then
    begin
      FMultiPanel.PanelType := ptHorizontal;
    end
    else
    begin
      FMultiPanel.PanelType := ptVertical;
    end;
  end;

  if Data.MultipleResultSets and (Data.DataSetCount > 1) then
  begin
    for I := 0 to Data.DataSetCount - 1 do
    begin
      DV := TDataGrabberFactories.CreateDataView(
        Self,
        Manager,
        Data.Items[I]
      );
//      DV.PopupMenu := Manager.ConnectionViewPopupMenu;
      if B then
      begin
        TS := TTabSheet.Create(FPageControl);
        TS.Caption := Format(SResultSet, [I]);
        TS.PageControl := FPageControl;
        DV.AssignParent(TS);
      end
      else
      begin
        DV.AssignParent(FMultiPanel);
      end;
      FDataViewList.Add(DV);
    end;
    FActiveDataView := FDataViewList.FirstOrDefault;
  end
  else
  begin
    DV := TDataGrabberFactories.CreateDataView(
      Self,
      Manager,
      Data.ResultSet
    );
//    DV.PopupMenu := Manager.ConnectionViewPopupMenu;
    DV.AssignParent(pnlBottom);
    FActiveDataView := DV;
  end;
  pnlMain.SplitterSize := 4;
  if Data.RecordCount > 20 then
    pnlMain.PanelCollection[0].Position := 0.2
  else
  begin
    pnlMain.PanelCollection[0].Position := 0.6
  end;
end;

procedure TfrmConnectionView.DataBeforeExecute(Sender: TObject);
begin
  FActiveDataView := nil;
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

procedure TfrmConnectionView.FVSTProfilesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Logger.Track(Self, 'FVSTProfilesFocusChanged');
  if ContainsFocus(Self) then
  begin
    FActiveConnectionProfile := Manager.Settings.ConnectionProfiles[FVSTProfiles.FocusedNode.Index];
    ApplySettings;
  end;
end;

procedure TfrmConnectionView.FVSTProfilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  CellText := Manager.Settings.ConnectionProfiles[Node.Index].DisplayName;
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
  EditorView.SetFocus;
end;

procedure TfrmConnectionView.FPageControlChange(Sender: TObject);
begin
  FActiveDataView := FDataViewList[FPageControl.ActivePageIndex];
end;

procedure TfrmConnectionView.splVerticalMoved(Sender: TObject);
begin
  FVSTProfiles.Invalidate;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmConnectionView.GetActiveConnectionProfile: TConnectionProfile;
begin
  Result := FActiveConnectionProfile;
end;

function TfrmConnectionView.GetData: IData;
begin
  Result := FData;
end;

function TfrmConnectionView.GetDataView(AIndex: Integer): IDataView;
begin
  Guard.CheckIndex(FDataViewList.Count, AIndex);
  Result := FDataViewList[AIndex];
end;

function TfrmConnectionView.GetActiveDataView: IDataView;
begin
  Result := FActiveDataView;
end;

function TfrmConnectionView.GetManager: IConnectionViewManager;
begin
  Result := FManager;
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
function TfrmConnectionView.ExportAsWiki: string;
var
  DV : IDataView;
  S  : string;
begin
  Logger.Track(Self, 'ExportAsWiki');
  if not FDataViewList.IsEmpty then
  begin
    for DV in FDataViewList do
    begin
      S := S + DV.ResultsToWikiTable(True);
      S := S + #13#10;
    end;
  end;
  Result := S;
end;

procedure TfrmConnectionView.InitializeConnectionProfilesView;
begin
  Logger.Track(Self, 'InitializeConnectionProfilesView');
  FVSTProfiles := TVirtualStringTreeFactory.CreateGrid(Self, pnlVST);
  FVSTProfiles.AlignWithMargins  := False;
  FVSTProfiles.RootNodeCount     := Manager.Settings.ConnectionProfiles.Count;
  FVSTProfiles.OnBeforeCellPaint := FVSTProfilesBeforeCellPaint;
  FVSTProfiles.OnGetText         := FVSTProfilesGetText;
  FVSTProfiles.OnFocusChanged    := FVSTProfilesFocusChanged;
  FVSTProfiles.OnPaintText       := FVSTProfilesPaintText;
  FVSTProfiles.Header.Options    := FVSTProfiles.Header.Options - [hoVisible];
  FVSTProfiles.TreeOptions.PaintOptions := FVSTProfiles.TreeOptions.PaintOptions
    - [toHideSelection, toHotTrack, toShowTreeLines, toShowHorzGridLines];
  FVSTProfiles.Colors.FocusedSelectionColor := clBtnHighlight;
  FVSTProfiles.Margins.Right := 0;
  FVSTProfiles.Indent        := 0;
  FVSTProfiles.Alignment     := taCenter;
  FVSTProfiles.BorderStyle   := bsNone;
  FVSTProfiles.Constraints.MinWidth  := 150;
  FVSTProfiles.Constraints.MinHeight := 100;
  FVSTProfiles.Constraints.MaxWidth  := 300;
  SelectNode(FVSTProfiles, FActiveConnectionProfile.Index);
end;

procedure TfrmConnectionView.ApplySettings;
begin
  Logger.Track(Self, 'ApplySettings');
  FVSTProfiles.RootNodeCount := Manager.Settings.ConnectionProfiles.Count;
  FVSTProfiles.Refresh;
  if FVSTProfiles.RootNodeCount > 0 then
  begin
    SelectNode(FVSTProfiles, FActiveConnectionProfile.Index);
    Application.Title := FActiveConnectionProfile.Name;
    Caption           := FActiveConnectionProfile.Name;
    FEditorView.Color :=
      ColorAdjustLuma(FActiveConnectionProfile.ProfileColor, 25, True);
    Data.ConnectionSettings.Assign(FActiveConnectionProfile.ConnectionSettings);
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
    if Assigned(FActiveDataView) and not FActiveDataView.IsActiveDataView then
      UpdateActiveDataView;
  end;
end;

procedure TfrmConnectionView.UpdateActiveDataView;
var
  DV : IDataView;
begin
  for DV in FDataViewList do
  begin
    if DV.IsActiveDataView then
      FActiveDataView := DV;
  end;
end;
{$ENDREGION}

end.

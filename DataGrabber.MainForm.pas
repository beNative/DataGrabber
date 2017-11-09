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

unit DataGrabber.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.GDIPOBJ,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.Threading, System.ImageList,
  Vcl.Menus, Vcl.ActnList, Vcl.Controls, Vcl.Forms, Vcl.ToolWin, Vcl.ExtCtrls,
  Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ImgList,
  Data.DB, Data.Win.ADODB,

  VirtualTrees,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,

  ts.Interfaces, ts.Classes.ConnectionSettings, ts.Data,

  DataGrabber.Interfaces, DataGrabber.Settings, DataGrabber.ConnectionProfiles;

{
  TODO:
    - autosize form (to data)
    - testing !!!
    - query tree (using gaSQLParser)
    - multiple statements => multiple resultsets
    - log executed statements to a local database
    - Use bindings in settings
    - Select <selected fields?> Where in
    - Presenter for vertical grid
    - Multiselect in vertical grid? => selection delimited quoted text fieldnames
    - Datainspector -> group by table?
    - Store executed sql’s
    - Multiple sessions
    - Smart grouping (detect common field prefixes/suffixes (vb. Date, ...Id,)
    - Working set of tables (cache meta info and links and make them
      customizable as a profile setting)
    - Statement builder
    - Smart joining
    - Profile color for tab backgrounds
}

type
  TModuleClass = class of TdmCustomModule;

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain                       : TActionList;
    actAddConnectionView          : TAction;
    actInspectChromeTab           : TAction;
    ctMain                        : TChromeTabs;
    imlSpinner                    : TImageList;
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
    tlbMain                       : TToolBar;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddConnectionViewExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure tlbMainCustomDraw(
      Sender          : TToolBar;
      const ARect     : TRect;
      var DefaultDraw : Boolean
    );
    procedure FormShortCut(
      var Msg     : TWMKey;
      var Handled : Boolean
    );
    procedure ctMainActiveTabChanged(
      Sender : TObject;
      ATab   : TChromeTab
    );
    procedure ctMainButtonAddClick(
      Sender      : TObject;
      var Handled : Boolean
    );
    procedure ctMainButtonCloseTabClick(
      Sender    : TObject;
      ATab      : TChromeTab;
      var Close : Boolean
    );
    procedure ctMainNeedDragImageControl(
      Sender          : TObject;
      ATab            : TChromeTab;
      var DragControl : TWinControl
    );
    procedure ctMainBeforeDrawItem(
      Sender       : TObject;
      TargetCanvas : TGPGraphics;
      ItemRect     : TRect;
      ItemType     : TChromeTabItemType;
      TabIndex     : Integer;
      var Handled  : Boolean
    );
    procedure actInspectChromeTabExecute(Sender : TObject);
    {$ENDREGION}

  private
    FManager     : IConnectionViewManager;
    FSettings    : IDGSettings;
    FConnections : IFuture<Boolean>;
    function GetActiveConnectionView: IConnectionView;
    function GetActiveConnectionProfile: TConnectionProfile;

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

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddConnectionView: IConnectionView;

    property Manager: IConnectionViewManager
      read GetManager;

    property ActiveConnectionView: IConnectionView
      read GetActiveConnectionView;

    property ActiveConnectionProfile: TConnectionProfile
      read GetActiveConnectionProfile;

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
  Winapi.ActiveX,
  Vcl.Clipbrd,

  Spring.Container,

  ts.Utils,

  DDuce.ObjectInspector, DDuce.Logger, DDuce.Logger.Factories,

  DataGrabber.Resources, DataGrabber.Factories;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  Logger.Channels.Add(TLoggerFactories.CreateWinIPCChannel);
  Logger.Clear;
  FManager  := GlobalContainer.Resolve<IConnectionViewManager>;
  FSettings := GlobalContainer.Resolve<IDGSettings>;
  AddConnectionView;
  tlbMain.Images := FManager.ActionList.Images;
  InitializeActions;
  pnlStatus.Caption := SReady;
  SetWindowSizeGrip(pnlStatusBar.Handle, True);

  FManager.ActiveConnectionView.EditorView.Text := EXAMPLE_QUERY;

  // prepare registered connections in seperate thread. This reduces load time
  // when the settings dialog is first invoked.
  FConnections := TTask.Future<Boolean>(function: Boolean
    var
      C : IConnection;
    begin
      Result := True;
      CoInitialize(nil);
      try
        for C in GlobalContainer.ResolveAll<IConnection> do
          Result := Result and Assigned(C);
      finally
        CoUninitialize;
      end;
    end
  );
end;

procedure TfrmMain.BeforeDestruction;
begin
  FManager  := nil;
  FSettings := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actAddConnectionViewExecute(Sender: TObject);
begin
  AddConnectionView;
end;

procedure TfrmMain.actInspectChromeTabExecute(Sender: TObject);
begin
  if Assigned(ctMain.ActiveTab) then
  begin
    InspectComponent(ctMain);
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMain.tlbMainCustomDraw(Sender: TToolBar; const ARect: TRect;
  var DefaultDraw: Boolean);
begin
  Sender.Canvas.FillRect(ARect);
end;

{ Needed to route shortcuts to the actionlist in the data module. }

procedure TfrmMain.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := Manager.ActionList.IsShortCut(Msg);
end;

{$REGION 'ctMain'}
procedure TfrmMain.ctMainActiveTabChanged(Sender: TObject; ATab: TChromeTab);
var
  CV : IConnectionView;
begin
  if Assigned(ATab.Data) then
  begin
    CV := IConnectionView(ATab.Data);
    CV.Form.Show;
    CV.Form.SetFocus;
    Manager.ActiveConnectionView := CV;
    ATab.Caption := Format('%s-%s', [
      CV.Form.Caption,
      CV.ActiveConnectionProfile.Name
    ]);
  end;
end;

procedure TfrmMain.ctMainBeforeDrawItem(Sender: TObject;
  TargetCanvas: TGPGraphics; ItemRect: TRect; ItemType: TChromeTabItemType;
  TabIndex: Integer; var Handled: Boolean);
var
  CV : IConnectionView;
begin

  if (ItemType = itTab)  then
  begin
    CV := IConnectionView(ctMain.Tabs[TabIndex].Data);
    if Assigned(CV) then
    begin

    //ctMain.LookAndFeel.Tabs.NotActive.Style.StartColor := CV.ActiveConnectionProfile.ProfileColor;
    //ctMain.LookAndFeel.Tabs.NotActive.Style.StopColor  := CV.ActiveConnectionProfile.ProfileColor;

//  if FBrush = nil then
//    FBrush := TGPLinearGradientBrush.Create(MakePoint(0, ClientRect.Top),
//                                            MakePoint(0, ClientRect.Bottom),
//                                            MakeGDIPColor(StartColor, StartAlpha),
//                                            MakeGDIPColor(StopColor, StopAlpha));

        end;

    //TargetCanvas.Brush.Color := CV.ActiveConnectionProfile.Color;
  end;
end;

procedure TfrmMain.ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
begin
  AddConnectionView;
  Handled := True;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
begin
//
end;

procedure TfrmMain.ctMainNeedDragImageControl(Sender: TObject; ATab: TChromeTab;
  var DragControl: TWinControl);
begin
  DragControl := pnlConnectionViews;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMain.GetActiveConnectionProfile: TConnectionProfile;
begin
  if Assigned(ActiveConnectionView) then
    Result := ActiveConnectionView.ActiveConnectionProfile
  else
    Result := nil;
end;

function TfrmMain.GetActiveConnectionView: IConnectionView;
begin
  Result := Manager.ActiveConnectionView;
end;

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
  CV   : IConnectionView;
  LTab : TChromeTab;
begin
  LockPaint(Self);
  try
    CV := Manager.AddConnectionView;
    CV.Form.Parent := pnlConnectionViews;
    CV.Form.BorderStyle := bsNone;
    CV.Form.Align := alClient;
    CV.Form.Visible := True;
    LTab := ctMain.Tabs.Add;
    LTab.Data := Pointer(CV);
  finally
    UnlockPaint(Self);
  end;
end;

procedure TfrmMain.InitializeActions;
begin
  AddToolbarButtons(tlbMain, Manager);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.ShowToolWindow(AForm: TForm);
begin
  if Assigned(AForm) then
  begin
    LockPaint(AForm);
    try
      AForm.BorderStyle := bsSizeToolWin;
      AForm.Visible := True;
      ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
      SetWindowPos(
        AForm.Handle,
        GetNextWindow(Self.Handle, GW_HWNDNEXT),
        0,
        0,
        0,
        0,
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
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

procedure TfrmMain.UpdateActions;
var
  V: IConnectionView;
begin
  inherited UpdateActions;
  UpdateStatusBar;
  if Assigned(Manager.ActiveConnectionView) then
  begin
    V := Manager.ActiveConnectionView;
    if Assigned(ctMain.ActiveTab) then
    begin
      ctMain.ActiveTab.Caption := Format('%s', [
        V.Form.Caption
      ]);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMain.UpdateStatusBar;
var
  S: string;
begin
  if Assigned(Data) and Assigned(Data.DataSet) and Data.DataSet.Active then
  begin
    pnlRecordCount.Caption := Format(SRecordCount, [Data.RecordCount]);
    pnlFieldCount.Caption  := Format(SFieldCount, [Data.DataSet.FieldCount]);
    pnlConstantFieldsCount.Caption :=
      Format(SConstantFieldCount, [(Data as IFieldLists).ConstantFields.Count]);
    pnlEmptyFieldsCount.Caption :=
      Format(SEmptyFieldCount, [(Data as IFieldLists).EmptyFields.Count]);
    if Data.CanModify then
      S := SUpdateable
    else
      S := SReadOnly;
  end
  else
  begin
    S                              := '';
    pnlRecordCount.Caption         := '';
    pnlFieldCount.Caption          := '';
    pnlElapsedTime.Caption         := '';
    pnlConstantFieldsCount.Caption := '';
    pnlEmptyFieldsCount.Caption    := '';
    if Assigned(ActiveConnectionProfile) then
    begin
      if ActiveConnectionProfile.ProviderMode then
        pnlProviderMode.Caption := SProviderMode
      else
        pnlProviderMode.Caption := SNativeMode;
      pnlConnectionType.Caption := ActiveConnectionProfile.ConnectionType;
      pnlGridType.Caption := Settings.GridType;
    end;
  end;
  if Assigned(Data) and Assigned(Data.Connection) and Data.Connection.Connected then
    pnlConnectionStatus.Caption := SConnected
  else
    pnlConnectionStatus.Caption := SDisconnected;

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

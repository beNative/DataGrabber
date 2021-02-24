{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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
  System.ImageList,
  Vcl.Menus, Vcl.ActnList, Vcl.Controls, Vcl.Forms, Vcl.ToolWin, Vcl.ExtCtrls,
  Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ImgList,

  VirtualTrees,

  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,

  DataGrabber.ConnectionSettings, DataGrabber.Interfaces, DataGrabber.Settings,
  DataGrabber.ConnectionProfiles;

{ Main application form.

  TODO:
    - log executed statements to a local database
    - Use bindings in settings
    - Select <selected fields?> Where in
    - Smart grouping (detect common field prefixes/suffixes (eg. Date, ...Id,)
    - Working set of tables (cache meta info and links and make them
      customizable as a profile setting)
    - Statement builder, smart joining
    - Profile color for tab backgrounds
}

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain                : TActionList;
    actAddConnectionView   : TAction;
    actCloseAllOtherTabs   : TAction;
    actCloseTab            : TAction;
    actInspectChromeTab    : TAction;
    ctMain                 : TChromeTabs;
    imlSpinner             : TImageList;
    mniCloseAllOtherTabs   : TMenuItem;
    mniCloseTab            : TMenuItem;
    pnlConnectionStatus    : TPanel;
    pnlConnectionViews     : TPanel;
    pnlConstantFieldsCount : TPanel;
    pnlEditMode            : TPanel;
    pnlElapsedTime         : TPanel;
    pnlEmptyFieldsCount    : TPanel;
    pnlFieldCount          : TPanel;
    pnlHiddenFieldsCount   : TPanel;
    pnlRecordCount         : TPanel;
    pnlStatusBar           : TPanel;
    pnlTop                 : TPanel;
    ppmCVTabs              : TPopupMenu;
    tlbMain                : TToolBar;
    tlbTopRight            : TToolBar;
    shpLine                : TShape;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddConnectionViewExecute(Sender: TObject);
    procedure actInspectChromeTabExecute(Sender : TObject);
    procedure actCloseAllOtherTabsExecute(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
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
    {$ENDREGION}

  private
    FManager  : IConnectionViewManager;
    FSettings : ISettings;

    procedure SettingsChanged(Sender: TObject);
    procedure UpdateTabs;

  protected
    function GetActiveConnectionView: IConnectionView;
    function GetActiveConnectionProfile: TConnectionProfile;
    function GetData: IData;
    function GetSettings: ISettings;
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

    property Settings: ISettings
      read GetSettings;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.UITypes,
  Vcl.Clipbrd,
  Data.DB,

  Spring.Utils,

  DDuce.ObjectInspector.zObjectInspector, DDuce.Utils, DDuce.Logger,

  DataGrabber.Utils, DataGrabber.Resources, DataGrabber.Factories;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
var
  FVI : TFileVersionInfo;
begin
  inherited AfterConstruction;
  FVI := TFileVersionInfo.GetVersionInfo(Application.ExeName);
  Caption := Format('%s %s', [FVI.ProductName, FVI.ProductVersion]);
  FSettings := TDataGrabberFactories.CreateSettings(Self);
  FSettings.OnChanged.Add(SettingsChanged);
  FManager  := TDataGrabberFactories.CreateManager(Self, FSettings);
  FSettings.FormSettings.AssignTo(Self);
  Logger.Enabled := FSettings.EmitLogMessages;
  AddConnectionView;
  tlbMain.Images     := FManager.ActionList.Images;
  tlbTopRight.Images := FManager.ActionList.Images;
  InitializeActions;
end;

procedure TfrmMain.BeforeDestruction;
begin
  FSettings.FormSettings.Assign(Self);
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

procedure TfrmMain.actCloseAllOtherTabsExecute(Sender: TObject);
var
  I         : Integer;
  LActiveCV : IConnectionView;
  CV        : IConnectionView;
begin
  I  := 0;
  LActiveCV := IConnectionView(ctMain.ActiveTab.Data);
  while (Manager.Count > 1) and (I < Manager.Count) do
  begin
    CV := IConnectionView(ctMain.Tabs[I].Data);
    if CV <> LActiveCV then
    begin
      Manager.DeleteConnectionView(CV);
      ctMain.Tabs.DeleteTab(ctMain.Tabs[I].Index, True);
    end
    else
    begin
      Inc(I);
    end;
  end;
end;

procedure TfrmMain.actCloseTabExecute(Sender: TObject);
begin
  if Manager.Count > 1 then
  begin
    Manager.DeleteConnectionView(IConnectionView(ctMain.ActiveTab.Data));
    ctMain.Tabs.DeleteTab(ctMain.ActiveTabIndex, True);
  end;
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

procedure TfrmMain.ctMainButtonAddClick(Sender: TObject; var Handled: Boolean);
begin
  AddConnectionView;
  Handled := True;
end;

procedure TfrmMain.ctMainButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
  var Close: Boolean);
begin
  if Manager.Count > 1 then
  begin
    Close := Manager.DeleteConnectionView(IConnectionView(ATab.Data));
  end
  else
    Close := False;
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

function TfrmMain.GetSettings: ISettings;
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
  S : string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
  begin
    APanel.Width := GetTextWidth(APanel.Caption, APanel.Font) + 10;
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
  TDataGrabberFactories.AddMainToolbarButtons(tlbMain, Manager);
  TDataGrabberFactories.AddTopRightToolbarButtons(tlbTopRight, Manager);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmMain.SettingsChanged(Sender: TObject);
begin
  FormStyle      := FSettings.FormSettings.FormStyle;
  WindowState    := FSettings.FormSettings.WindowState;
  Logger.Enabled := FSettings.EmitLogMessages;
end;

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
begin
  inherited UpdateActions;
  UpdateStatusBar;
  if Assigned(Manager.ActiveConnectionView) then
  begin
    UpdateTabs;
    actCloseTab.Enabled          := Manager.Count > 1;
    actCloseAllOtherTabs.Enabled := Manager.Count > 1;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmMain.UpdateStatusBar;
var
  S   : string;
  TFC : Integer;
  CFC : Integer;
  EFC : Integer;
  HFC : Integer;
  RS  : IResultSet;
begin
  if Assigned(Data) and Assigned(ActiveConnectionView.ActiveDataView)
    and ActiveConnectionView.ActiveDataView.ResultSet.DataSet.Active then
  begin
    RS := ActiveConnectionView.ActiveDataView.ResultSet;
    pnlRecordCount.Caption := Format(SRecordCount, [RS.DataSet.RecordCount]);
    TFC := RS.DataSet.FieldCount;
    CFC := RS.ConstantFields.Count;
    EFC := RS.EmptyFields.Count;
    HFC := RS.HiddenFields.Count;
    pnlFieldCount.Caption          := Format(SFieldCount, [TFC]);
    pnlConstantFieldsCount.Caption := Format(SConstantFieldCount, [CFC]);
    pnlEmptyFieldsCount.Caption    := Format(SEmptyFieldCount, [EFC]);
    pnlHiddenFieldsCount.Caption   := Format(SHiddenFieldCount, [HFC]);
    if HFC > 0 then
      pnlHiddenFieldsCount.Font.Style :=
        pnlHiddenFieldsCount.Font.Style + [fsBold]
    else
      pnlHiddenFieldsCount.Font.Style :=
        pnlHiddenFieldsCount.Font.Style - [fsBold];
    pnlElapsedTime.Caption         := Format(
      '%5.0f ms', [Data.ElapsedTime.TotalMilliseconds]
    );
    if Data.CanModify then
      S := SUpdateable
    else
      S := SReadOnly;
  end
  else
  begin
    pnlEditMode.Caption            := '';
    pnlRecordCount.Caption         := '';
    pnlFieldCount.Caption          := '';
    pnlElapsedTime.Caption         := '';
    pnlConstantFieldsCount.Caption := '';
    pnlEmptyFieldsCount.Caption    := '';
    pnlHiddenFieldsCount.Caption   := '';
  end;
  if Assigned(Data) and Assigned(Data.Connection) and Data.Connection.Connected then
  begin
    pnlConnectionStatus.Caption    := SConnected;
    pnlConnectionStatus.Font.Color := clGreen;
  end
  else
  begin
    pnlConnectionStatus.Caption := SDisconnected;
    pnlConnectionStatus.Font.Color := clBlack;
  end;

  if Data.CanModify then
  begin
    pnlEditMode.Font.Style := pnlEditMode.Font.Style + [fsBold];
    pnlEditMode.Font.Color := clRed;
  end
  else
  begin
    pnlEditMode.Font.Style := pnlEditMode.Font.Style - [fsBold];
    pnlEditMode.Font.Color := clGreen;
  end;
  pnlEditMode.Caption := S;
  OptimizeWidth(pnlConnectionStatus);
  OptimizeWidth(pnlEditMode);
  OptimizeWidth(pnlRecordCount);
  OptimizeWidth(pnlElapsedTime);
  OptimizeWidth(pnlFieldCount);
  OptimizeWidth(pnlEmptyFieldsCount);
  OptimizeWidth(pnlConstantFieldsCount);
  OptimizeWidth(pnlHiddenFieldsCount);
  FSettings.FormSettings.WindowState := WindowState;
end;

procedure TfrmMain.UpdateTabs;
var
  V : IConnectionView;
  C : Integer;
begin
  V := Manager.ActiveConnectionView;
  if Assigned(ctMain.ActiveTab) and Assigned(V) then
  begin
    ctMain.ActiveTab.Caption := Format('%s', [V.Form.Caption]);
    C := V.ActiveConnectionProfile.ProfileColor;
    ctMain.LookAndFeel.Tabs.Active.Style.StartColor := C;
    ctMain.LookAndFeel.Tabs.Active.Style.StopColor  := C;
  end;
end;
{$ENDREGION}

end.

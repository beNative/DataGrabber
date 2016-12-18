{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

  DDuce.Components.PropertyInspector,

  VirtualTrees,

  ts.Interfaces, ts.Classes.ConnectionSettings, ts.Data,

  ts.Modules.DataInspector, ts.Modules.FieldInspector,

  DataGrabber.Data, DataGrabber.EditorView, DataGrabber.Interfaces,
  DataGrabber.Settings;

{
  TODO:
    - replace dx docking with TChromeTab
    - registration mechanism for dataviews + use Spring.Services
    - autosize form (to data)
    - testing !!!
    - query tree (using gaSQLParser)
    - multiple statements => multiple resultsets
    - log executed statements to a local database
    - Use bindings in settings
    -  Select <selected fields?> Where in
    -  Presenter for vertical grid
    -  Multiselect in vertical grid? => selection delimited quoted text fieldnames
    -  Datainspector -> group by table?
    -  Store executed sql’s
    -  Multiple sessions
    -  Smart grouping (detect common field prefixes/suffixes (vb. Date, ...Id,)
    -  Working set of tables (cache meta info and links and make them
       customizable as a profile setting)
    -  Statement builder
}

type
  TModuleClass = class of TdmCustomModule;

type
  TfrmMain = class(TForm)
    {$REGION 'designer controls'}
    aclMain                       : TActionList;
    actAddConnectionView          : TAction;
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
    mniFireDAC                    : TMenuItem;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actAddConnectionViewExecute(Sender: TObject);
    {$ENDREGION}

    {$REGION 'event handlers'}
    procedure tlbMainCustomDraw(Sender: TToolBar; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    {$ENDREGION}

  private
    FManager  : IConnectionViewManager;
    FSettings : IDGSettings;

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

  Spring.Container,

  ts.Utils,

  DDuce.ObjectInspector,

  DataGrabber.Utils, DataGrabber.Resources, DataGrabber.SettingsDialog,
  DataGrabber.ModelData,

  DataGrabber.ConnectionProfiles, DataGrabber.Factories,
  DataGrabber.RegisterServices;

{$REGION 'construction and destruction'}
procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  FManager  := GlobalContainer.Resolve<IConnectionViewManager>;
  FSettings := GlobalContainer.Resolve<IDGSettings>;
  AddConnectionView;
  tlbMain.DrawingStyle := dsNormal;
  tlbMain.Images       := FManager.ActionList.Images;
  InitializeActions;
  pnlStatus.Caption := SReady;
  SetWindowSizeGrip(pnlStatusBar.Handle, True);
end;

procedure TfrmMain.BeforeDestruction;
begin
  FManager := nil;
  FSettings  := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMain.actAddConnectionViewExecute(Sender: TObject);
begin
  AddConnectionView;
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
  CV  : IConnectionView;
begin
  LockPaint(Self);
  try
    CV := Manager.AddConnectionView;
    CV.Form.Parent := pnlConnectionViews;
    CV.Form.BorderStyle := bsNone;
    CV.Form.Align := alClient;
    CV.Form.Visible := True;
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
    if Data.ProviderMode then
    begin
      pnlProviderMode.Caption := SProviderMode;
    end
    else
    begin
      pnlProviderMode.Caption := SNativeMode;
    end;
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
      pnlProviderMode.Caption := SProviderMode;
    end
    else
    begin
      pnlProviderMode.Caption := SNativeMode;
    end;
  end;

  if Assigned(Settings) then
  begin
    pnlConnectionType.Caption := Settings.ConnectionType;
    pnlGridType.Caption := Settings.GridType;
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

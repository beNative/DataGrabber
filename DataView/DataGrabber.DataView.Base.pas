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

unit DataGrabber.DataView.Base;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Data.DB,

  DataGrabber.Interfaces;

type
  TBaseDataView = class(TForm, IDataView)
    dscMain : TDataSource;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    FManager      : IConnectionViewManager;
    FToolBar      : TToolBar;
    FToolBarPanel : TPanel;
    FResultSet    : IResultSet;

  protected
    {$REGION 'property access methods'}
    function GetName: string; virtual;
    function GetGridType: string; virtual;
    function GetSettings: IDataViewSettings;
    function GetData: IData;
    function GetRecordCount: Integer; virtual;
    function GetPopupMenu: TPopupMenu; reintroduce; virtual;
    procedure SetPopupMenu(const Value: TPopupMenu); virtual;
    function GetDataSet: TDataSet; virtual;
    function GetResultSet: IResultSet;
    {$ENDREGION}

    procedure DataAfterExecute(Sender: TObject); virtual;
    procedure SettingsChanged(Sender: TObject); virtual;

    procedure AssignParent(AParent: TWinControl); virtual;
    procedure UpdateView; virtual;
    procedure HideSelectedColumns; virtual;
    function IsActiveDataView: Boolean; virtual;
    procedure Inspect; virtual;
    procedure AutoSizeColumns; virtual;
    procedure Copy; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function ResultsToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;
    function ResultsToTextTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;

    function SelectionToCommaText(
      AQuoteItems: Boolean = True
    ): string; virtual;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string; virtual;
    function SelectionToTextTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;
    function SelectionToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;
    function SelectionToFields(
      AQuoteItems: Boolean = True
    ): string; virtual;

    procedure ApplyGridSettings; virtual;
    procedure UpdateActions; override;

   public
     constructor Create(
       AOwner     : TComponent;
       AManager   : IConnectionViewManager;
       AResultSet : IResultSet
     ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Name: string
      read GetName;

    property GridType: string
      read GetGridType;

    property DataSet: TDataSet
      read GetDataSet;

    property Data: IData
      read GetData;

    property RecordCount: Integer
      read GetRecordCount;

    property ResultSet: IResultSet
      read GetResultSet;

    property Settings: IDataViewSettings
      read GetSettings;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DDuce.Utils,

  DataGrabber.Utils, DataGrabber.Factories;

{$REGION 'construction and destruction'}
constructor TBaseDataView.Create(AOwner: TComponent; AManager:
  IConnectionViewManager; AResultSet: IResultSet);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckNotNull(AResultSet, 'AResultSet');
  FResultSet := AResultSet;
  FManager   := AManager;
  dscMain.DataSet := AResultSet.DataSet;
end;

procedure TBaseDataView.AfterConstruction;
begin
  inherited AfterConstruction;
  FToolBarPanel             := TPanel.Create(Self);
  FToolBarPanel.Parent      := Self;
  FToolBarPanel.BevelOuter  := bvNone;
  FToolBarPanel.BevelInner  := bvNone;
  FToolBarPanel.BorderStyle := bsNone;
  FToolBarPanel.Color       := clBtnFace;
  FToolBarPanel.Align       := alRight;
  FToolBarPanel.Visible     := False;
  FToolBarPanel.AutoSize    := True;
  FToolbar := TDataGrabberFactories.CreateDataViewToolbar(
    Self,
    FToolBarPanel,
    FManager
  );
  FToolBar.Transparent   := False;
  FToolbar.Align         := alRight;
  FToolbar.Visible       := True;
  FToolbar.Color         := clBtnFace;
  FToolbar.AutoSize      := True;
  Data.OnAfterExecute.Add(DataAfterExecute);
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TBaseDataView.BeforeDestruction;
begin
  Settings.OnChanged.Remove(SettingsChanged);
  Data.OnAfterExecute.Remove(DataAfterExecute);
  FResultSet := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TBaseDataView.GetData: IData;
begin
  if Assigned(FResultSet) then
    Result := FResultSet.Data
  else
    Result := nil;
end;

function TBaseDataView.GetDataSet: TDataSet;
begin
  if Assigned(FResultSet) then
    Result := FResultSet.DataSet
  else
    Result := nil;
end;

function TBaseDataView.GetGridType: string;
begin
  Result := '';
end;

function TBaseDataView.GetName: string;
begin
  Result := inherited Name;
end;

function TBaseDataView.GetPopupMenu: TPopupMenu;
begin
  Result := inherited PopupMenu;
end;

procedure TBaseDataView.SetPopupMenu(const Value: TPopupMenu);
begin
  inherited PopupMenu := Value;
end;

function TBaseDataView.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TBaseDataView.GetResultSet: IResultSet;
begin
  Result := FResultSet;
end;

function TBaseDataView.GetSettings: IDataViewSettings;
begin
  Result := FManager.Settings as IDataViewSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TBaseDataView.DataAfterExecute(Sender: TObject);
begin
  if Assigned(Data) then
    UpdateView;
end;

procedure TBaseDataView.SettingsChanged(Sender: TObject);
begin
  ApplyGridSettings;
end;

procedure TBaseDataView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TBaseDataView.ApplyGridSettings;
begin
//
end;

procedure TBaseDataView.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

procedure TBaseDataView.AutoSizeColumns;
begin
//
end;

procedure TBaseDataView.BeginUpdate;
begin
//
end;

procedure TBaseDataView.EndUpdate;
begin
//
end;

procedure TBaseDataView.Copy;
begin
//
end;

procedure TBaseDataView.HideSelectedColumns;
begin
//
end;

procedure TBaseDataView.Inspect;
begin
//
end;

function TBaseDataView.IsActiveDataView: Boolean;
begin
  Result := ContainsFocus(Self);
end;

function TBaseDataView.ResultsToTextTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.ResultsToWikiTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToCommaText(AQuoteItems: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToFields(AQuoteItems: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToTextTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

procedure TBaseDataView.UpdateActions;
begin
  inherited UpdateActions;
  FToolBarPanel.Visible := IsActiveDataView;
end;

procedure TBaseDataView.UpdateView;
begin
  UpdateActions;
end;
{$ENDREGION}

end.

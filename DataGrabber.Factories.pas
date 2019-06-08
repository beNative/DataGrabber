{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.Factories;

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Menus,
  Data.DB,

  DataGrabber.Interfaces, DataGrabber.ConnectionSettings;

type
  TDataGrabberFactories = class sealed
  private
    class procedure AddActionButton(
      AParent      : TToolBar;
      AAction      : TBasicAction = nil;
      AShowCaption : Boolean = False
    );
    class procedure AddButton(
      AManager          : IConnectionViewManager;
      AToolBar          : TToolBar;
      const AActionName : string = '';
      AShowCaption      : Boolean = False
    );

  public
    class function CreateSettings(
      AOwner : TComponent
    ): ISettings;

    class function CreateManager(
      AOwner    : TComponent;
      ASettings : ISettings = nil
     ): IConnectionViewManager;

    class function CreateData(
      AOwner    : TComponent;
      ASettings : TConnectionSettings
    ): IData;

    class function CreateConnectionView(
      AOwner   : TComponent;
      AManager : IConnectionViewManager;
      AData    : IData
    ): IConnectionView;

    class function CreateEditorView(
      AOwner   : TComponent;
      AManager : IConnectionViewManager
    ): IEditorView;

    class function CreateDataView(
      AOwner     : TComponent;
      AManager   : IConnectionViewManager;
      AResultSet : IResultSet = nil
    ): IDataView;

    class procedure AddMainToolbarButtons(
      AToolBar : TToolbar;
      AManager : IConnectionViewManager
    );

    class procedure AddTopRightToolbarButtons(
      AToolBar : TToolbar;
      AManager : IConnectionViewManager
    );

    class function CreateDataViewToolbar(
      AOwner   : TComponent;
      AParent  : TWinControl;
      AManager : IConnectionViewManager
    ): TToolbar;

  end;

implementation

uses
  Vcl.Forms, Vcl.Graphics,

  Spring,

  DDuce.Factories.ToolBar,

  DataGrabber.ConnectionViewManager, DataGrabber.Settings,
  DataGrabber.EditorView, DataGrabber.Data, DataGrabber.ConnectionView,
  DataGrabber.DataView.cxGrid, DataGrabber.DataView.GridView,
  DataGrabber.DataView.KGrid;

{$REGION 'TDataGrabberFactories'}
class function TDataGrabberFactories.CreateSettings(AOwner: TComponent): ISettings;
begin
  Result := TSettings.Create(AOwner);
end;

class procedure TDataGrabberFactories.AddActionButton(AParent: TToolBar;
  AAction: TBasicAction; AShowCaption: Boolean);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  if not Assigned(AAction) then
    TB.Style := tbsDivider
  else
  begin
    if AShowCaption then
      TB.Style := tbsTextButton
    else
      TB.Style := tbsButton;
    TB.Action := AAction;
  end;
end;

class procedure TDataGrabberFactories.AddButton(AManager: IConnectionViewManager;
  AToolBar: TToolBar; const AActionName: string; AShowCaption: Boolean);
begin
  if AActionName <> '' then
  begin
    Guard.CheckNotNull(AManager.Actions[AActionName], AActionName);
    AddActionButton(AToolBar, AManager.Actions[AActionName], AShowCaption);
  end
  else
  begin
    AddActionButton(AToolBar);
  end;
end;

class procedure TDataGrabberFactories.AddMainToolbarButtons(AToolBar: TToolbar;
  AManager: IConnectionViewManager);
begin
  Guard.CheckNotNull(AToolBar, 'AToolBar');
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckTrue(AToolBar.ButtonCount = 0, '0');

  //AddButton(AManager, AToolBar, 'actFireDACInfo');
  AddButton(AManager, AToolBar, 'actAbout');
  //AddButton(AManager, AToolBar, 'actExecuteTestSequence');
  AddButton(AManager, AToolBar);
  AddButton(AManager, AToolBar, 'actDataInspector');
  AddButton(AManager, AToolBar);
  AddButton(AManager, AToolBar, 'actMergeColumnCells');
  AddButton(AManager, AToolBar, 'actAutoSizeCols');
  AddButton(AManager, AToolBar);
  AddButton(AManager, AToolBar, 'actGroupByBoxVisible', True);
  AddButton(AManager, AToolBar, 'actGroupBySelection', True);
  AddButton(AManager, AToolBar, 'actClearGrouping', True);
  AddButton(AManager, AToolBar, 'actExpandAll', True);
  AddButton(AManager, AToolBar, 'actCollapseAll', True);
  AddButton(AManager, AToolBar);
  AddButton(AManager, AToolBar, 'actShowAllColumns', True);
  AddButton(AManager, AToolBar, 'actHideSelectedColumns', True);
  AddButton(AManager, AToolBar, 'actHideConstantColumns', True);
  AddButton(AManager, AToolBar, 'actHideEmptyColumns', True);
  AddButton(AManager, AToolBar);
  AddButton(AManager, AToolBar, 'actSettings', True);
  AddButton(AManager, AToolBar, 'actExecuteLiveResultSet', True);
  AddButton(AManager, AToolBar, 'actExecute', True);

  TToolBarFactory.CleanupToolBar(AToolBar);
end;

class procedure TDataGrabberFactories.AddTopRightToolbarButtons(
  AToolBar: TToolbar; AManager: IConnectionViewManager);
begin
  Guard.CheckNotNull(AToolBar, 'AToolBar');
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckTrue(AToolBar.ButtonCount = 0, '0');

  AddButton(AManager, AToolBar, 'actToggleFullScreen');
  AddButton(AManager, AToolBar, 'actToggleStayOnTop');

  TToolBarFactory.CleanupToolBar(AToolBar);
end;

class function TDataGrabberFactories.CreateConnectionView(AOwner: TComponent;
  AManager: IConnectionViewManager; AData: IData): IConnectionView;
begin
  Result := TfrmConnectionView.Create(AOwner, AManager, AData);
end;

class function TDataGrabberFactories.CreateData(AOwner: TComponent;
  ASettings: TConnectionSettings): IData;
begin
  Result := TdmData.Create(AOwner, ASettings);
end;

class function TDataGrabberFactories.CreateDataView(AOwner: TComponent;
  AManager: IConnectionViewManager; AResultSet: IResultSet): IDataView;
var
  LGridType : string;
begin
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckNotNull(AResultSet, 'AResultSet');

  LGridType := AManager.Settings.GridType;
  if LGridType = 'cxGrid' then
  begin
    Result := TfrmcxGrid.Create(AOwner, AManager, AResultSet);
  end
  else if LGridType = 'KGrid' then
  begin
    Result := TfrmKGrid.Create(AOwner, AManager, AResultSet);
  end
  else
  begin
    Result := TfrmGridView.Create(AOwner, AManager, AResultSet);
  end;
end;

class function TDataGrabberFactories.CreateDataViewToolbar(AOwner: TComponent;
  AParent: TWinControl; AManager : IConnectionViewManager): TToolbar;
var
  TB : TToolbar;

  function CreateToolButton(
    AParent           : TToolBar;
    const AActionName : string = '';
    APopupMenu        : TPopupMenu = nil
  ): TToolButton;
  begin
    if AActionName = '' then
      Result := TToolBarFactory.CreateToolButton(AParent, nil)
   else
     Result := TToolBarFactory.CreateToolButton(
       AParent, AManager.Actions[AActionName], APopupMenu
     );
  end;

begin
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  TB.Images := AManager.ActionList.Images;
  TB.Transparent := True;
  CreateToolButton(TB, 'actAutoSizeCols');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actShowAllColumns');
  CreateToolButton(TB, 'actHideSelectedColumns');
  CreateToolButton(TB, 'actHideConstantColumns');
  CreateToolButton(TB, 'actHideEmptyColumns');
  CreateToolButton(TB);
  CreateToolButton(TB, 'actExpandAll');
  CreateToolButton(TB, 'actCollapseAll');
  CreateToolButton(TB, 'actGroupByBoxVisible');
  CreateToolButton(TB, 'actGroupBySelection');
  CreateToolButton(TB, 'actClearGrouping');
  TToolBarFactory.CleanupToolBar(TB);
  Result := TB;
end;

class function TDataGrabberFactories.CreateEditorView(AOwner: TComponent;
  AManager: IConnectionViewManager): IEditorView;
begin
  Result := TfrmEditorView.Create(AOwner, AManager);
end;

class function TDataGrabberFactories.CreateManager(AOwner: TComponent;
  ASettings: ISettings): IConnectionViewManager;
begin
  Result := TdmConnectionViewManager.Create(AOwner, ASettings);
end;
{$ENDREGION}
end.

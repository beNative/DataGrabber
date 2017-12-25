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

unit DataGrabber.Factories;

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.Controls,
  Data.DB,

  DataGrabber.Interfaces, DataGrabber.ConnectionSettings;

type
  TDataGrabberFactories = class sealed
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
      AOwner          : TComponent;
      ASettings       : IDataViewSettings;
      const AGridType : string;
      AData           : IData;
      ADataSet        : TDataSet = nil
    ): IDataView;

    class procedure AddToolbarButtons(
      AToolBar : TToolbar;
      AManager : IConnectionViewManager
    );
  end;

implementation

uses
  Vcl.Forms,

  Spring,

  DataGrabber.ConnectionViewManager, DataGrabber.Settings,
  DataGrabber.EditorView, DataGrabber.Data, DataGrabber.ConnectionView,
  DataGrabber.DataView.cxGrid, DataGrabber.DataView.GridView,
  DataGrabber.DataView.KGrid;

{$REGION 'TDataGrabberFactories'}
class function TDataGrabberFactories.CreateSettings(AOwner: TComponent): ISettings;
begin
  Result := TSettings.Create(AOwner);
end;

class procedure TDataGrabberFactories.AddToolbarButtons(AToolBar: TToolbar;
  AManager: IConnectionViewManager);

  procedure AddActionButton(AParent: TToolBar; AAction: TBasicAction);
  var
    TB: TToolButton;
  begin
    TB := TToolButton.Create(AParent.Owner);
    TB.Parent := AParent;
    if not Assigned(AAction) then
      TB.Style := tbsSeparator
    else
      TB.Action := AAction;
  end;

  procedure AddButton(const AActionName: string);
  begin
    Guard.CheckNotNull(AManager.Actions[AActionName], AActionName);
    AddActionButton(AToolBar, AManager.Actions[AActionName]);
  end;

begin
  AddButton('actPreview');
  AddButton('actDesigner');
  AddButton('actPrint');
  AddButton('actDataInspector');
  AddButton('actMergeColumnCells');
  AddButton('actGroupByBoxVisible');
  AddButton('actShowAllColumns');
  AddButton('actHideSelectedColumns');
  AddButton('actHideConstantColumns');
  AddButton('actHideEmptyColumns');
  AddButton('actAutoSizeCols');
  AddButton('actToggleFullScreen');
  AddButton('actSettings');
  AddButton('actExecute');
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
  ASettings: IDataViewSettings; const AGridType: string; AData: IData;
  ADataSet: TDataSet): IDataView;
begin
  if AGridType = 'cxGrid' then
  begin
    Result := TfrmcxGrid.Create(AOwner, ASettings, AData, ADataSet);
  end
  else if AGridType = 'KGrid' then
  begin
    Result := TfrmKGrid.Create(AOwner, ASettings, AData, ADataSet);
  end
  else
  begin
    Result := TfrmGridView.Create(AOwner, ASettings, AData, ADataSet);
  end;
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

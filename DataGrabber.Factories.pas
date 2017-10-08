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
  System.Classes, Vcl.ComCtrls, Vcl.Controls,

  DDuce.Components.PropertyInspector,

  DataGrabber.Interfaces;

procedure AddToolbarButtons(
  AToolBar : TToolbar;
  AManager : IConnectionViewManager
);

function CreateInspector(
  AOwner  : TComponent;
  AParent : TWinControl;
  AObject : TPersistent = nil
): TPropertyInspector;

implementation

uses
  Vcl.Forms,

  ts.Utils.Actions;

procedure AddToolbarButtons(AToolBar: TToolbar; AManager: IConnectionViewManager);

  procedure AddButton(const AActionName: string);
  begin
    AddActionButton(AToolBar, AManager[AActionName]);
  end;

begin
  AddButton('actSettings');
  //AddButton('actFavoriteFieldsOnly');
  AddButton('actShowAllColumns');
  AddButton('actMergeAllColumnCells');
  AddButton('actAutoSizeCols');
  AddButton('actHideSelectedColumns');
  AddButton('actHideConstantColumns');
  AddButton('actHideEmptyColumns');
  AddButton('actExecute');
//  AddButton('actCreateModel');
//  AddButton('actProviderMode');
//  AddButton('actToggleFullScreen');
//  AddButton('actRTTI');
//  AddButton('actSyncEditorWithRepository');
//  AddButton('actToggleRepositoryTree');


//  AddButton('actPreview');
//  AddButton('actDesigner');
//  AddButton('actPrint');

  //AddButton('actAddConnectionView');
end;

function CreateInspector(AOwner : TComponent; AParent : TWinControl;
  AObject : TPersistent): TPropertyInspector;
var
  PI : TPropertyInspector;
begin
  PI                  := TPropertyInspector.Create(AOwner);
  PI.AlignWithMargins := True;
  PI.Parent           := AParent;
  PI.BorderStyle      := bsSingle;
  PI.PropKinds        := PI.PropKinds + [pkReadOnly];
  PI.Align            := alClient;
  PI.Splitter         := PI.Width div 2;
  if Assigned(AObject) then
  begin
    PI.Add(AObject);
    PI.UpdateItems;
  end;
  Result := PI;
end;

end.

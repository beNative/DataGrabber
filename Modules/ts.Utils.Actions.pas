{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Utils.Actions;

{ Helper routines to assign actions to toolbars and menu's. }

interface

uses
  System.Classes, System.SysUtils,
  Vcl.ComCtrls, Vcl.Menus, Vcl.Controls, Vcl.ActnList;

procedure AddActionButton(
  AParent : TToolBar;
  AAction : TBasicAction
);

procedure AddDropDownButton(
  AParent    : TToolBar;
  AAction    : TBasicAction;
  APopupMenu : TPopupMenu
);

procedure AddActionMenuItem(
  AParent : TMenuItem;
  AAction : TBasicAction
);

implementation

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

procedure AddDropDownButton(AParent: TToolBar; AAction: TBasicAction;
  APopupMenu: TPopupMenu);
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent.Owner);
  TB.Parent := AParent;
  TB.Style := tbsDropDown;
  TB.DropdownMenu := APopupMenu;
  TB.Action := AAction;
end;

procedure AddActionMenuItem(AParent: TMenuItem; AAction: TBasicAction);
var
  MI: TMenuItem;
begin
  MI := TMenuItem.Create(AParent.Owner);
  if Assigned(AAction) then
  begin
    MI.Action := AAction;
    if (AAction is TAction) and (TAction(AAction).GroupIndex > 0) then
      MI.RadioItem := True;
  end
  else
  begin
    MI.Caption := '-';
  end;
  AParent.Add(MI);
end;

end.

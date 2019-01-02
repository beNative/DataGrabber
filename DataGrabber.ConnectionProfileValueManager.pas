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

unit DataGrabber.ConnectionProfileValueManager;

{ The TConnectionProfileValueManager class allows us to customize the property
  inspector used in the settings dialog. }

interface

uses
  System.Classes, System.Rtti, System.Types,
  Vcl.Graphics,

  zObjInspTypes, zValueManager,

  DataGrabber.ConnectionProfiles;

type
  TConnectionProfileValueManager = class(TzCustomValueManager)
  public
    procedure SetValue(
      const PItem : PPropItem;
      var Value   : TValue
    ); override;

    function HasButton(const PItem: PPropItem): Boolean; override;
    function HasList(const PItem: PPropItem): Boolean; override;
    function HasDialog(const PItem: PPropItem): Boolean; override;

    function GetDialog(const PItem: PPropItem): TComponentClass; override;
    function DialogResultValue(
      const PItem : PPropItem;
      Dialog      : TComponent
    ): TValue; override;

    procedure GetListItems(
      const PItem : PPropItem;
      Items       : TStrings
    ); override;
  end;

implementation

uses
  System.StrUtils,
  FireDAC.Comp.Client, FireDAC.VCLUI.ConnEdit,

  Spring.Container,

  DDuce.Logger;

{$REGION 'protected methods'}
function TConnectionProfileValueManager.HasButton(
  const PItem: PPropItem): Boolean;
begin
  Result := inherited HasButton(PItem);
end;

function TConnectionProfileValueManager.HasDialog(
  const PItem: PPropItem): Boolean;
begin
  Result := inherited HasDialog(PItem);
end;

function TConnectionProfileValueManager.HasList(
  const PItem: PPropItem): Boolean;
begin
  if MatchStr(PItem.Name, ['DriverName', 'ConnectionDefName']) then
    Result := True
  else
  begin
    Result := inherited HasList(PItem);
  end;
end;

procedure TConnectionProfileValueManager.SetValue(const PItem: PPropItem;
  var Value: TValue);
begin
  inherited SetValue(PItem, Value);
end;

function TConnectionProfileValueManager.GetDialog(
  const PItem: PPropItem): TComponentClass;
begin
  Result := inherited GetDialog(PItem);
end;

function TConnectionProfileValueManager.DialogResultValue(
  const PItem: PPropItem; Dialog: TComponent): TValue;
begin
  Result := inherited DialogResultValue(PItem, Dialog);
end;

procedure TConnectionProfileValueManager.GetListItems(
  const PItem: PPropItem; Items: TStrings);
begin
  if PItem.Name = 'DriverName' then
  begin
    FDManager.GetDriverNames(Items);
  end
  else if PItem.Name = 'ConnectionDefName' then
  begin
    FDManager.GetConnectionDefNames(Items);
  end
  else
  begin
    inherited GetListItems(PItem, Items);
  end;
end;
{$ENDREGION}

end.

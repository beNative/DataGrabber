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

unit DataGrabber.ConnectionProfileValueManager;

interface

{ This class allows us to customize the property inspector used in the settings
  dialog. }

uses
  System.Classes, System.Rtti, System.Types,
  Vcl.Graphics,

  zObjInspector, zObjInspTypes, zValueManager,

  DataGrabber.ConnectionProfiles;

type
  TConnectionProfileValueManager = class(TzCustomValueManager)
  private
    function GetConnectionProfile(
      const PItem: PPropItem
    ): TConnectionProfile;

  public
    procedure SetValue(
      const PItem : PPropItem;
      var Value   : TValue
    ); override;

    function HasButton(const PItem: PPropItem): Boolean; override;
    function HasList(const PItem: PPropItem): Boolean; override;
    function HasDialog(const PItem: PPropItem): Boolean; override;

    function GetDialog(const PItem: PPropItem): TComponentClass; override;

    procedure GetListItems(
      const PItem : PPropItem;
      Items       : TStrings
    ); override;
  end;

implementation

uses
  System.StrUtils,
  Data.Win.ADODb,

  Spring.Container,

  DataGrabber.Interfaces,

  DDuce.Logger;

{$REGION 'private methods'}
function TConnectionProfileValueManager.GetConnectionProfile(
  const PItem: PPropItem): TConnectionProfile;
begin
  if Assigned(PItem.Parent) then
    Result := PItem.Parent.Instance as TConnectionProfile
  else
    Result := PItem.Instance as TConnectionProfile;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TConnectionProfileValueManager.HasButton(
  const PItem: PPropItem): Boolean;
var
  CP : TConnectionProfile;
begin
  CP := GetConnectionProfile(PItem);

  //CP.

  Result := inherited HasButton(PItem);
end;

function TConnectionProfileValueManager.HasDialog(
  const PItem: PPropItem): Boolean;
begin
  if PItem.Name = 'ConnectionString' then
    Result := True
  else
    Result := inherited HasDialog(PItem);
end;

function TConnectionProfileValueManager.HasList(
  const PItem: PPropItem): Boolean;
begin
  if MatchStr(PItem.Name, ['ConnectionType', 'Protocol']) then
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

procedure TConnectionProfileValueManager.GetListItems(
  const PItem: PPropItem; Items: TStrings);
var
  CP : TConnectionProfile;
begin
  CP := nil;
//  if PItem.Name = 'ConnectionType' then
//  begin
//    for C in GlobalContainer.ResolveAll<IConnection> do
//      Items.Add(C.ConnectionType);
//  end
  if PItem.Name = 'Protocol' then
  begin
    CP := PItem.Component as TConnectionProfile;
//    if CP.ConnectionType <> '' then
//    begin
//      C := GlobalContainer.Resolve<IConnection>(CP.ConnectionType);
//      Items.Assign(C.Protocols);
//    end;
  end
  else
  begin
    inherited GetListItems(PItem, Items);
  end;
end;
{$ENDREGION}

end.

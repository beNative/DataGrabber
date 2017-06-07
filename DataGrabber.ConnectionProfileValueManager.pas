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

{ This class allows us to customize the property editor. }

uses
  System.Classes, System.Rtti, System.Types,
  Vcl.Graphics,

  zObjInspector;

type
  TConnectionProfileValueManager = class(TzCustomValueManager)

    /// <summary> Use custom ListBox .
    /// </summary>
    //class function GetListClass(const PItem: PPropItem): TPopupListClass; override;
    class procedure SetValue(const PItem: PPropItem; var Value: TValue); override;
///    class function GetValue(const PItem: PPropItem; const Value): TValue; override;
//    class function GetValueName(const PItem: PPropItem): string; override;
    /// <summary> Check if item can assign value that is not listed in ListBox .
    /// </summary>
    //class function ValueHasOpenProbabilities(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if value has an ExtraRect like (Color,Boolean)type .
    /// </summary>
    /// <returns> non zero to indicate that value must use an ExtraRect .
    /// </returns>
    //class function GetExtraRectWidth(const PItem: PPropItem): Integer; override;
    //class function GetValueType(const PItem: PPropItem): Integer; override;
    /// <summary> Paint item value name .
    /// </summary>
    //class procedure PaintValue(Canvas: TCanvas; Index: Integer; const PItem: PPropItem; R: TRect); override;
    /// <summary> Check if the current item can have button .
    /// </summary>
    //class function HasButton(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if the current item can drop ListBox .
    /// </summary>
    class function HasList(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if the current item have customized dialog .
    /// </summary>
    class function HasDialog(const PItem: PPropItem): Boolean; override;
    /// <summary> Get customized dialog for current item .
    /// </summary>
    //class function GetDialog(const PItem: PPropItem): TComponentClass; override;
    //class procedure DialogCode(const PItem: PPropItem; Dialog: TComponent; Code: Integer); override;
    /// <summary> Get the value returned after editing from the dialog .
    /// </summary>
    //class function DialogResultValue(const PItem: PPropItem; Dialog: TComponent): TValue; override;
    /// <summary> Return ListBox items for the current item .
    /// </summary>
    class procedure GetListItems(const PItem: PPropItem; Items: TStrings); override;
    /// <summary> Get the value when the user click the ExtraRect .
    /// </summary>
    //class function GetExtraRectResultValue(const PItem: PPropItem): TValue; override;



  end;

implementation

uses
  System.StrUtils,

  Spring.Container,

  ts.Interfaces,

  DDuce.Logger,

  DataGrabber.ConnectionProfiles;

{ TConnectionProfileValueManager }

class procedure TConnectionProfileValueManager.GetListItems(
  const PItem: PPropItem; Items: TStrings);
var
  C  : IConnection;
  CP : TConnectionProfile;
begin
  CP := nil;
  if PItem.Name = 'ConnectionType' then
  begin
    for C in GlobalContainer.ResolveAll<IConnection> do
      Items.Add(C.ConnectionType);
  end
  else if PItem.Name = 'Protocol' then
  begin
    CP := PItem.Component as TConnectionProfile;
    if CP.ConnectionType <> '' then
    begin
      C := GlobalContainer.Resolve<IConnection>(CP.ConnectionType);
      Items.Assign(C.Protocols);
    end;
  end
  else
  begin
     inherited GetListItems(PItem, Items);
  end;
end;

class function TConnectionProfileValueManager.HasDialog(
  const PItem: PPropItem): Boolean;
begin
  if PItem.Name = 'ConnectionString' then
    Result := True
  else
    Result := inherited HasDialog(PItem);
end;

class function TConnectionProfileValueManager.HasList(
  const PItem: PPropItem): Boolean;
begin
  if MatchStr(PItem.Name, ['ConnectionType', 'Protocol']) then
    Result := True
  else
  begin
    Result := inherited HasList(PItem);
  end;
end;

class procedure TConnectionProfileValueManager.SetValue(const PItem: PPropItem;
  var Value: TValue);
begin
  Logger.Watch('Value', Value.ToString);
  inherited;

end;

initialization
  DefaultValueManager := TConnectionProfileValueManager;

end.

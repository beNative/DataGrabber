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

unit DataGrabber.PropertyEditors;

{
   TODO: rewrite for new property grid
}

interface

uses
  System.Classes,

  DDuce.Components.PropertyInspector,

  ts.Interfaces,

  DataGrabber.ConnectionProfiles;

type
  TProtocolPropertyEditor = class(TStringPropertyEditor)
  private
    function GetConnectionProfile: TConnectionProfile;

  protected
    procedure GetValues(AValues: TStrings); override;
    function GetAttrs: TPropertyAttributes; override;

    property ConnectionProfile: TConnectionProfile
      read GetConnectionProfile;
  end;

  TConnectionTypePropertyEditor = class(TStringPropertyEditor)
  protected
    procedure GetValues(AValues: TStrings); override;
    function GetAttrs: TPropertyAttributes; override;
  end;

implementation

uses
  System.SysUtils,

  Spring.Container;

{$REGION 'TProtocolPropertyEditor'}
function TProtocolPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praValueList];
end;

function TProtocolPropertyEditor.GetConnectionProfile: TConnectionProfile;
begin
  Result := TConnectionProfile(Designer);
end;

procedure TProtocolPropertyEditor.GetValues(AValues: TStrings);
var
  S: IConnection;
begin
  if ConnectionProfile.ConnectionType <> '' then
  begin
    S := GlobalContainer.Resolve<IConnection>(ConnectionProfile.ConnectionType);
    AValues.Assign(S.Protocols);
  end;
end;
{$ENDREGION}

{$REGION 'TConnectionTypePropertyEditor'}
function TConnectionTypePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praValueList];
end;

procedure TConnectionTypePropertyEditor.GetValues(AValues: TStrings);
var
  C : IConnection;
begin
  for C in GlobalContainer.ResolveAll<IConnection> do
    AValues.Add(C.ConnectionType);
end;
{$ENDREGION}

end.

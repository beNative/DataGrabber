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


unit DataGrabber.ConnectionSettings;

interface

uses
  System.Classes;

type
  TConnectionSettings = class(TPersistent)
  private
    FUser             : string;
    FProtocol         : string;
    FPort             : Integer;
    FPassword         : string;
    FHostName         : string;
    FDatabase         : string;
    FCatalog          : string;
    FReadOnly         : Boolean;
    FOnChanged        : TNotifyEvent;
    FFetchOnDemand    : Boolean;
    FPacketRecords    : Integer;
    FConnectionString : string;

    {$REGION 'property access methods'}
    function GetCatalog: string;
    function GetDatabase: string;
    function GetHostName: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetDriverName: string;
    function GetUser: string;
    procedure SetCatalog(const Value: string);
    procedure SetDatabase(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetDriverName(const Value: string);
    procedure SetUser(const Value: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetFetchOnDemand: Boolean;
    function GetPacketRecords: Integer;
    procedure SetFetchOnDemand(const Value: Boolean);
    procedure SetPacketRecords(const Value: Integer);
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    {$ENDREGION}

  protected
    procedure Changed;

  public
    procedure Assign(Source: TPersistent); override;

  published
    property DriverName: string
      read GetDriverName write SetDriverName;

    property HostName: string
      read GetHostName write SetHostName;

    property Port: Integer
      read GetPort write SetPort default 0;

    property Database: string
      read GetDatabase write SetDatabase;

    property User: string
      read GetUser write SetUser;

    property Password: string
      read GetPassword write SetPassword;

    property Catalog: string
      read GetCatalog write SetCatalog;

    property ReadOnly: Boolean
      read GetReadOnly write SetReadOnly;

    property FetchOnDemand: Boolean
      read GetFetchOnDemand write SetFetchOnDemand;

    property PacketRecords: Integer
      read GetPacketRecords write SetPacketRecords;

    property ConnectionString: string
      read GetConnectionString write SetConnectionString;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;
  end;

implementation

{$REGION 'property access methods'}
function TConnectionSettings.GetCatalog: string;
begin
  Result := FCatalog;
end;

procedure TConnectionSettings.SetCatalog(const Value: string);
begin
  if Value <> Catalog then
  begin
    FCatalog := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

procedure TConnectionSettings.SetConnectionString(const Value: string);
begin
  if Value <> ConnectionString then
  begin
    FConnectionString := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetDatabase: string;
begin
  Result := FDatabase;
end;

procedure TConnectionSettings.SetDatabase(const Value: string);
begin
  if Value <> Database then
  begin
    FDatabase := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetFetchOnDemand: Boolean;
begin
  Result := FFetchOnDemand;
end;

procedure TConnectionSettings.SetFetchOnDemand(const Value: Boolean);
begin
  if Value <> FetchOnDemand then
  begin
    FFetchOnDemand := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetHostName: string;
begin
  Result := FHostName;
end;

procedure TConnectionSettings.SetHostName(const Value: string);
begin
  if Value <> HostName then
  begin
    FHostName := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetPacketRecords: Integer;
begin
  Result := FPacketRecords;
end;

procedure TConnectionSettings.SetPacketRecords(const Value: Integer);
begin
  if Value <> PacketRecords then
  begin
    FPacketRecords := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetPassword: string;
begin
  Result := FPassword;
end;

procedure TConnectionSettings.SetPassword(const Value: string);
begin
  if Value <> Password then
  begin
    FPassword := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TConnectionSettings.SetPort(const Value: Integer);
begin
  if Value <> Port then
  begin
    FPort := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TConnectionSettings.SetReadOnly(const Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetDriverName: string;
begin
  Result := FProtocol;
end;

procedure TConnectionSettings.SetDriverName(const Value: string);
begin
  if Value <> DriverName then
  begin
    FProtocol := Value;
    Changed;
  end;
end;

function TConnectionSettings.GetUser: string;
begin
  Result := FUser;
end;

procedure TConnectionSettings.SetUser(const Value: string);
begin
  if Value <> User then
  begin
    FUser := Value;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TConnectionSettings.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TConnectionSettings.Assign(Source: TPersistent);
var
  CS: TConnectionSettings;
begin
  if (Source <> Self) and (Source is TConnectionSettings) then
  begin
    CS := TConnectionSettings(Source);
    FProtocol         := CS.DriverName;
    FPassword         := CS.Password;
    FCatalog          := CS.Catalog;
    FHostName         := CS.HostName;
    FUser             := CS.User;
    FPort             := CS.Port;
    FDatabase         := CS.Database;
    FFetchOnDemand    := CS.FetchOnDemand;
    FPacketRecords    := CS.PacketRecords;
    FReadOnly         := CS.ReadOnly;
    FConnectionString := CS.ConnectionString;
    Changed;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

initialization
  RegisterClasses([TConnectionSettings]);

end.

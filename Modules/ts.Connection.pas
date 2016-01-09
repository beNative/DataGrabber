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

unit ts.Connection;

{ Abstract datamodule intended to host a database connection. }

interface

uses
  System.SysUtils, System.Classes,
  Data.DB,

  ts.Interfaces, ts.Classes.ConnectionSettings;

type
  TdmConnection = class (TDataModule, IConnection)
  private
    FConnectionSettings : TConnectionSettings;
    FProtocols          : TStringList;

    procedure ConnectionSettingsChanged(Sender: TObject);

  protected
    procedure AssignConnectionString(const AValue: string); virtual; abstract;
    procedure AssignConnectionSettings; virtual; abstract;

    { IConnection }
    function GetConnectionSettings: TConnectionSettings; virtual;
    function GetConnectionType: string; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    procedure SetConnected(const Value: Boolean); virtual; abstract;
    function GetConnectionString: string; virtual; abstract;
    procedure SetConnectionString(const Value: string); virtual;
    function GetConnection: TComponent; virtual; abstract;
    function Execute(const ACommandText: string): Boolean; virtual; abstract;
    function CreateNativeDataSet: INativeDataSet; virtual; abstract;
    function GetProtocols: TStrings; virtual;

  public
    constructor Create(
      AOwner    : TComponent;
      ASettings : TConnectionSettings = nil
    ); reintroduce; virtual;
    procedure BeforeDestruction; override;

    property Connected: Boolean
      read GetConnected write SetConnected;

    property ConnectionString : string
      read GetConnectionString write SetConnectionString;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property ConnectionType: string
      read GetConnectionType;

    property Connection: TComponent
      read GetConnection;

    property Protocols: TStrings
      read GetProtocols;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TdmConnection.Create(AOwner: TComponent;
  ASettings: TConnectionSettings);
begin
  inherited Create(AOwner);
  FConnectionSettings := TConnectionSettings.Create;
  FConnectionSettings.OnChanged := ConnectionSettingsChanged;
  AssignConnectionSettings;
  FProtocols := TStringList.Create;
  if Assigned(ASettings) then
    ConnectionSettings.Assign(ASettings);
end;

procedure TdmConnection.BeforeDestruction;
begin
  FreeAndNil(FProtocols);
  FreeAndNil(FConnectionSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TdmConnection.SetConnectionString(const Value: string);
var
  B: Boolean;
begin
  if Value <> ConnectionString then
  begin
    B := Connected;
    Connected := False;
    AssignConnectionString(Value);
    Connected := B;
  end;
end;

function TdmConnection.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

function TdmConnection.GetProtocols: TStrings;
begin
  Result := FProtocols;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TdmConnection.ConnectionSettingsChanged(Sender: TObject);
begin
  AssignConnectionSettings;
end;
{$ENDREGION}

end.

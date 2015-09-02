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

unit ts.Connection.CustomConnectionAdaptor;

{ Abstract datamodule intended to host a database connection.
  This datamodule will act as a connection adaptor. }

interface

uses
  System.SysUtils, System.Classes,
  Data.DB,

  ts.Interfaces, ts.Classes.ConnectionSettings;

type
  TCustomConnectionAdaptor = class (TInterfacedObject, IConnection)
  private
    FConnectionSettings : TConnectionSettings;
    FProtocols          : TStringList;

    procedure ConnectionSettingsChanged(Sender: TObject);
    function GetProtocols: TStrings;

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

    property Connected: Boolean
      read GetConnected write SetConnected;

    property ConnectionString : string
      read GetConnectionString write SetConnectionString;

    property ConnectionSettings: TConnectionSettings
      read GetConnectionSettings;

    property Connection: TComponent
      read GetConnection;

    property Protocols: TStrings
      read GetProtocols;

    property ConnectionType: string
      read GetConnectionType;

  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TCustomConnectionAdaptor.AfterConstruction;
begin
  inherited AfterConstruction;
  FConnectionSettings           := TConnectionSettings.Create;
  FConnectionSettings.OnChanged := ConnectionSettingsChanged;
  FProtocols                    := TStringList.Create;
end;

procedure TCustomConnectionAdaptor.BeforeDestruction;
begin
  FreeAndNil(FProtocols);
  FreeAndNil(FConnectionSettings);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TCustomConnectionAdaptor.SetConnectionString(const Value: string);
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

function TCustomConnectionAdaptor.GetConnectionSettings: TConnectionSettings;
begin
  Result := FConnectionSettings;
end;

function TCustomConnectionAdaptor.GetProtocols: TStrings;
begin
  Result := FProtocols;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TCustomConnectionAdaptor.ConnectionSettingsChanged(Sender: TObject);
begin
  AssignConnectionSettings;
end;
{$ENDREGION}

end.

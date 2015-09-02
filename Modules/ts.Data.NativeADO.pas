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

unit ts.Data.NativeADO;

interface

uses
  Data.DB, Data.Win.ADODB,

  ts.Interfaces, ts.Data.Native;

type
  TNativeADODataSet = class(TNativeDataSet, INativeDataSet)
  private
    FDataSet: TADODataSet;

  protected
    function GetDataSet: TDataSet; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeforeExecute; override;
    procedure AfterExecute; override;
    procedure UpdateProviderMode(var AEnabled: Boolean); override;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TNativeADODataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FDataSet := TADODataSet.Create(nil);
  FDataSet.Connection := Connection.Connection as TADOConnection;
end;

procedure TNativeADODataSet.BeforeDestruction;
begin
  FreeAndNil(FDataSet);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TNativeADODataSet.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TNativeADODataSet.BeforeExecute;
begin
  if Connection.ConnectionSettings.DisconnectedMode then
    FDataSet.Connection := Connection.Connection as TADOConnection;
end;

procedure TNativeADODataSet.AfterExecute;
begin
  if Connection.ConnectionSettings.DisconnectedMode then
    FDataSet.Connection := nil;
  if (Connection.Connection as TADOConnection).DataSetCount = 0 then
    Connection.Connected := False;
end;

procedure TNativeADODataSet.UpdateProviderMode(var AEnabled: Boolean);
begin
  if AEnabled then
  begin
    FDataSet.CursorLocation := clUseServer;
    FDataSet.CursorType     := ctOpenForwardOnly;
  end
  else
  begin
    FDataSet.CursorLocation := clUseClient;
    FDataSet.CursorType     := ctStatic;
  end;
  inherited;
end;
{$ENDREGION}

end.

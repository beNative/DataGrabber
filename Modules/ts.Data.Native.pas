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

unit ts.Data.Native;

{ TNativeDataSet is an interfaced object acting as an adapter class for the
  underlying native dataset. }

interface

uses
  Data.DB,

  ts.Interfaces;

type
  TNativeDataSet = class abstract(TInterfacedObject, INativeDataSet)
  private
    FConnection: IConnection;

    function GetConnection: IConnection;

  protected
    function GetDataSet: TDataSet; virtual; abstract;

    property Connection: IConnection
      read GetConnection;

    property DataSet: TDataSet
      read GetDataSet;

    procedure BeforeExecute; virtual;
    procedure AfterExecute; virtual;
    procedure UpdateProviderMode(var AEnabled: Boolean); virtual;

  public
    constructor Create(AConnection: IConnection); reintroduce; virtual;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TNativeDataSet.Create(AConnection: IConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TNativeDataSet.GetConnection: IConnection;
begin
  Result := FConnection;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TNativeDataSet.BeforeExecute;
begin
//
end;

procedure TNativeDataSet.AfterExecute;
begin
//
end;

{ If the native dataset is used in provider mode (with datasetprovider and
  clientdataset) adjustments can be done to the dataset here before it is
  opened. Certain connections always require providermode (DBX), and will
  set AEnabled explicitly.
}

procedure TNativeDataSet.UpdateProviderMode(var AEnabled: Boolean);
begin
//
end;
{$ENDREGION}

end.

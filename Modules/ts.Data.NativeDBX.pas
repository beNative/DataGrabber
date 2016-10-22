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

unit ts.Data.NativeDBX;

interface

uses
  Data.DB, Data.FMTBcd, Data.SqlExpr,

  ts.Interfaces, ts.Data.Native;

type
  TNativeDBXDataSet = class(TNativeDataSet, INativeDataSet)
  private
    FDataSet: TSQLDataSet;

  protected
    function GetDataSet: TDataSet; override;

    procedure UpdateProviderMode(var AEnabled: Boolean); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TNativeDBXDataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FDataSet := TSQLDataSet.Create(nil);
  FDataSet.SQLConnection := Connection.Connection as TSQLConnection;
end;

procedure TNativeDBXDataSet.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FreeAndNil(FDataSet);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TNativeDBXDataSet.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TNativeDBXDataSet.UpdateProviderMode(var AEnabled: Boolean);
begin
  // DBX always works in ProviderMode
  AEnabled := True;
end;
{$ENDREGION}

end.

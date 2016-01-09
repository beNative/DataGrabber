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

unit ts.Data.NativeZEOS;

interface

uses
  Data.DB,

  ZAbstractRODataset, ZAbstractDataset, ZDataset,

  ts.Interfaces, ts.Data.Native;

type
  TNativeZEOSDataSet = class(TNativeDataSet, INativeDataSet)
  private
    FDataSet: TZQuery;

  protected
    function GetDataSet: TDataSet; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.SysUtils,

  ZAbstractConnection;

{$REGION 'construction and destruction'}
procedure TNativeZEOSDataSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FDataSet := TZQuery.Create(nil);
  FDataSet.Connection := Connection.Connection as TZAbstractConnection;
end;

procedure TNativeZEOSDataSet.BeforeDestruction;
begin
  FreeAndNil(FDataSet);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TNativeZEOSDataSet.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;
{$ENDREGION}

end.

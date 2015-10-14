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


unit DataGrabber.ModelData;

interface

uses
  System.SysUtils, System.Classes, Data.DB;

//  ZAbstractRODataset, ZAbstractDataset, ZAbstractTable, ZDataset,
//  ZAbstractConnection, ZConnection;

type
  TdmModelData = class(TDataModule)
//    conMain: TZConnection;
//    tblModel: TZTable;
//    tblTable: TZTable;
//    tblField: TZTable;
    dscModel: TDataSource;
    dscTable: TDataSource;

  public
    procedure AddTables(ATables: TStrings);
    procedure AddFields(const ATableName: string; AFields: TStrings);

    procedure AfterConstruction; override;
  end;

function ModelData: TdmModelData;

implementation

uses
  Variants, Forms;

var
  FModelData: TdmModelData;

{$R *.dfm}

function ModelData: TdmModelData;
begin
  if not Assigned(FModelData) then
    FModelData := TdmModelData.Create(Application);
  Result := FModelData;
end;

{ TdmModelData }

procedure TdmModelData.AddFields(const ATableName: string; AFields: TStrings);
//var
//  S: string;
begin
//  tblTable.Locate('Name', VarArrayOf([ATableName]), []);
//  for S in AFields do
//  begin
//    tblField.Append;
//    tblField.FieldByName('Name').AsString := S;
//    tblField.FieldByName('TableId').AsInteger := tblTable.FieldByName('ID').AsInteger;
//    tblField.Post;
//  end;

end;

procedure TdmModelData.AddTables(ATables: TStrings);
var
  S: string;
begin
  for S in ATables do
  begin
//    tblTable.Append;
//    tblTable.FieldByName('ModelID').AsInteger := tblModel.FieldByName('ID').AsInteger;
//    tblTable.FieldByName('Name').AsString := S;
//    tblTable.Post;
  end;
end;

procedure TdmModelData.AfterConstruction;
begin
  inherited AfterConstruction;
//  conMain.Connected := True;
//  tblModel.Active := True;
//  tblTable.Active := True;
//  tblField.Active := True;
end;

end.

{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.MetaData.Dialog;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  FireDAC.Comp.Client,

  OMultiPanel;

type
  TfrmMetaData = class(TForm)
    pnlMain     : TOMultiPanel;
    lstCatalogs : TListBox;
    lstSchemas  : TListBox;
    lstTables   : TListBox;
    lstFields   : TListBox;

    {$REGION 'event handlers'}
    procedure lstCatalogsClick(Sender: TObject);
    procedure lstSchemasClick(Sender: TObject);
    procedure lstTablesClick(Sender: TObject);
    procedure lstSchemasEnter(Sender: TObject);
    {$ENDREGION}

  private
    FConnection : TFDConnection;

    {$REGION 'property access methods'}
    function GetCatalogs: TStrings;
    function GetSchemas: TStrings;
    function GetFields: TStrings;
    function GetTables: TStrings;
    function GetCatalog: string;
    function GetSchema: string;
    function GetTable: string;
    {$ENDREGION}

  public
    constructor Create(
      AOwner      : TComponent;
      AConnection : TFDConnection
    ); reintroduce;

    property Catalogs: TStrings
      read GetCatalogs;

    property Schemas: TStrings
      read GetSchemas;

    property Tables: TStrings
      read GetTables;

    property Fields: TStrings
      read GetFields;

    property Catalog: string
      read GetCatalog;

    property Schema: string
      read GetSchema;

    property Table: string
      read GetTable;
  end;

implementation

uses
  FireDAC.Phys.Intf;

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmMetaData.Create(AOwner: TComponent; AConnection: TFDConnection);
begin
  inherited Create(AOwner);
  FConnection  := AConnection;
  FConnection.GetCatalogNames('', Catalogs);
  FConnection.GetSchemaNames('', '', Schemas);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMetaData.lstCatalogsClick(Sender: TObject);
begin
  FConnection.GetSchemaNames('', '', Schemas);
end;

procedure TfrmMetaData.lstSchemasClick(Sender: TObject);
begin
  FConnection.GetTableNames(Catalog , Schema, '',  Tables, [osMy], [tkTable], False);
end;

procedure TfrmMetaData.lstSchemasEnter(Sender: TObject);
begin
  FConnection.GetTableNames(Catalog , Schema, '',  Tables, [osMy], [tkTable], False);
end;

procedure TfrmMetaData.lstTablesClick(Sender: TObject);
begin
  FConnection.GetFieldNames(Catalog, Schema, Table, '', Fields);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmMetaData.GetCatalog: string;
begin
  if lstCatalogs.ItemIndex >= 0 then
    Result := Catalogs[lstCatalogs.ItemIndex]
  else
    Result := '';
end;

function TfrmMetaData.GetTable: string;
begin
  if lstTables.ItemIndex >= 0 then
    Result := Tables[lstTables.ItemIndex]
  else
    Result := '';
end;

function TfrmMetaData.GetSchema: string;
begin
  if lstSchemas.ItemIndex >= 0 then
    Result := Schemas[lstSchemas.ItemIndex]
  else
    Result := '';
end;

function TfrmMetaData.GetCatalogs: TStrings;
begin
  Result := lstCatalogs.Items;
end;

function TfrmMetaData.GetFields: TStrings;
begin
  Result := lstFields.Items;
end;

function TfrmMetaData.GetSchemas: TStrings;
begin
  Result := lstSchemas.Items;
end;

function TfrmMetaData.GetTables: TStrings;
begin
  Result := lstTables.Items;
end;
{$ENDREGION}

end.

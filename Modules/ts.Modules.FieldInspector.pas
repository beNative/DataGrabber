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

unit ts.Modules.FieldInspector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids,
  Data.DB,

  DDuce.Components.PropertyInspector,

  VirtualTrees,

  ts.Interfaces;

type
  TfrmFieldInspector = class(TForm)
    splVertical : TSplitter;
    pnlLeft     : TPanel;
    pnlRight    : TPanel;

    procedure vstFieldsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstFieldsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);

  private
    FData     : IData;
    piField   : TPropertyInspector;
    vstFields : TVirtualStringTree;

    function GetData: IData;
    function GetDataSet: TDataSet;
    procedure SetData(const Value: IData);

  public
    constructor Create(AOwner: TComponent; AData: IData = nil); reintroduce;

    procedure UpdateView;

    property DataSet : TDataSet
      read GetDataSet;

    property Data: IData
      read GetData write SetData;
  end;

implementation

{$R *.dfm}

uses
  System.Math;

{$REGION 'construction and destruction'}
constructor TfrmFieldInspector.Create(AOwner: TComponent; AData: IData);
begin
  inherited Create(AOwner);
  Data := AData;
  piField := TPropertyInspector.Create(Self);
  piField.Parent := pnlRight;
  piField.Align := alClient;

  vstFields := TVirtualStringTree.Create(Self);
  vstFields.Parent := pnlLeft;
  vstFields.Align := alClient;
  vstFields.OnGetText := vstFieldsGetText;
  vstFields.OnFocusChanged := vstFieldsFocusChanged;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmFieldInspector.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmFieldInspector.SetData(const Value: IData);
begin
  if Assigned(Value) and (Value <> Data) then
  begin
    FData := Value;
    UpdateView;
  end;
end;

function TfrmFieldInspector.GetDataSet: TDataSet;
begin
  if Assigned(FData) then
    Result := FData.DataSet
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFieldInspector.vstFieldsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  piField.Clear;
  piField.Add(DataSet.Fields[Node.Index]);
end;

procedure TfrmFieldInspector.vstFieldsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Column = 0 then
    CellText := DataSet.Fields[Node.Index].FieldName
  else if Column = 1 then
    CellText := DataSet.Fields[Node.Index].ClassName;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmFieldInspector.UpdateView;
begin
  vstFields.RootNodeCount := IfThen(Assigned(FData), DataSet.FieldCount, 0);
end;
{$ENDREGION}

end.

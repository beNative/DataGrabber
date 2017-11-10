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

{ A TField inspector for any given TDataSet descendant. }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Data.DB,

  DDuce.Components.PropertyInspector,

  zObjInspector,

  VirtualTrees,

  ts.Interfaces, System.Actions, Vcl.ActnList;

type
  TfrmFieldInspector = class(TForm)
    {$REGION 'designer controls'}
    aclMain     : TActionList;
    actInspect  : TAction;
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    splVertical : TSplitter;
    {$ENDREGION}

    procedure vstFieldsGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure vstFieldsFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );
    procedure actInspectExecute(Sender: TObject);

  private
    FData      : IData;
    FOIField   : TzObjectInspector;
    FVSTFields : TVirtualStringTree;

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
  System.Math,

  DDuce.Factories, DDuce.Components.Factories,

  DDuce.ObjectInspector.zObjectInspector;

{$REGION 'construction and destruction'}
constructor TfrmFieldInspector.Create(AOwner: TComponent; AData: IData);
var
  C: TVirtualTreeColumn;
begin
  inherited Create(AOwner);
  Data := AData;
  FOIField   := TFactories.CreatezObjectInspector(Self, pnlRight);
  FVSTFields := TFactories.CreateVirtualStringTree(Self, pnlLeft);
    // cell in first column is fully selected
  FVSTFields.TreeOptions.MiscOptions :=
    FVSTFields.TreeOptions.MiscOptions + [toGridExtensions];
  FVSTFields.TreeOptions.PaintOptions :=
    FVSTFields.TreeOptions.PaintOptions - [toHideSelection];
  FVSTFields.TreeOptions.PaintOptions :=
    FVSTFields.TreeOptions.PaintOptions + [toPopupMode];
  FVSTFields.TreeOptions.SelectionOptions :=
    FVSTFields.TreeOptions.SelectionOptions - [toDisableDrawSelection];
  FVSTFields.TreeOptions.SelectionOptions :=
    FVSTFields.TreeOptions.SelectionOptions + [toAlwaysSelectNode];
  FVSTFields.OnGetText := vstFieldsGetText;
  FVSTFields.OnFocusChanged := vstFieldsFocusChanged;
  FVSTFields.LineStyle := lsSolid;
  FVSTFields.Header.Font.Style := FVSTFields.Header.Font.Style + [fsBold];
  // required when using it as a grid
  FVSTFields.Indent    := 0;
  C := FVSTFields.Header.Columns.Add;
  C.CaptionAlignment := taCenter;
  C.Text             := 'Fieldname';
  C := FVSTFields.Header.Columns.Add;
  C.CaptionAlignment := taCenter;
  C.Text             := 'Field class';
  C.CaptionAlignment := taCenter;
  C.Alignment        := taCenter;
  C.Width := 120;
  FVSTFields.Margins.Right := 0;
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

{$REGION 'action handlers'}
procedure TfrmFieldInspector.actInspectExecute(Sender: TObject);
begin
  InspectObject(FVSTFields);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFieldInspector.vstFieldsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FOIField.Component := DataSet.Fields[Node.Index];
  FOIField.SelectItem(-1); // this prevents any invalid selection
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
  FVSTFields.RootNodeCount := IfThen(Assigned(FData), DataSet.FieldCount, 0);
end;
{$ENDREGION}

end.

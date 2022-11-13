{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DataGrabber.inc}

unit DataGrabber.FieldInspector;

interface

{ A TField inspector for any given TDataSet descendant. }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ActnList,
  Data.DB,

  zObjInspector,

  VirtualTrees, VirtualTrees.BaseTree;

type
  TfrmFieldInspector = class(TForm)
    {$REGION 'designer controls'}
    aclMain     : TActionList;
    actInspect  : TAction;
    pnlLeft     : TPanel;
    pnlRight    : TPanel;
    splVertical : TSplitter;
    {$ENDREGION}

    procedure FVSTFieldsGetText(
      Sender       : TBaseVirtualTree;
      Node         : PVirtualNode;
      Column       : TColumnIndex;
      TextType     : TVSTTextType;
      var CellText : string
    );
    procedure FVSTFieldsFocusChanged(
      Sender : TBaseVirtualTree;
      Node   : PVirtualNode;
      Column : TColumnIndex
    );

    procedure actInspectExecute(Sender: TObject);

  private
    FDataSet   : TDataSet;
    FOIField   : TzObjectInspector;
    FVSTFields : TVirtualStringTree;

    function GetDataSet: TDataSet;
    procedure SetDataSet(const Value: TDataSet);

  protected
    procedure UpdateActions; override;

  public
    constructor Create(
      AOwner   : TComponent;
      ADataSet : TDataSet = nil
    ); reintroduce; virtual;

    procedure UpdateView;

    property DataSet : TDataSet
      read GetDataSet write SetDataSet;
  end;

implementation

{$R *.dfm}

uses
  System.Math, System.UITypes,

  VirtualTrees.Types, VirtualTrees.Header,

  Spring,

  DDuce.Factories.VirtualTrees, DDuce.Factories.zObjInspector,
  DDuce.ObjectInspector.zObjectInspector,

  DataGrabber.Resources;

{$REGION 'construction and destruction'}
constructor TfrmFieldInspector.Create(AOwner: TComponent; ADataSet: TDataSet);
var
  C : TVirtualTreeColumn;
begin
  inherited Create(AOwner);
  FDataSet   := ADataSet;
  FOIField   := TzObjectInspectorFactory.Create(Self, pnlRight);
  FVSTFields := TVirtualStringTreeFactory.CreateGrid(Self, pnlLeft);
  // cell in first column is fully selected
  FVSTFields.TreeOptions.PaintOptions := // always show selection
    FVSTFields.TreeOptions.PaintOptions - [toHideSelection];
  FVSTFields.TreeOptions.PaintOptions := // show always as focused
    FVSTFields.TreeOptions.PaintOptions + [toPopupMode];
  FVSTFields.TreeOptions.SelectionOptions := // disable selection rectangle
    FVSTFields.TreeOptions.SelectionOptions - [toDisableDrawSelection];
  FVSTFields.TreeOptions.SelectionOptions := // show always a selected node
    FVSTFields.TreeOptions.SelectionOptions + [toAlwaysSelectNode];
  FVSTFields.OnGetText := FVSTFieldsGetText;
  FVSTFields.OnFocusChanged := FVSTFieldsFocusChanged;
  FVSTFields.LineStyle := lsSolid;
  FVSTFields.Header.Font.Style := FVSTFields.Header.Font.Style + [fsBold];
  // required when using it as a grid
  FVSTFields.Indent  := 0;
  C := FVSTFields.Header.Columns.Add;
  C.CaptionAlignment := taCenter;
  C.Text             := SFieldName;
  C := FVSTFields.Header.Columns.Add;
  C.CaptionAlignment := taCenter;
  C.Text             := SFieldClass;
  C.CaptionAlignment := taCenter;
  C.Alignment        := taCenter;
  C.Width            := 120;
  FVSTFields.Margins.Right := 0;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmFieldInspector.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TfrmFieldInspector.SetDataSet(const Value: TDataSet);
begin
  if Value <> DataSet then
  begin
    FDataSet := Value;
    if FDataSet.FieldCount > 0 then
    begin
      FOIField.Component := DataSet.Fields[0];
    end
    else
      FOIField.Component := nil;
    UpdateView;
  end;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmFieldInspector.actInspectExecute(Sender: TObject);
begin
  InspectObject(FVSTFields);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFieldInspector.FVSTFieldsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FOIField.Component := DataSet.Fields[Node.Index];
  FOIField.SelectItem(-1); // this prevents any invalid selection
end;

procedure TfrmFieldInspector.FVSTFieldsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if Column = 0 then
    CellText := DataSet.Fields[Node.Index].FieldName
  else if Column = 1 then
    CellText := DataSet.Fields[Node.Index].ClassName;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmFieldInspector.UpdateActions;
begin
  inherited UpdateActions;
  //FOIField.Invalidate;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmFieldInspector.UpdateView;
begin
  FVSTFields.RootNodeCount := IfThen(Assigned(DataSet), DataSet.FieldCount, 0);
end;
{$ENDREGION}

end.

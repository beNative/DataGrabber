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

unit DataGrabber.DataInspector;

{
  TODO: support for custom inplace editors, especially for memo-fields
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.Generics.Collections,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ActnList, Vcl.Menus,
  Data.DB,

  DDuce.Components.GridView, DDuce.Components.Inspector,

  DataGrabber.Interfaces;

const
  MAX_WIDTH = 400;

type
  TfrmDataInspector = class(TForm)
    aclMain            : TActionList;
    actHideEmptyFields : TAction;
    dscMain            : TDataSource;
    mniHideEmptyFields : TMenuItem;
    pnlMain            : TPanel;
    ppmMain            : TPopupMenu;

    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure FormShow(Sender: TObject);

    procedure FInspectorGetCellText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FInspectorSetEditText(
      Sender    : TObject;
      Cell      : TGridCell;
      var Value : string
    );
    procedure FInspectorGetCellReadOnly(
      Sender           : TObject;
      Cell             : TGridCell;
      var CellReadOnly : Boolean
    );
    procedure FInspectorEditCanModify(
      Sender        : TObject;
      Cell          : TGridCell;
      var CanModify : Boolean
    );
    procedure FInspectorGetEditStyle(
      Sender    : TObject;
      Cell      : TGridCell;
      var Style : TGridEditStyle
    );
    procedure FInspectorGetCheckKind(
      Sender        : TObject;
      Cell          : TGridCell;
      var CheckKind : TGridCheckKind
    );
    procedure FInspectorGetCheckAlignment(
      Sender             : TObject;
      Cell               : TGridCell;
      var CheckAlignment : TAlignment
    );
    procedure FInspectorGetCheckState(
      Sender         : TObject;
      Cell           : TGridCell;
      var CheckState : TCheckBoxState
    );
    procedure FInspectorCheckClick(
      Sender : TObject;
      Cell   : TGridCell
    );
    procedure FInspectorEditCanShow(
      Sender      : TObject;
      Cell        : TGridCell;
      var CanShow : Boolean
    );

    procedure actHideEmptyFieldsExecute(Sender: TObject);

  strict private
    FHideEmptyFields : Boolean;
    FData            : IData;
    FNonEmptyFields  : TObjectList<TField>;
    FInspector       : TInspector;

    procedure SetHideEmptyFields(const Value: Boolean);
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetDataSet: TDataSet;
    function GetFieldCount: Integer;

  protected
    procedure AutoSizeWidth;
    procedure UpdateFieldList;

    function IsCellReadOnly(const ACell: TGridCell): Boolean; inline;
    function IsCellCheckBox(const ACell: TGridCell): Boolean; inline;

    function FieldOf(const ACell: TGridCell): TField;

  public
    constructor Create(AOwner: TComponent; AData: IData = nil); reintroduce;
    procedure UpdateView;
    procedure BeforeDestruction; override;

    property DataSet : TDataSet
      read GetDataSet;

    property Data: IData
      read GetData write SetData;

    property HideEmptyFields: Boolean
      read FHideEmptyFields write SetHideEmptyFields;

    property FieldCount: Integer
      read GetFieldCount;
  end;

implementation

{$R *.dfm}

uses
  System.Math;

{$REGION 'construction and destruction'}
constructor TfrmDataInspector.Create(AOwner: TComponent; AData: IData);
begin
  inherited Create(AOwner);
  FNonEmptyFields := TObjectList<TField>.Create(False);
  FInspector := TInspector.Create(Self);

  FInspector.Color      := clWhite;
  FInspector.FlatBorder := True;
  FInspector.Parent     := pnlMain;
  FInspector.Align      := alClient;

  FInspector.OnGetCellText       := FInspectorGetCellText;
  FInspector.OnSetEditText       := FInspectorSetEditText;
  FInspector.OnGetCellReadOnly   := FInspectorGetCellReadOnly;
  FInspector.OnEditCanModify     := FInspectorEditCanModify;
  FInspector.OnGetEditStyle      := FInspectorGetEditStyle;
  FInspector.OnGetCheckKind      := FInspectorGetCheckKind;
  FInspector.OnGetCheckAlignment := FInspectorGetCheckAlignment;
  FInspector.OnGetCheckState     := FInspectorGetCheckState;
  FInspector.OnCheckClick        := FInspectorCheckClick;
  FInspector.OnEditCanShow       := FInspectorEditCanShow;
  Data := AData;
end;

procedure TfrmDataInspector.BeforeDestruction;
begin
  FreeAndNil(FNonEmptyFields);
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Data|--------------------------------------------------------------------

function TfrmDataInspector.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmDataInspector.SetData(const Value: IData);
begin
  if Assigned(Value) then
  begin
    FData := Value;
    dscMain.DataSet := FData.DataSet;
    UpdateView;
  end;
end;

//---|DataSet|-----------------------------------------------------------------

function TfrmDataInspector.GetDataSet: TDataSet;
begin
  if Assigned(FData) then
    Exit(FData.DataSet)
  else
    Exit(nil);
end;

procedure TfrmDataInspector.SetHideEmptyFields(const Value: Boolean);
begin
  if Value <> HideEmptyFields then
  begin
    FHideEmptyFields := Value;
    UpdateView;
  end;
end;

function TfrmDataInspector.GetFieldCount: Integer;
begin
  if HideEmptyFields then
    Exit(FNonEmptyFields.Count)
  else
    Exit(IfThen(Assigned(DataSet), DataSet.FieldCount, 0));
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmDataInspector.actHideEmptyFieldsExecute(Sender: TObject);
begin
  HideEmptyFields := actHideEmptyFields.Checked;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDataInspector.FormShow(Sender: TObject);
begin
  UpdateView;
end;

procedure TfrmDataInspector.dscMainDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then
    UpdateView;
end;

//---|FInspector|-----------------------------------------------------------------

procedure TfrmDataInspector.FInspectorGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  if Cell.Col = 0 then
    Value := FieldOf(Cell).FieldName
  else
  begin
    if IsCellCheckBox(Cell) then
      Value := ''
    else
      Value := FieldOf(Cell).AsString;
  end;
end;

procedure TfrmDataInspector.FInspectorGetCheckAlignment(Sender: TObject;
  Cell: TGridCell; var CheckAlignment: TAlignment);
begin
  CheckAlignment := taCenter;
end;

procedure TfrmDataInspector.FInspectorGetCheckKind(Sender: TObject;
  Cell: TGridCell; var CheckKind: TGridCheckKind);
begin
  if IsCellCheckBox(Cell) then
    CheckKind := gcCheckBox;
end;

procedure TfrmDataInspector.FInspectorGetCheckState(Sender: TObject;
  Cell: TGridCell; var CheckState: TCheckBoxState);
begin
  if IsCellCheckBox(Cell) then
  begin
    if FieldOf(Cell).AsBoolean then
      CheckState := cbChecked
    else
      CheckState := cbUnchecked;
  end;
end;

procedure TfrmDataInspector.FInspectorGetEditStyle(Sender: TObject;
  Cell: TGridCell; var Style: TGridEditStyle);
begin
  Style := geEllipsis;
end;

procedure TfrmDataInspector.FInspectorSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
  if not IsCellReadOnly(Cell) and (Value <> FieldOf(Cell).AsString) then
  begin
    DataSet.Edit;
    DataSet.Fields[Cell.Row].AsString := Value;
    DataSet.Post;
  end;
end;

procedure TfrmDataInspector.FInspectorGetCellReadOnly(Sender: TObject;
  Cell: TGridCell; var CellReadOnly: Boolean);
begin
  CellReadOnly := IsCellReadOnly(Cell);
end;

procedure TfrmDataInspector.FInspectorCheckClick(Sender: TObject; Cell: TGridCell);
var
  F: TField;
begin
  inherited;
  if IsCellCheckBox(Cell) then
  begin
    F := FieldOf(Cell);
    DataSet.Edit;
    if F.IsNull then
       F.Value := 1
    else
    begin
      if F.Value = 0 then
        F.Value := 1
      else
        F.Value := 0
    end;
    if DataSet.State in dsEditModes then
      DataSet.Post;
  end;
end;

procedure TfrmDataInspector.FInspectorEditCanModify(Sender: TObject;
  Cell: TGridCell; var CanModify: Boolean);
begin
  CanModify := not IsCellReadOnly(Cell);
end;

procedure TfrmDataInspector.FInspectorEditCanShow(Sender: TObject; Cell: TGridCell;
  var CanShow: Boolean);
begin
  CanShow := not IsCellCheckBox(Cell);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDataInspector.AutoSizeWidth;
var
  Left  : Integer;
  Right : Integer;
begin
  if Assigned(DataSet) and DataSet.Active and not DataSet.IsEmpty then
  begin
    Left  := FInspector.GetColumnMaxWidth(0, False, False) + 2;
    Right := FInspector.GetColumnMaxWidth(1, False, False) + 2;
    if Right > MAX_WIDTH then
      Right := MAX_WIDTH;
    FInspector.Columns[0].Width := Left;
    FInspector.Columns[1].Width := Right;
    Width := Left + Right + 32;
  end;
end;

function TfrmDataInspector.FieldOf(const ACell: TGridCell): TField;
begin
  if ACell.Row < FieldCount then
  begin
    if HideEmptyFields then
      Result := FNonEmptyFields[ACell.Row]
    else
      Result := DataSet.Fields[ACell.Row];
  end
  else
    Exit(nil);
end;

function TfrmDataInspector.IsCellCheckBox(const ACell: TGridCell): Boolean;
begin
  Result := (ACell.Col = 1) and (ACell.Row < FieldCount)
    and (Data as IDisplayData).IsCheckBoxField(FieldOf(ACell).FieldName);
end;

function TfrmDataInspector.IsCellReadOnly(const ACell: TGridCell): Boolean;
begin
  Result := (ACell.Col = 0) or IsCellCheckBox(ACell) or FieldOf(ACell).ReadOnly;
end;

procedure TfrmDataInspector.UpdateFieldList;
var
  F: TField;
begin
  FNonEmptyFields.Clear;
  if Assigned(FData) and Assigned(DataSet) and not DataSet.IsEmpty then
  begin
    for F in DataSet.Fields do
    begin
      if (not F.IsNull) and (Trim(F.AsString) <> '') then
        FNonEmptyFields.Add(F);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmDataInspector.UpdateView;
begin
  if Assigned(FData) then
  begin
    FInspector.LockUpdate;
    try
      UpdateFieldList;
      FInspector.Rows.Count := FieldCount;
      FInspector.UpdateEditContents(False);
      AutoSizeWidth;
    finally
      FInspector.UnLockUpdate(True);
    end;
  end;
end;
{$ENDREGION}

end.

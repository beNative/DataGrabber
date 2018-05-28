{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.Data.ResultSet;

{ A (decorator) class composed of a dataset with some extra information. }

interface

uses
  System.SysUtils,
  Data.DB,

  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  Spring.Collections, Spring.Collections.Lists,

  DataGrabber.Interfaces;

type
  TResultSet = class(TInterfacedObject, IResultSet)
  private
    FDataSet               : TFDMemTable; // reference to a returned TFDMemTable instance
    FData                  : IData;
    FConstantFields        : IList<TField>;
    FEmptyFields           : IList<TField>;
    FNonEmptyFields        : IList<TField>;
    FHiddenFields          : IList<TField>;
    FFavoriteFields        : IList<TField>;
    FConstantFieldsVisible : Boolean;
    FEmptyFieldsVisible    : Boolean;
    FHiddenFieldCount      : Integer;

    {$REGION 'property access methods'}
    function GetConstantFields: IList<TField>;
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFields: IList<TField>;
    function GetEmptyFieldsVisible: Boolean;
    function GetHiddenFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;
    function GetShowFavoriteFieldsOnly: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    procedure SetShowFavoriteFieldsOnly(const Value: Boolean);
    function GetDataSet: TFDDataSet;
    function GetData: IData;
    {$ENDREGION}

  protected
    procedure UpdateFieldLists;
    function ShowAllFields: Boolean;
    procedure InitFields(ADataSet: TDataSet);
    procedure InitField(AField: TField);

  public
    constructor Create(
      AData               : IData;
      AFDDataSetReference : IFDDataSetReference
    );
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property DataSet: TFDDataSet
      read GetDataSet;

    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;

    property HiddenFields: IList<TField>
      read GetHiddenFields;

    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;
  end;

  TResultSets = class(TList<IResultSet>)
  end;

implementation

uses
  Spring;

{$REGION 'construction and destruction'}
procedure TResultSet.AfterConstruction;
begin
  inherited AfterConstruction;
  FConstantFields := TCollections.CreateObjectList<TField>(False);
  FEmptyFields    := TCollections.CreateObjectList<TField>(False);
  FNonEmptyFields := TCollections.CreateObjectList<TField>(False);
  FFavoriteFields := TCollections.CreateObjectList<TField>(False);
  FHiddenFields   := TCollections.CreateObjectList<TField>(False);
  FConstantFieldsVisible := True;
  FEmptyFieldsVisible    := True;
  UpdateFieldLists;
end;

procedure TResultSet.BeforeDestruction;
begin
  FData := nil;
  FreeAndNil(FDataSet);
  inherited BeforeDestruction;
end;

constructor TResultSet.Create(AData: IData; AFDDataSetReference:
  IFDDataSetReference);
begin
  inherited Create;
  Guard.CheckNotNull(AData, 'AData');
  Guard.CheckNotNull(AFDDataSetReference, 'AFDDataSetReference');
  FData := AData;
  FDataSet := TFDMemTable.Create(nil);;
  FDataSet.Data := AFDDataSetReference;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TResultSet.GetData: IData;
begin
  Result := FData;
end;

function TResultSet.GetDataSet: TFDDataSet;
begin
  Result := FDataSet;
end;

function TResultSet.GetConstantFields: IList<TField>;
begin
  Result := FConstantFields;
end;

function TResultSet.GetEmptyFields: IList<TField>;
begin
  Result := FEmptyFields;
end;

function TResultSet.GetHiddenFields: IList<TField>;
begin
  Result := FHiddenFields;
end;

function TResultSet.GetNonEmptyFields: IList<TField>;
begin
  Result := FNonEmptyFields;
end;

function TResultSet.GetEmptyFieldsVisible: Boolean;
begin
  Result := FEmptyFieldsVisible;
end;

procedure TResultSet.SetEmptyFieldsVisible(const Value: Boolean);
begin
  if Value <> EmptyFieldsVisible then
  begin
    FEmptyFieldsVisible := Value;
    UpdateFieldLists;
    InitFields(DataSet);
  end;
end;

function TResultSet.GetShowFavoriteFieldsOnly: Boolean;
begin

end;

procedure TResultSet.SetShowFavoriteFieldsOnly(const Value: Boolean);
begin

end;

function TResultSet.GetConstantFieldsVisible: Boolean;
begin
  Result := FConstantFieldsVisible;
end;

procedure TResultSet.SetConstantFieldsVisible(const Value: Boolean);
begin
  if Value <> ConstantFieldsVisible then
  begin
    FConstantFieldsVisible := Value;
    UpdateFieldLists;
    InitFields(DataSet);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TResultSet.InitField(AField: TField);
var
  B : Boolean;
begin
  B := True;
//  if ShowFavoriteFieldsOnly then
//    B := FFavoriteFields.Contains(AField);
  if B and not ConstantFieldsVisible then
    B := not FConstantFields.Contains(AField);
  if B and not EmptyFieldsVisible then
    B := not FEmptyFields.Contains(AField);
  if not B and not FHiddenFields.Contains(AField) then
    FHiddenFields.Add(AField);
  B := not FHiddenFields.Contains(AField);
  AField.Visible := B;
end;

procedure TResultSet.InitFields(ADataSet: TDataSet);
var
  Field : TField;
begin
  FHiddenFieldCount := 0;
  for Field in ADataSet.Fields do
  begin
    InitField(Field);
  end;
end;

function TResultSet.ShowAllFields: Boolean;
var
  F : TField;
begin
  Result := False;
  DataSet.DisableControls;
  try
    FConstantFieldsVisible := True;
    FEmptyFieldsVisible    := True;
    for F in DataSet.Fields do
    begin
      if not F.Visible then
      begin
        F.Visible := True;
        Result := True;
      end;
    end;
    FHiddenFields.Clear;
    FHiddenFieldCount := 0;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TResultSet.UpdateFieldLists;
var
  S        : string;
  T        : string;
  F        : TField;
  LIsEmpty : Boolean;
  LIsConst : Boolean;
begin
  DataSet.DisableControls;
  FConstantFields.Clear;
  FEmptyFields.Clear;
  FNonEmptyFields.Clear;
  try
    if DataSet.FindFirst then
    begin
      for F in DataSet.Fields do
      begin
        // constant fields
        DataSet.FindFirst;
        S := F.AsString;
        LIsConst := True;
        LIsEmpty := F.IsNull or F.AsString.IsEmpty;
        while (LIsConst or LIsEmpty) and DataSet.FindNext do
        begin
          T := F.AsString;
          LIsConst := LIsConst and (S = T);
          LIsEmpty := LIsEmpty and (F.IsNull or T.IsEmpty);
        end;
        if LIsConst then
          FConstantFields.Add(F);
        if LIsEmpty then
          FEmptyFields.Add(F)
        else
          FNonEmptyFields.Add(F);
      end;
    end;
  finally
    DataSet.EnableControls;
  end;
end;
{$ENDREGION}

end.

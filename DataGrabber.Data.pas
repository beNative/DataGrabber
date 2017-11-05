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

unit DataGrabber.Data;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB, Data.Win.ADODB,
  Datasnap.DBClient, Datasnap.Provider,

  Spring, Spring.Collections,

  ts.Interfaces, ts.Data,

  DataGrabber.Interfaces;

type
  TdmData = class(TdmCustomModule, IData, IFieldLists, IFieldVisiblity)
  private
    FConstantFields         : IList<TField>;
    FEmptyFields            : IList<TField>;
    FNonEmptyFields         : IList<TField>;
    FFavoriteFields         : IList<TField>;
    FConstantFieldsVisible  : Boolean;
    FEmptyFieldsVisible     : Boolean;
    FShowFavoriteFieldsOnly : Boolean;

    function GetConstantFields: IList<TField>;
    function GetEmptyFields: IList<TField>;
    function GetNonEmptyFields: IList<TField>;
    function GetConstantFieldsVisible: Boolean;
    function GetEmptyFieldsVisible: Boolean;
    function GetShowFavoriteFieldsOnly: Boolean;
    procedure SetConstantFieldsVisible(const Value: Boolean);
    procedure SetEmptyFieldsVisible(const Value: Boolean);
    procedure SetShowFavoriteFieldsOnly(const Value: Boolean);

  protected
    procedure InitFields(ADataSet: TDataSet); override;
    procedure UpdateFieldLists;

  public
    procedure AfterConstruction; override;

    procedure InitField(AField: TField); override;

    { IFieldLists }
    property ConstantFields: IList<TField>
      read GetConstantFields;

    property EmptyFields: IList<TField>
      read GetEmptyFields;

    property NonEmptyFields: IList<TField>
      read GetNonEmptyFields;


    { IFieldVisibility }
    property ConstantFieldsVisible: Boolean
      read GetConstantFieldsVisible write SetConstantFieldsVisible;

    property EmptyFieldsVisible: Boolean
      read GetEmptyFieldsVisible write SetEmptyFieldsVisible;

    property ShowFavoriteFieldsOnly: Boolean
      read GetShowFavoriteFieldsOnly write SetShowFavoriteFieldsOnly;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TdmData.AfterConstruction;
begin
  inherited AfterConstruction;
  FConstantFields := TCollections.CreateObjectList<TField>(False);
  FEmptyFields    := TCollections.CreateObjectList<TField>(False);
  FNonEmptyFields := TCollections.CreateObjectList<TField>(False);
  FFavoriteFields := TCollections.CreateObjectList<TField>(False);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TdmData.GetConstantFields: IList<TField>;
begin
  Result := FConstantFields;
end;

function TdmData.GetConstantFieldsVisible: Boolean;
begin
  Result := FConstantFieldsVisible;
end;

procedure TdmData.SetConstantFieldsVisible(const Value: Boolean);
begin
  if Value <> ConstantFieldsVisible then
  begin
    FConstantFieldsVisible := Value;
    UpdateFieldLists;
  end;
end;

function TdmData.GetShowFavoriteFieldsOnly: Boolean;
begin
  Result := FShowFavoriteFieldsOnly;
end;

procedure TdmData.SetShowFavoriteFieldsOnly(const Value: Boolean);
begin
  if Value <> ShowFavoriteFieldsOnly then
  begin
    FShowFavoriteFieldsOnly := Value;
    InitFields(DataSet);
  end;
end;

function TdmData.GetEmptyFieldsVisible: Boolean;
begin
  Result := FEmptyFieldsVisible;
end;

procedure TdmData.SetEmptyFieldsVisible(const Value: Boolean);
begin
  if Value <> EmptyFieldsVisible then
  begin
    FEmptyFieldsVisible := Value;
    UpdateFieldLists;
  end;
end;

function TdmData.GetEmptyFields: IList<TField>;
begin
  Result := FEmptyFields;
end;

function TdmData.GetNonEmptyFields: IList<TField>;
begin
  Result := FNonEmptyFields;
end;
{$ENDREGION}

{$REGION 'private methods'}
{ Updates field lists that depend on the fetched data. }

procedure TdmData.UpdateFieldLists;
var
  B : Boolean;
  S : string;
  T : string;
  F : TField;
begin
  FConstantFields.Clear;
  FEmptyFields.Clear;
  FNonEmptyFields.Clear;
  DataSet.DisableControls;
  try
    if DataSet.FindFirst then
    begin
      for F in DataSet.Fields do
      begin
        // constant fields
        DataSet.FindFirst;
        B := True;
        S := F.AsString;;
        while B and DataSet.FindNext do
        begin
          T := F.AsString;
          B := S = T;
        end;
        if B then
          FConstantFields.Add(F);
        // empty fields
        DataSet.FindFirst;
        S := F.AsString;
        B := (S = '') or (S = '0') or (S = 'False');
        while B and DataSet.FindNext do
        begin
          S := F.AsString;
          B := (S = '') or (S = '0') or (S = 'False');
        end;
        if B then
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

{$REGION 'protected methods'}
procedure TdmData.InitField(AField: TField);
var
  B: Boolean;
begin
  inherited;
  B := True;
//  if ShowFavoriteFieldsOnly then
//    B := FFavoriteFields.Contains(AField);
  if B and not ConstantFieldsVisible then
    B := not FConstantFields.Contains(AField);
  if B and not EmptyFieldsVisible then
    B := not FEmptyFields.Contains(AField);
  AField.Visible := B;
end;

procedure TdmData.InitFields(ADataSet: TDataSet);
begin
  if ADataSet = DataSet then
    UpdateFieldLists;
  inherited InitFields(ADataSet);
end;
{$ENDREGION}

end.

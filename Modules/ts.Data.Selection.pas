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

unit ts.Data.Selection;

{ Class intended to make aggregated objects from a list. This object implements
  the IDataSelection interface.
  Instances of this class are intended to be integrated in modules that
  implement the IData interface. }

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  DataSnap.DBClient,

  ts.Interfaces, ts.Classes.KeyValues;

type
  TDataSelection = class(TAggregatedObject, IDataSelection)
  private
    FSelectionDataSet  : TClientDataSet;
    FSelectedRecords   : TtsKeyValues;
    FMaxSelectionCount : Integer;

  protected
    function GetData: IData;
    function GetSelectedRecords: TtsKeyValues;
    function GetSelectionDataSet: TDataSet;
    function GetMaxSelectionCount: Integer;
    procedure SetMaxSelectionCount(const Value: Integer);

  public
    constructor Create(const AData : IData);
    destructor Destroy; override;

    { IDataSelection }
    procedure PrepareSelectionDataSet;
    procedure ResetSelectionDataSet;
    procedure Update(
      const AFieldName  : string;
      const AFieldValue : Variant
    );

    property Data: IData
      read GetData;

    property SelectionDataSet : TDataSet
      read GetSelectionDataSet;

    property SelectedRecords : TtsKeyValues
      read GetSelectedRecords;

    property MaxSelectionCount : Integer
      read GetMaxSelectionCount write SetMaxSelectionCount;

  end;

implementation

uses
  System.Variants,

  ts.DBUtils;

{$REGION 'construction and destruction'}
constructor TDataSelection.Create(const AData: IData);
begin
  inherited Create(AData);
  FSelectionDataSet := TClientDataSet.Create(nil);
  FSelectedRecords  := TtsKeyValues.Create;
end;

destructor TDataSelection.Destroy;
begin
  FSelectionDataSet.Free;
  FSelectedRecords.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Data|--------------------------------------------------------------------

function TDataSelection.GetData: IData;
begin
  Result := Controller as IData;
end;

//---|MaxSelectionCount|-------------------------------------------------------

function TDataSelection.GetMaxSelectionCount: Integer;
begin
  Result := FMaxSelectionCount;
end;

procedure TDataSelection.SetMaxSelectionCount(const Value: Integer);
begin
  FMaxSelectionCount := Value;
end;

//---|SelectedRecords|---------------------------------------------------------

function TDataSelection.GetSelectedRecords: TtsKeyValues;
begin
  Result := FSelectedRecords;
end;

//---|SelectionDataSet|--------------------------------------------------------

function TDataSelection.GetSelectionDataSet: TDataSet;
begin
  Result := FSelectionDataSet;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TDataSelection.PrepareSelectionDataSet;
var
  I, J    : Integer;
  DataSet : TDataSet;
begin
  DataSet := Data.DataSet;
  FSelectionDataSet.DisableControls;
  DataSet.DisableControls;
  try
    FSelectionDataSet.Data := Null;
    FSelectionDataSet.Fields.Clear;
    FSelectionDataSet.FieldDefs.Clear;
    // REMARK : This line was added to be sure the FieldDefs are up-to-date.
    // Sometimes (after preparing a report) definitions were removed from the
    // list.
    DataSet.FieldDefs.Update;
    FSelectionDataSet.FieldDefs.Assign(DataSet.FieldDefs);

    for I := 0 to FSelectionDataSet.FieldDefs.Count - 1 do
    begin
      FSelectionDataSet.FieldDefs[I].Attributes :=
        FSelectionDataSet.FieldDefs[I].Attributes - [faReadOnly];
      if FSelectionDataSet.FieldDefs[I].DataType = ftAutoInc then
        FSelectionDataSet.FieldDefs[I].DataType := ftInteger;
    end;
    if FSelectionDataSet.FieldDefs.Count > 0 then
    begin
      FSelectionDataSet.Active := False;
      FSelectionDataSet.CreateDataSet;
    end;
    for I := 0 to FSelectionDataSet.Fields.Count - 1 do
    begin
      FSelectionDataSet.Fields[I].Required := False;
      FSelectionDataSet.Fields[I].ReadOnly := False;
      FSelectionDataSet.Fields[I].Visible := DataSet.Fields[I].Visible;
      FSelectionDataSet.Fields[I].DisplayLabel := DataSet.Fields[I].DisplayLabel;
    end;
    for I := 0 to SelectedRecords.Count - 1 do
    begin
      DataSet[(Data as IUpdatable).KeyName] := SelectedRecords.Items[I].Value;
      FSelectionDataSet.Append;
      for J := 0 to FSelectionDataSet.FieldCount - 1 do
      begin
        // even though the FieldDef's faReadOnly Attribute is unset, the created
        // field can still be readonly.
        FSelectionDataSet.Fields[J].Value :=
          DataSet.FieldByName(FSelectionDataSet.Fields[J].FieldName).Value;
      end;
      FSelectionDataSet.Post;
    end;
  finally
    FSelectionDataSet.EnableControls;
    DataSet.EnableControls;
  end;
end;

procedure TDataSelection.ResetSelectionDataSet;
begin
  FSelectionDataSet.EmptyDataSet;
end;

{ Updates the fieldvalue for a given field and table to the database for all
  records in the selection dataset. }

procedure TDataSelection.Update(const AFieldName: string;
  const AFieldValue: Variant);
var
  SL      : TStringList;
  KV      : TtsKeyValues;
  I       : Integer;
  DataSet : TDataSet;
begin
  if Supports(Data, IUpdatable) then
  begin
    DataSet := Data.DataSet;
    KV      := SelectedRecords;
    SL      := TStringList.Create;
    try
      for I := 0 to KV.Count - 1 do
        SL.Add(KV.Items[I].Name);
      DataSet.DisableControls;
      (Data as IUpdatable).BeginUpdate;
      try
        UpdateRecords(Data as IConnection,
                      (Data as IUpdatable).TableName,
                      AFieldName,
                      AFieldValue,
                      SL.CommaText);
        DataSet.Refresh;
        DataSet.Edit;
      finally
        (Data as IUpdatable).EndUpdate;
        DataSet.EnableControls;
      end;
    finally
      SL.Free;
    end;
  end;
end;
{$ENDREGION}

end.

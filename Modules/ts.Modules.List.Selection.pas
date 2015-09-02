unit ts.Modules.List.Selection;

{ Class intended to make aggregated objects from a list. This object implements
  the IListSelection interface.
  Instances of this class are intended to be integrated in modules that
  implement the IData interface. }

//*****************************************************************************

interface

uses
  Classes, SysUtils,

  DB, DBClient,

  ts.Classes.KeyValues,

  ts.Modules.Interfaces;

//=============================================================================

type
  TListSelection = class(TAggregatedObject, IListSelection)
  private
    FSelectionDataSet : TClientDataSet;
    FSelectedRecords  : TtsKeyValues;

  protected
    function GetData: IData;
    function GetSelectedRecords: TtsKeyValues;
    function GetSelectionDataSet: TDataSet;
    function GetMaxSelectionCount: Integer;
    procedure SetMaxSelectionCount(const Value: Integer);    

  public
    constructor Create(const AData : IData);
    destructor Destroy; override;

    { IListSelection }
    procedure PrepareSelectionDataSet;
    procedure ResetSelectionDataSet;
    procedure Update(const ATableName  : string;
                     const AFieldName  : string;
                     const AFieldValue : Variant);

    property Data: IData
      read GetData;

    property SelectionDataSet : TDataSet
      read GetSelectionDataSet;

    property SelectedRecords : TtsKeyValues
      read GetSelectedRecords;

    property MaxSelectionCount : Integer
      read GetMaxSelectionCount write SetMaxSelectionCount;

  end;

//*****************************************************************************

implementation

uses
  Variants,

  ts.DBUtils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TListSelection.Create(const AData: IData);
begin
  inherited Create(AData);
  FSelectionDataSet := TClientDataSet.Create(nil);
  FSelectedRecords  := TtsKeyValues.Create;
end;

//-----------------------------------------------------------------------------

destructor TListSelection.Destroy;
begin
  FSelectionDataSet.Free;
  FSelectedRecords.Free;
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|Data|--------------------------------------------------------------------

function TListSelection.GetData: IData;
begin
  Result := Controller as IData;
end;

//---|MaxSelectionCount|-------------------------------------------------------

function TListSelection.GetMaxSelectionCount: Integer;
begin
  Result := 0;
end;

procedure TListSelection.SetMaxSelectionCount(const Value: Integer);
begin

end;

//---|SelectedRecords|---------------------------------------------------------

function TListSelection.GetSelectedRecords: TtsKeyValues;
begin
  Result := FSelectedRecords;
end;

//---|SelectionDataSet|--------------------------------------------------------

function TListSelection.GetSelectionDataSet: TDataSet;
begin
  Result := FSelectionDataSet;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TListSelection.PrepareSelectionDataSet;
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
      DataSet.Bookmark := BytesOf(SelectedRecords.Items[I].Value);
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

//-----------------------------------------------------------------------------

procedure TListSelection.ResetSelectionDataSet;
begin
  FSelectionDataSet.EmptyDataSet;
end;

//-----------------------------------------------------------------------------

{ Updates the fieldvalue for a given field and table to the database for all
  records in the selection dataset. }

procedure TListSelection.Update(const ATableName : string;
  const AFieldName: string; const AFieldValue: Variant);
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
        UpdateRecords((Data as IADOConnection).ADOConnection,
                      ATableName,
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

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.
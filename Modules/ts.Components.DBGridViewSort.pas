{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Components.DBGridViewSort;

{ Author: Tim Sinaeve }

{ Helper component to implement sortable columns in a DB Gridview.
  (DDuce.Components.GridView, DDuce.Components.DBGridView).
 }

{
  KNOWN ISSUES
  - when one of the redirected event properties of the gridview was already
    assigned to an eventhandler, this assignment will disappear in the form
    designer after assigning the DBGridView property of the
    TtsCustomDBGridViewSort component. (probably solved by now)
  - on opening / closing the dataset we should clear the current indexname
      => detect state changes of the dataset with a datasource?
      => clear FSortedFieldName and FSortDirection

  TODO
    - use anonymous callback function to implement sorting logic on the dataset,
      so we can remove depencies on TClientDataSet and TZAbstractRODataset
}

interface

uses
  System.Classes,
  Vcl.Graphics,

  Data.DB, Data.Win.ADODB,
  Datasnap.DBClient,

  DDuce.Components.GridView, DDuce.Components.DBGridView;

const
  DEFAULT_SORTED_COLUMN_COLOR = $00E8E8E8;
    //$00E9E9E9; // a light shade of gray
  DEFAULT_ALPHA_BLEND_VALUE   = 128;

type
  TtsCustomDBGridViewSort = class(TComponent)
  private
    FDBGridView               : TDBGridView;
    FOnBeforeSort             : TNotifyEvent;
    FOnAfterSort              : TNotifyEvent;
    FOnGetSortDirection       : TGridSortDirectionEvent;
    FOnHeaderClick            : TGridHeaderClickEvent;
    FOnGetCellColors          : TGridCellColorsEvent;
    FSortDirection            : TGridSortDirection;
    FOnDataLayoutChanged      : TNotifyEvent;
    FSortedFieldName          : string;
    FSortedColumnColor        : TColor;
    FAlphaBlendValue          : Byte;
    FSortedColumnColorEnabled : Boolean;
    FGotoFirstAfterSort       : Boolean;

    procedure SetDBGridView(const Value: TDBGridView);
    procedure SetSortDirection(const Value: TGridSortDirection);
    procedure SetSortedFieldName(const Value: string);
    procedure SetSortedColumnColor(const Value: TColor);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetSortedColumnColorEnabled(const Value: Boolean);
    procedure SetGotoFirstAfterSort(const Value: Boolean);
    function GetDataSet: TDataSet;

  public
    constructor Create(AOwner: TComponent); override;

  protected
    // event handlers
    procedure OnGetSortDirection(Sender: TObject; Section: TGridHeaderSection;
      var SortDirection: TGridSortDirection);
    procedure OnHeaderClick(Sender: TObject; Section: TGridHeaderSection);
    procedure OnGetCellColors(
      Sender : TObject;
      Cell   : TGridCell;
      Canvas : TCanvas
    );
    procedure OnDataLayoutChanged(Sender: TObject);

    // protected methods
    procedure AssignProperties;
    procedure RestoreProperties;
    procedure Notification(
      AComponent : TComponent;
      Operation  : TOperation
    ); override;

    function SortDataSet(
      const ASortedFieldName : string;
      var   ADescending      : Boolean
    ): Boolean;

    function InternalSortDataSet(
            ADataSet         : TDataSet;
      const ASortedFieldName : string;
      var   ADescending      : Boolean
    ): Boolean; virtual; abstract;

    // event dispatch methods
    procedure DoGetSortDirection(Section: TGridHeaderSection;
      var SortDirection: TGridSortDirection); virtual;
    procedure DoHeaderClick(Section: TGridHeaderSection); virtual;
    procedure DoGetCellColors(Cell: TGridCell; Canvas: TCanvas); virtual;

    procedure DoBeforeSort; dynamic;
    procedure DoAfterSort; dynamic;

    property DataSet : TDataSet
      read GetDataSet;

    property DBGridView : TDBGridView
      read FDBGridView write SetDBGridView;

    property SortedFieldName : string
      read FSortedFieldName write SetSortedFieldName;

    property SortDirection : TGridSortDirection
      read FSortDirection write SetSortDirection;

    property SortedColumnColor : TColor
      read FSortedColumnColor
      write SetSortedColumnColor
      default DEFAULT_SORTED_COLUMN_COLOR;

    property SortedColumnColorEnabled : Boolean
      read FSortedColumnColorEnabled
      write SetSortedColumnColorEnabled
      default False;

    property AlphaBlendValue : Byte
      read FAlphaBlendValue
      write SetAlphaBlendValue
      default DEFAULT_ALPHA_BLEND_VALUE;

    property GotoFirstAfterSort : Boolean
      read FGotoFirstAfterSort
      write SetGotoFirstAfterSort
      default True;

    property OnBeforeSort : TNotifyEvent
      read FOnBeforeSort write FOnBeforeSort;

    property OnAfterSort : TNotifyEvent
      read FOnAfterSort write FOnAfterSort;
  end;

  TtsDBGridViewSort = class(TtsCustomDBGridViewSort)
  protected
    function InternalSortDataSet(
            ADataSet         : TDataSet;
      const ASortedFieldName : string;
      var   ADescending      : Boolean): Boolean; override;
  published
    property DBGridView;
    property SortedFieldName;
    property SortDirection;
    property SortedColumnColor;
    property SortedColumnColorEnabled;
    property AlphaBlendValue;
    property GotoFirstAfterSort;
    property OnBeforeSort;
    property OnAfterSort;
  end;

implementation

uses
  Vcl.Forms, Vcl.Controls,

    //ZAbstractRODataSet,

  ts.Utils, ts.DBUtils;

{$REGION 'TtsCustomDBGridViewSort'}
{$REGION 'construction and destruction'}
constructor TtsCustomDBGridViewSort.Create(AOwner: TComponent);
begin
  inherited;
  FSortedFieldName          := '';
  FSortDirection            := gsNone;
  FSortedColumnColor        := DEFAULT_SORTED_COLUMN_COLOR;
  FSortedColumnColorEnabled := False;
  FGotoFirstAfterSort       := True;
  FAlphaBlendValue          := DEFAULT_ALPHA_BLEND_VALUE;
  AssignProperties;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TtsCustomDBGridViewSort.SetAlphaBlendValue(const Value: Byte);
begin
  FAlphaBlendValue := Value;
end;

function TtsCustomDBGridViewSort.GetDataSet: TDataSet;
begin
  if Assigned(FDBGridView) and Assigned(FDBGridView.DataSource) then
    Result := FDBGridView.DataSource.DataSet
  else
    Result := nil;
end;

procedure TtsCustomDBGridViewSort.SetDBGridView(const Value: TDBGridView);
begin
  if Value <> FDBGridView then
  begin
    RestoreProperties;
    FDBGridView := Value;
    AssignProperties;
  end;
end;

procedure TtsCustomDBGridViewSort.SetGotoFirstAfterSort(const Value: Boolean);
begin
  FGotoFirstAfterSort := Value;
end;

procedure TtsCustomDBGridViewSort.SetSortDirection(
  const Value: TGridSortDirection);
begin
  if Value <> SortDirection then
  begin
    FSortDirection := Value;
  end;
end;

procedure TtsCustomDBGridViewSort.SetSortedColumnColor(const Value: TColor);
begin
  if Value <> SortedColumnColor then
  begin
    FSortedColumnColor := Value;
  end;
end;

procedure TtsCustomDBGridViewSort.SetSortedColumnColorEnabled(
  const Value: Boolean);
begin
  if Value <> SortedColumnColorEnabled then
  begin
    FSortedColumnColorEnabled := Value;
  end;
end;

procedure TtsCustomDBGridViewSort.SetSortedFieldName(const Value: string);
begin
  if Value <> SortedFieldName then
  begin
    FSortedFieldName := Value;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TtsCustomDBGridViewSort.DoAfterSort;
begin
  if Assigned(FOnAfterSort) then
    FOnAfterSort(Self);
end;

procedure TtsCustomDBGridViewSort.DoBeforeSort;
begin
  if Assigned(FOnBeforeSort) then
    FOnBeforeSort(Self);
end;

procedure TtsCustomDBGridViewSort.DoGetCellColors(Cell: TGridCell;
  Canvas: TCanvas);
var
  S : string;
begin
  if FSortedColumnColorEnabled and
     not FDBGridView.IsRowMultiSelected(Cell.Row) and
     not FDBGridView.IsCellHighlighted(Cell) then
  begin // assign a color to the sorted column
    S := FDBGridView.Columns[Cell.Col].FieldName;
    if (S <> '') and (S = FSortedFieldName) then
    // Abs(Canvas.Brush.Color) is used because predefined colors (from Windows)
    // are always negative values.
      Canvas.Brush.Color := MixColors(SortedColumnColor,
                                      Abs(Canvas.Brush.Color),
                                      AlphaBlendValue);
  end
end;

procedure TtsCustomDBGridViewSort.DoGetSortDirection(
  Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
var
  Field : TField;
begin
  if Section.ColumnIndex < FDBGridView.Columns.Count then
  begin
    Field := FDBGridView.Columns[Section.ColumnIndex].Field;
    if Assigned(Field) then
    begin
      if Field.FieldName = FSortedFieldName then
        SortDirection := FSortDirection;
    end;
  end;
end;

procedure TtsCustomDBGridViewSort.DoHeaderClick(Section: TGridHeaderSection);
var
  bDesc : Boolean;
  Field : TField;
begin
  Screen.Cursor := crSQLWait;
  try
    Field := FDBGridView.Columns[Section.ColumnIndex].Field;
    if Assigned(Field) and (Field.FieldKind = fkData) then
    begin
      DoBeforeSort;
      FSortedFieldName := Field.FieldName;
      if SortDataSet(FSortedFieldName, bDesc) then
        if bDesc then
          FSortDirection := gsDescending
        else
          FSortDirection := gsAscending
      else
      begin
        FSortDirection   := gsNone;
        FSortedFieldName := '';
      end;
      if GotoFirstAfterSort then
        DataSet.First;
    end
    else
    begin
      FSortDirection   := gsNone;
      FSortedFieldName := '';
    end;
    // TODO: should maybe only be triggered after a successful sort operation.
    DoAfterSort;
  finally
    Screen.Cursor := crDefault;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TtsCustomDBGridViewSort.SortDataSet(const ASortedFieldName: string;
  var ADescending: Boolean): Boolean;
var
  DS: TDataSet;
begin
  if Assigned(FDBGridView) and Assigned(FDBGridView.DataSource) and
     Assigned(FDBGridView.DataSource.DataSet) then
  begin
    DS := FDBGridView.DataSource.DataSet;
    Result := InternalSortDataSet(DS, ASortedFieldName, ADescending);
  end
  else
    Result := False;
end;

procedure TtsCustomDBGridViewSort.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDBGridView) then
    FDBGridView := nil;
end;

procedure TtsCustomDBGridViewSort.AssignProperties;
begin
  if Assigned(FDBGridView) and (not (csDesigning in ComponentState)) then
  begin
    FOnGetSortDirection  := FDBGridView.OnGetSortDirection;
    FOnHeaderClick       := FDBGridView.OnHeaderClick;
    FOnGetCellColors     := FDBGridView.OnGetCellColors;
    FOnDataLayoutChanged := FDBGridView.OnDataLayoutChanged;
    FDBGridView.OnGetSortDirection  := OnGetSortDirection;
    FDBGridView.OnHeaderClick       := OnHeaderClick;
    FDBGridView.OnGetCellColors     := OnGetCellColors;
    FDBGridView.OnDataLayoutChanged := OnDataLayoutChanged;
  end;
end;

procedure TtsCustomDBGridViewSort.OnDataLayoutChanged(Sender: TObject);
begin
  if Assigned(FOnDataLayoutChanged) then
    FOnDataLayoutChanged(Sender);
  SortedFieldName := '';
  SortDirection   := gsNone;
end;

procedure TtsCustomDBGridViewSort.OnGetCellColors(Sender: TObject;
  Cell: TGridCell; Canvas: TCanvas);
begin
  if Assigned(FOnGetCellColors) then
    FOnGetCellColors(Sender, Cell, Canvas);
  DoGetCellColors(Cell, Canvas);
end;

procedure TtsCustomDBGridViewSort.OnGetSortDirection(Sender: TObject;
  Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
begin
  if Assigned(FOnGetSortDirection) then
    FOnGetSortDirection(Sender, Section, SortDirection);
  DoGetSortDirection(Section, SortDirection);
end;

procedure TtsCustomDBGridViewSort.OnHeaderClick(Sender: TObject;
  Section: TGridHeaderSection);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(Sender, Section);
  DoHeaderClick(Section);
end;

procedure TtsCustomDBGridViewSort.RestoreProperties;
begin
  if Assigned(FDBGridView) and (not (csDesigning in ComponentState)) then
  begin
    FDBGridView.OnGetSortDirection  := FOnGetSortDirection;
    FDBGridView.OnHeaderClick       := FOnHeaderClick;
    FDBGridView.OnGetCellColors     := FOnGetCellColors;
    FDBGridView.OnDataLayoutChanged := FOnDataLayoutChanged;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TtsDBGridViewSort'}
{$REGION 'protected methods'}
function TtsDBGridViewSort.InternalSortDataSet(ADataSet: TDataSet;
  const ASortedFieldName: string; var ADescending: Boolean): Boolean;
//var
//  ZD: TZAbstractRODataset;
begin
  if ADataSet is TClientDataSet then
    Result :=
      SortClientDataSet(TClientDataSet(ADataSet), ASortedFieldName, ADescending)
  else if ADataSet is TADODataSet then
  begin
    ADescending := False;
    if ASortedFieldName = TADODataSet(ADataSet).Sort then
    begin
      TADODataSet(ADataSet).Sort := ASortedFieldName + ' DESC';
      ADescending := True;
    end
    else
      TADODataSet(ADataSet).Sort := ASortedFieldName;
    Result := True;
  end
//  else if DataSet is TZAbstractRODataset then
//  begin
//    ZD := TZAbstractRODataset(ADataSet);
//    ADescending := False;
//    if (ASortedFieldName = ZD.SortedFields) and (ZD.SortType = stAscending) then
//    begin
//      ADescending := True;
//      ZD.SortType := stDescending;
//    end
//    else
//    begin
//      ZD.SortedFields := ASortedFieldName;
//      ZD.SortType     := stAscending;
//    end;
//    Result := True;
//  end
  else
  begin
    Result := False;
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

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

unit ts.Classes.ListReport.Columns;

{
 This unit consists of a TCollection/TCollectionItem implementation represen-
 ting the properties of a list report.
 It has no notion of the underlying report engine.

 The TtsListReportColumns class holds the list of column properties and
 report settings:
   - Items (the actual columns of the report)

 each column item (TtsListReportColumn) holds the following properties:
   - Font
   - Title
   - TitleAlignment
   - TitleCellColor
   - TitleBorders
   - Alignment
   - CellBorders
   - CellColor
   - Index
   - SuppressRepeated [False]
   - HideZeros [False]
}

{
  TODO
    - check default values for properties.
    - property Sequence is probably of no use

  REMARKS:
    - clWhite color for backgrounds caused overlapping problems to lines in the
      column header. After adjusting the default color to clNone the frame of
      the column header is drawn as it should.
}

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,
  Vcl.Graphics,

  ts.Classes.Alignment;

const
  DEFAULT_WIDTH    = 50;
  DEFAULT_MAXWIDTH = 500;
  DEFAULT_MINWIDTH = 0;

type
  TtsCellBorder = (cbLeft, cbRight, cbTop, cbBottom);
  TtsCellBorders = set of TtsCellBorder;

  TtsDataType = (dtNumeric, dtDateTime, dtString);

  TtsListReportColumns = class;
  TtsListReportColumn  = class;

  TtsRptColumnCountChangeEvent = procedure(
    Sender : TObject;
    ACount : Integer
  ) of object;
  TtsRptColumnChangeEvent = procedure(
    Sender  : TObject;
    AColumn : TtsListReportColumn
  ) of object;

  TtsListReportColumn = class(TCollectionItem)
  private
    FCellBorders      : TtsCellBorders;
    FCellColor        : TColor;
    FDataAlignment    : TtsAlignment;
    FDataType         : TtsDataType;
    FDisplayFormat    : string;
    FFieldName        : string;
    FFont             : TFont;
    FFooterAlignment  : TtsAlignment;
    FFooterBorders    : TtsCellBorders;
    FFooterCellColor  : TColor;
    FFooterExpression : string;
    FFooterFont       : TFont;
    FFooterVisible    : Boolean;
    FHeight           : Extended;
    FHideZeros        : Boolean;
    FMaxWidth         : Extended;
    FMinWidth         : Extended;
    FName             : string;
    FSequence         : Integer;
    FSuppressRepeated : Boolean;
    FTitle            : string;
    FTitleAlignment   : TtsAlignment;
    FTitleBorders     : TtsCellBorders;
    FTitleCellColor   : TColor;
    FTitleFont        : TFont;
    FVisible          : Boolean;
    FWidth            : Extended;

    procedure SetCollection(const Value: TtsListReportColumns); reintroduce;
    function GetCollection: TtsListReportColumns;

    function PointsFieldLength(ACharCount: Integer; AChar: Char; AFont: TFont):
      Integer;
    procedure SetMaxWidth(const Value: Extended);
    procedure SetMinWidth(const Value: Extended);

  protected
    // protected property access methods
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;
    procedure SetFont(const Value: TFont); virtual;
    procedure SetTitle(const Value: string); virtual;
    procedure SetWidth(const Value: Extended); virtual;
    procedure SetCellColor(const Value: TColor); virtual;
    function GetFieldLength: Integer; virtual;
    procedure SetFieldLength(const Value: Integer); virtual;
    procedure SetTitleAlignment(const Value: TtsAlignment); virtual;
    procedure SetTitleFont(const Value: TFont); virtual;
    procedure SetDataAlignment(const Value: TtsAlignment); virtual;
    procedure SetCellBorders(const Value: TtsCellBorders); virtual;
    procedure SetDataType(const Value: TtsDataType); virtual;
    procedure SetDisplayFormat(const Value: string); virtual;
    procedure SetHeight(const Value: Extended); virtual;
    procedure SetSequence(const Value: Integer); virtual;
    function GetFieldName: string; virtual;
    procedure SetFieldName(const Value: string); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetTitleBorders(const Value: TtsCellBorders); virtual;
    procedure SetTitleCellColor(const Value: TColor);
    procedure SetHideZeros(const Value: Boolean);
    procedure SetSuppressRepeated(const Value: Boolean);
    procedure SetFooterFont(const Value: TFont);
    procedure SetFooterCellColor(const Value: TColor);
    procedure SetFooterExpression(const Value: string);
    procedure SetFooterBorders(const Value: TtsCellBorders);
    procedure SetFooterAlignment(const Value: TtsAlignment);
    procedure SetFooterVisible(const Value: Boolean);

    procedure ValueChanged(Sender: TObject); dynamic;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { collection that owns the instance of current TtsListReportColumn item }
    property Collection: TtsListReportColumns
      read GetCollection write SetCollection;

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

  published
    property CellBorders: TtsCellBorders
      read FCellBorders write SetCellBorders;

    property FieldName: string
      read GetFieldName write SetFieldName;

    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write SetDisplayName;

    { The columntitle to be displayed in the columnheader. }
    property Title: string
      read FTitle write SetTitle;

    { Column width in pixels }
    property Width: Extended
      read FWidth write SetWidth;

    property MinWidth: Extended
      read FMinWidth write SetMinWidth;

    property MaxWidth: Extended
      read FMaxWidth write SetMaxWidth;

    { Line height in pixels. }
    property Height: Extended
      read FHeight write SetHeight;

    { Font used for column data. }
    property Font: TFont
      read FFont write SetFont;

    property Visible: Boolean
      read FVisible write SetVisible default True;

    { Font used for column header. }
    property TitleFont: TFont
      read FTitleFont write SetTitleFont;

    property TitleAlignment: TtsAlignment
      read FTitleAlignment write SetTitleAlignment;

    property TitleCellColor: TColor
      read FTitleCellColor write SetTitleCellColor default clNone;

    property DataAlignment: TtsAlignment
      read FDataAlignment write SetDataAlignment;

    property FieldLength: Integer
      read GetFieldLength write SetFieldLength;

    property HideZeros: Boolean
      read FHideZeros write SetHideZeros default False;

    property TitleBorders: TtsCellBorders
      read FTitleBorders write SetTitleBorders;

    property CellColor: TColor
      read FCellColor write SetCellColor default clNone;

    property DataType: TtsDataType
      read FDataType write SetDataType default dtString;

    property DisplayFormat: string
      read FDisplayFormat write SetDisplayFormat;

    property Sequence: Integer
      read FSequence write SetSequence;

    property SuppressRepeated: Boolean
      read FSuppressRepeated write SetSuppressRepeated default False;

    property FooterFont: TFont
      read FFooterFont write SetFooterFont;

    property FooterCellColor: TColor
       read FFooterCellColor write SetFooterCellColor;

    property FooterExpression: string
      read FFooterExpression write SetFooterExpression;

    property FooterBorders: TtsCellBorders
      read FFooterBorders write SetFooterBorders;

    property FooterAlignment: TtsAlignment
      read FFooterAlignment write SetFooterAlignment;

    property FooterVisible: Boolean
      read FFooterVisible write SetFooterVisible default False;

    property Index;
 end;

  TtsListReportColumnClass = class of TtsListReportColumn;

  { TtsListReportColumns inherits from TOwnedCollection to show the items in
    the Object Treeview of the Delphi IDE at designtime. }
  TtsListReportColumns = class(TOwnedCollection)
  private
    FOnColumnCountChange : TtsRptColumnCountChangeEvent;
    FOnChange            : TtsRptColumnChangeEvent;
    // property access methods
    function GetItem(Index: Integer): TtsListReportColumn;
    procedure SetItem(Index: Integer; const Value: TtsListReportColumn);

  protected
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;

  public
    constructor Create(AOwner : TPersistent);

    function Add: TtsListReportColumn;
    function Insert(Index: Integer): TtsListReportColumn;
    function Owner: TComponent; reintroduce;

    procedure UpdateItems;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TtsListReportColumn;
    function Find(const AName: string): TtsListReportColumn;
    function FindField(const AFieldName : string): TtsListReportColumn;
    function FindSequence(ASequence: Integer): TtsListReportColumn;

    // public properties
    { The TCollectionItem decended class of the Items in the collection }
    property ItemClass;

    { provides indexed access to the list of collection items }
    property Items[Index: Integer]: TtsListReportColumn
      read GetItem write SetItem; default;

    property OnColumnCountChange: TtsRptColumnCountChangeEvent
      read FOnColumnCountChange write FOnColumnCountChange;

    property OnChange: TtsRptColumnChangeEvent
      read FOnChange write FOnChange;
  end;

implementation

uses
  System.Math,
  Vcl.Forms,

  ts.Utils, ts.Classes.ListReport;

resourcestring
  SDuplicateName      = 'Item with name %s already exist in collection of %s';

{$REGION 'TtsListReportColumns'}
{$REGION 'construction and destruction'}
constructor TtsListReportColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtsListReportColumn);
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Items|-------------------------------------------------------------------

function TtsListReportColumns.GetItem(Index: Integer): TtsListReportColumn;
begin
  Result := inherited Items[Index] as TtsListReportColumn;
end;

procedure TtsListReportColumns.SetItem(Index: Integer;
  const Value: TtsListReportColumn);
begin
  Items[Index].Assign(Value);
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Responds when items are added to or removed from the collection. }

procedure TtsListReportColumns.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
// The actions that can be used to respond to are:
//   - cnAdded      : an item was just added to the collection
//   - cnExtracting : an item is about to be removed from the collection (but
//                    not freed)
//   - cnDeleting   : an item is about to be removed from the collection and
//                    then freed
  if (Action in [cnAdded, cnExtracting]) and Assigned(FOnColumnCountChange) then
  begin
    if Action = cnAdded then
      FOnColumnCountChange(Self, Count)
    else
      FOnColumnCountChange(Self, Count - 1);
  end;
end;

{ Constructs a unique itemname for a new collection-item. }

procedure TtsListReportColumns.SetItemName(Item: TCollectionItem);
var
  S : string;
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  S := Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
  TtsListReportColumn(Item).Name := S;
end;

{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection }

procedure TtsListReportColumns.Update(AItem: TCollectionItem);
begin
// make necessary adjustments when items in the collection change
// Update gets called from TCollection.Changed.
  if Assigned(OnChange) then
    if Assigned(AItem) then
      OnChange(Self, AItem as TtsListReportColumn)
    else
     UpdateItems;
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new TtsListReportColumn instance to the TtsListReportColumns
  collection. }

function TtsListReportColumns.Add: TtsListReportColumn;
begin
  Result := inherited Add as TtsListReportColumn;
end;

{ Inserts a new TtsListReportColumn instance to the TtsListReportColumns
  collection before position specified with Index }

function TtsListReportColumns.Insert(Index: Integer): TtsListReportColumn;
begin
  Result := inherited Insert(Index) as TtsListReportColumn;
end;

function TtsListReportColumns.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

function TtsListReportColumns.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil }

function TtsListReportColumns.FindItemID(ID: Integer): TtsListReportColumn;
begin
  Result := inherited FindItemID(ID) as TtsListReportColumn;
end;

function TtsListReportColumns.Find(const AName: string): TtsListReportColumn;
var
  I : Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

function TtsListReportColumns.FindField(
  const AFieldName: string): TtsListReportColumn;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].FieldName = AFieldName then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

procedure TtsListReportColumns.UpdateItems;
var
  I : Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Changed(False);
end;

function TtsListReportColumns.FindSequence(
  ASequence: Integer): TtsListReportColumn;
var
  I : Integer;
begin
  I := 0;
  Result := nil;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].Sequence = ASequence then
      Result := Items[I];
    Inc(I);
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TtsListReportColumn'}
{$REGION 'construction and destruction'}
constructor TtsListReportColumn.Create(Collection: TCollection);
var
  FRListReport : TtsFRListReport;
begin
  inherited Create(Collection);
  FRListReport := nil;
  if Assigned(Collection) and (Collection.Owner <> nil) then
    FRListReport := Collection.Owner as TtsFRListReport;

  FFooterVisible       := False;
  FVisible             := True;
  FHideZeros           := False;
  FSuppressRepeated    := False;
  FFont                := TFont.Create;
  FFont.OnChange       := ValueChanged;
  FTitleFont           := TFont.Create;
  FTitleFont.OnChange  := ValueChanged;
  FFooterFont          := TFont.Create;
  FFooterFont.OnChange := ValueChanged;
  if Assigned(FRListReport) then
  begin
    FFont.Assign(FRListReport.DefaultFont);
    FTitleFont.Assign(FRListReport.DefaultTitleFont);
    FFooterFont.Assign(FRListReport.DefaultFooterFont);
  end
  else
  begin
    FFont.Name       := 'Arial';
    FTitleFont.Name  := 'Arial';
    FFooterFont.Name := 'Arial';
  end;
  FTitleAlignment           := TtsAlignment.Create;
  FTitleAlignment.OnChange  := ValueChanged;
  FTitleCellColor           := clNone;
  FTitleBorders             := [];
  FDataAlignment            := TtsAlignment.Create;
  FDataType                 := dtString;
  FDataAlignment.OnChange   := ValueChanged;
  FCellColor                := clNone;
  FCellBorders              := [];
  FFooterAlignment          := TtsAlignment.Create;
  FFooterAlignment.OnChange := ValueChanged;
  FFooterCellColor          := clNone;
  FFooterBorders            := [cbLeft, cbRight, cbTop, cbBottom];
  FWidth                    := DEFAULT_WIDTH;
  FMinWidth                 := DEFAULT_MINWIDTH;
  FMaxWidth                 := DEFAULT_MAXWIDTH;
end;

destructor TtsListReportColumn.Destroy;
begin
  FreeAndNil(FTitleAlignment);
  FreeAndNil(FDataAlignment);
  FreeAndNil(FFooterAlignment);
  FreeAndNil(FFont);
  FreeAndNil(FTitleFont);
  FreeAndNil(FFooterFont);
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Collection|--------------------------------------------------------------

function TtsListReportColumn.GetCollection: TtsListReportColumns;
begin
  Result := inherited Collection as TtsListReportColumns;
end;

procedure TtsListReportColumn.SetCollection(const Value: TtsListReportColumns);
begin
  inherited Collection := Value;
end;

//---|DisplayName|-------------------------------------------------------------

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

function TtsListReportColumn.GetDisplayName: string;
begin
  if FieldName <> '' then
    Result := FieldName
  else
    Result := FName;
end;

procedure TtsListReportColumn.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TtsListReportColumns) and
    (TtsListReportColumns(Collection).IndexOf(Value) >= 0) then
    raise Exception.CreateFmt(SDuplicateName, [Value, Collection.ClassName]);
  FName := Value;
  inherited;
end;

//---|Font|--------------------------------------------------------------------

procedure TtsListReportColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed(False);
end;

//---|FooterAlignment|---------------------------------------------------------

procedure TtsListReportColumn.SetFooterAlignment(const Value: TtsAlignment);
begin
  FFooterAlignment.Assign(Value);
  Changed(False);
end;

//---|FooterBorders|-----------------------------------------------------------

procedure TtsListReportColumn.SetFooterBorders(const Value: TtsCellBorders);
begin
  if Value <> FooterBorders then
  begin
    FFooterBorders := Value;
    Changed(False);
  end;
end;

//---|FooterCellColor|---------------------------------------------------------

procedure TtsListReportColumn.SetFooterCellColor(const Value: TColor);
begin
  if Value <> FooterCellColor then
  begin
    FFooterCellColor := Value;
    Changed(False);
  end;
end;

//---|FooterExpression|--------------------------------------------------------

procedure TtsListReportColumn.SetFooterExpression(const Value: string);
begin
  if Value <> FooterExpression then
  begin
    FFooterExpression := Value;
    Changed(False);
  end;
end;

//---|FooterFont|--------------------------------------------------------------

procedure TtsListReportColumn.SetFooterFont(const Value: TFont);
begin
  FFooterFont.Assign(Value);
  Changed(False);
end;

//---|FooterVisible|-----------------------------------------------------------

procedure TtsListReportColumn.SetFooterVisible(const Value: Boolean);
begin
  if Value <> FooterVisible then
  begin
    FFooterVisible := Value;
    Changed(False);
  end;
end;

//---|TitleFont|---------------------------------------------------------------

procedure TtsListReportColumn.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
  Changed(False);
end;

//---|Title|-------------------------------------------------------------------

procedure TtsListReportColumn.SetTitle(const Value: string);
begin
  if Value <> Title then
  begin
    FTitle := Value;
    Changed(False);
  end;
end;

//---|Width|-------------------------------------------------------------------

procedure TtsListReportColumn.SetWidth(const Value: Extended);
var
  V : Extended;
begin
  V := Value;
  if Value < FMinWidth then
    V := FMinWidth;
  if Value > FMaxWidth then
    V := FMaxWidth;
  if CompareValue(FWidth, V) <> 0 then
  begin
    FWidth := V;
    Changed(True);
  end;
end;

//---|Height|------------------------------------------------------------------

procedure TtsListReportColumn.SetHeight(const Value: Extended);
begin
  FHeight := Value;
  Changed(False);
end;

//---|HideZeros|---------------------------------------------------------------

procedure TtsListReportColumn.SetHideZeros(const Value: Boolean);
begin
  if Value <> HideZeros then
  begin
    FHideZeros := Value;
    Changed(False);
  end;
end;

//---|MaxWidth|----------------------------------------------------------------

procedure TtsListReportColumn.SetMaxWidth(const Value: Extended);
var
  V : Extended;
begin
  V := Value;
  if Value < FMinWidth then
    V := FMinWidth;
  if Value > 10000 then
    V := 10000;
  FMaxWidth := V;
  SetWidth(FWidth);
end;

//---|MinWidth|----------------------------------------------------------------

procedure TtsListReportColumn.SetMinWidth(const Value: Extended);
var
  V : Extended;
begin
  V := Value;
  if Value < 0 then
    V := 0;
  if Value > FMaxWidth then
    V := FMaxWidth;
  FMinWidth := V;
  SetWidth(FWidth);
end;

//---|TitleAlignment|----------------------------------------------------------

procedure TtsListReportColumn.SetTitleAlignment(const Value: TtsAlignment);
begin
  FTitleAlignment.Assign(Value);
  Changed(False);
end;

//---|DataAlignment|-----------------------------------------------------------

procedure TtsListReportColumn.SetDataAlignment(const Value: TtsAlignment);
begin
  FDataAlignment.Assign(Value);
  Changed(False);
end;

//---|FieldLength|-------------------------------------------------------------

function TtsListReportColumn.GetFieldLength: Integer;
begin
//  Result := FWidth div PointsFieldLength(1, 'W', FTitleFont);
  Result := 0;
end;

{ in cm }

procedure TtsListReportColumn.SetFieldLength(const Value: Integer);
begin
  Width  := PointsFieldLength(Value, 'O', FFont);
  Height := GetTextHeight('Wg', FFont);
end;

//---|CellBorders|-------------------------------------------------------------

procedure TtsListReportColumn.SetCellBorders(const Value: TtsCellBorders);
begin
  if Value <> CellBorders then
  begin
    FCellBorders := Value;
    Changed(False);
  end;
end;

//---|TitleBorders|------------------------------------------------------------

procedure TtsListReportColumn.SetTitleBorders(const Value: TtsCellBorders);
begin
  if Value <> TitleBorders then
  begin
    FTitleBorders := Value;
    Changed(False);
  end;
end;

//---|CellColor|---------------------------------------------------------------

procedure TtsListReportColumn.SetCellColor(const Value: TColor);
begin
  if Value <> CellColor then
  begin
    FCellColor := Value;
    Changed(False);
  end;
end;

//---|DataType|----------------------------------------------------------------

procedure TtsListReportColumn.SetDataType(const Value: TtsDataType);
begin
  if Value <> DataType then
  begin
    FDataType := Value;
    Changed(False);
  end;
end;

//---|DisplayFormat|-----------------------------------------------------------

procedure TtsListReportColumn.SetDisplayFormat(const Value: string);
begin
  if Value <> DisplayFormat then
  begin
    FDisplayFormat := Value;
    Changed(False);
  end;
end;

//---|Visible|-----------------------------------------------------------------

procedure TtsListReportColumn.SetVisible(const Value: Boolean);
begin
  if Value <> Visible then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

//---|Sequence|----------------------------------------------------------------

procedure TtsListReportColumn.SetSequence(const Value: Integer);
begin
  if Value <> Sequence then
  begin
    FSequence := Value;
    Changed(False);
  end;
end;

//---|SuppressRepeated|--------------------------------------------------------

procedure TtsListReportColumn.SetSuppressRepeated(const Value: Boolean);
begin
  if Value <> SuppressRepeated then
  begin
    FSuppressRepeated := Value;
    Changed(False);
  end;
end;

//---|FieldName|---------------------------------------------------------------

function TtsListReportColumn.GetFieldName: string;
begin
  Result := FFieldName;
end;

procedure TtsListReportColumn.SetFieldName(const Value: string);
begin
  if Value <> FieldName then
  begin
    FFieldName := Value;
    Changed(False);
  end;
end;

//---|TitleCellColor|----------------------------------------------------------

procedure TtsListReportColumn.SetTitleCellColor(const Value: TColor);
begin
  if Value <> TitleCellColor then
  begin
    FTitleCellColor := Value;
    Changed(False);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TtsListReportColumn.Assign(Source: TPersistent);
var
  C : TtsListReportColumn;
begin
 if Source is TtsListReportColumn then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   C := TtsListReportColumn(Source);
   try
     { <PropertyName> := C.<PropertyName> }
     CellBorders      := C.CellBorders;
     CellColor        := C.CellColor;
     DataType         := C.DataType;
     DisplayFormat    := C.DisplayFormat;
     FieldName        := C.FieldName;
     FooterBorders    := C.FooterBorders;
     FooterCellColor  := C.FooterCellColor;
     FooterExpression := C.FooterExpression;
     FooterVisible    := C.FooterVisible;
     Height           := C.Height;
     HideZeros        := C.HideZeros;
     MaxWidth         := C.MaxWidth;
     MinWidth         := C.MinWidth;
     Sequence         := C.Sequence;
     SuppressRepeated := C.SuppressRepeated;
     Title            := C.Title;
     TitleBorders     := C.TitleBorders;
     TitleCellColor   := C.TitleCellColor;
     Visible          := C.Visible;
     Width            := C.Width;
     Font.Assign(C.Font);
     FooterFont.Assign(C.FooterFont);
     TitleFont.Assign(C.TitleFont);
     DataAlignment.Assign(C.DataAlignment);
     FooterAlignment.Assign(C.FooterAlignment);
     TitleAlignment.Assign(C.TitleAlignment);
   finally
     if Assigned(Collection) then
       Collection.EndUpdate;
   end;
 end
 else
   inherited Assign(Source);
end;
{$ENDREGION}

{$REGION 'private methods'}
function TtsListReportColumn.PointsFieldLength(ACharCount: Integer;
  AChar: Char; AFont: TFont): Integer;
var
  Bitmap  : TBitmap;
  OldFont : TFont;
  Canvas  : TCanvas;
  S       : string;
begin
  Bitmap  := TBitmap.Create;
  Canvas  := Bitmap.Canvas;
  OldFont := TFont.Create;
  try
    OldFont.Assign(Canvas.Font);
    Canvas.Font.Assign(AFont);
    S := AChar;
    Result := ACharCount * Canvas.TextWidth(S);
    Canvas.Font.Assign(OldFont);
  finally
    Bitmap.Free;
    OldFont.Free;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TtsListReportColumn.ValueChanged(Sender: TObject);
begin
  Changed(False);
end;
{$ENDREGION}
{$ENDREGION}

end.

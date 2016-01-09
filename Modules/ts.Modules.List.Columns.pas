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

unit ts.Modules.List.Columns;

{ Persistable collection to store column settings of listings. }

interface

uses
  System.SysUtils, System.Classes;

type
  TtsListColumn = class;

  TtsListColumnsUpdateEvent = procedure(
    ASender : TObject;
    AItem   : TtsListColumn
  ) of object;

  TtsListColumns = class;

  TtsListColumn = class(TCollectionItem)
  private
    FName      : string;
    FWidth     : Integer;
    FVisible   : Boolean;
    FCaption   : string;
    FAlignment : TAlignment;
    FMaxWidth  : Integer;
    FMinWidth  : Integer;
    FFixedSize : Boolean;

    procedure SetCollection(const Value: TtsListColumns); reintroduce;
    function GetCollection: TtsListColumns;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetMinWidth(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetFixedSize(const Value: Boolean);

  protected
    procedure SetIndex(Value: Integer); override;
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;

    procedure Assign(Source: TPersistent); override;

    { Collection that owns the instance of current TtsListColumn item. }
    property Collection: TtsListColumns
      read GetCollection write SetCollection;

  published
    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write SetDisplayName;

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    property Alignment : TAlignment
      read FAlignment write SetAlignment;

    property FixedSize : Boolean
      read FFixedSize write SetFixedSize;

    { Width in characters. }
    property Width : Integer
      read FWidth write SetWidth;

    { Minimum width in characters. }
    property MinWidth : Integer
      read FMinWidth write SetMinWidth;

    { Maximum width in characters. }
    property MaxWidth : Integer
      read FMaxWidth write SetMaxWidth;

    property Caption: string
      read FCaption write SetCaption;

    property Visible: Boolean
      read FVisible write SetVisible;

  end;

  TtsListColumnClass = class of TtsListColumn;

  { TtsListColumns inherits from TOwnedCollection to show the items in
    the Object Treeview of the Delphi IDE at designtime. }

  TtsListColumns = class(TOwnedCollection)
  private
    FOnUpdate : TtsListColumnsUpdateEvent;

    // property access methods
    function GetItem(Index: Integer): TtsListColumn;
    procedure SetItem(Index: Integer; const Value: TtsListColumn);
    function GetOnUpdate: TtsListColumnsUpdateEvent;
    procedure SetOnUpdate(const Value: TtsListColumnsUpdateEvent);

  protected
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;

  public
    // constructors and destructors
    constructor Create(AOwner : TPersistent);

    function Add: TtsListColumn;
    function Insert(Index: Integer): TtsListColumn;
    function Owner: TComponent; reintroduce;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TtsListColumn;
    function Find(const AName: string): TtsListColumn;

    // public properties
    { The TCollectionItem decendant class of the collection items. }
    property ItemClass;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TtsListColumn
      read GetItem write SetItem; default;

    property OnUpdate : TtsListColumnsUpdateEvent
      read GetOnUpdate write SetOnUpdate;
  end;

implementation

uses
  Winapi.Windows;

const
  MAX_WIDTH = 500;

{$REGION 'TtsListColumns'}
{$REGION 'construction and destruction'}
constructor TtsListColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtsListColumn);
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Items|-------------------------------------------------------------------

function TtsListColumns.GetItem(Index: Integer): TtsListColumn;
begin
  Result := inherited Items[Index] as TtsListColumn;
end;

procedure TtsListColumns.SetItem(Index: Integer; const Value: TtsListColumn);
begin
  Items[Index].Assign(Value);
end;

//---|OnUpdate|----------------------------------------------------------------

function TtsListColumns.GetOnUpdate: TtsListColumnsUpdateEvent;
begin
  Result := FOnUpdate;
end;

procedure TtsListColumns.SetOnUpdate(
  const Value: TtsListColumnsUpdateEvent);
begin
  FOnUpdate := Value;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection }

procedure TtsListColumns.Update(AItem: TCollectionItem);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, AItem as TtsListColumn);
end;

{ Responds when items are added to or removed from the collection. }

procedure TtsListColumns.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
// The actions that can be used to respond to are:
//   - cnAdded      : an item was just added to the collection
//   - cnExtracting : an item is about to be removed from the collection (but
//                    not freed)
//   - cnDeleting   : an item is about to be removed from the collection and
//                    then freed
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new TtsListColumn instance to the TtsListColumns
  collection. }

function TtsListColumns.Add: TtsListColumn;
begin
  Result := inherited Add as TtsListColumn;
end;

{ Inserts a new TtsListColumn instance to the TtsListColumns
  collection before position specified with Index. }

function TtsListColumns.Insert(Index: Integer): TtsListColumn;
begin
  Result := inherited Insert(Index) as TtsListColumn;
end;

{ Constructs a unique itemname for a new collection item. }

procedure TtsListColumns.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TtsListColumn(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

function TtsListColumns.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

function TtsListColumns.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil. }

function TtsListColumns.FindItemID(ID: Integer): TtsListColumn;
begin
  Result := inherited FindItemID(ID) as TtsListColumn;
end;

function TtsListColumns.Find(const AName: string): TtsListColumn;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Name = AName then
      Result := Items[I];
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TtsListColumn'}
{$REGION 'construction and destruction'}
constructor TtsListColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  // Add your property storage initializations here.
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Alignment|---------------------------------------------------------------

procedure TtsListColumn.SetAlignment(const Value: TAlignment);
begin
  if Value <> Alignment then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

//---|Caption|-----------------------------------------------------------------

procedure TtsListColumn.SetCaption(const Value: string);
begin
  if Value <> Caption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

//---|Collection|--------------------------------------------------------------

function TtsListColumn.GetCollection: TtsListColumns;
begin
  Result := inherited Collection as TtsListColumns;
end;

procedure TtsListColumn.SetCollection(const Value: TtsListColumns);
begin
  inherited Collection := Value;
end;

//---|DisplayName|-------------------------------------------------------------

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

function TtsListColumn.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TtsListColumn.SetDisplayName(const Value: string);
begin
  FName := Value;
  inherited;
end;

//---|FixedSize|---------------------------------------------------------------

procedure TtsListColumn.SetFixedSize(const Value: Boolean);
begin
  if Value <> FixedSize then
  begin
    FFixedSize := Value;
    Changed(False);
  end;
end;

//---|Index|-------------------------------------------------------------------

procedure TtsListColumn.SetIndex(Value: Integer);
begin
  if Value <> Index then
  begin
    inherited;
    Changed(False);
  end;
end;

//---|MaxWidth|----------------------------------------------------------------

procedure TtsListColumn.SetMaxWidth(const Value: Integer);
var
  N : Integer;
begin
  if Value <> MaxWidth then
  begin
    N := Value;
    if N < MinWidth then
      N := FMinWidth;
    if N > MAX_WIDTH then
      N := MAX_WIDTH;
    FMaxWidth := N;
    SetWidth(FWidth);
    Changed(False);
  end;
end;

//---|MinWidth|----------------------------------------------------------------

procedure TtsListColumn.SetMinWidth(const Value: Integer);
var
  N : Integer;
begin
  if Value <> MinWidth then
  begin
    N := Value;
    if N < 0 then
      N := 0;
    if N > FMaxWidth then
      N := FMaxWidth;
    FMinWidth := N;
    SetWidth(FWidth);
    Changed(False);
  end;
end;

//---|Visible|-----------------------------------------------------------------

procedure TtsListColumn.SetVisible(const Value: Boolean);
begin
  if Value <> Visible then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

//---|Width|-------------------------------------------------------------------

procedure TtsListColumn.SetWidth(const Value: Integer);
var
  N : Integer;
begin
  N := Value;
  if N < FMinWidth then
    N := FMinWidth;
  if Value > FMaxWidth then
    N := FMaxWidth;
  if N <> Width then
  begin
    FWidth := N;
    Changed(False);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TtsListColumn.Assign(Source: TPersistent);
begin
 if (Source <> Self) and (Source is TtsListColumn) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     { <PropertyName> := TtsListColumn(Source).<PropertyName> }
   Alignment := TtsListColumn(Source).Alignment;
   Caption   := TtsListColumn(Source).Caption;
   Visible   := TtsListColumn(Source).Visible;
   Width     := TtsListColumn(Source).Width;
   MinWidth  := TtsListColumn(Source).MinWidth;
   MaxWidth  := TtsListColumn(Source).MaxWidth;
   finally
     if Assigned(Collection) then
       Collection.EndUpdate;
   end;
 end
 else
   inherited Assign(Source);
end;
{$ENDREGION}
{$ENDREGION}

end.


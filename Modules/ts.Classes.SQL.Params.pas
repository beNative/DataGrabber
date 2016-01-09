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

unit ts.Classes.SQL.Params;

interface

uses
  System.SysUtils, System.Classes;

type
  TSQLParams = class;

  TSQLParam = class(TCollectionItem)
  private
    FName  : string;
    FValue : Variant;
  protected
    procedure SetCollection(const Value: TSQLParams); reintroduce;
    function GetCollection: TSQLParams;
    procedure SetValue(const Value: Variant);
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;
  public
    // public methods
    procedure Assign(Source: TPersistent); override;

    // public properties
    { Collection that owns the instance of current TSQLParam item. }
    property Collection: TSQLParams
      read GetCollection write SetCollection;
  published
    // published properties
    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write SetDisplayName;

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    property Value : Variant
      read FValue write SetValue;
  end;

  TSQLParamClass = class of TSQLParam;

  { TSQLParams inherits from TOwnedCollection to show the items in
    the Object Treeview of the Delphi IDE at designtime. }
  TSQLParams = class(TOwnedCollection)
  private
    FOnChanged : TNotifyEvent;
  protected
    function GetItem(Index: Integer): TSQLParam;
    procedure SetItem(Index: Integer; const Value: TSQLParam);
    function GetItemValue(AName: string): Variant;
    procedure SetItemValue(AName: string; const Value: Variant);
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
  public
    // constructors and destructors
    constructor Create(AOwner : TPersistent);

    function Add: TSQLParam;
    function Insert(Index: Integer): TSQLParam;
    function Owner: TComponent; reintroduce;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TSQLParam;
    function Find(const AName: string): TSQLParam;

    // public properties
    { The TCollectionItem decended class of the Items in the collection. }
    property ItemClass;

    property ItemsByName[Name : string]: Variant
      read GetItemValue write SetItemValue; default;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TSQLParam
      read GetItem write SetItem;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;
  end;

implementation

uses
  Winapi.Windows,
  System.Variants;

resourcestring
  SParamNotFoundError = 'Error: Param with name ''%s'' not found';
  SDuplicateName      = 'Item with name %s already exist in collection of %s';

{$REGION 'TSQLParams'}
{$REGION 'construction and destruction'}
constructor TSQLParams.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSQLParam);
  FOnChanged := nil;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Items|-------------------------------------------------------------------

function TSQLParams.GetItem(Index: Integer): TSQLParam;
begin
  Result := inherited Items[Index] as TSQLParam;
end;

procedure TSQLParams.SetItem(Index: Integer; const Value: TSQLParam);
begin
  Items[Index].Assign(Value);
end;

//---|ItemValues|--------------------------------------------------------------

function TSQLParams.GetItemValue(AName: string): Variant;
var
  P : TSQLParam;
begin
  P := Find(AName);
  if Assigned(P) then
    Result := P.Value
  else
    Result := Unassigned;
end;

procedure TSQLParams.SetItemValue(AName: string; const Value: Variant);
var
  P : TSQLParam;
begin
  P := Find(AName);
  if Assigned(P) then
    P.Value := Value
  else
  begin
    P := Add;
    P.Name  := AName;
    P.Value := Value;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}

{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection. }

procedure TSQLParams.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change.
// Update gets called from TCollection.Changed.
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ Constructs a unique itemname for a new collection-item. }

procedure TSQLParams.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TSQLParam(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new TSQLParam instance to the TSQLParams collection. }

function TSQLParams.Add: TSQLParam;
begin
  Result := inherited Add as TSQLParam;
end;

{ Inserts a new TSQLParam instance to the TSQLParams collection before position
  specified with Index }

function TSQLParams.Insert(Index: Integer): TSQLParam;
begin
  Result := inherited Insert(Index) as TSQLParam;
end;

function TSQLParams.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

function TSQLParams.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil. }

function TSQLParams.FindItemID(ID: Integer): TSQLParam;
begin
  Result := inherited FindItemID(ID) as TSQLParam;
end;

function TSQLParams.Find(const AName: string): TSQLParam;
var
  I : Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TSQLParam'}
{$REGION 'property access methods'}
//---|Collection|--------------------------------------------------------------

function TSQLParam.GetCollection: TSQLParams;
begin
  Result := inherited Collection as TSQLParams;
end;

procedure TSQLParam.SetCollection(const Value: TSQLParams);
begin
  inherited Collection := Value;
end;

//---|DisplayName|-------------------------------------------------------------

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

function TSQLParam.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TSQLParam.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TSQLParams) and
    (TSQLParams(Collection).IndexOf(Value) >= 0) then
    raise Exception.CreateFmt(SDuplicateName, [Value, Collection.ClassName]);
  FName := Value;
  inherited;
end;

//---|Value|-------------------------------------------------------------------

procedure TSQLParam.SetValue(const Value: Variant);
begin
  FValue := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TSQLParam.Assign(Source: TPersistent);
begin
 if (Source <> Self) and (Source is TSQLParam) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     { <PropertyName> := TSQLParam(Source).<PropertyName> }
     Value := TSQLParam(Source).Value;
     Name  := TSQLParam(Source).Name;
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

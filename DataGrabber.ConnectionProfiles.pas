{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.ConnectionProfiles;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,

  DataGrabber.ConnectionSettings;

type
  TConnectionProfiles = class;

  TConnectionProfile = class(TCollectionItem)
  private
    FName               : string;
    FProfileColor       : TColor;
    FConnectionSettings : TConnectionSettings;

  protected
    {$REGION 'property access methods'}
    procedure SetCollection(const Value: TConnectionProfiles); reintroduce;
    function GetCollection: TConnectionProfiles;
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;
    {$ENDREGION}

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { Collection that owns the instance of current TConnectionProfile item. }
    property Collection: TConnectionProfiles
      read GetCollection write SetCollection;

  published
    { The name displayed in the collection editor at design time. }
    property Name: string
      read FName write SetDisplayName;

    { The name as shown in the user interface. }
    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    property ProfileColor: TColor
      read FProfileColor write FProfileColor default clWhite;

    property ConnectionSettings: TConnectionSettings
      read FConnectionSettings write FConnectionSettings;
  end;

  TConnectionProfileClass = class of TConnectionProfile;

  { TConnectionProfiles inherits from TOwnedCollection to show the items in
    the Object Treeview of the Delphi IDE at designtime. }

  TConnectionProfiles = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TConnectionProfile;
    procedure SetItem(Index: Integer; const Value: TConnectionProfile);
    procedure SetItemName(Item: TCollectionItem); override;

    procedure Update(AItem: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;
    function FindUniqueName(const Name: string): string;

  public
    constructor Create(AOwner : TPersistent);

    function Add: TConnectionProfile;
    function Insert(Index: Integer): TConnectionProfile;
    function Owner: TComponent; reintroduce;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TConnectionProfile;
    function Find(const AName: string): TConnectionProfile;

    { The TCollectionItem decendant class of the collection items. }
    property ItemClass;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TConnectionProfile
      read GetItem write SetItem; default;
  end;

implementation

{$REGION 'TConnectionProfiles'}
{$REGION 'construction and destruction'}
constructor TConnectionProfiles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TConnectionProfile);
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TConnectionProfiles.GetItem(Index: Integer): TConnectionProfile;
begin
  Result := inherited Items[Index] as TConnectionProfile;
end;

procedure TConnectionProfiles.SetItem(Index: Integer; const Value: TConnectionProfile);
begin
  Items[Index].Assign(Value);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TConnectionProfiles.FindUniqueName(const Name: string): string;
var
  I : Integer;
  S : string;
begin
  I := 0;
  S := Name;
  while Assigned(Find(S)) do
  begin
    Inc(I);
    S := Format('%s%d', [Name, I]);
  end;
  Result := S;
end;

{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection }

procedure TConnectionProfiles.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change
// Update gets called from TCollection.Changed.
end;

{ Responds when items are added to or removed from the collection. }

procedure TConnectionProfiles.Notify(Item: TCollectionItem; Action: TCollectionNotification);
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
{ Adds a new TConnectionProfile instance to the TConnectionProfiles
  collection. }

function TConnectionProfiles.Add: TConnectionProfile;
begin
  Result := inherited Add as TConnectionProfile;
end;

{ Inserts a new TConnectionProfile instance to the TConnectionProfiles
  collection before position specified with Index. }

function TConnectionProfiles.Insert(Index: Integer): TConnectionProfile;
begin
  Result := inherited Insert(Index) as TConnectionProfile;
end;

{ Constructs a unique itemname for a new collection item. }

procedure TConnectionProfiles.SetItemName(Item: TCollectionItem);
var
  S : string;
begin
  S := Copy(Item.ClassName, 2, Length(Item.ClassName));
  TConnectionProfile(Item).Name := FindUniqueName(S);
end;

function TConnectionProfiles.Owner: TComponent;
var
  LOwner : TPersistent;
begin
  LOwner := inherited Owner;
  if LOwner is TComponent then
    Result := TComponent(LOwner)
  else
    Result := nil;
end;

function TConnectionProfiles.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil. }

function TConnectionProfiles.FindItemID(ID: Integer): TConnectionProfile;
begin
  Result := inherited FindItemID(ID) as TConnectionProfile;
end;

function TConnectionProfiles.Find(const AName: string): TConnectionProfile;
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

{$REGION 'TConnectionProfile'}
{$REGION 'construction and destruction'}
constructor TConnectionProfile.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FConnectionSettings := TConnectionSettings.Create;
  FProfileColor       := clWhite;
end;

destructor TConnectionProfile.Destroy;
begin
  FreeAndNil(FConnectionSettings);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TConnectionProfile.GetCollection: TConnectionProfiles;
begin
  Result := inherited Collection as TConnectionProfiles;
end;

procedure TConnectionProfile.SetCollection(const Value: TConnectionProfiles);
begin
  inherited Collection := Value;
end;

function TConnectionProfile.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TConnectionProfile.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TConnectionProfiles) and
    (TConnectionProfiles(Collection).IndexOf(Value) >= 0) then
      raise Exception.CreateFmt('Duplicate name [%s]!', [Value]);
  FName := Value;
  inherited SetDisplayName(Value);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TConnectionProfile.Assign(Source: TPersistent);
var
  CP : TConnectionProfile;
begin
 if (Source <> Self) and (Source is TConnectionProfile) then
 begin
   CP := TConnectionProfile(Source);
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     ProfileColor     := CP.ProfileColor;
     ConnectionSettings.Assign(CP.ConnectionSettings);
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

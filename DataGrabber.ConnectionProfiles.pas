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

unit DataGrabber.ConnectionProfiles;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,

  ts.Classes.ConnectionSettings;

type
  TConnectionProfiles = class;

  TConnectionProfile = class(TCollectionItem)
  private
    FName               : string;
    FConnectionString   : string;
    FProfileColor       : TColor;
    FVisibleItems       : TStrings;
    FFavoriteFields     : TStrings;
    FConnectionSettings : TConnectionSettings;
    FConnectionType     : string;
    FFetchOnDemand      : Boolean;
    FPacketRecords      : Integer;
    FProviderMode       : Boolean;

  protected
    procedure SetCollection(const Value: TConnectionProfiles); reintroduce;
    function GetCollection: TConnectionProfiles;
    function GetVisibleItems: string;
    procedure SetVisibleItems(const Value: string);
    function GetFavoriteFields: string;
    procedure SetFavoriteFields(const Value: string);
    procedure SetConnectionType(const Value: string);
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;

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

    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    property ConnectionString: string
      read FConnectionString write FConnectionString;

    property ProfileColor: TColor
      read FProfileColor write FProfileColor default clWhite;

    property VisibleItems: string
      read GetVisibleItems write SetVisibleItems;

    property FavoriteFields: string
      read GetFavoriteFields write SetFavoriteFields;

    property ConnectionSettings: TConnectionSettings
      read FConnectionSettings write FConnectionSettings;

    property ProviderMode: Boolean
      read FProviderMode write FProviderMode;

    property FetchOnDemand: Boolean
      read FFetchOnDemand write FFetchOnDemand;

    property PacketRecords: Integer
      read FPacketRecords write FPacketRecords;

    property ConnectionType: string
      read FConnectionType write SetConnectionType;
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
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TConnectionProfile(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;

function TConnectionProfiles.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
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
  FVisibleItems := TStringList.Create;
  TStringList(FVisibleItems).Sorted     := True;
  TStringList(FVisibleItems).Duplicates := dupIgnore;
  FFavoriteFields := TStringList.Create;
  TStringList(FFavoriteFields).Sorted     := True;
  TStringList(FFavoriteFields).Duplicates := dupIgnore;
  FProfileColor                           := clWhite;
  // Add your property storage initializations here.
end;

destructor TConnectionProfile.Destroy;
begin
  FreeAndNil(FVisibleItems);
  FreeAndNil(FFavoriteFields);
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

procedure TConnectionProfile.SetConnectionType(const Value: string);
begin
  if Value <> ConnectionType then
  begin
    FConnectionType := Value;
    ConnectionSettings.Protocol := '';
  end;
end;

// By default, DisplayName is the name of the TCollectionItem descendant class
// of which the item is an instance. By providing a dedicated field each item
// in the Collection editor can be displayed with a unique name.

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
  inherited;
end;

function TConnectionProfile.GetFavoriteFields: string;
begin
  Result := FFavoriteFields.Text;
end;

procedure TConnectionProfile.SetFavoriteFields(const Value: string);
begin
  FFavoriteFields.Text := Value;
end;

function TConnectionProfile.GetVisibleItems: string;
begin
  Result := FVisibleItems.Text;
end;

procedure TConnectionProfile.SetVisibleItems(const Value: string);
begin
  FVisibleItems.Text := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TConnectionProfile.Assign(Source: TPersistent);
var
  CP: TConnectionProfile;
begin
 if (Source <> Self) and (Source is TConnectionProfile) then
 begin
   CP := TConnectionProfile(Source);
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     ProfileColor     := CP.ProfileColor;
     ConnectionString := CP.ConnectionString;
     ConnectionType   := CP.ConnectionType;
     VisibleItems     := CP.VisibleItems;
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

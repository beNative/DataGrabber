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

unit ts.Classes.ModuleManager;

{
  Collection holding information about application modules that are registered
  and can be loaded depending on application parameters.

  Each module can register itself to a globally iniitialized instance of this
  class.

  Each TtsModuleItem instance in the collection holds all information needed
  to create a given module.

  RegisterModule(
  RegisterForm(Name, FormClass, [Description])
  RegisterDataModule(Name, DataModuleClass, [Description])
}

{
  TODO:
    - FormClassName/DataModuleClassName ? (alternative to create using RTTI)
    - module versioning (using CVS tag information)
    - default printer for current module

    - make it generic
}

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms;

type
  TtsModuleManager  = class;

  TFormClass       = class of TCustomForm;
  TDataModuleClass = class of TDataModule;
  TModuleClass     = class of TComponent;

  TtsModuleItem = class(TCollectionItem)
  private
    FName            : string;
    FDisplayName     : string;
    FValue           : Variant;
    FFormClass       : TFormClass;
    FDataModuleClass : TDataModuleClass;
    FModuleClass     : TModuleClass;

  protected
    procedure SetName(const Value: string);
    procedure SetCollection(const Value: TtsModuleManager); reintroduce;
    function GetCollection: TtsModuleManager;
    procedure SetValue(const Value: Variant);
    procedure SetDisplayName(const Value: string); override;
    function GetDisplayName: string; override;

  public
    // public methods
    procedure Assign(Source: TPersistent); override;

    // public properties
    { Collection that owns the instance of current TtsModuleItem item. }
    property Collection: TtsModuleManager
      read GetCollection write SetCollection;

    { The item's key name. Code used to identify the application mode. In most
      cases this code will correspond to a commandline parameter. This code
      should be unique throughout the collection. }
    property Name: string
      read FName write SetName;

  published
    property DisplayName: string
      read GetDisplayName write SetDisplayName;

    { The item's key value. }
    property Value : Variant
      read FValue write SetValue;

    property FormClass : TFormClass
      read FFormClass write FFormClass;

    property DataModuleClass : TDataModuleClass
      read FDataModuleClass write FDataModuleClass;

    property ModuleClass : TModuleClass
      read FModuleClass write FModuleClass;
  end;

  TtsModuleManager = class(TOwnedCollection)
  private
    FOnChanged : TNotifyEvent;

  protected
    function GetItem(Index: Integer): TtsModuleItem;
    procedure SetItem(Index: Integer; const Value: TtsModuleItem);
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    function GetItemByName(Name: string): TtsModuleItem;
    procedure SetItemByName(Name: string; const Value: TtsModuleItem);

  public
    // constructors and destructors
    constructor Create(AOwner: TPersistent);

    function Add: TtsModuleItem;
    function Insert(Index: Integer): TtsModuleItem;
    function Owner: TComponent; reintroduce;

    function IndexOf(const AName: string): Integer; virtual;
    function FindItemID(ID: Integer): TtsModuleItem;
    function Find(const AName: string): TtsModuleItem;

    procedure RegisterModule(
            ADataModuleClass : TDataModuleClass;
            AFormClass       : TFormClass;
      const AName            : string;
      const ADisplayName     : string = '';
      const ADescription     : string = ''
    ); overload;

    procedure RegisterModule(
            AModuleClass : TModuleClass;
      const AName        : string;
      const ADisplayName : string = '';
      const ADescription : string = ''
    ); overload;

    procedure RegisterForm(
            AFormClass   : TFormClass;
      const AName        : string;
      const ADisplayName : string = '';
      const ADescription : string = ''
    );

    procedure RegisterDataModule(
            ADataModuleClass : TDataModuleClass;
      const AName            : string;
      const ADisplayName     : string = '';
      const ADescription     : string = ''
    );

    // public properties
    { The TCollectionItem decended class of the Items in the collection. }
    property ItemClass;

    property ItemsByName[Name : string]: TtsModuleItem
      read GetItemByName write SetItemByName; default;

    { Provides indexed access to the list of collection items. }
    property Items[Index: Integer]: TtsModuleItem
      read GetItem write SetItem;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;
  end;

function ModuleManager : TtsModuleManager;

implementation

uses
  Winapi.Windows,
  System.Variants;

resourcestring
  SDuplicateName = 'Item with name %s already exist in collection of %s';

var
  FModuleManager : TtsModuleManager;

{$REGION 'interfaced routines'}
{ Singleton instance. }

function ModuleManager : TtsModuleManager;
begin
  if not Assigned(FModuleManager) then
    FModuleManager := TtsModuleManager.Create(Application);
  Result := FModuleManager;
end;
{$ENDREGION}

{$REGION 'TtsModuleManager'}
{$REGION 'construction and destruction'}
constructor TtsModuleManager.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtsModuleItem);
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Items|-------------------------------------------------------------------

function TtsModuleManager.GetItem(Index: Integer): TtsModuleItem;
begin
  Result := inherited Items[Index] as TtsModuleItem;
end;

procedure TtsModuleManager.SetItem(Index: Integer; const Value: TtsModuleItem);
begin
  Items[Index].Assign(Value);
end;

//---|ItemsByName|-------------------------------------------------------------

function TtsModuleManager.GetItemByName(Name: string): TtsModuleItem;
var
  P : TtsModuleItem;
begin
  P := Find(Name);
  if Assigned(P) then
    Result := P
  else
    Result := nil;
end;

procedure TtsModuleManager.SetItemByName(Name: string;
  const Value: TtsModuleItem);
var
  P : TtsModuleItem;
begin
  P := Find(Name);
  if Assigned(P) then
    P.Assign(Value)
  else
  begin
    P := Add;
    P.Assign(Value);
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Overridden method from TCollection to make any necessary changes when the
  items in the collection change. This method is called automatically when an
  update is issued.
  Item = Item that changed. If the Item parameter is nil, then the change
         affects more than one item in the collection. }

procedure TtsModuleManager.Update(AItem: TCollectionItem);
begin
// Make necessary adjustments when items in the collection change.
// Update gets called from TCollection.Changed.
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ Constructs a unique itemname for a new collection-item. }

procedure TtsModuleManager.SetItemName(Item: TCollectionItem);
begin
// The Insert method calls SetItemName to initialize the Name property of items
// when it inserts them into the collection. This overridden version provides
// collection items with default names.
  TtsModuleItem(Item).Name :=
    Copy(Item.ClassName, 2, Length(Item.ClassName)) + IntToStr(Item.ID + 1);
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new TtsStarupInfoItem instance to the TtsStarupInfo collection. }

function TtsModuleManager.Add: TtsModuleItem;
begin
  Result := inherited Add as TtsModuleItem;
end;

{ Inserts a new TtsStarupInfoItem instance to the TtsStarupInfo collection before
  position specified with Index }

function TtsModuleManager.Insert(Index: Integer): TtsModuleItem;
begin
  Result := inherited Insert(Index) as TtsModuleItem;
end;

function TtsModuleManager.Owner: TComponent;
var
  AOwner: TPersistent;
begin
  AOwner := inherited Owner;
  if AOwner is TComponent then
    Result := TComponent(AOwner)
  else
    Result := nil;
end;

procedure TtsModuleManager.RegisterModule(ADataModuleClass: TDataModuleClass;
  AFormClass: TFormClass; const AName, ADisplayName, ADescription: string);
var
  MI : TtsModuleItem;
begin
  MI := Find(AName);
  if not Assigned(MI) then
  begin
    MI := Add;
    MI.Name := AName;
  end;
  if ADisplayName <> '' then
    MI.DisplayName := ADisplayName;
  if ADataModuleClass <> nil then
    MI.DataModuleClass := ADataModuleClass;
  if AFormClass <> nil then
    MI.FormClass := AFormClass;
end;

procedure TtsModuleManager.RegisterModule(AModuleClass: TModuleClass;
  const AName, ADisplayName, ADescription: string);
var
  MI : TtsModuleItem;
begin
  MI := Find(AName);
  if not Assigned(MI) then
  begin
    MI := Add;
    MI.Name := AName;
  end;
  if ADisplayName <> '' then
    MI.DisplayName := ADisplayName;
  if AModuleClass <> nil then
    MI.ModuleClass := AModuleClass;
end;

procedure TtsModuleManager.RegisterDataModule(ADataModuleClass:
  TDataModuleClass; const AName, ADisplayName, ADescription: string);
begin
  RegisterModule(ADataModuleClass, nil, AName, ADisplayName, ADescription);
end;

procedure TtsModuleManager.RegisterForm(AFormClass: TFormClass; const AName,
  ADisplayName, ADescription: string);
begin
  RegisterModule(nil, AFormClass, AName, ADisplayName, ADescription);
end;

function TtsModuleManager.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if AnsiCompareText((Items[Result]).Name, AName) = 0 then
      Exit;
  Result := -1;
end;

{ The FindItemID method returns the item in the collection whose ID property
  is passed to it as a parameter. If no item has the specified ID, FindItemID
  returns nil. }

function TtsModuleManager.FindItemID(ID: Integer): TtsModuleItem;
begin
  Result := inherited FindItemID(ID) as TtsModuleItem;
end;

function TtsModuleManager.Find(const AName: string): TtsModuleItem;
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

{$REGION 'TtsModuleItem'}
{$REGION 'property access methods'}
//---|Collection|--------------------------------------------------------------

function TtsModuleItem.GetCollection: TtsModuleManager;
begin
  Result := inherited Collection as TtsModuleManager;
end;

procedure TtsModuleItem.SetCollection(const Value: TtsModuleManager);
begin
  inherited Collection := Value;
end;

//---|DisplayName|-------------------------------------------------------------

function TtsModuleItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TtsModuleItem.SetDisplayName(const Value: string);
begin
  if Value <> DisplayName then
  begin
    Changed(False);
    FDisplayName := Value;
  end;
end;

//---|Name|--------------------------------------------------------------------

procedure TtsModuleItem.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    Changed(False);
    FName := Value;
  end;
end;

//---|Value|-------------------------------------------------------------------

procedure TtsModuleItem.SetValue(const Value: Variant);
begin
  if Value <> Self.Value then
  begin
    Changed(False);
    FValue := Value;
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TtsModuleItem.Assign(Source: TPersistent);
begin
 if (Source <> Self) and (Source is TtsModuleItem) then
 begin
   if Assigned(Collection) then
     Collection.BeginUpdate;
   try
     { <PropertyName> := TtsModuleItem(Source).<PropertyName> }
     Value           := TtsModuleItem(Source).Value;
     Name            := TtsModuleItem(Source).Name;
     DisplayName     := TtsModuleItem(Source).DisplayName;
     FormClass       := TtsModuleItem(Source).FormClass;
     DataModuleClass := TtsModuleItem(Source).DataModuleClass;
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

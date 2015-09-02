{ $Id: tsXMLSettings.pas,v 1.4 2009/02/10 13:09:20 Tim Sinaeve Exp $ }

unit ts.Classes.XMLSettings;

{ Author: Tim Sinaeve }

{
  Components are written with their name as the key. Other items (descending
  from TPersistent) must be saved with a given key name, so that they can
  be assigned to the right object afterwards. Saving the values this way allows
  also to save multiple states of the same object in the same file.

  The classes defined in this unit make use of, and are based on Miha Remec's
  excellent OmniXML library, which is released under the MPL license
  (http://www.omnixml.com/).

  TODO
    - add processing instruction
    - support for keynames
    - wrapper class for the XML-file

  Because TFields is a special case, we need to implement dedicated methods to
  store and read field settings.
   LoadFields -> supports TFields
   SaveFields
   LoadList   -> supports TList (of components!)
   SaveList

  - extra parameter for the Load<Instance> and Save<Instance> methods Load and
    Save to indicate that the document is loaded from/saved to file immediately

  ADDED
   - support for Variant properties
}

// OwnersAsElements setting

{
  Layout Setup XML:

    <RootName>

    </RootName>
}

//*****************************************************************************

interface

uses
  Classes, SysUtils, Controls, TypInfo, Variants,

  DB,

  OmniXML, OmniXML_Types, OmniXMLUtils;

//=============================================================================

type
  TtsXMLSettings = class(TComponent)
  private
    FFileName    : string;
    FXMLDocument : IXMLDocument;
    FXMLRoot     : IXMLElement;
    FRootName    : string;

    function FindElement(const ARoot    : IXMLElement;
                         const ATagName : string): IXMLElement;
    function GetFullComponentName(AComponent: TComponent) : string;
    function IsElementEmpty(AElement: IXMLElement): Boolean;
    function FindField(const AComponentName : string;
                             AFieldList     : TFieldList): TField;
    function GetXML: string;

  protected
    function InternalReadText(    ARoot  : IXMLElement;
                                  AName  : string;
                              var AValue : string): Boolean;
    procedure InternalWriteText(ARoot  : IXMLElement;
                                AName  : string;
                                AValue : string);
    procedure Read(      AInstance : TPersistent;
                         ARoot     : IXMLElement;
                   const AReadRoot : Boolean = False);
    procedure ReadCollection(AInstance : TCollection;
                             ARoot       : IXMLElement);
    procedure ReadFieldList(AInstance : TFieldList;
                            ARoot     : IXMLElement);
    procedure ReadProperty(AInstance : TPersistent;
                           APropInfo : Pointer;
                           AElement  : IXMLElement);
    procedure WriteCollection(ACollection : TCollection;
                              ARoot       : IXMLElement);
    procedure WriteFieldList(AFieldList : TFieldList;
                          ARoot   : IXMLElement);
    procedure WriteProperty(AInstance           : TPersistent;
                            APropInfo           : PPropInfo;
                            AElement            : IXMLElement;
                            AWriteDefaultValues : Boolean);
    procedure Write(      AInstance           : TPersistent;
                          ARoot               : IXMLElement;
                    const AName               : string = '';
                          AWriteRoot          : Boolean = True;
                    const ACheckIfEmpty       : Boolean = True;
                    const AWriteDefaultValues : Boolean = True);

  public
    // construction and destruction
    constructor Create(      AOwner    : TComponent;
                       const AFileName : string = '';
                       const ARootName : string = ''); reintroduce;

    // public methods
    function FileExists: Boolean;

    function Load: Boolean;
    procedure Save;

    function LoadPersistent(      AInstance : TPersistent;
                            const AName     : string) : Boolean;
    function LoadComponent(      AInstance : TComponent;
                           const AName     : string = '') : Boolean;
    function LoadCollection(      AInstance : TCollection;
                            const AName     : string) : Boolean;

    procedure SavePersistent(      AInstance : TPersistent;
                             const AName     : string);
    procedure SaveComponent(      AInstance : TComponent;
                            const AName     : string = '');
    procedure SaveCollection(      AInstance : TCollection;
                             const AName     : string);
    procedure LoadFieldList(      AInstance : TFieldList;
                            const AName     : string);
    procedure SaveFieldList(      AInstance : TFieldList;
                            const AName     : string);

    function Delete(const AName : string): Boolean;

  published
    // published properties
    { Name of the XML file. }
    property FileName : string
      read FFileName write FFileName;

    { Name of the root element in the XML-file. }
    property RootName : string
      read FRootName;

    property XML: string
      read GetXML;

  end;

//*****************************************************************************

implementation

uses
  Forms,

  ts.Modules.Memo;

//=============================================================================

const
  COLLECTIONITEM_NODENAME = 'item';
  STRINGS_COUNT_NODENAME  = 'count';
  STRINGS_PREFIX          = 'line';
  DEFAULT_ROOTNAME        = 'Settings';

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TtsXMLSettings.Create(AOwner: TComponent;
  const AFileName: string; const ARootName: string);
begin
  inherited Create(AOwner);

  if ARootName <> '' then
    FRootName := ARootName
  else
    FRootName := DEFAULT_ROOTNAME;

  if AFileName <> '' then
    FFileName := AFileName
  else
    FFileName := ChangeFileExt(Application.ExeName, '.XML');

  FXMLDocument := CreateXMLDoc;
  FXMLDocument.PreserveWhiteSpace := False;
  if not FileExists then
  begin
    FXMLDocument.AppendChild(
      FXMLDocument.CreateProcessingInstruction('xml', 'version="1.0"'));
    FXMLRoot := FXMLDocument.CreateElement(FRootName);
    FXMLDocument.DocumentElement := FXMLRoot;
  end
  else
  begin
    FXMLDocument.Load(FFileName);
    FXMLRoot := FXMLDocument.DocumentElement;
    FRootName := FXMLRoot.TagName;
  end;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|XML|---------------------------------------------------------------------

function TtsXMLSettings.GetXML: string;
begin
  Result := FXMLDocument.XML;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// private methods                                                       BEGIN
//*****************************************************************************

function TtsXMLSettings.FindElement(const ARoot: IXMLElement;
  const ATagName: string): IXMLElement;
var
  I : Integer;
begin
  Result := nil;
  if ARoot = nil then
    Exit;
  I := 0;
  while (Result = nil) and (I < ARoot.ChildNodes.Length) do
  begin
    if (ARoot.ChildNodes.Item[I].NodeType = ELEMENT_NODE) and
      (CompareText(ARoot.ChildNodes.Item[I].NodeName, ATagName) = 0) then
        Result := ARoot.ChildNodes.Item[I] as IXMLElement
    else
      Inc(I);
  end;
end;

//-----------------------------------------------------------------------------

function TtsXMLSettings.FindField(const AComponentName: string;
  AFieldList: TFieldList): TField;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to AFieldList.Count - 1 do
  begin
    if AFieldList[I].Name = AComponentName then
    begin
      Result := AFieldList[I];
      Break;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{ Returns the fully qualified component name through the component's owner(s),
  seperated by dots. }

function TtsXMLSettings.GetFullComponentName(AComponent: TComponent): string;
var
  S : string;
begin
  S := AComponent.Name;
  while Assigned(AComponent.Owner) do
  begin
    if AComponent.Owner.Name <> '' then
    begin
      if S <> '' then
        S := AComponent.Owner.Name + '.' + S
      else
        S := AComponent.Owner.Name;
    end;
    AComponent := AComponent.Owner;
  end;
  Result := S;
end;

//-----------------------------------------------------------------------------

function TtsXMLSettings.IsElementEmpty(AElement: IXMLElement): Boolean;
begin
  Result := AElement.ChildNodes.Length = 0;
end;

//*****************************************************************************
// private methods                                                         END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

function TtsXMLSettings.InternalReadText(ARoot: IXMLElement; AName: string;
  var AValue: string): Boolean;
var
  PropNode : IXMLElement;
begin
  PropNode := FindElement(ARoot, AName);
  Result := PropNode <> nil;
  if Result then
    AValue := PropNode.Text;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.InternalWriteText(ARoot: IXMLElement; AName,
  AValue: string);
var
  PropNode : IXMLElement;
begin
  PropNode := FXMLDocument.CreateElement(AName);
  PropNode.Text := AValue;
  ARoot.AppendChild(PropNode);
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.Read(AInstance: TPersistent; ARoot: IXMLElement;
  const AReadRoot: Boolean);
var
  PropCount : Integer;
  PropList  : PPropList;
  I         : Integer;
  PropInfo  : PPropInfo;
begin
  if AReadRoot then
  begin
    if AInstance is TComponent then
      ARoot := FindElement(ARoot, (AInstance as TComponent).Name)
    else
      ARoot := FindElement(ARoot, AInstance.ClassName);
  end;

  if ARoot = nil then
    Exit;

  PropCount := GetTypeData(AInstance.ClassInfo)^.PropCount;
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropInfos(AInstance.ClassInfo, PropList);
      for I := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;
        ReadProperty(AInstance, PropInfo, ARoot);
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.ReadCollection(AInstance: TCollection;
  ARoot: IXMLElement);
var
  I    : Integer;
  Item : TCollectionItem;
begin
  AInstance.Clear;
  if ARoot = nil then
    Exit;
  for I := 0 to ARoot.ChildNodes.Length - 1 do
  begin
    if ARoot.ChildNodes.Item[I].NodeType = ELEMENT_NODE then
    begin
      if ARoot.ChildNodes.Item[I].NodeName = COLLECTIONITEM_NODENAME then
      begin
        Item := AInstance.Add;
        Read(Item, ARoot.ChildNodes.Item[I] as IXMLElement, False);
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.ReadFieldList(AInstance: TFieldList; ARoot: IXMLElement);
var
  I    : Integer;
  Item : TField;
begin
  if ARoot = nil then
    Exit;
  for I := 0 to ARoot.ChildNodes.Length - 1 do
  begin
    if ARoot.ChildNodes.Item[I].NodeType = ELEMENT_NODE then
    begin
      Item := FindField(ARoot.ChildNodes.Item[I].NodeName, AInstance);
      if Assigned(Item) then
        Read(Item, ARoot.ChildNodes.Item[I] as IXMLElement, False);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.ReadProperty(AInstance: TPersistent;
  APropInfo: Pointer; AElement: IXMLElement);
var
  PropType : PTypeInfo;

  {$REGION 'Nested'}

  //---------------------------------------------------------------------------

  procedure ReadFloatProp;
  var
    Value : Extended;
    Text  : string;
  begin
    if InternalReadText(AElement, PPropInfo(APropInfo)^.Name, Text) then
      Value := XMLStrToRealDef(Text, 0)
    else
      Value := 0;
    SetFloatProp(AInstance, APropInfo, Value)
  end;

  //---------------------------------------------------------------------------

  procedure ReadDateTimeProp;
  var
    Value : TDateTime;
    Text  : string;
  begin
    if InternalReadText(AElement, PPropInfo(APropInfo)^.Name, Text) then
    begin
      if XMLStrToDateTime(Text, Value) then
        SetFloatProp(AInstance, APropInfo, Value)
      else
        raise Exception.CreateFmt('Error in datetime property %s',
                                  [PPropInfo(APropInfo)^.Name]);
    end
    else
      SetFloatProp(AInstance, APropInfo, 0);
  end;

  //---------------------------------------------------------------------------

  procedure ReadStrProp;
  var
    Value: string;
  begin
    if not InternalReadText(AElement, PPropInfo(APropInfo)^.Name, Value) then
      Value := '';

    if PropType^.Kind = tkWString then
      SetWideStrProp(AInstance, APropInfo, Value)
    else if PropType^.Kind = tkUString then
      SetUnicodeStrProp(AInstance, APropInfo, Value)
    else
      SetStrProp(AInstance, APropInfo, Value);
  end;

  //---------------------------------------------------------------------------

  procedure ReadOrdProp;
  var
    Value     : string;
    IntValue  : Integer;
    BoolValue : Boolean;
  begin
    if InternalReadText(AElement, PPropInfo(APropInfo)^.Name, Value) then
    begin
      case PropType^.Kind of
        tkInteger:
          if XMLStrToInt(Value, IntValue) then
            SetOrdProp(AInstance, APropInfo, XMLStrToIntDef(Value, 0))
          else
            raise Exception.CreateFmt('Invalid integer value (%s).', [Value]);
        tkChar: SetOrdProp(AInstance, APropInfo, Ord(Value[1]));
        tkWChar: SetOrdProp(AInstance, APropInfo, Cardinal(Value[1]));
        tkSet: SetSetProp(AInstance, APropInfo, Value);
        tkEnumeration:
        begin
          if PropType = System.TypeInfo(Boolean) then
          begin
            if XMLStrToBool(Value, BoolValue) then
              SetOrdProp(AInstance, APropInfo, Ord(BoolValue))
            else
              raise Exception.CreateFmt('Invalid boolean value (%s).', [Value]);
          end
          else if PropType^.Kind = tkInteger then
          begin
            if XMLStrToInt(Value, IntValue) then
              SetOrdProp(AInstance, APropInfo, IntValue)
            else
              raise Exception.CreateFmt('Invalid enum value (%s).', [Value]);
          end
          else if PropType^.Kind = tkEnumeration then
            SetEnumProp(AInstance, APropInfo, Value);
        end;
      end;
    end
    else
      SetOrdProp(AInstance, APropInfo, PPropInfo(APropInfo)^.Default)
  end;

  //---------------------------------------------------------------------------

  procedure ReadInt64Prop;
  var
    Value    : string;
    IntValue : Int64;
  begin
    if InternalReadText(AElement, PPropInfo(APropInfo)^.Name, Value) then
    begin
      if XMLStrToInt64(Value, IntValue) then
        SetInt64Prop(AInstance, APropInfo, IntValue)
      else
        raise Exception.CreateFmt('Invalid int64 value (%s).', [Value]);
    end
    else
      SetFloatProp(AInstance, APropInfo, 0)
  end;

  //---------------------------------------------------------------------------

  procedure ReadObjectProp;
  var
    Value    : TObject;
    PropNode : IXMLElement;

    procedure ReadStrings(const Strings: TStrings);
    var
      I     : Integer;
      Count : Integer;
      Value : string;
    begin
      Strings.Clear;

      Count := GetNodeAttrInt(PropNode, STRINGS_COUNT_NODENAME, 0);
      for I := 0 to Count - 1 do
        Strings.Add('');

      for I := 0 to Strings.Count - 1 do
      begin
        if InternalReadText(PropNode, STRINGS_PREFIX + IntToStr(I), Value) then
          Strings[I] := Value;
      end;
    end;
  begin
    Value := TObject(GetOrdProp(AInstance, APropInfo));
    if (Value <> nil) and (Value is TPersistent) then
    begin
      PropNode := FindElement(AElement, PPropInfo(APropInfo)^.Name);
      Read(TPersistent(Value), PropNode);
      if Value is TCollection then
        ReadCollection(TCollection(Value), PropNode)
      else if Value is TStrings then
        ReadStrings(TStrings(Value));
    end;
  end;

  //---------------------------------------------------------------------------

  {$ENDREGION}

begin
  PropType := PPropInfo(APropInfo)^.PropType^;
  if (PPropInfo(APropInfo)^.GetProc <> nil) and
     ((PropType^.Kind = tkClass) or (PPropInfo(APropInfo)^.SetProc <> nil)) then
     //and
     //(IsStoredProp(AInstance, PPropInfo(APropInfo))) then
  begin
    case PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: ReadOrdProp;
      tkString, tkUString, tkLString, tkWString: ReadStrProp;
      tkFloat:
        if (PropType = System.TypeInfo(TDateTime)) or
           (PropType = System.TypeInfo(TTime)) or
           (PropType = System.TypeInfo(TDate)) then
            ReadDateTimeProp
        else
          ReadFloatProp;
      tkInt64: ReadInt64Prop;
      tkClass: ReadObjectProp;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.WriteCollection(ACollection: TCollection;
  ARoot: IXMLElement);
var
  I : Integer;
begin
  for I := 0 to ACollection.Count - 1 do
    Write(ACollection.Items[I], ARoot, '', True, False);
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.WriteFieldList(AFieldList: TFieldList; ARoot: IXMLElement);
var
  I : Integer;
begin
  for I := 0 to AFieldList.Count - 1 do
    Write(AFieldList[I], ARoot, '', True, False);
end;

//-----------------------------------------------------------------------------

{
  AInstance
    The object to be saved in the XML document.

  APropInfo
    The associated PPropInfo record with the current property.

  AElement
    The assiociated XML element to write the property data to.

  AWriteDefaultValues
    If False, only values that differ from the default values are written to the
    document.
}

procedure TtsXMLSettings.WriteProperty(AInstance: TPersistent;
  APropInfo: PPropInfo; AElement: IXMLElement; AWriteDefaultValues: Boolean);
var
  PropType : PTypeInfo;

  {$REGION 'Nested'}
  //---------------------------------------------------------------------------

  procedure WriteStrProp;
  var
    Value : string;
  begin
    if PropType^.Kind = tkWString then
      Value := GetWideStrProp(AInstance, APropInfo)
    else if PropType^.Kind = tkUString then
      Value := GetUnicodeStrProp(AInstance, APropInfo)
    else
      Value := GetStrProp(AInstance, APropInfo);

    if Value <> '' then
      InternalWriteText(AElement, PPropInfo(APropInfo)^.Name, Value);
  end;

  //---------------------------------------------------------------------------

  procedure WriteOrdProp;
  var
    Value : Longint;
  begin
    Value := GetOrdProp(AInstance, APropInfo);
    if AWriteDefaultValues or (Value <> PPropInfo(APropInfo)^.Default) then
    begin
      case PropType^.Kind of
        tkInteger:
          InternalWriteText(AElement,
                            PPropInfo(APropInfo)^.Name,
                            XMLIntToStr(Value));
        tkChar:
          InternalWriteText(AElement, PPropInfo(APropInfo)^.Name, Chr(Value));
        tkWChar:
          InternalWriteText(AElement,
                            PPropInfo(APropInfo)^.Name,
                            WideChar(Value));
        tkSet:
          InternalWriteText(AElement,
                            PPropInfo(APropInfo)^.Name,
                            GetSetProp(AInstance, PPropInfo(APropInfo), True));
        tkEnumeration:
        begin
          if PropType = System.TypeInfo(Boolean) then
            InternalWriteText(AElement,
                              PPropInfo(APropInfo)^.Name,
                              XMLBoolToStr(Boolean(Value)))
          else if PropType^.Kind = tkInteger then
            InternalWriteText(AElement,
                              PPropInfo(APropInfo)^.Name,
                              XMLIntToStr(Value))
          else if PropType^.Kind = tkEnumeration then
            InternalWriteText(AElement,
                              PPropInfo(APropInfo)^.Name,
                              GetEnumName(PropType, Value));
        end;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteFloatProp;
  var
    Value : Real;
  begin
    Value := GetFloatProp(AInstance, APropInfo);
    if Value <> 0 then
      InternalWriteText(AElement,
                        PPropInfo(APropInfo)^.Name,
                        XMLRealToStr(Value));
  end;

  //---------------------------------------------------------------------------

  procedure WriteDateTimeProp;
  var
    Value : TDateTime;
  begin
    Value := VarAsType(GetFloatProp(AInstance, APropInfo), varDate);
    if Value <> 0 then
      InternalWriteText(AElement,
                        PPropInfo(APropInfo)^.Name,
                        XMLDateTimeToStrEx(Value));
  end;

  //---------------------------------------------------------------------------

  procedure WriteInt64Prop;
  var
    Value : Int64;
  begin
    Value := GetInt64Prop(AInstance, APropInfo);
    if Value <> 0 then
      InternalWriteText(AElement,
                        PPropInfo(APropInfo)^.Name,
                        XMLInt64ToStr(Value));
  end;

  //---------------------------------------------------------------------------

  procedure WriteVariantProp;
  var
    Value : Variant;
  begin
    Value := GetVariantProp(AInstance, APropInfo);
    InternalWriteText(AElement,
                      PPropInfo(APropInfo)^.Name,
                      XMLVariantToStr(Value));
  end;

  //---------------------------------------------------------------------------

  procedure WriteObjectProp;
  var
    Value    : TObject;
    PropNode : IXMLElement;

    //-------------------------------------------------------------------------

    procedure WriteStrings(const AStrings: TStrings);
    var
      I : Integer;
    begin
      SetNodeAttrInt(PropNode, STRINGS_COUNT_NODENAME, AStrings.Count);
      for I := 0 to AStrings.Count - 1 do
      begin
        if AStrings[I] <> '' then
          InternalWriteText(PropNode, STRINGS_PREFIX + IntToStr(I), AStrings[I]);
      end;
    end;

    //-------------------------------------------------------------------------

  begin
    Value := TObject(GetOrdProp(AInstance, APropInfo));
    if (Value <> nil) and (Value is TPersistent) then
    begin
      PropNode := FXMLDocument.CreateElement(PPropInfo(APropInfo)^.Name);

      // write object's properties
      Write(TPersistent(Value), PropNode, '', False, True, AWriteDefaultValues);
      if Value is TCollection then
      begin
        WriteCollection(TCollection(Value), PropNode);
        if not IsElementEmpty(PropNode) then
          AElement.AppendChild(PropNode);
      end
      else if Value is TStrings then
      begin
        WriteStrings(TStrings(Value));
        AElement.AppendChild(PropNode);
      end
      else if not IsElementEmpty(PropNode) then
        AElement.AppendChild(PropNode);
    end;
  end;

  {$ENDREGION}
  //---------------------------------------------------------------------------

begin
  if (PPropInfo(APropInfo)^.GetProc <> nil) then
  begin
    PropType := PPropInfo(APropInfo)^.PropType^;
    case PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: WriteOrdProp;
      tkString, tkUString, tkLString, tkWString: WriteStrProp;
      tkFloat:
        if (PropType = System.TypeInfo(TDateTime)) or
          (PropType = System.TypeInfo(TTime)) or
          (PropType = System.TypeInfo(TDate)) then
            WriteDateTimeProp
        else
          WriteFloatProp;
      tkInt64: WriteInt64Prop;
      tkClass: WriteObjectProp;
      tkVariant: WriteVariantProp;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{
  Writes a TPersistent instance to the XML document. All published properties
  are stores as XML elements in the file.

  AInstance
    The object to be persisted (of type TPersistent or any descending class).

  ARoot
    XML document root element.

  AWriteRoot [True]

  ACheckIfEmpty [True]

  AWriteDefaultValues [False]
}

procedure TtsXMLSettings.Write(AInstance: TPersistent; ARoot: IXMLElement;
  const AName: string; AWriteRoot: Boolean; const ACheckIfEmpty,
  AWriteDefaultValues: Boolean);
var
  PropCount : Integer;
  PropList  : PPropList;
  I         : Integer;
  PropInfo  : PPropInfo;
  Element   : IXMLElement;
  S         : string;
begin
  PropCount := GetTypeData(AInstance.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  if AInstance is TCollectionItem then
    Element := FXMLDocument.CreateElement(COLLECTIONITEM_NODENAME)
  else if AWriteRoot then
  begin
    if AName <> '' then
      S := AName
    else
    begin
      if AInstance is TComponent then
      begin
        S := TComponent(AInstance).Name;
      end;
    end;
    Element := FXMLDocument.CreateElement(S);
  end
  else
    Element := ARoot;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropInfos(AInstance.ClassInfo, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo = nil then
        Break;
      //if IsStoredProp(AInstance, PropInfo) then
        WriteProperty(AInstance, PropInfo, Element, AWriteDefaultValues)
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;

  if AWriteRoot then
  begin
    if ACheckIfEmpty and IsElementEmpty(Element) then
      Exit
    else
    begin
      if ARoot <> nil then
        ARoot.AppendChild(Element)
      else
        FXMLDocument.DocumentElement := Element;
    end;
  end;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Deletes an entry from the XML file and returns True if the operation was
  successful. }

function TtsXMLSettings.Delete(const AName: string): Boolean;
begin
  if Assigned(FindElement(FXMLRoot, AName)) then
  begin
    DeleteNode(FXMLRoot, AName);
    Result := True;
    Save;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

{ Returns True if the XML-file exists. }

function TtsXMLSettings.FileExists: Boolean;
begin
  Result := SysUtils.FileExists(FFileName);
end;

//-----------------------------------------------------------------------------

{ Load XML document from file. }

function TtsXMLSettings.Load: Boolean;
begin
  if FileExists then
  begin
    FXMLDocument.Load(FFileName);
    FXMLRoot := FXMLDocument.DocumentElement;
    FRootName := FXMLRoot.TagName;
    Result := True;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

function TtsXMLSettings.LoadCollection(AInstance: TCollection;
  const AName: string): Boolean;
var
  Node : IXMLNode;
begin
  Node := FindNode(FXMLRoot, AName, '', []);
  if Assigned(Node) then
  begin
    ReadCollection(AInstance, Node as IXMLElement);
    Result := True;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

function TtsXMLSettings.LoadComponent(AInstance: TComponent;
  const AName: string): Boolean;
var
  Node : IXMLNode;
  S    : string;
begin
  if AName <> '' then
    S := AName
  else
    S := GetFullComponentName(AInstance);
  Node := FindNode(FXMLRoot, S, '', []);
  if Assigned(Node) then
  begin
    Read(AInstance, Node as IXMLElement);
    Result := True;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.LoadFieldList(AInstance: TFieldList; const AName: string);
var
  Node : IXMLNode;
begin
  Node := FindNode(FXMLRoot, AName, '', []);
  ReadFieldList(AInstance, Node as IXMLElement);
end;

//-----------------------------------------------------------------------------

{ Loads an instance of any TPersistent descendent class from the given XML-file }

function TtsXMLSettings.LoadPersistent(AInstance: TPersistent;
  const AName: string) : Boolean;
var
  Node : IXMLNode;
begin
  Node := FindNode(FXMLRoot, AName, '', []);
  if Assigned(Node) then
  begin
    Read(AInstance, Node as IXMLElement);
    Result := True;
  end
  else
    Result := False;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.SaveCollection(AInstance: TCollection;
  const AName: string);
var
  Root : IXMLElement;
begin
  DeleteNode(FXMLRoot, AName);
  Root := FXMLDocument.CreateElement(AName);
  FXMLRoot.AppendChild(Root);
  WriteCollection(AInstance, Root);
  Save;
end;

//-----------------------------------------------------------------------------

{ Save XML document to file. }

procedure TtsXMLSettings.Save;
begin
  FXMLDocument.Save(FFileName, ofIndent);
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.SaveComponent(AInstance: TComponent;
  const AName: string);
var
  S : string;
begin
  if AName <> '' then
    S := AName
  else
    S := GetFullComponentName(AInstance);
  DeleteNode(FXMLRoot, S);
  Write(AInstance, FXMLRoot, S);
  Save;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.SaveFieldList(AInstance: TFieldList; const AName: string);
var
  Root : IXMLElement;
begin
  DeleteNode(FXMLRoot, AName);
  Root := FXMLDocument.CreateElement(AName);
  FXMLRoot.AppendChild(Root);
  WriteFieldList(AInstance, Root);
  Save;
end;

//-----------------------------------------------------------------------------

procedure TtsXMLSettings.SavePersistent(AInstance: TPersistent;
  const AName: string);
begin
  DeleteNode(FXMLRoot, AName);
  Write(AInstance, FXMLRoot, AName);
  Save;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.

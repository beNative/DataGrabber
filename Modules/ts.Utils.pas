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

unit ts.Utils;

{
  TODO:
    - Make a test application or test unit to test each routine.
}

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.Rtti, System.TypInfo,
  Vcl.Graphics, Vcl.Controls,

  Data.DB;

type
  TVarRecArray = array of TVarRec;
  TBitmap      = Vcl.Graphics.TBitmap;

const
  AnsiWhitespace = [' '];

var
  FRtti: TRttiContext;

procedure CloneComponent(AFrom: TComponent; ATo: TComponent);

function VirtualKeyToChar(AKey : Word) : string;

function FindUniqueName(const AName : string): string;

function GetUnitName(AObject: TObject): string;

function GetFullName(AComponent: TComponent) : string;

// string manipulation routines

procedure StrToStrings(
  const AString : string;
  AList         : TStrings;
  ASeparator    : Char
);

function StringsToStr(
  const List             : TStrings;
  const Sep              : string;
  const AllowEmptyString : Boolean = True
): string;

function Unformat(
  const ASource, APattern : string;
  const AArgs             : array of const
): Integer;

function Like(const ASource, ATemplate: string): Boolean;

{ original author: Vladimir Gaitanoff }
{ Returns a number of words delimited with AWordDelims }
function WordCount(
  const AString     : string;
  const AWordDelims : TSysCharSet = AnsiWhitespace
): Integer;

{ Returns a position of word number AIndex in the string AString }
function WordPosition(
  const AIndex      : Integer;
  const AString     : string;
  const AWordDelims : TSysCharSet = AnsiWhitespace
): Integer;

{ Returns a word number AIndex in the string AString }
function ExtractWord(
  const AIndex      : Integer;
  const AString     : string;
  const AWordDelims : TSysCharSet = AnsiWhitespace
): string;

function URLEncode(const AString: string): string;

// string formatting routines

function FormatElapsedTime(ASeconds: Extended): string;

function FormatByteText(ABytes: Integer): string;

function CreateGUIDString: string;
function CreateUniqueID: string;

// windows utilities

function GetLocalUserName: string;
function GetLocalComputerName: string;

procedure RunApplication(
  AParams : string;
  AFile   : string;
  AWait   : Boolean = True
);

// VCL utilities

procedure ChangeOwner(AComponent, ANewOwner : TComponent);

procedure EnableControls(
  AControlContainer : TWinControl;
  AEnabled          : Boolean = True
);

procedure DisableControls(AControlContainer : TWinControl);

function GetTextWidth(
  const AText : string;
  AFont       : TFont
): Integer;

function GetTextHeight(
  const AText : string;
  AFont       : TFont
): Integer;

// Variants and TVarRec conversions

procedure VariantToVarRec(
  AVariant         : Variant;
  var AVarRecArray : TVarRecArray
);
procedure ClearVarRec(var AVarRecArray: TVarRecArray);

function VarRecToVariant(const AVarRec: TVarRec): Variant;
function VarRecToString(const AVarRec: TVarRec): string;
function VarRecToOleVariant(const AVarRec: TVarRec): OleVariant;

function VarArrayElemCount(const AVarArray: Variant): Integer;
function VarIsValue(const V : Variant): Boolean;
function VarAsTypeDef(
  const AValue    : Variant;
  AVarType        : TVarType;
  const ADefValue : Variant
): Variant;

procedure OleVarFromVariant(
  var AOleVariant : OleVariant;
  const AVariant  : Variant
);

function GetVariantTypeName(const AVariant: Variant): string;

function VariantCompare(AVariant1, AVariant2: Variant): Boolean;

function VariantTypeForFieldType(const AFieldType: TFieldType): Integer;
function FieldTypeForVariant(const AVariant: Variant) : TFieldType;
function ConvertValueToFieldType(
  const AVariant : Variant;
  const AField   : TField
): Variant;
function ValueNeedsConversion(
  const AVarType   : Integer;
  const AFieldType : TFieldType
): Boolean;

function StringToVariant(AString : string): Variant;

function MixColors(FG, BG: TColor; T: Byte): TColor;

procedure Delay(Milliseconds: Integer);

// Dialog boxes

procedure ShowInfo(
  const AInfoString : string;
  const AArguments  : array of const
); overload;

procedure ShowInfo(const AInfoString : string); overload;

procedure ShowError(
  const AErrorString : string;
  const AArguments   : array of const
); overload;

procedure ShowError(const AErrorString : string); overload;

procedure ShowWarning(const AWarningString : string); overload;

procedure ShowWarning(
  const AWarningString : string;
  const AArguments     : array of const
); overload;

// Interface utility routines

function GetPIMTOffset(const I : IInterface): Integer;

// support was included in D2010

// Extended RTTI tools

function CallEventHandler(
  Instance        : TObject;
  const EventName : string;
  const Args      : array of TValue
): TValue; overload;

function CallEventHandler(
  Instance   : TObject;
  Event      : TRttiProperty;
  const Args : array of TValue
): TValue; overload;

procedure LockPaint(AControl: TWinControl);

procedure UnlockPaint(AControl: TWinControl);

procedure AppendLine(
  var sToString : string;
  const sLine   : string
); overload;

procedure AppendLine(
  var sToString : string;
  const sLine   : string;
  const Args    : array of const
); overload;

function AsPropString(AValue: TValue): string;

function AsFieldString(AValue: TValue): string;

function SetToString(
  TypeInfo : PTypeInfo;
  const Value;
  QuoteValues : Boolean = True;
  Brackets    : Boolean = True;
  TrimChars   : Integer = -1
): string;

function TryGetUnderlyingValue(
  const AValue    : TValue;
  out AInnerValue : TValue
): Boolean;

// UI windows utils

{ Displays a size grip on a window. }
procedure SetWindowSizeGrip(hWnd: HWND; Enable: Boolean);

procedure FixControlStylesForDrag(AParent: TControl);

procedure DebugString(const AString: string); overload;

procedure DebugString(
  const AString : string;
  const Args    : array of const
); overload;

implementation

uses
  Winapi.ShellAPI, Winapi.ActiveX, Winapi.Messages,
  System.Math, System.Variants, System.StrUtils, System.Character,
  Vcl.Forms, Vcl.GraphUtil;

resourcestring
  SNoCorrespondingFieldType = 'No corresponding fieldtype found for Variant ' +
                              'with value %s';
  SMissingEvent = '%s does not have an event called %s';
  SPropertyNotAnEvent = '%s.%s is not an event';
  SEventHandlerHasInsufficientRTTI = 'Event handler does not ' +
    'have the required RTTI to be dynamically invoked';

var
  FContext: TRttiContext;

// code used by SetWindowSizeGrip

const
  SizeGripProp = 'SizeGrip';

type
  TWndProc = function(hWnd: hWnd; Msg: UINT; wParam: wParam; lParam: lParam)
    : LRESULT; stdcall;
  PGripInfo = ^TGripInfo;

  TGripInfo = record
    OldWndProc : TWndProc;
    Enabled    : Boolean;
    GripRect   : TRect;
  end;

{
  Code taken from SizeGripHWND.pas:
  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  Alle Rechte vorbehalten.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
}
function SizeGripWndProc(hWnd: hWnd; Msg: UINT; wParam: wParam; lParam: lParam)
  : LRESULT; stdcall;
var
  Info : PGripInfo;
  dc   : HDC;
  pt   : TPoint;

  // Invalidate the current grip rectangle
  procedure InvalidateGrip;
  begin
    with Info^ do
      if (GripRect.Right > GripRect.Left)
        and (GripRect.Bottom > GripRect.Top) then
        InvalidateRect(hWnd, @GripRect, true);
  end;

  // Update (and invalidate) the current grip rectangle
  procedure UpdateGrip;
  begin
    with Info^ do
    begin
      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;
    InvalidateGrip;
  end;

  function CallOld: LRESULT;
  begin
    Result := CallWindowProc(@Info^.OldWndProc, hWnd, Msg, wParam, lParam);
  end;

begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if Info = nil then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam)
  else if not Info^.Enabled then
    Result := CallOld
  else
  begin
    case Msg of
      WM_NCDESTROY:
      begin
        Result := CallOld;
        SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@Info^.OldWndProc));
        RemoveProp(hWnd, SizeGripProp);
        Dispose(Info);
      end;

      WM_PAINT:
      begin
        Result := CallOld;
        if wParam = 0 then
        begin
          dc := GetDC(hWnd);
          DrawFrameControl(dc, Info^.GripRect, DFC_SCROLL,
            DFCS_SCROLLSIZEGRIP);
          ReleaseDC(hWnd, dc);
        end;
      end;

      WM_NCHITTEST:
      begin
        pt.x := TSmallPoint(lParam).x;
        pt.y := TSmallPoint(lParam).y;
        ScreenToClient(hWnd, pt);
        if PtInRect(Info^.GripRect, pt) then
          Result := HTBOTTOMRIGHT
        else
          Result := CallOld;
      end;

      WM_SIZE:
      begin
        InvalidateGrip;
        Result := CallOld;
        UpdateGrip;
      end;

    else
      Result := CallOld;
    end;
  end;
end;

function GetLightness(AColor: TColor): Byte;
var
  R, G, B: Byte;
  MaxValue, MinValue: Double;
  Lightness: Double;
begin
  R := GetRValue(ColorToRGB(AColor));
  G := GetGValue(ColorToRGB(AColor));
  B := GetBValue(ColorToRGB(AColor));
  MaxValue := Max(Max(R,G),B);
  MinValue := Min(Min(R,G),B);
  Lightness := (((MaxValue + MinValue) * 240) + 255 ) / 510;
  Result := Round(Lightness);
end;

// non-interfaced routines                                                 END

// interfaced routines                                                   BEGIN

{ Based on code of Xavier Pacheco. Fixed name assignment of ATo and removed
  assignment of Parent property.

   This procedure clones the published properties of AFrom and writes them to
   ATo.
   AFrom and ATo must be of the same type.  Use it for components that do
   not have an Assign method.

   AOwner is the owner of the cloned component. If AOwner is nil, the owner of
   the source component is used.
}

procedure CloneComponent(AFrom: TComponent; ATo: TComponent);
var
  MS : TMemoryStream;
  S1 : string;
  S2 : string;
begin
  if AFrom.ClassType <> ATo.ClassType then
    raise EComponentError.Create('Object types are incompatible');

  MS := TMemoryStream.Create; // Create the memory stream.
  with MS do try
    S1 := AFrom.Name;
    S2 := ATo.Name;
    AFrom.Name := '';
    WriteComponent(AFrom);        // Write C1 properties to stream
    AFrom.Name := S1;
    Seek(0, soFromBeginning);  // Position to beginning of stream.
    ReadComponent(ATo);         // read properties from stream into C2
    ATo.Name := S2;
  finally
    Free;                      // IAC, free stream.
  end;
end;

{ Returns a unique component name based on AName. If the given name is not
  unique, the given AName will be suffixed with an underscore followed by a
  number. }

function FindUniqueName(const AName: string): string;
var
  I: Integer;
begin
  I := 0;
  Result := AName;
  while not IsUniqueGlobalComponentName(Result) do
  begin
    Inc(I);
    Result := Format('%s_%d', [AName, I]);
  end;
end;

{ Returns the name of the unit where the object's class is declared. }

function GetUnitName(AObject: TObject): string;
var
  TypeData: PTypeData;
begin
  if (AObject.ClassInfo <> nil) then
  begin
    TypeData := GetTypeData(AObject.ClassInfo);
    Result := string(TypeData.UnitName);
  end;
end;

{ Returns fully qualified component instance name. The name is preceded by its
  owner name(s) (seperated by dots). }

function GetFullName(AComponent: TComponent) : string;
begin
  Result := AComponent.Name;
  while Assigned(AComponent.Owner) do
  begin
    if AComponent.Owner.Name <> '' then
      Result := AComponent.Owner.Name + '.' + Result;
    AComponent := AComponent.Owner;
  end;
end;

procedure ClearVarRec(var AVarRecArray: TVarRecArray);
var
  I : Integer;
begin
  for I := 0 to Length(AVarRecArray) - 1 do
    if AVarRecArray[I].VType in [vtExtended, vtString, vtVariant, vtInt64] then
      Dispose(AVarRecArray[I].VExtended);
  Finalize(AVarRecArray);
end;

function ConvertValueToFieldType(const AVariant: Variant;
  const AField: TField): Variant;
begin
  if (AField <> nil) and ValueNeedsConversion(VarType(AVariant), AField.DataType) then
    Result := VarAsType(AVariant, VariantTypeForFieldType(AField.DataType))
  else
   Result := AVariant;
end;

{
  Returns True if the Variant represents a value of a primitive type.

  Following variant values will return False:
    - Null
    - Unassigned
    - EmptyParam
    - varError
    - empty strings (because they are the same as Unassigned)
}

function VarIsValue(const V: Variant): Boolean;
begin
  Result := (not VarIsNull(V)) and (not VarIsEmpty(V)) and
            (not VarIsEmptyParam(V)) and (not VarIsError(V)) and
            not (VarIsStr(V) and (V = ''));
end;

function FieldTypeForVariant(const AVariant: Variant): TFieldType;
begin
  case VarType(AVariant) and varTypeMask of
   //varEmpty:    Result := ftVariant;
   varSmallint: Result := ftSmallint;
   varInteger:  Result := ftInteger;
   varSingle:   Result := ftFloat;
   varDouble:   Result := ftFloat;
   varCurrency: Result := ftCurrency;
   varDate:     Result := ftDateTime;
   varOleStr:   Result := ftString;
   varBoolean:  Result := ftBoolean;
   varString:   Result := ftString;
   else
     raise Exception.CreateFmt(SNoCorrespondingFieldType, [AVariant]);
  end;
end;

function GetLocalComputerName: string;
var
  Count : DWORD;
  S     : string;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(S, Count);
  if GetComputerName(PChar(S), Count) then
    SetLength(S, StrLen(PChar(S)))
  else
    S := '';
  Result := S;
end;

function GetLocalUserName: string;
var
  Count : DWORD;
  S     : string;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(S, Count);
  if GetUserName(PChar(S), Count) then
    SetLength(S, StrLen(PChar(S)))
  else
    S := '';
  Result := S;
end;

function GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cy;
  finally
    Bitmap.Free;
  end;
end;

function GetTextWidth(const AText: string; AFont: TFont): Integer;
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cx;
  finally
    Bitmap.Free;
  end;
end;

function GetVariantTypeName(const AVariant: Variant): String;
begin
  case VarType(AVariant) and varTypeMask of
   varEmpty:    Result := 'varEmpty';
   varNull:     Result := 'varNull';
   varSmallint: Result := 'varSmallint';
   varInteger:  Result := 'varInteger';
   varSingle:   Result := 'varSingle';
   varDouble:   Result := 'varDouble';
   varCurrency: Result := 'varCurrency';
   varDate:     Result := 'varDate';
   varOleStr:   Result := 'varOleStr';
   varDispatch: Result := 'varDispatch';
   varError:    Result := 'varError';
   varBoolean:  Result := 'varBoolean';
   varVariant:  Result := 'varVariant';
   varUnknown:  Result := 'varUnknown';
   varByte:     Result := 'varByte';
   varString:   Result := 'varString';
   else
     Result := 'Unkown Variant Type';
  end;
  if VarType(AVariant) and varArray <> 0 then
    Result := Result + ' [Array]';
  if VarType(AVariant) and varByRef <> 0 then
    Result := Result + ' (By Reference)';
end;

procedure OleVarFromVariant(var AOleVariant: OleVariant; const AVariant: Variant);
begin
  if VarType(AVariant) = varString then
   AOleVariant := WideString(AVariant)
  else
   AOleVariant := AVariant;
end;

function StringToVariant(AString : string): Variant;
var
  I    : Int64;
  K    : Integer;
  iPos : Integer;
begin
  Result := Null;
  if AString <> '' then
    if AString[1] = '''' then
      Result := Copy(AString, 2, Length(AString) - 2)
    else
    begin
      Val(AString, I, K);
      if K = 0 then
        Result := Integer(I)
      else
      begin
        if FormatSettings.DecimalSeparator <> '.' then
        begin
          iPos := Pos('.', AString);
          if iPos > 0 then
            AString[iPos] := FormatSettings.DecimalSeparator;
        end;
        Result := StrToFloat(AString);
      end;
    end;
end;

function VirtualKeyToChar(AKey : Word) : string;
var
  KS : TKeyboardState;
  AR : Integer;
begin
  GetKeyboardState(KS);
  SetLength(Result, 2);
  AR := ToAscii(AKey, MapVirtualKey(AKey, 0), KS, @Result[1], 0);
  case AR of
    0 : Result := '';
    1 : SetLength(Result, 1);
    2 : ;
    else
      Result := '';
  end;
end;

procedure StrToStrings(const AString: string; AList: TStrings; ASeparator: Char);
var
  S : string;
  I : Integer;
begin
  if Assigned(AList) then
  try
    AList.BeginUpdate;
    AList.Clear;
    S := '';
    for I := 1 to Length(AString) do
      if AString[I] = ASeparator then
      begin
        AList.Add(Trim(S));
        S := '';
      end
      else if AString <> ' ' then
        S := S + AString[I];
    if S <> '' then
      AList.Add(Trim(S));
  finally
    AList.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: string; const AllowEmptyString: Boolean = True): string;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count > 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

//-----------------------------------------------------------------------------

function ValueNeedsConversion(const AVarType: Integer;
  const AFieldType: TFieldType): Boolean;
begin
   if AVarType and varByRef <> 0 then
    Result := (AFieldType <> ftReference) and
              ValueNeedsConversion(AVarType and not varByRef, AFieldType)
  else
  if AVarType and varArray <> 0 then
    Result := (AFieldType <> ftArray) and
              ValueNeedsConversion(AVarType and not varArray, AFieldType)
  else
  case AVarType and VarTypeMask of
   varSmallint, varInteger:
     Result := not (AFieldType in
                 [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint]);
   varSingle, varDouble:
     Result := not (AFieldType in [ftFloat]);
   varCurrency:
     Result := not (AFieldType in [ftCurrency, ftBCD]);
   varDate:
     Result := not (AFieldType in [ftDate, ftTime, ftDateTime]);
   varOleStr, varString:
     Result := not (AFieldType in [ftString, ftFixedChar, ftWideString]);
   varBoolean:
     Result := not (AFieldType in [ftBoolean]);
   varUnknown, varDispatch:
     Result := not (AFieldType in [ftDataSet, ftADT]);
   varByte:
     Result := not (AFieldType in
                 [ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
                  ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor]);
   //varEmpty, varAny, varError varVariant ???
   else
     Result := False;
  end;
end;

function VarArrayElemCount(const AVarArray: Variant): Integer;
begin
  if not VarIsArray(AVarArray) then
    Result := 1
  else
  begin
    Assert(VarArrayDimCount(AVarArray) = 1, 'VarArrayDimCount(Value) = 1');
    Result := VarArrayHighBound(AVarArray, 1) - VarArrayLowBound(AVarArray, 1) + 1;
  end;
end;

function VarAsTypeDef(const AValue: Variant; AVarType: TVarType;
  const ADefValue: Variant): Variant;
begin
  if VarIsType(AValue, AVarType) then
    Result := VarAsType(AValue, AVarType)
  else
    Result := ADefValue;
end;

procedure VariantToVarRec(AVariant: Variant; var AVarRecArray: TVarRecArray);
var
  I : Integer;
begin
  SetLength(AVarRecArray, VarArrayHighBound(AVariant, 1) + 1);

  for I := 0 to VarArrayHighBound(AVariant, 1) do
    case TVarData(AVariant[I]).VType of
      varSmallint, varInteger, varByte:
        begin
          AVarRecArray[I].VType := vtInteger;
          AVarRecArray[I].VInteger := AVariant[I];
        end;
      varInt64:
        begin
          AVarRecArray[I].VType := vtInt64;
          New(AVarRecArray[I].VInt64);
          AVarRecArray[I].VInt64^ := AVariant[I];
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          AVarRecArray[I].VType := vtExtended;
          New(AVarRecArray[I].VExtended);
          AVarRecArray[I].VExtended^ := AVariant[I];
        end;
      varBoolean:
        begin
          AVarRecArray[I].VType := vtBoolean;
          AVarRecArray[I].VBoolean := AVariant[I];
        end;
      varOleStr, varString:
        begin
          AVarRecArray[I].VType := vtString;
          New(AVarRecArray[I].VString);
          AVarRecArray[I].VString^ := ShortString(AVariant[I]);
        end;
      varVariant:
        begin
          AVarRecArray[I].VType := vtVariant;
          New(AVarRecArray[I].VVariant);
          AVarRecArray[I].VVariant^ := AVariant[I];
        end;
    end;
end;

function VariantTypeForFieldType(const AFieldType: TFieldType): Integer;
begin
  case AFieldType of
   ftString:
     Result := varString;
   ftSmallint:
     Result := varSmallint;
   ftInteger, ftWord, ftAutoInc:
     Result := varInteger;
   ftBoolean:
     Result := varBoolean;
   ftFloat:
     Result := varDouble;
   ftCurrency, ftBCD:
     Result := varCurrency;
   ftDate, ftTime, ftDateTime:
     Result := varDate;
   ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
     ftDBaseOle, ftTypedBinary, ftCursor:
     Result := varByte or varArray;
   ftFixedChar:
     Result := varString;
   ftLargeint:
     Result := varInteger;
   ftWideString:
     Result := varOleStr;
   ftArray:
     Result := varVariant or varArray;
   ftReference:
     Result := varVariant or varByRef;
   ftDataSet, ftADT:
     Result := varUnknown;

   // ftUnknown
   else
     Result := varNull;
  end;
end;

function VarRecToOleVariant(const AVarRec: TVarRec): OleVariant;
begin
  case AVarRec.VType of
   vtInteger    : Result := AVarRec.VInteger;
   vtBoolean    : Result := AVarRec.VBoolean;
   vtChar       : Result := AVarRec.VChar;
   vtExtended   : Result := AVarRec.VExtended^;
   vtString     : Result := AVarRec.VString^;
   vtPChar      : Result := String(AVarRec.VPChar);
   vtWideChar   : Result := AVarRec.VWideChar;
   vtPWideChar  : Result := WideString(AVarRec.VPWideChar);
   vtAnsiString : Result := String(AVarRec.VAnsiString);
   vtCurrency   : Result := AVarRec.VCurrency^;
   vtVariant    : Result := AVarRec.VVariant^;
   vtWideString : Result := WideString(AVarRec.VWideString);
   vtPointer    : TVarData(Result).VPointer := AVarRec.VPointer;
   vtObject     : TVarData(Result).VPointer := AVarRec.VObject;
   vtClass      : TVarData(Result).VPointer := AVarRec.VClass;
   vtInterface  : TVarData(Result).VDispatch := AVarRec.VInterface;
  end;
end;

function VarRecToString(const AVarRec: TVarRec): string;
var
  S : string;
begin
  case AVarRec.VType of
    vtInteger    : S := Format('%d', [AVarRec.VInteger]);
    vtInt64      : S := Format('%d', [AVarRec.VInt64^]);
    vtString     : S := Format('%s', [AVarRec.VString^]);
    vtWideString : S := Format('%s', [WideString(AVarRec.VWideString)]);
    vtAnsiString : S := Format('%s', [AnsiString(AVarRec.VAnsiString)]);
    vtCurrency   : S := Format('%s', [CurrToStr(AVarRec.VCurrency^)]);
    vtExtended   : S := Format('%s', [FloatToStr(AVarRec.VExtended^)]);
    vtChar       : S := Format('%s', [AVarRec.VChar]);
    vtPChar      : S := Format('%s', [AVarRec.VPChar]);
    vtWideChar   : S := Format('%s', [AVarRec.VWideChar]);
    vtPWideChar  : S := Format('%s', [AVarRec.VPWideChar]);
    vtVariant    : S := Format('%s', [VarToStr(AVarRec.VVariant^)]);
  else
    S := 'Unassigned'
    // TODO: better: raise exception
  end;
  Result := S;
end;

function VarRecToVariant(const AVarRec: TVarRec): Variant;
begin
  with AVarRec do
    case VType of
      vtInteger,
      vtObject     : Result := VInteger;
      vtBoolean    : Result := VBoolean;
      vtExtended,
      vtCurrency   : Result := VExtended^;
      vtChar       : Result := VChar;
      vtString     : Result := VString^;
      vtAnsiString : Result := string(VAnsiString);
      vtVariant    : Result := VVariant^;
      else
        Result := Null;
    end;
end;

procedure ChangeOwner(AComponent, ANewOwner: TComponent);
begin
  if Assigned(AComponent) and Assigned(ANewOwner) then
  begin
    AComponent.Owner.RemoveComponent(AComponent);
    ANewOwner.InsertComponent(AComponent);
  end;
end;

function Unformat(const ASource, APattern: string; const AArgs: array of const)
  : Integer;

{ The opposite of Format, Unformat splits up a formatted source string into
  substrings and Integers.  It is an alternative to parsing when the format is
  known to be fixed. The pattern parameter contains the format string, which is
  a combination of plain characters and format specifiers.

  The following specifiers are supported:

  %s   indicates that a string value is required
  %d   indicates that an integer value is required
  %S   indicates that a string value should be ignored
  %D   indicates that an integer value should be ignored

  Unformat compares the source with the pattern, and plain characters
  that do not match will raise an EConvertError.  When a format specifier
  is encountered in the pattern, an argument is fetched and used to
  store the result that is obtained from the source.  Then the comparison
  continues.

  For each %s, the args list must contain a pointer to a string variable,
  followed by an integer specifying the maximum length of the string.
  For each %d, the args list must contain a pointer to an integer variable.

  When the end of the source string is reached, the function returns
  without modifying the remaining arguments, so you might wish to initialize
  your variables to "default" values before the function call.

  Unformat returns the number of values it has extracted.

  Examples:

  var
    s1, s2: string[31];
    i: Integer;

  Unformat('[abc]123(def)', '[%s]%d(%s)', [@s1, 31, @i, @s2, 31]);
    (* s1 = 'abc', i = 123, s2 = 'def' *)

  Unformat('Hello, Universe!!!', '%s, %s%d', [@s1, 31, @s2, 31, @i]);
    (* s1 = 'Hello', s2 = 'Universe!!!', i is untouched *)

  Unformat('How much wood could a woodchuck chuck...',
           '%S %S %s could a %S %s...', [@s1, 31, @s2, 31]);
    (* s1 = 'wood', s2 = 'chuck' *) }

const
  Digits = ['0'..'9'];

var
  I, J      : Integer;
  iArgIndex : Integer;
  iStart    : Integer;
  iEnd      : Integer;
  iMaxLen   : Integer;
  C         : Char;
begin
  Result := 0;
  iArgIndex := 0;
  I := 1;
  J := 1;
  while (I < Length(APattern)) and (J <= Length(ASource)) do
  begin
    if APattern[I] = '%' then
      case APattern[I+1] of
        'D': begin
               Inc(I, 2);
               while (J <= Length(ASource)) and
                 (CharInSet(ASource[J], Digits) or (ASource[J] = '-')) do Inc(J);
               Inc(Result);
             end;
        'S': begin
               Inc(I, 2);
               if I > Length(APattern) then break
               else
               begin
                 c := APattern[I];
                 while (J <= Length(ASource)) and (ASource[J] <> c) do
                   Inc(J);
               end;
               Inc(Result);
             end;
        'd': begin
               if iArgIndex > High(AArgs) then
                 raise EConvertError.Create('Not enough arguments');
               Inc(I, 2);
               iStart := J;
               while (J <= Length(ASource)) and
                 (CharInSet(ASource[J], Digits) or (ASource[J] = '-')) do Inc(J);
               iEnd := J;
               if iEnd > iStart then
                 PInteger(AArgs[iArgIndex].VPointer)^ :=
                   StrToInt(Copy(ASource, iStart, iEnd - iStart));
               Inc(iArgIndex);
               Inc(Result);
             end;
        's': begin
               if iArgIndex > High(AArgs) - 1 then
                 raise EConvertError.Create('Not enough arguments');
               if AArgs[iArgIndex + 1].VType <> vtInteger then
                 raise EConvertError.Create('No string size specified');
               iMaxLen := AArgs[iArgIndex+1].VInteger;
               Inc(I, 2);
               if I > Length(APattern) then
               begin
                 AArgs[iArgIndex].VString^ := ShortString(
                   Copy(ASource, J, Min(Length(ASource) + 1 - J, iMaxLen))
                 );
                 Break;
               end
               else
               begin
                 c := APattern[I];
                 iStart := J;
                 while (J <= Length(ASource)) and (ASource[J] <> c) do
                   Inc(J);
                 iEnd := J;
                 AArgs[iArgIndex].VString^ := ShortString(
                   Copy(ASource, iStart, Min(iEnd - iStart, iMaxLen))
                 );
                 Inc(iArgIndex, 2);
               end;
               Inc(Result);
             end;
      else Inc(I);
      end
    else
      {if APattern[I] <> ASource[J] then
        raise EConvertError.Create('Pattern mismatch')
      else}
      begin
        Inc(I);
        Inc(J);
      end;
  end;
end;

function VariantCompare(AVariant1, AVariant2 : Variant) : Boolean;
begin
  Result := False;
  if (VarType(AVariant1) = VarType(AVariant2)) then
     Result := AVariant1 = AVariant2;
end;

{ 'Like' code is written by Wladimir Perepletchick }

function Like(const ASource, ATemplate: string): Boolean;
const
  SpecialChars: TSysCharSet = ['%', '*', '?', '_'];
var
 I, J, K, LTemplate, LSource: Integer;
begin
  Result := False;
  LTemplate := Length(ATemplate);
  LSource := Length(ASource);
  I := 1;
  J := 1;
  while (I <= LTemplate) and (J <= LSource) do
  begin
    case ATemplate[I] of
      '?', '_': ;
      '*', '%':
      begin
        while CharInSet(ATemplate[I], SpecialChars) and (I <= LTemplate) do
          Inc(I);
        if I > LTemplate then
          Result := True
        else
          while J <= LSource do
          begin
            while (ASource[J] <> ATemplate[I]) and (J <= LSource) do
              Inc(J);
            if J > LSource then Break;
            K := 0;
            while (ASource[J + K] = ATemplate[I + K]) and
                  (J + K <= LSource) and (I + K <= LTemplate) and
                  (not CharInSet(ATemplate[I + K], SpecialChars)) do
              Inc(K);
            if CharInSet(ATemplate[I + K], SpecialChars) or (I + K > LTemplate) then
            begin
              Inc(I, K - 1);
              Inc(J, K - 1);
              Break;
            end;
            Inc(J, K);
          end;
          if J > LSource then
            Break;
      end;
      else
        if (ASource[J] <> ATemplate[I]) then
          Break;
    end;
    Inc(I);
    Inc(J);
    if (J > LSource) then
    begin
      K := 0;
      while CharInSet(ATemplate[I + K], ['%', '*']) and (I + K <= LTemplate) do
        Inc(K);
      if (I + K > LTemplate) then
        Result := True;
    end;
  end;
end;

function CreateGUIDString: string;
var
  ClassID : TCLSID;
  P       : PWideChar;
begin
  CoCreateGuid(ClassID);
  StringFromCLSID(ClassID, P);
  Result := P;
  CoTaskMemFree(P);
end;

function CreateUniqueID: String;
var
  AGUID       : TGUID;
  AGUIDString : Widestring;
begin
  CoCreateGUID(AGUID);
  SetLength(AGUIDString, 39);
  StringFromGUID2(AGUID, PWideChar(AGUIDString), 39);
  Result := string(PWideChar(AGUIDString));
  Result := Copy(Result, 2, 36);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

{ Formats the given time amount (in seconds) to the form:
  <Hours>:<Minutes>:<Seconds>.<Hundreds> }

function FormatElapsedTime(ASeconds: Extended): string;
var
  H  : Integer;
  M  : Integer;
  S  : Integer;
  HS : Integer;
begin
  HS := Round(Int(100 * Frac(ASeconds)));
  S  := Round(Int(ASeconds));
  H  := S div 3600;
  S  := S mod 3600;
  M  := S div 60;
  S  := S mod 60;

  Result := Format('%2.2d:%2.2d:%2.2d.%2.2d', [H, M, S, HS]);
end;

{ Author: Michael Haller }

function FormatByteText(ABytes: Integer): string;
var
  D : Double;
begin
  Result := 'n.a.';
  try
    D := ABytes / 1024;
    if ABytes = 0 then
      Result := '0 Byte'
    else
      if ABytes < 1048576 then
        Result := FloatToStrF(D, ffNumber, 18, 1)+' KB'
      else
        if ABytes < 1073741824 then begin
          D := D / 1024;
          ABytes := Round(D);
          if ABytes < 10 then
            Result := FloatToStrF(D, ffNumber, 18, 1)+' MB'
          else
            Result := IntToStr(ABytes)+' MB';
        end else begin
          ABytes := Round(D / 1024 / 1024);
          Result := IntToStr(ABytes)+' GB';
        end;
    if (Result[Length(Result)-3] = '0') and (Result[Length(Result)-4] = ',') then
      Delete(Result, Length(Result)-4, 2);
  except
    // ignore exceptions
  end;
end;

{ Mixes two colors for a given transparancy level (Author: Yurii Zhukow). }

function MixColors(FG, BG: TColor; T: Byte): TColor;
var
  R, G, B : Byte;

  function MixBytes(FG, BG, T: Byte): Byte;
  asm
    push bx
    push cx
    push dx
    mov DH,T
    mov BL,FG
    mov AL,DH
    mov CL,BG
    xor AH,AH
    xor BH,BH
    xor CH,CH
    mul BL
    mov BX,AX
    xor AH,AH
    mov AL,DH
    xor AL,$FF
    mul CL
    add AX,BX
    shr AX,8
    pop dx
    pop cx
    pop bx
  end;

begin
  R := MixBytes(FG and 255, BG and 255, T);
  G := MixBytes((FG shr 8) and 255,(BG shr 8) and 255, T);
  B := MixBytes((FG shr 16) and 255,(BG shr 16) and 255, T);
  Result := R + G * 256 + B * 65536;
end; // MixColors

function ColorAdjustBrightness(Col: TColor; Shift: SmallInt): TColor;
var
  Lightness: Byte;
begin
  // If base color is bright, make bg color darker (grey), and vice versa, so that
  // colors work with high contrast mode for accessibility
  Lightness := GetLightness(Col);
  if (Lightness < 128) and (Shift < 0) then
    Shift := Abs(Shift)
  else if (Lightness > 128) and (Shift > 0) then
    Shift := 0 - Abs(Shift);
  Result := ColorAdjustLuma(Col, Shift, true);
end;

procedure Delay(Milliseconds: Integer);
var
  Tick  : DWord;
  Event : THandle;
begin
  Event := CreateEvent(nil, False, False, nil);
  try
    Tick := GetTickCount + DWord(Milliseconds);
    while (Milliseconds > 0) and
          (MsgWaitForMultipleObjects(1, Event, False, Milliseconds, QS_ALLINPUT)
          <> WAIT_TIMEOUT) do
    begin
      Application.ProcessMessages;
      if Application.Terminated then Exit;
      Milliseconds := Tick - GetTickcount;
    end;
  finally
    CloseHandle(Event);
  end;
end;

procedure ShowInfo(const AInfoString : string);
begin
  MessageBox(
    Application.Handle,
    PChar(AInfoString),
    PChar(Application.Title),
    MB_ICONINFORMATION or MB_OK or MB_SETFOREGROUND or MB_TOPMOST or MB_APPLMODAL
  );
end;

procedure ShowInfo(const AInfoString : string;
  const AArguments : array of const);
begin
  ShowInfo(Format(AInfoString, AArguments));
end;

procedure ShowError(const AErrorString: string);
begin
  MessageBox(
    Application.Handle,
    PChar(AErrorString),
    PChar(Application.Title),
    MB_ICONERROR or MB_OK or MB_SETFOREGROUND or MB_TOPMOST or MB_APPLMODAL
  );
end;

procedure ShowError(const AErrorString: string;
  const AArguments: array of const);
begin
  ShowError(Format(AErrorString, AArguments));
end;

procedure ShowWarning(const AWarningString : string);
begin
  MessageBox(
    Application.Handle,
    PChar(AWarningString),
    PChar(Application.Title),
    MB_ICONWARNING or MB_OK or MB_SETFOREGROUND or MB_TOPMOST or MB_APPLMODAL
  );
end;

procedure ShowWarning(const AWarningString : string;
  const AArguments: array of const);
begin
  ShowWarning(Format(AWarningString, AArguments));
end;

{ Returns the offset to the Pointer to the Interface Method Table.

  Author : Hallvard Vassbotn

  PIMT is short for pointer to interface method table. This is a special
  compiler generated "field" that is added to an object instance by the compiler
  when you declare that the class implements an interface. The "field" is a
  pointer to a kind of virtual method table for the methods declared on the
  interface. The function returns the offset of this field. Note that the
  compiler uses an ADD assembly instruction to adjust the Self parameter -
  but the value added is actually negative. That's why we return the negated
  value of the adjustment offset. }

function GetPIMTOffset(const I: IInterface): Integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte : ShortInt);
      AddLong : (AdjustmentLong : LongInt);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := -1;
  if Assigned(Pointer(I)) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Result := -QueryInterfaceThunk.AdjustmentByte;
        AddLong: Result := -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;

procedure RunApplication(AParams: string; AFile: string; AWait : Boolean);
  // borrowed from Project JEDI Code Library (JCL)
  procedure ResetMemory(out P; Size: Longint);
  begin
    if Size > 0 then
    begin
      Byte(P) := 0;
      FillChar(P, Size, 0);
    end;
  end;
  // borrowed from Project JEDI Code Library (JCL)
  function PCharOrNil(const S: string): PChar;
  begin
    Result := Pointer(S);
  end;
  // borrowed from Project JEDI Code Library (JCL)
  function ShellExecAndWait(const FileName: string;
  const Parameters: string = ''; const Verb: string = '';
  CmdShow: Integer = SW_HIDE; const Directory: string = ''): Boolean;
  var
    Sei: TShellExecuteInfo;
    Res: LongBool;
    Msg: tagMSG;
  begin
    ResetMemory(Sei, SizeOf(Sei));
    Sei.cbSize := SizeOf(Sei);
    Sei.fMask := SEE_MASK_DOENVSUBST  or SEE_MASK_FLAG_NO_UI  or SEE_MASK_NOCLOSEPROCESS or
      SEE_MASK_FLAG_DDEWAIT;
    Sei.lpFile := PChar(FileName);
    Sei.lpParameters := PCharOrNil(Parameters);
    Sei.lpVerb := PCharOrNil(Verb);
    Sei.nShow := CmdShow;
    Sei.lpDirectory := PCharOrNil(Directory);
    {$TYPEDADDRESS ON}
    Result := ShellExecuteEx(@Sei);
    {$IFNDEF TYPEDADDRESS_ON}
    {$TYPEDADDRESS OFF}
    {$ENDIF ~TYPEDADDRESS_ON}
    if Result then
    begin
      WaitForInputIdle(Sei.hProcess, INFINITE);
      while WaitForSingleObject(Sei.hProcess, 10) = WAIT_TIMEOUT do
        repeat
          Msg.hwnd := 0;
          Res := PeekMessage(Msg, Sei.Wnd, 0, 0, PM_REMOVE);
          if Res then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        until not Res;
      CloseHandle(Sei.hProcess);
    end;
  end;
   // borrowed from Project JEDI Code Library (JCL)
  function ShellExecEx(const FileName: string; const Parameters: string = '';
  const Verb: string = ''; CmdShow: Integer = SW_SHOWNORMAL): Boolean;
  var
    Sei: TShellExecuteInfo;
  begin
    ResetMemory(Sei, SizeOf(Sei));
    Sei.cbSize := SizeOf(Sei);
    Sei.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
    Sei.lpFile := PChar(FileName);
    Sei.lpParameters := PCharOrNil(Parameters);
    Sei.lpVerb := PCharOrNil(Verb);
    Sei.nShow := CmdShow;
    {$TYPEDADDRESS ON}
    Result := ShellExecuteEx(@Sei);
    {$IFNDEF TYPEDADDRESS_ON}
    {$TYPEDADDRESS OFF}
    {$ENDIF ~TYPEDADDRESS_ON}
  end;

begin
  if FileExists(AFile) then
  begin
    if AWait then
      ShellExecAndWait(AFile, AParams)
    else
      ShellExecEx(AFile, AParams);
  end
  else
    raise Exception.CreateFmt('"%s" not found', [AFile]);
end;

function WordCount(const AString: string; const AWordDelims: TSysCharSet)
  : Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(AString);
  while I <= SLen do
    begin
      while (I <= SLen) and CharInSet(AString[I], AWordDelims) do
        Inc(I);
      if I <= SLen then
        Inc(Result);
      while (I <= SLen) and not CharInSet(AString[I], AWordDelims) do
        Inc(I);
    end;
end;

function WordPosition(const AIndex: Integer; const AString: string;
  const AWordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(AString)) and (Count <> AIndex) do
    begin
    { skip over delimiters }
      while (I <= Length(AString)) and CharInSet(AString[I], AWordDelims) do
        Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
      if I <= Length(AString) then
        Inc(Count);
    { if not finished, find the end of the current word }
      if Count <> AIndex then
        while (I <= Length(AString)) and not CharInSet(AString[I], AWordDelims) do
          Inc(I)
      else
        Result := I;
    end;
end;

function ExtractWord(const AIndex: Integer; const AString: string;
  const AWordDelims: TSysCharSet): string;
var
  I   : Integer;
  Len : Integer;
begin
  Len := 0;
  I := WordPosition(AIndex, AString, AWordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(AString)) and not CharInSet(AString[I], AWordDelims) do
      begin
      { add the I'th character to result }
        Inc(Len);
        SetLength(Result, Len);
        Result[Len] := AString[I];
        Inc(I);
      end;
  SetLength(Result, Len);
end;

procedure DisableControls(AControlContainer : TWinControl);
begin
  EnableControls(AControlContainer, False)
end;

{ Enables/disables all child controls of the AControlContainer control (
  eg. TPanel, TGroupBox, etc.). }

procedure EnableControls(AControlContainer : TWinControl; AEnabled : Boolean);
var
  I : Integer;
  C : TControl;
begin
  if not (csAcceptsControls in AControlContainer.ControlStyle) then
    raise Exception.Create('Invalid control container!');

  for I := 0 to AControlContainer.ControlCount - 1 do
  begin
    C := AControlContainer.Controls[I];
    if (csAcceptsControls in C.ControlStyle) then
      EnableControls(TWinControl(C), AEnabled);

    if C is TWinControl then
    begin
      C.Enabled := AEnabled;
      if not (csParentBackground in C.ControlStyle) and
         IsPublishedProp(C, 'Color') then
      begin
        if AEnabled then
          SetOrdProp(C, 'Color', clWhite)
        else
          SetOrdProp(C, 'Color', clBtnFace);
      end;
    end
    else if IsPublishedProp(C, 'Enabled') then
    begin
      if AEnabled then
        SetEnumProp(C, 'Enabled', 'True')
      else
        SetEnumProp(C, 'Enabled', 'False');
    end;
  end;
end;

function URLEncode(const AString: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    case AString[I] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + AString[I];
      ' ':
        Result := Result + '%20';
      else
        Result := Result + '%' + System.SysUtils.IntToHex(Ord(AString[I]), 2);
    end;
  end;
end;

function CallEventHandler(Instance: TObject; Event: TRttiProperty;
  const Args: array of TValue): TValue; overload;
var
  HandlerValue: TValue;
  HandlerObj: TObject;
  MethodRecPtr: ^TMethod;
  RttiContext: TRttiContext;
  RttiMethod: TRttiMethod;
begin
  if Event.PropertyType.TypeKind <> tkMethod then
    raise EInvocationError.CreateFmt(SPropertyNotAnEvent, [Instance.ClassName, Event.Name]);
  Result := nil;
  HandlerValue := Event.GetValue(Instance);
  if HandlerValue.IsEmpty then Exit;
  MethodRecPtr := HandlerValue.GetReferenceToRawData;
  { check for event types we know }
  if HandlerValue.TypeInfo = TypeInfo(TNotifyEvent) then
  begin
    TNotifyEvent(MethodRecPtr^)(Args[0].AsObject);
    Exit;
  end;
  if HandlerValue.TypeInfo = TypeInfo(TMouseEvent) then
  begin
    TMouseEvent(MethodRecPtr^)(Args[0].AsObject, TMouseButton(Args[1].AsOrdinal),
      Args[2].AsType<TShiftState>, Args[3].AsInteger, Args[4].AsInteger);
    Exit;
  end;
  if HandlerValue.TypeInfo = TypeInfo(TMouseMoveEvent) then
  begin
    TMouseMoveEvent(MethodRecPtr^)(Args[0].AsObject,
      Args[1].AsType<TShiftState>, Args[2].AsInteger, Args[3].AsInteger);
    Exit;
  end;
  { still here? well, let's go for the generic approach }
  HandlerObj := MethodRecPtr.Data;
  for RttiMethod in RttiContext.GetType(HandlerObj.ClassType).GetMethods do
    if RttiMethod.CodeAddress = MethodRecPtr.Code then
    begin
      Result := RttiMethod.Invoke(HandlerObj, Args);
      Exit;
    end;
  raise EInsufficientRtti.Create(SEventHandlerHasInsufficientRTTI);
end;

function CallEventHandler(Instance: TObject; const EventName: string;
  const Args: array of TValue): TValue; overload;
var
  RttiContext: TRttiContext;
  Prop: TRttiProperty;
begin
  Prop := RttiContext.GetType(Instance.ClassType).GetProperty(EventName);
  if Prop = nil then
    raise EInvocationError.CreateFmt(SMissingEvent, [Instance.ClassName, EventName]);
  Result := CallEventHandler(Instance, Prop, Args);
end;

procedure LockPaint(AControl: TWinControl);
begin
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    SendMessage(AControl.Handle, WM_SETREDRAW, 0, 0);
  end;
end;

procedure UnlockPaint(AControl: TWinControl);
begin
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    SendMessage(AControl.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(
      AControl.Handle,
      nil,
      0,
      RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN
    );
  end;
end;

{ Note that SetWindowSizeGrip(..., false) does not really remove the hook -
  it just sets "Enabled" to false. The hook plus all data is removed when
  the window is destroyed.
}
procedure SetWindowSizeGrip(hWnd: hWnd; Enable: boolean);
var
  Info: PGripInfo;
begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if (Info = nil) and Enable then
  begin
    New(Info);
    FillChar(Info^, SizeOf(TGripInfo), 0);

    with Info^ do
    begin
      Info^.OldWndProc := TWndProc(Pointer(GetWindowLong(hWnd, GWL_WNDPROC)));

      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    SetProp(hWnd, SizeGripProp, Cardinal(Info));
    SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@SizeGripWndProc));
  end;

  if Info <> nil then
    if Enable <> Info^.Enabled then
      with Info^ do
      begin
        Enabled := Enable;
        if (GripRect.Right > GripRect.Left) and
          (GripRect.Bottom > GripRect.Top) then
          InvalidateRect(hWnd, @GripRect, true);
      end;
end;

procedure AppendLine(
               var sToString : string;
             const sLine     : string); overload;
begin
  if sLine <> '' then
  begin
    sToString := IfThen(sToString = '', sLine, sToString + #13#10 + sLine);
  end;
end;

procedure AppendLine(
               var sToString : string;
             const sLine     : string;
             const Args      : array of const); overload;
begin
  AppendLine(sToString, Format(sLine, Args));
end;

{ Returns all property values of the instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record and an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);     // record types need to be casted to TValue
      S := AsPropString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsPropString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;
}

function AsPropString(AValue: TValue): string;
var
  P             : TRttiProperty;
  S             : string;
  V             : TValue;
  V2            : TValue;
  N             : Integer;
  ExcludedTypes : TTypeKinds;
begin
  Result := '';
  if not AValue.IsEmpty then
  begin
    ExcludedTypes := [
      tkClassRef, tkMethod, tkInterface, tkPointer, tkUnknown, tkArray,
      tkDynArray, tkClass
    ];
    N := 0;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
      begin
        if Length(P.Name) > N then
          N := Length(P.Name);
      end;
    end;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      try
        if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
        begin
          if AValue.IsObject then
            V := P.GetValue(AValue.AsObject)
          else
            V := P.GetValue(AValue.GetReferenceToRawData);

          if V.Kind = tkClass then
          begin
            S := P.Name + ': ' + V.AsObject.ClassName;
          end
          else if V.Kind = tkVariant then
            S := VarToStrDef(V.AsVariant, '')
          else if V.Kind = tkRecord then
          begin
            if TryGetUnderlyingValue(V, V2) then
            begin
              S := V2.ToString;
            end
            else
            begin
    //          raise Exception.Create('no TryGetUnderlyingValue');
            end
          end
          else
            S := V.ToString;
          if S <> '' then
            AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
        end;
      except
         // continue
      end;
    end;
  end;
end;

{ Returns all field values of the given instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);      // record types need to be casted to TValue
      S := AsFieldString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsFieldString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;
}

function AsFieldString(AValue : TValue): string;
var
  F             : TRttiField;
  S             : string;
  V             : TValue;
  N             : Integer;
  ExcludedTypes : TTypeKinds;
begin
  Result := '';
  if not AValue.IsEmpty then
  begin
    ExcludedTypes := [
      tkClassRef, tkMethod, tkInterface, tkPointer, tkUnknown, tkArray,
      tkDynArray
    ];
    N := 0;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if Length(F.Name) > N then
          N := Length(F.Name);
      end;
    end;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if AValue.IsObject then
          V := F.GetValue(AValue.AsObject)
        else
          V := F.GetValue(AValue.GetReferenceToRawData);
        if V.Kind = tkClass then
        begin
          if V.AsObject is TComponent then
            S := TComponent(V.AsObject).Name + ': ' + V.AsObject.ClassName;
        end
        else if V.Kind = tkVariant then
          S := VarToStrDef(V.AsVariant, '')
        else
          S := V.ToString;
        AppendLine(Result, Format('%-*s = %s', [N, F.Name, S]));
      end;
    end;
  end;
end;

{ Converts a given set instance to comma seperated string values, which can
  be optionally quoted. The whole string can be optionally enclosed between
  brackets.
  TrimChars defines the count of the prefix characters which should be omitted
  when contructing the value names. If TrimChars is not specified, the function
  will determine the prefix automatically (if the prefix is lowercase as
  usually is the case for enumerated type values in Delphi.)

  Example:

  var
    S: string;
  begin
    S := SetToString(TypeInfo(TAnchors), [akLeft, akTop]);
    ShowMessage(S);
  end;

  => Following string is shown: '(Left, Top)'
}

function SetToString(TypeInfo: PTypeInfo; const Value;
  QuoteValues: Boolean = True; Brackets: Boolean = True;
  TrimChars: Integer = -1): string;
var
  S    : TIntegerSet;
  I    : Integer;
  N    : Integer;
  Name : string;

  function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
  begin
    Result := 0;

    case GetTypeData(Info)^.OrdType of
      otSByte, otUByte:
        Result := Byte(SetParam);
      otSWord, otUWord:
        Result := Word(SetParam);
      otSLong, otULong:
        Result := Integer(SetParam);
    end;
  end;

  function GetPrefixLength(const AString: string): Integer;
  var
    C: Char;
    N: Integer;
  begin
    N := 0;
    if Length(AString) > 0 then
    begin
      C := AString[1];
      while (N < Length(AString)) and C.IsLower do
      begin
        Inc(N);
        C := AString[N + 1];
      end;
    end;
    Result := N;
  end;

begin
  Result := '';
  Integer(S) := GetOrdValue(TypeInfo, Value);
  TypeInfo := GetTypeData(TypeInfo)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Name := GetEnumName(TypeInfo, I);

      if TrimChars >= 0 then
        N := TrimChars
      else
        N := GetPrefixLength(Name);

      if N > 0 then
        Name := Copy(Name, N + 1, Length(Name) - N + 1);

      if QuoteValues then
        Name := QuotedStr(Name);

      Result := Result + Name;
    end;
  end;
  if Brackets and (Result <> '') then
    Result := '(' + Result + ')';
end;

function TryGetUnderlyingValue(const AValue: TValue; out AInnerValue: TValue)
  : Boolean;
var
  T : TRttiType;
  F : TRttiField;
begin
  T := FRtti.GetType(AValue.TypeInfo);
  F := T.GetField('FValue');
  Result := False;

  if Assigned(F) then
  begin
    AInnerValue := F.GetValue(AValue.GetReferenceToRawData);
    Result := True;
  end
  else
  begin
    AInnerValue := TValue.Empty;
  end;
end;

{ Adjusts the control style of all child controls of a given control so that
  during a drag operation the drag image is shown when dragging over the
  control.
  If csDisplayDragImage is not set only the drag cursor will be shown. }

procedure FixControlStylesForDrag(AParent: TControl);
var
  I: Integer;
begin
  AParent.ControlStyle := AParent.ControlStyle + [csDisplayDragImage];
  if AParent is TWinControl then
    with TWinControl(AParent) do
      for I := 0 to ControlCount - 1 do
        FixControlStylesForDrag(Controls[I]);
end;

procedure DebugString(const AString: string); overload;
begin
  OutputDebugString(PChar(AString));
end;

procedure DebugString(const AString: string; const Args: array of const); overload;
begin
  OutputDebugString(PChar(Format(AString, Args)));
end;

function ExtractGenericArguments(ATypeInfo: PTypeInfo): string;
var
  i: Integer;
  Name: string;
begin
  Name := string(ATypeInfo.Name);
  i := Pos('<', Name);
  if i > 0 then
  begin
    Result := Copy(Name, Succ(i), Length(Name) - Succ(i));
  end
  else
  begin
    Result := ''
  end;
end;

function IsGenericTypeOf(ATypeInfo: PTypeInfo;
  const ABaseTypeName: string): Boolean;
var
  Name: string;
begin
  Name := string(ATypeInfo.Name);
  Result := (Copy(Name, 1, Succ(Length(ABaseTypeName))) = (ABaseTypeName + '<'))
    and (Copy(Name, Length(Name), 1) = '>');
end;

function TryGetUnderlyingTypeName(ATypeInfo: PTypeInfo;
  out InnerTypeName: string): Boolean;
begin
  if (ATypeInfo = nil) or (ATypeInfo.Kind <> tkRecord) then
  begin
    Exit(False);
  end;
  if IsGenericTypeOf(ATypeInfo,'Nullable')
    or IsGenericTypeOf(ATypeInfo,'NotNullable') then
  begin
    Result := True;
    InnerTypeName := ExtractGenericArguments(ATypeInfo);
  end
  else
  begin
    Exit(False);
  end;
end;

function TryGetUnderlyingTypeInfo(ATypeInfo: PTypeInfo;
  out AInnerTypeInfo: PTypeInfo): Boolean;
var
  InnerTypeName : string;
  RttiType      : TRttiType;
begin
  Result := TryGetUnderlyingTypeName(ATypeInfo, InnerTypeName);
  if Result then
  begin
    RttiType := FContext.FindType(InnerTypeName);
    if RttiType <> nil then
      AInnerTypeInfo := RttiType.Handle
    else
      AInnerTypeInfo := nil;
    Result := AInnerTypeInfo <> nil;
  end;
end;

//function TryGetUnderlyingValue(const AValue: TValue; out AInnerValue: TValue)
//  : Boolean;
//var
//  T : TRttiType;
//  F : TRttiField;
//begin
//  T := FContext.GetType(AValue.TypeInfo);
//  F := T.GetField('FValue');
//  Result := True;
//  if Assigned(F) then
//  begin
//    if T.GetField('FHasValue').GetValue(AValue.GetReferenceToRawData).AsBoolean then
//      AInnerValue := F.GetValue(AValue.GetReferenceToRawData)
//    else
//      AInnerValue := TValue.Empty;
//  end
//  else
//  begin
//    AInnerValue := AValue;
//  end;
//end;

function TrySetUnderlyingValue(var AValue: TValue; const AInnerValue: TValue)
  : Boolean;
var
  T : TRttiType;
  F : TRttiField;
  H : TRttiField;
  A : TRttiField;
begin
  T := FContext.GetType(AValue.TypeInfo);
  F := T.GetField('FValue');
  H := T.GetField('FHasValue');
  A := T.GetField('FAddr');
  Result := False;

  if Assigned(F) then
  begin
    try
      F.SetValue(AValue.GetReferenceToRawData, AInnerValue);
    except

    end;
    Result := True;
  end
  else
  begin
    F.SetValue(AValue.GetReferenceToRawData, TValue.Empty);
  end;
  if Assigned(H) and Assigned(A) and Result then
  begin
    H.SetValue(AValue.GetReferenceToRawData, True);
    A.SetValue(AValue.GetReferenceToRawData, '');
  end;
end;

end.

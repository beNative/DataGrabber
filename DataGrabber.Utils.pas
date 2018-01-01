{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.Utils;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Controls, Vcl.Graphics,

  VirtualTrees;

function ContainsFocus(AControl: TWinControl): Boolean;

function GetTextWidth(const AText: string): Integer; overload;

function GetMaxTextWidth(AStrings: TStrings): Integer;

function GetTextWidth(
  const AText : string;
  AFont       : TFont
): Integer; overload;

function GetTextHeight(
  const AText : string;
  AFont       : TFont
): Integer;

function Explode(ASeparator, AText: string): TStringList;

function FindNode(
  AVT         : TVirtualStringTree;
  AIdx        : Integer;
  AParentNode : PVirtualNode
): PVirtualNode;

procedure SelectNode(
  AVT         : TVirtualStringTree;
  AIdx        : Integer;
  AParentNode : PVirtualNode = nil
); overload;

procedure SelectNode(
  AVT   : TVirtualStringTree;
  ANode : PVirtualNode
); overload;

procedure RunApplication(
  AParams : string;
  AFile   : string;
  AWait   : Boolean = True
);

procedure LockPaint(AControl: TWinControl);

procedure UnlockPaint(AControl: TWinControl);

implementation

uses
  Winapi.ShellAPI, Winapi.Messages,
  System.SysUtils,
  Vcl.Forms;

{$REGION 'interfaced routines'}
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

function GetTextWidth(const AText: string): Integer;
var
  SL      : TStringList;
  I, W, R : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AText;
    R := 0;
    for I := 0 to SL.Count - 1 do
    begin
      W := Length(SL[I]);
      if W > R then
        R := W;
    end;
    Result := R;
  finally
    SL.Free;
  end;
end;

function GetMaxTextWidth(AStrings: TStrings): Integer;
var
  I : Integer;
  N : Integer;
begin
  Result := 0;
  if Assigned(AStrings) then
  begin
    for I := 0 to AStrings.Count - 1 do
    begin
      N := GetTextWidth(AStrings[I]);
      if N > Result then
        Result := N;
    end;
  end;
end;

function RunFormatterProcess(const AExeName: string; const AParams: string;
  const AString: string; const ATempFile: string): string;
var
  SL : TStringList;
  S  : string;
  T  : string;
begin
  S := ExtractFilePath(Application.ExeName) + AExeName;
  T := ExtractFilePath(Application.ExeName) + ATempFile;
  if FileExists(S) then
  begin
    SL := TStringList.Create;
    try
      SL.Text := AString;
      SL.SaveToFile(T);
      RunApplication(Format(AParams, [T]), S);
      SL.LoadFromFile(T);
      Result := SL.Text;
    finally
      FreeAndNil(SL);
    end;
    if FileExists(T) then
      DeleteFile(T);
  end
  else
    raise Exception.CreateFmt('%s not found!', [S]);
end;

function FormatSQL(const AString: string): string;
begin
  Result := RunFormatterProcess(
    'SQLFormatter.exe',
    '%s /is:"  " /st:2 /mw:80 /tc /uk- /ae',
    AString,
    'Formatter.sql'
  );
end;

function Explode(ASeparator, AText: string): TStringList;
var
  I    : Integer;
  Item : string;
begin
  // Explode a string by separator into a TStringList
  Result := TStringList.Create;
  while True do
  begin
    I := Pos(ASeparator, AText);
    if I = 0 then
    begin
      // Last or only segment: Add to list if it's the last. Add also if it's not empty and list is empty.
      // Do not add if list is empty and text is also empty.
      if (Result.Count > 0) or (AText <> '') then
        Result.Add(AText);
      Break;
    end;
    Item := Trim(Copy(AText, 1, I - 1));
    Result.Add(Item);
    Delete(AText, 1, I - 1 + Length(ASeparator));
  end;
end;

function FindNode(AVT: TVirtualStringTree; AIdx: Integer; AParentNode: PVirtualNode): PVirtualNode;
var
  Node: PVirtualNode;
begin
  // Helper to find a node by its index
  Result := nil;
  if Assigned(AParentNode) then
    Node := AVT.GetFirstChild(AParentNode)
  else
    Node := AVT.GetFirst;
  while Assigned(Node) do
  begin
    if Node.Index = Cardinal(AIdx) then
    begin
      Result := Node;
      break;
    end;
    Node := AVT.GetNextSibling(Node);
  end;
end;

procedure SelectNode(AVT: TVirtualStringTree; AIdx: Integer; AParentNode: PVirtualNode);
var
  Node: PVirtualNode;
begin
  // Helper to focus and highlight a node by its index
  Node := FindNode(AVT, AIdx, AParentNode);
  if Assigned(Node) then
    SelectNode(AVT, Node);
end;

procedure SelectNode(AVT: TVirtualStringTree; ANode: PVirtualNode);
begin
  AVT.ClearSelection;
  AVT.FocusedNode := ANode;
  AVT.Selected[ANode] := True;
  AVT.ScrollIntoView(ANode, False);
end;

function ContainsFocus(AControl: TWinControl): Boolean;
var
  H : HWND;
  C : TWinControl;
begin
  H := Winapi.Windows.GetFocus;
  C := FindControl(H);
  if not Assigned(C) then
    Result := False
  else
  begin
    Result := AControl.ContainsControl(C);
  end;
end;
{$ENDREGION}

end.

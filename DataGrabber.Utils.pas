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

implementation

uses
  Winapi.ShellAPI, Winapi.Messages,
  System.SysUtils,
  Vcl.Forms,

  DDuce.Utils, DDuce.Utils.Winapi;

{$REGION 'interfaced routines'}
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
      Break;
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
{$ENDREGION}

end.

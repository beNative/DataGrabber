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

unit DataGrabber.EditorView;

{ Simple BCEditor-based SQL editor. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  BCEditor.Editor.Base, BCEditor.Editor,

  DataGrabber.Interfaces;

type
  TfrmEditorView = class(TForm, IEditorView)
  private
    FEditor : TBCEditor;

    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEditorFocused: Boolean;

  protected
    procedure CreateEditor;

  public
    procedure AfterConstruction; override;
    constructor Create; reintroduce; virtual;

    procedure CopyToClipboard;
    procedure FillCompletionLists(ATables, AAttributes : TStrings);

    procedure SetFocus; override;

    property EditorFocused: Boolean
      read GetEditorFocused;

    property Color: TColor
      read GetColor write SetColor;

    property Text: string
      read GetText write SetText;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
constructor TfrmEditorView.Create;
begin
  inherited Create(Application);
end;

procedure TfrmEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateEditor;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmEditorView.GetColor: TColor;
begin
  Result := FEditor.BackgroundColor;
end;

procedure TfrmEditorView.SetColor(const Value: TColor);
begin
  FEditor.BackgroundColor := Value;
end;

function TfrmEditorView.GetEditorFocused: Boolean;
begin
  Result := FEditor.Focused;
end;

function TfrmEditorView.GetText: string;
begin
  Result := FEditor.Text;
end;

procedure TfrmEditorView.SetText(const Value: string);
begin
  FEditor.Text := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditorView.CreateEditor;
begin
  FEditor := TBCEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.AlignWithMargins := False;
  FEditor.BorderStyle := bsSingle;
  FEditor.Directories.Colors := 'Colors';
  FEditor.Directories.Highlighters := 'Highlighters';
  FEditor.Highlighter.LoadFromFile('SQL - Standard' + '.json');
  FEditor.Highlighter.Colors.LoadFromFile('tsColors' + '.json');
  FEditor.CodeFolding.Visible := True;
  FEditor.Font.Name := 'Consolas';
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmEditorView.SetFocus;
begin
  if FEditor.CanFocus then
    FEditor.SetFocus;
end;

procedure TfrmEditorView.CopyToClipboard;
begin
  FEditor.CopyToClipboard;
end;

procedure TfrmEditorView.FillCompletionLists(ATables, AAttributes: TStrings);
var
  I       : Integer;
//  Items   : TStringList;
//  Inserts : TStringList;
begin
  inherited SetFocus;
//  Items  := FSynCP.ItemList as TStringList;
//  Inserts := FSynCP.InsertList  as TStringList;
//  Items.Clear;
//  Inserts.Clear;
//  FSynSQL.TableNames.Clear;
  if Assigned(AAttributes) then
  begin
    for I := 0 to Pred(AAttributes.Count) do
    begin
//      if Inserts.IndexOf(AAttributes.Strings[I]) = -1 then
//      begin
//        Items.Insert(0, Format(SFieldItem, [AAttributes.Strings[I]]));
//        Inserts.Insert(0, AAttributes.Strings[I]);
//      end;
    end;
  end;
  if Assigned(ATables) then
  begin
    for I := 0 to Pred(ATables.Count) do
    begin
//      if Inserts.IndexOf(ATables.Strings[I]) = -1 then
//      begin
//        Items.Add(Format(STableItem, [ATables.Strings[I]]));
//        Inserts.Add(ATables.Strings[I]);
//        //FSynSQL.TableNames.Add(ATables.Strings[I]);
//      end;
    end;
  end;
end;
{$ENDREGION}

end.

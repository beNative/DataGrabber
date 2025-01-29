{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DataGrabber.inc}

unit DataGrabber.EditorView;

{ Simple TSynEdit based SQL editor. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,

  SynEdit, SynEditHighlighter, SynHighlighterSQL, SynCompletionProposal,
  SynEditMiscClasses,

  DataGrabber.Interfaces;

type
  TfrmEditorView = class(TForm, IEditorView)
    synSQL  : TSynSQLSyn;
    scpMain : TSynCompletionProposal;

  private
    FEditor  : TSynEdit;
    FManager : IConnectionViewManager;

    procedure SettingsChanged(Sender: TObject);

  protected
    {$REGION 'property access methods'}
    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEditorFocused: Boolean;
    function GetPopupMenu: TPopupMenu; reintroduce;
    procedure SetPopupMenu(const Value: TPopupMenu);
    {$ENDREGION}

    procedure CreateEditor;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(
      AOwner   : TComponent;
      AManager : IConnectionViewManager
    ); reintroduce; virtual;

    procedure AssignParent(AParent: TWinControl);
    procedure CopyToClipboard;
    procedure FillCompletionLists(ATables, AAttributes : TStrings);
    procedure ApplySettings;

    procedure SetFocus; override;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property EditorFocused: Boolean
      read GetEditorFocused;

    property Color: TColor
      read GetColor write SetColor;

    property Text: string
      read GetText write SetText;
  end;

implementation

uses
  System.UITypes,

  DataGrabber.Resources;

{$R *.dfm}

type
  TScrollStyle = System.UITypes.TScrollStyle;

{$REGION 'construction and destruction'}
constructor TfrmEditorView.Create(AOwner: TComponent;
  AManager: IConnectionViewManager);
begin
  inherited Create(AOwner);
  FManager := AManager;

end;

procedure TfrmEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateEditor;
  FManager.Settings.OnChanged.Add(SettingsChanged);
end;

procedure TfrmEditorView.BeforeDestruction;
begin
  FManager.Settings.OnChanged.Remove(SettingsChanged);
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmEditorView.GetColor: TColor;
begin
  Result := FEditor.Color;
end;

procedure TfrmEditorView.SetColor(const Value: TColor);
begin
  if Value <> Color then
  begin
    FEditor.Color := Value;
    FEditor.Gutter.GradientEndColor := Value;
  end;
end;

function TfrmEditorView.GetEditorFocused: Boolean;
begin
  Result := FEditor.Focused;
end;

function TfrmEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := FEditor.PopupMenu;
end;

procedure TfrmEditorView.SetPopupMenu(const Value: TPopupMenu);
begin
  FEditor.PopupMenu := Value;
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
procedure TfrmEditorView.SettingsChanged(Sender: TObject);
begin
  ApplySettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditorView.ApplySettings;
begin
  FEditor.Font.Assign(FManager.Settings.EditorFont);
  FEditor.Font.Size := 12;
end;

procedure TfrmEditorView.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

procedure TfrmEditorView.CreateEditor;
begin
  FEditor := TSynEdit.Create(Self);
  FEditor.Parent           := Self;
  FEditor.Align            := alClient;
  FEditor.AlignWithMargins := False;
  FEditor.BorderStyle      := bsNone;
  FEditor.Font.Assign(FManager.Settings.EditorFont);
  FEditor.Font.Size        := 12;
  FEditor.Highlighter      := synSQL;
  FEditor.Options := [
    eoAltSetsColumnMode,
    eoAutoIndent,
    eoDragDropEditing,
    eoDropFiles,
    eoEnhanceHomeKey,
    eoEnhanceEndKey,
    eoGroupUndo,
    eoScrollPastEol,
    eoShowScrollHint,
    eoSmartTabDelete,
    eoSmartTabs,
    eoSpecialLineDefaultFg,
    eoTabIndent,
    eoTabsToSpaces,
    eoTrimTrailingSpaces
  ];
  FEditor.ActiveLineColor           := clYellow;
  FEditor.WordWrap                  := True;
  FEditor.Gutter.AutoSize           := True;
  FEditor.Gutter.ShowLineNumbers    := True;
  FEditor.Gutter.Font.Name          := 'Consolas';
  FEditor.Gutter.Font.Color         := clSilver;
  FEditor.Gutter.Gradient           := True;
  FEditor.Gutter.GradientStartColor := clWhite;
  FEditor.Gutter.GradientEndColor   := clWhite;
  FEditor.Gutter.LeftOffset         := 0;
  FEditor.Gutter.RightOffset        := 0;
  FEditor.Gutter.RightMargin        := 2;
  FEditor.Gutter.Color              := cl3DLight;
  FEditor.RightEdgeColor            := cl3DLight;
  scpMain.Editor                    := FEditor;
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
  Items   : TStringList;
  Inserts : TStringList;
begin
  inherited SetFocus;
  Items   := scpMain.ItemList as TStringList;
  Inserts := scpMain.InsertList  as TStringList;
  Items.Clear;
  Inserts.Clear;
  synSQL.TableNames.Clear;
  if Assigned(AAttributes) then
  begin
    for I := 0 to Pred(AAttributes.Count) do
    begin
      if Inserts.IndexOf(AAttributes.Strings[I]) = -1 then
      begin
        Items.Insert(0, Format(SFieldItem, [AAttributes.Strings[I]]));
        Inserts.Insert(0, AAttributes.Strings[I]);
      end;
    end;
  end;
  if Assigned(ATables) then
  begin
    for I := 0 to Pred(ATables.Count) do
    begin
      if Inserts.IndexOf(ATables.Strings[I]) = -1 then
      begin
        Items.Add(Format(STableItem, [ATables.Strings[I]]));
        Inserts.Add(ATables.Strings[I]);
        synSQL.TableNames.Add(ATables.Strings[I]);
      end;
    end;
  end;
end;
{$ENDREGION}

end.

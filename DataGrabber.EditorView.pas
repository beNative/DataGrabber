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

unit DataGrabber.EditorView;

{ Simple SynEdit-based SQL editor. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  BCEditor.Editor.Base, BCEditor.Editor,

  DataGrabber.Interfaces;

type
  TfrmEditorView = class(TForm, IEditorView)
  private
    FSynEditor : TBCEditor;
//    FSynSQL    : TSynSQLSyn;
//    FSynCP     : TSynCompletionProposal;

    function GetText: string;
    procedure SetText(const Value: string);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEditorFocused: Boolean;
//    function GetOnStatusChange: TStatusChangeEvent;
//    procedure SetOnStatusChange(const Value: TStatusChangeEvent);

    procedure FSynEditorDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
//    procedure FSynCPExecute(Kind: SynCompletionType; Sender: TObject;
//      var CurrentInput: UnicodeString; var x, y: Integer; var CanExecute: Boolean);

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

//    property OnStatusChange: TStatusChangeEvent
//      read GetOnStatusChange write SetOnStatusChange;
  end;

implementation

{$R *.dfm}

uses
  DataGrabber.Resources;

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
//  Result := FSynEditor.Color;
end;

procedure TfrmEditorView.SetColor(const Value: TColor);
begin
//  FSynEditor.Color := Value;
end;

//function TfrmEditorView.GetOnStatusChange: TStatusChangeEvent;
//begin
//  Result := FSynEditor.OnStatusChange;
//end;
//
//procedure TfrmEditorView.SetOnStatusChange(const Value: TStatusChangeEvent);
//begin
//  FSynEditor.OnStatusChange := Value;
//end;

function TfrmEditorView.GetEditorFocused: Boolean;
begin
  Result := FSynEditor.Focused;
end;

function TfrmEditorView.GetText: string;
begin
  Result := FSynEditor.Text;
end;

procedure TfrmEditorView.SetText(const Value: string);
begin
  FSynEditor.Text := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmEditorView.FSynEditorDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
var
  S: string;
begin
  if AFiles.Count > 0 then
    S := AFiles[0];
  FSynEditor.Lines.LoadFromFile(S);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmEditorView.CreateEditor;
begin
  FSynEditor := TBCEditor.Create(Self);
  FSynEditor.Parent := Self;
  FSynEditor.Align := alClient;
  FSynEditor.AlignWithMargins := True;
  FSynEditor.Directories.Colors := '';
  FSynEditor.Directories.Highlighters := '';
//  if AFileName <> '' then
//    FSynEditor.LoadFromFile(AFileName);
//  if AHighlighter <> '' then
//    FSynEditor.Highlighter.LoadFromFile(AHighlighter + '.json');
//  if AColorMap <> '' then
//    FSynEditor.Highlighter.Colors.LoadFromFile(AColorMap + '.json');

  FSynEditor.CodeFolding.Visible := True;
  FSynEditor.Font.Name := 'Consolas';




//  FSynEditor           := TSynEdit.Create(Self);
//  FSynEditor.Font.Name := 'Consolas';
//  FSynEditor.Parent    := Self;
//  FSynEditor.Align     := alClient;
//
//  FSynEditor.Gutter.Gradient        := True;
//  FSynEditor.Gutter.AutoSize        := True;
//  FSynEditor.Gutter.LeftOffset      := 0;
//  FSynEditor.Gutter.RightOffset     := 0;
//  FSynEditor.Gutter.DigitCount      := 3;
//  FSynEditor.Gutter.ShowLineNumbers := True;
//
//  FSynCP := TSynCompletionProposal.Create(Self);
//  FSynCP.Options := [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText,
//    scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
//  FSynCP.NbLinesInWindow := 30;
//  FSynCP.TriggerChars := '.';
//  FSynCP.Font.Name := 'Tahoma';
//  FSynCP.Font.Style := [];
//  FSynCP.TitleFont.Name := 'Tahoma';
//  FSynCP.TitleFont.Style :=  [fsBold];
//  with FSynCP.Columns.Add do
//  begin
//    DefaultFontStyle := [fsBold];
//  end;
//  FSynCP.Columns.Add;
//  FSynCP.OnExecute := FSynCPExecute;
//  FSynCP.ShortCut := 16416;
//  FSynCP.Editor := FSynEditor;
//
//  FSynSQL                                    := TSynSQLSyn.Create(Self);
//  FSynSQL.NumberAttri.Style                  := [fsBold, fsItalic];
//  FSynSQL.NumberAttri.Foreground             := clRed;
//  FSynSQL.KeyAttri.Foreground                := clBlue;
//  FSynSQL.KeyAttri.Style                     := [fsBold];
//  FSynSQL.FunctionAttri.Foreground           := clBlue;
//  FSynSQL.FunctionAttri.Style                := [fsBold];
//  FSynSQL.PLSQLAttri.Style                   := [fsBold];
//  FSynSQL.PLSQLAttri.Foreground              := clBlue;
//  FSynSQL.SQLPlusAttri.Style                 := [fsBold];
//  FSynSQL.SQLPlusAttri.Foreground            := clBlue;
//  FSynSQL.KeywordAttribute.Foreground        := clBlue;
//  FSynSQL.KeywordAttribute.Style             := [fsBold];
//  FSynSQL.StringAttri.Foreground             := clGreen;
//  FSynSQL.StringAttri.Style                  := [fsBold, fsItalic];
//  FSynSQL.SymbolAttri.Style                  := [fsBold];
//  FSynSQL.SymbolAttri.Foreground             := clRed;
//  FSynSQL.CommentAttri.Foreground            := clNavy;
//  FSynSQL.CommentAttri.Style                 := [fsItalic];
//  FSynSQL.ConditionalCommentAttri.Foreground := clNavy;
//  FSynSQL.ConditionalCommentAttri.Style      := [fsItalic];
//  FSynSQL.TableNameAttri.ForeGround          := clMaroon;
//  FSynSQL.TableNameAttri.Style               := [fsBold];
//  FSynSQL.VariableAttri.Foreground           := clPurple;
//  FSynSQL.VariableAttri.Style                := [fsBold];
//  FSynSQL.DataTypeAttri.Foreground           := clBlack;
//  FSynSQL.DataTypeAttri.Style                := [fsBold];
//
////  FSynSQL.SQLDialect := sqlMSSQL2K;  // setting SQLDialect causes memory leak!
//  FSynEditor.OnDropFiles := FSynEditorDropFiles;
//  FSynEditor.WordWrap := False;
//  FSynEditor.Options := FSynEditor.Options +
//  [
//    eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
//    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
//    eoAutoSizeMaxScrollWidth,  //Automatically resizes the MaxScrollWidth property when inserting text
//    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
//    eoDropFiles,               //Allows the editor accept OLE file drops
//    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
//    eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
//    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
//    eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
//    eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
//    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
//    eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
//    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
//    eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
//    eoTrimTrailingSpaces      //Spaces at the end of lines will be trimmed and not saved
//    {eoColumnEditExtension }];
//  FSynEditor.Highlighter := FSynSQL;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TfrmEditorView.SetFocus;
begin
  if FSynEditor.CanFocus then
    FSynEditor.SetFocus;
end;

procedure TfrmEditorView.CopyToClipboard;
begin
  FSynEditor.CopyToClipboard;
end;

procedure TfrmEditorView.FillCompletionLists(ATables, AAttributes: TStrings);
var
  I       : Integer;
  Items   : TStringList;
  Inserts : TStringList;
begin
  inherited SetFocus;
//  Items  := FSynCP.ItemList as TStringList;
//  Inserts := FSynCP.InsertList  as TStringList;
  Items.Clear;
  Inserts.Clear;
//  FSynSQL.TableNames.Clear;
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
        //FSynSQL.TableNames.Add(ATables.Strings[I]);
      end;
    end;
  end;
end;
{$ENDREGION}

end.

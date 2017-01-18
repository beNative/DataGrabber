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

unit ts.Modules.Memo;

{  Simple form with a memo to quickly display multi-line text.  }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.StdActns;

type
  TfrmMemo = class(TForm)
    aclMain       : TActionList;
    actFileSaveAs : TFileSaveAs;
    btnOK         : TButton;
    btnSaveAs     : TButton;
    mmoMain       : TMemo;

    procedure actFileSaveAsAccept(Sender: TObject);
    procedure mmoMainKeyPress(Sender: TObject; var Key: Char);

    private
      procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
  end;

function ShowMemo(
  const AText     : string;
  const ACaption  : string = '';
        AReadOnly : Boolean = False
): string;

implementation

{$R *.dfm}

{$REGION 'interfaced routines'}
function ShowMemo(const AText: string; const ACaption: string;
  AReadOnly: Boolean): string;
var
  frmMemo : TfrmMemo;
begin
  frmMemo := TfrmMemo.Create(Application);
  try
    frmMemo.Caption := ACaption;
    frmMemo.mmoMain.Text := AText;
    frmMemo.mmoMain.ReadOnly := AReadOnly;
    frmMemo.ShowModal;
    Result := frmMemo.mmoMain.Text;
  finally
    frmMemo.Free;
  end;
end;
{$ENDREGION}

{$REGION 'message handlers'}
procedure TfrmMemo.CMDialogKey(var Msg: TCMDialogKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    ModalResult := mrOK;
    Close;
  end
  else
    inherited;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMemo.actFileSaveAsAccept(Sender: TObject);
begin
  mmoMain.Lines.SaveToFile(actFileSaveAs.Dialog.FileName);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMemo.mmoMainKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_ESCAPE then
  begin
    ModalResult := mrOK;
    Close;
  end
end;
{$ENDREGION}

end.

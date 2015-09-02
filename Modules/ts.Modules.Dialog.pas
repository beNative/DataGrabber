{ $Id: FtsDialog.pas,v 1.11 2007/09/18 12:18:14 Tim Sinaeve Exp $ }

unit ts.Modules.Dialog;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ActnList, ImgList,

  JvExStdCtrls, JvButton, JvCtrls;

//=============================================================================

type
  TfrmtsDialog = class(TForm)
    aclMain        : TActionList;
    actCancel      : TAction;
    actClose       : TAction;
    actOK          : TAction;
    btnCancel      : TJvImgBtn;
    btnClose       : TJvImgBtn;
    btnOK          : TJvImgBtn;
    grpLine        : TGroupBox;
    imlMain        : TImageList;
    pnlBottom      : TPanel;
    pnlFlowButtons : TFlowPanel;
    pnlTop         : TPanel;

    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
  private
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMChildKey(var Msg: TWMKey); message CM_CHILDKEY;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;
  protected
    function HandleReturnPressed(AControl : TWinControl) : Boolean; virtual;
    function HandleArrowUpKey(AControl : TWinControl) : Boolean; virtual;
    function HandleArrowDownKey(AControl : TWinControl) : Boolean; virtual;

    procedure HandleKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

//*****************************************************************************

implementation

{$R *.dfm}

//*****************************************************************************
// message handlers                                                      BEGIN
//*****************************************************************************

// needed to trap VK_UP and VK_DOWN in a TCustomListControl descendant
// This will be routed to HandleKeyDown

procedure TfrmtsDialog.CMChildKey(var Msg: TWMKey);
begin
  HandleKeyDown(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
end;

//-----------------------------------------------------------------------------

procedure TfrmtsDialog.CNKeyDown(var Msg: TWMKeyDown);
begin
  HandleKeyDown(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
end;

//-----------------------------------------------------------------------------

procedure TfrmtsDialog.CMDialogKey(var Msg: TCMDialogKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else if (Msg.Charcode = VK_RETURN) and (not (ActiveControl is TButton)) then
  begin
    if HandleReturnPressed(ActiveControl) then
      SelectNext(ActiveControl, True, True)
    else
    begin
      Msg.Charcode := 0;
      inherited;
    end;
  end
  else
    inherited;
end;

//-----------------------------------------------------------------------------

procedure TfrmtsDialog.WMKeyDown(var Msg: TWMKeyDown);
begin
  HandleKeyDown(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
end;

//*****************************************************************************
// message handlers                                                        END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmtsDialog.actCloseExecute(Sender: TObject);
begin
  Close; // Close before assignment of ModalResult because Close alters
         // ModalResult when the form is modal.
  ModalResult := mrOk;
end;

procedure TfrmtsDialog.actOKExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

procedure TfrmtsDialog.actCancelExecute(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event dispatch methods                                                BEGIN
//*****************************************************************************

procedure TfrmtsDialog.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleKeyDown(Key, Shift);
end;

//-----------------------------------------------------------------------------

function TfrmtsDialog.HandleArrowDownKey(AControl: TWinControl): Boolean;
begin
  Result := True;
end;

//-----------------------------------------------------------------------------

function TfrmtsDialog.HandleArrowUpKey(AControl: TWinControl): Boolean;
begin
  Result := True;
end;

//-----------------------------------------------------------------------------

{ Method that gets called like <KeyDown>, but also for special keys
  like [RETURN], [ESC], etc. }

procedure TfrmtsDialog.HandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (
     (ActiveControl is TCustomEdit) or (ActiveControl is TButtonControl) or
     (ActiveControl is TCustomListControl)) and ActiveControl.TabStop then
  begin
    case Key of
      VK_DOWN :
      begin
        if HandleArrowDownKey(ActiveControl) then
        begin
          Key := 0;
          SelectNext(ActiveControl, True, True);
        end;
      end;
      VK_UP :
      begin
        if HandleArrowUpKey(ActiveControl) then
        begin
          Key := 0;
          SelectNext(ActiveControl, False, True);
        end;
      end;
    end; // case Key of
  end;
  if Key <> 0 then
    inherited;
end;

//-----------------------------------------------------------------------------

{ Intended to be overridden in descendant classes. }

function TfrmtsDialog.HandleReturnPressed(AControl : TWinControl) : Boolean;
begin
  Result := True;
end;

//*****************************************************************************
// event dispatch methods                                                  END
//*****************************************************************************

end.

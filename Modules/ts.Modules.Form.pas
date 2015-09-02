{ $Id: FtsForm.pas,v 1.5 2008/11/05 15:57:55 Tim Sinaeve Exp $ }

unit ts.Modules.Form;

{
  Base form with following features:
    - Actionlist aclMain with actCLose and actToggleFullScreen actions.
    - sbrMain, skhMain, feMain and imlMain
    - Minimize will cause the whole application to minimize, and not only the
      form.
    - FullScreen property.
    - The ESC key closes the form.

    Following form properties are adjusted:
      - ScreenSnap = True
      - ShowHint   = True
      - Position   = poScreenCenter
      - PopupMode  = pmAuto

    Following application properties are adjusted:
      - HintPause     = 100
      - HintHidePause = 20000
}

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList,

  SBPro,

  tsFocusEnhancer, tsSpecialKeyHandler;

//=============================================================================

type
  TfrmtsForm = class(TForm)
    aclMain             : TActionList;
    actClose            : TAction;
    actToggleFullScreen : TAction;
    imlMain             : TImageList;

    procedure actToggleFullScreenExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);

  private
    FBorderStyle : TFormBorderStyle;

    // property access methods
    function GetFullScreen: Boolean;
    procedure SetFullScreen(const Value: Boolean);

    // message handlers
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;

  protected
    procedure Initialize; virtual;

    property FullScreen : Boolean
      read GetFullScreen write SetFullScreen;

  public
    procedure AfterConstruction; override;

  end;

//*****************************************************************************

implementation

{$R *.dfm}

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|FullScreen|--------------------------------------------------------------

function TfrmtsForm.GetFullScreen: Boolean;
begin
  Result := (WindowState = wsMaximized) and (BorderStyle = bsNone);
end;

procedure TfrmtsForm.SetFullScreen(const Value: Boolean);
begin
  if Value <> FullScreen then
  begin
    if Value then
    begin
      FBorderStyle := BorderStyle;
      BorderStyle  := bsNone;
      WindowState  := wsMaximized;
    end
    else
    begin
      BorderStyle := FBorderStyle;
      WindowState := wsNormal;
    end;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// message handlers                                                      BEGIN
//*****************************************************************************

{ Enable ESC key to close the form. }

procedure TfrmtsForm.CMDialogKey(var Msg: TCMDialogKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Close;
  end
  else
    inherited;
end;

//-----------------------------------------------------------------------------

{ Minimizes the application window if the form is minimized. }

procedure TfrmtsForm.WMSysCommand(var Msg: TWMSysCommand);
begin
 if Msg.CmdType = SC_MINIMIZE then
 begin
   EnableWindow(Application.Handle, True);
   Application.Minimize;
 end
 else
   inherited;
end;

//*****************************************************************************
// message handlers                                                        END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmtsForm.actCloseExecute(Sender: TObject);
begin
  Close;
end;

//-----------------------------------------------------------------------------

procedure TfrmtsForm.actToggleFullScreenExecute(Sender: TObject);
begin
  FullScreen := not FullScreen;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmtsForm.Initialize;
var
  I : Integer;
begin
  Application.HintPause     := 100;
  Application.HintHidePause := 20000;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TWinControl then
      TWinControl(Components[I]).DoubleBuffered := True;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TfrmtsForm.AfterConstruction;
begin
  inherited;
  Initialize;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.

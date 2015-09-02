unit ts.Modules.ExceptionDialog;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, ImgList, ComCtrls,

  JclSysUtils, JclMapi, JclDebug;

//=============================================================================

const
  UM_CREATEDETAILS = WM_USER + $100;
  AnsiCrLf = #13#10;

//-----------------------------------------------------------------------------  

type
  TfrmDlgException = class(TForm)
    aclMain             : TActionList;
    actDetails          : TAction;
    actMail             : TAction;
    actOK               : TAction;
    actTerminate        : TAction;
    bvlLine             : TBevel;
    imgScreenshot       : TImage;
    imlMain             : TImageList;
    mmoDetails          : TMemo;
    mmoText             : TMemo;
    pgcExceptionDetails : TPageControl;
    sbScreenshot        : TScrollBox;
    tsExceptionLog      : TTabSheet;
    tsScreenshot        : TTabSheet;
    btnSend: TButton;
    btnOK: TButton;
    btnDetails: TButton;
    btnTerminate: TButton;

    procedure btnDetailsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure actMailExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);

  private
    FDetailsVisible    : Boolean;
    FIsMainThead       : Boolean;
    FLastActiveControl : TWinControl;
    FNonDetailsHeight  : Integer;
    FFullHeight        : Integer;

    function GetReportAsText: AnsiString;
    procedure SetDetailsVisible(const Value: Boolean);

    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;

  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetails; dynamic;
    procedure CreateReport;
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: AnsiChar; virtual;
    procedure NextDetailBlock;
    procedure UpdateTextLabelScrollbars;

  public
    procedure CopyReportToClipboard;

    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: Exception; Thread: TJclDebugThread);

    property DetailsVisible: Boolean
      read FDetailsVisible write SetDetailsVisible;

    property ReportAsText: AnsiString
      read GetReportAsText;

  end;

//-----------------------------------------------------------------------------

type
  TfrmDlgExceptionClass = class of TfrmDlgException;

//=============================================================================

var
  frmDlgExceptionClass : TfrmDlgExceptionClass = TfrmDlgException;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  ClipBrd, jpeg,

  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclSysInfo, JclWin32,
  JclStrings;

//=============================================================================

resourcestring
  SApplicationError   = '%s - application error';
  SExceptionClass     = 'Exception class: %s';
  SExceptionAddress   = 'Exception address: %p';
  SStackList          = 'Stack list, generated %s';
  SModulesList        = 'List of loaded modules:';
  SOSVersion          = 'System: %s %s, Version: %d.%d, Build: %x, "%s"';
  SProcessor          = 'Processor: %s, %s, %d MHz';
  SMemory             = 'Memory: %d; free %d';
  SScreenResolution   = 'Display: %dX%d pixels, %d bpp';
  SActiveControl      = 'Active Controls hierarchy:';
  SThread             = 'Thread: %s';
  SMissingVersionInfo = '(no version info)';

//=============================================================================

var
  frmDlgException  : TfrmDlgException;
  ExceptionShowing : Boolean;

//*****************************************************************************
// non-interfaced routines                                               BEGIN
//*****************************************************************************

// SortModulesListByAddressCompare
// sorts module by address

function SortModulesListByAddressCompare(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  Addr1, Addr2: Cardinal;
begin
  Addr1 := Cardinal(List.Objects[Index1]);
  Addr2 := Cardinal(List.Objects[Index2]);
  if Addr1 > Addr2 then
    Result := 1
  else if Addr1 < Addr2 then
    Result := -1
  else
    Result := 0;
end;

//-----------------------------------------------------------------------------

procedure ScreenShot(ABitmap : TBitmap; AActiveWindow: Boolean = True) ;
var
   W,H  : Integer;
   DC   : HDC;
   hWin : Cardinal;
   R    : TRect;
begin
  if AActiveWindow then
  begin
    hWin := GetForegroundWindow;
    DC   := GetWindowDC(hWin);
    GetWindowRect(hWin,R);
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
  end
  else
  begin
    hWin := GetDesktopWindow;
    DC   := GetDC(hWin);
    W    := GetDeviceCaps(DC, HORZRES);
    H    := GetDeviceCaps(DC, VERTRES);
  end;
  try
    ABitmap.Width := W;
    ABitmap.Height := H;
    BitBlt(
      ABitmap.Canvas.Handle,
      0,
      0,
      ABitmap.Width,
      ABitmap.Height,
      DC,
      0,
      0,
      SRCCOPY
    );
  finally
     ReleaseDC(hWin, DC);
  end;
end;

//============================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//============================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end
//   else
//     SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  if JclValidateModuleAddress(ExceptAddr)
    and (ExceptObject.InstanceSize >= Exception.InstanceSize) then
    TfrmDlgException.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//----------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;

type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;

var
  ApplicationHandleExceptionAddr : Pointer;
  SysUtilsShowExceptionAddr      : Pointer;
  CALLInstruction                : TCALLInstruction;
  CallAddress                    : Pointer;
  WrittenBytes                   : Cardinal;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(Cardinal(ApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(
            Pointer(Integer(CallAddress) +
            Integer(PCALLInstruction(CallAddress)^.Address) +
            SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(
            CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) -
            Integer(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;
  end;

begin
  ApplicationHandleExceptionAddr :=
    PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr :=
    PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  Result :=
    CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
  if Result then
  begin
    CALLInstruction.Address :=
      Integer(@HookShowException) - Integer(CallAddress) -
      SizeOf(CALLInstruction);
    Result :=
      WriteProtectedMemory(CallAddress, @CallInstruction,
                           SizeOf(CallInstruction), WrittenBytes);
  end;
end;

//=============================================================================
// Exception handler initialization code
//=============================================================================

procedure InitializeHandler;
begin
  JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
  JclStackTrackingOptions := JclStackTrackingOptions + [stDelayedTrace];
  JclDebugThreadList.OnSyncException := TfrmDlgException.ExceptionThreadHandler;
  JclStartExceptionTracking;
  if HookTApplicationHandleException then
    JclTrackExceptionsFromLibraries;
  Application.OnException := TfrmDlgException.ExceptionHandler;
end;

//-----------------------------------------------------------------------------

procedure UnInitializeHandler;
begin
  Application.OnException := nil;
  JclDebugThreadList.OnSyncException := nil;
  JclUnhookExceptions;
  JclStopExceptionTracking;
end;

//*****************************************************************************
// non-interfaced routines                                                 END
//*****************************************************************************

{
_______________________________________________________________________________
_______________________________________________________________________________

                                 TfrmDlgException
_______________________________________________________________________________
_______________________________________________________________________________

}

//*****************************************************************************
// class methods                                                         BEGIN
//*****************************************************************************

class procedure TfrmDlgException.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if ExceptionShowing then
    Application.ShowException(E)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E, nil);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//-----------------------------------------------------------------------------

class procedure TfrmDlgException.ExceptionThreadHandler(Thread: TJclDebugThread);
begin
  if ExceptionShowing then
    Application.ShowException(Thread.SyncException as Exception)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(Thread.SyncException as Exception, Thread);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//-----------------------------------------------------------------------------

class procedure TfrmDlgException.ShowException(E: Exception;
  Thread: TJclDebugThread);
begin
  if frmDlgException = nil then
    frmDlgException := TfrmDlgExceptionClass.Create(Application);
  try
    with frmDlgException do
    begin
      ScreenShot(imgScreenshot.Picture.Bitmap);
       FIsMainThead := (GetCurrentThreadId = MainThreadID);
      FLastActiveControl := Screen.ActiveControl;
      mmoText.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.Message));
      UpdateTextLabelScrollbars;
      mmoDetails.Lines.Add(Format(SExceptionClass, [E.ClassName]));
      if Thread = nil then
        mmoDetails.Lines.Add(Format(SExceptionAddress, [ExceptAddr]))
      else
        mmoDetails.Lines.Add(Format(SThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      ShowModal;
    end;
  finally
    FreeAndNil(frmDlgException);
  end;
end;

//*****************************************************************************
// class methods                                                           END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|ReportAsText|------------------------------------------------------------

function TfrmDlgException.GetReportAsText: AnsiString;
begin
  Result := AnsiString(StrEnsureSuffix(AnsiCrLf, mmoText.Text) + AnsiCrLf + mmoDetails.Text);
end;

//---|DetailsVisible|----------------------------------------------------------

procedure TfrmDlgException.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  if Value <> DetailsVisible then
  begin
    FDetailsVisible := Value;
    DetailsCaption := Trim(StrRemoveChars(btnDetails.Caption, ['<', '>']));
    if Value then
    begin
      Constraints.MaxHeight := Screen.Height;
      DetailsCaption := '<< ' + DetailsCaption;
      ClientHeight := FFullHeight;
      Width  := 800;
      bvlLine.Visible := False;
    end
    else
    begin
      FFullHeight := ClientHeight;
      DetailsCaption := DetailsCaption + ' >>';
      if FNonDetailsHeight = 0 then
      begin
        ClientHeight := 130;
        FNonDetailsHeight := Height;
      end
      else
      begin
        Height := FNonDetailsHeight;
      end;
      bvlLine.Visible := False;
      Constraints.MinHeight := FNonDetailsHeight;
      Constraints.MaxHeight := FNonDetailsHeight;
      Width := 438;
    end;
    Position := poScreenCenter;
    btnDetails.Caption := DetailsCaption;
  end;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// message handlers                                                      BEGIN
//*****************************************************************************

procedure TfrmDlgException.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//*****************************************************************************
// message handlers                                                        END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmDlgException.actMailExecute(Sender: TObject);
var
  S   : AnsiString;
  JPG : TJPEGImage;
begin
  with TJclEmail.Create do
  try
    S := PathGetTempPath + 'Screenshot.jpg';
    JPG := TJPEGImage.Create;
    try
      JPG.ProgressiveEncoding := True;
      JPG.ProgressiveDisplay  := True;
      JPG.Smoothing           := True;
      JPG.Performance := jpBestQuality;
      JPG.CompressionQuality  := 100;
      JPG.Assign(imgScreenshot.Picture.Bitmap);
      JPG.Compress;
      JPG.SaveToFile(S);
    finally
      JPG.Free;
    end;
    ParentWnd := Application.Handle;
    Recipients.Add('tim.sinaeve@gmail.com');
    Attachments.Add(S);
    Subject := AnsiString(Application.ExeName) + '- exception trace';
    Body := ReportAsText;
    SaveTaskWindows;
    try
      try
        Send;
      except
        Send(False);
      end;
    finally
      RestoreTaskWindows;
      DeleteFile(S);
    end;
  finally
    Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.actOKExecute(Sender: TObject);
begin
  Close;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.actTerminateExecute(Sender: TObject);
begin
  try
    Application.Terminate;
  except
    Halt;
  end;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TfrmDlgException.btnDetailsClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.FormCreate(Sender: TObject);
begin
  FFullHeight := ClientHeight;
  FDetailsVisible := True;
  DetailsVisible := False;
  Caption := Format(SApplicationError, [Application.Title]);
  sbScreenshot.DoubleBuffered := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, mmoText.Left - GetSystemMetrics(SM_CXICON) - 15,
    mmoText.Top, LoadIcon(0, IDI_ERROR));
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.FormShow(Sender: TObject);
begin
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateReport;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmDlgException.AfterCreateDetails;
begin
  actMail.Enabled := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.BeforeCreateDetails;
begin
  actMail.Enabled := False;
end;

//-----------------------------------------------------------------------------

function TfrmDlgException.ReportMaxColumns: Integer;
begin
  Result := 78;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  mmoDetails.Lines.BeginUpdate;
  try
    CreateReport;
    mmoDetails.SelStart := 0;
    SendMessage(mmoDetails.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    mmoDetails.Lines.EndUpdate;
    btnOK.Enabled := True;
    btnDetails.Enabled := True;
    btnOK.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.CreateReport;
var
  SL               : TStringList;
  I                : Integer;
  ModuleName       : TFileName;
  NtHeaders        : PImageNtHeaders32;
  ModuleBase       : Cardinal;
  ImageBaseStr     : string;
  C                : TWinControl;
  CpuInfo          : TCpuInfo;
  ProcessorDetails : string;
  StackList        : TJclStackInfoList;
begin
  SL := TStringList.Create;
  try
    // Stack list
    StackList := JclLastExceptStackList;
    if Assigned(StackList) then
    begin
      mmoDetails.Lines.Add(Format(SStackList, [DateTimeToStr(StackList.TimeStamp)]));
      StackList.AddToStrings(mmoDetails.Lines, True, False, True, False);
      NextDetailBlock;
    end;

    // System and OS information
    mmoDetails.Lines.Add(
      Format(SOSVersion,
             [GetWindowsVersionString, NtProductTypeString, Win32MajorVersion,
              Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
    GetCpuInfo(CpuInfo);
    with CpuInfo do
    begin
      ProcessorDetails := Format(SProcessor, [Manufacturer, CpuName,
        RoundFrequency(FrequencyInfo.NormFreq)]);
      if not IsFDIVOK then
        ProcessorDetails := ProcessorDetails + ' [FDIV Bug]';
      if ExMMX then
        ProcessorDetails := ProcessorDetails + ' MMXex'
      else if MMX then
        ProcessorDetails := ProcessorDetails + ' MMX';
//      if SSE > 0 then
//        ProcessorDetails := Format('%S SSE%D', [ProcessorDetails, SSE]);
      if Ex3DNow then
        ProcessorDetails := ProcessorDetails + ' 3DNow!ex'
      else if _3DNow then
        ProcessorDetails := ProcessorDetails + ' 3DNow!';
      if Is64Bits then
        ProcessorDetails := ProcessorDetails + ' 64 bits';
      if DEPCapable then
        ProcessorDetails := ProcessorDetails + ' DEP';
    end;
    mmoDetails.Lines.Add(ProcessorDetails);
    mmoDetails.Lines.Add(Format(SMemory,
                                [GetTotalPhysicalMemory div 1024 div 1024,
                                 GetFreePhysicalMemory div 1024 div 1024]));
    mmoDetails.Lines.Add(Format(SScreenResolution,
                                [Screen.Width, Screen.Height, GetBPP]));
    NextDetailBlock;
    // Modules list
    if LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      mmoDetails.Lines.Add(SModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := Cardinal(SL.Objects[I]);
        mmoDetails.Lines.Add(Format('[%.8x] %S', [ModuleBase, ModuleName]));
        NtHeaders := PeMapImgNtHeaders32(Pointer(ModuleBase));
        if (NtHeaders <> nil) and
           (NtHeaders^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr :=
            Format('<%.8x> ', [NtHeaders^.OptionalHeader.ImageBase])
        else
          ImageBaseStr := StrRepeat(' ', 11);
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            mmoDetails.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              mmoDetails.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end
        else
          mmoDetails.Lines.Add(ImageBaseStr + SMissingVersionInfo);
      end;
      NextDetailBlock;
    end;

    // Active controls
    if (FLastActiveControl <> nil) then
    begin
      mmoDetails.Lines.Add(SActiveControl);
      C := FLastActiveControl;
      while C <> nil do
      begin
        mmoDetails.Lines.Add(Format('%S "%S"', [C.ClassName, C.Name]));
        C := C.Parent;
      end;
      NextDetailBlock;
    end;

  finally
    SL.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.UpdateTextLabelScrollbars;
begin
  Canvas.Font := mmoText.Font;
  if mmoText.Lines.Count * Canvas.TextHeight('Wg') > mmoText.ClientHeight then
    mmoText.ScrollBars := ssVertical
  else
    mmoText.ScrollBars := ssNone;
end;

//-----------------------------------------------------------------------------

procedure TfrmDlgException.NextDetailBlock;
begin
  mmoDetails.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

//-----------------------------------------------------------------------------

function TfrmDlgException.ReportNewBlockDelimiterChar: AnsiChar;
begin
  Result := '-';
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************

initialization
  InitializeHandler;

//*****************************************************************************

finalization
  UnInitializeHandler;

end.

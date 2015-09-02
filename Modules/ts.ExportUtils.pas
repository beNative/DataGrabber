{ $Id: tsExportUtils.pas,v 1.6 2009/01/15 09:41:48 Tim Sinaeve Exp $ }

unit ts.ExportUtils;

{ Author: Tim Sinaeve }

{
  REMARK:
    This unit requires the Open Source ExportSuite from Firesoft to export
    in native Excel format without the requirement to have Excel installed.

  TODO:
    - support for text exports (user defined layouts)
    - support for HTML exports
    - support for native ClientDataSet XML exports
}

//*****************************************************************************

interface

uses
  SysUtils, Graphics,

  DB;

//=============================================================================

procedure ExportExcel(      ADataSet         : TDataSet;
                      const AFileName        : string  = '';
                            AShowFileDialog  : Boolean = True;
                            AOpenAfterExport : Boolean = True;
                            AHeaderFont      : TFont   = nil;
                            ADetailFont      : TFont   = nil);

procedure ExportText(      ADataSet         : TDataSet;
                     const AFileName        : string  = '';
                           AShowFileDialog  : Boolean = True;
                           AOpenAfterExport : Boolean = True);

//procedure ExportExcel(      ADataSet         : TDataSet;
//                      const AFileName        : string  = '';
//                            AShowFileDialog  : Boolean = True;
//                            AOpenAfterExport : Boolean = True;
//                            AHeaderFont      : TFont   = nil;
//                            ADetailFont      : TFont   = nil);

//*****************************************************************************

implementation

uses
  Dialogs, Forms, ComObj, Controls,

  JclShell,

  DataToXLS, DataToAscii;

//=============================================================================

resourcestring
  SExcelDialogFilter = 'Excel files (*.xls)|*.XLS';
  STextDialogFilter = 'Text files (*.txt)|*.TXT';

//-----------------------------------------------------------------------------

const
  EXCEL_DEFAULT_EXT = 'XLS';
  TEXT_DEFAULT_EXT  = 'TXT';

//*****************************************************************************
// non-interfaced routines                                               BEGIN
//*****************************************************************************

function ExecuteSaveFileDialog(const AFilter     : string = '';
                               const ADefaultExt : string = '';
                               const AFileName   : string = '') : string;
var
  dlgSave : TSaveDialog;
begin
  dlgSave := TSaveDialog.Create(Application);
  try
    dlgSave.Filter     := AFilter;
    dlgSave.FileName   := AFileName;
    dlgSave.DefaultExt := ADefaultExt;
    dlgSave.Options := dlgSave.Options + [ofOverwritePrompt];
    if dlgSave.Execute then
      Result := dlgSave.FileName
    else
      Result := AFileName;
    Application.ProcessMessages;
  finally
    dlgSave.Free;
  end;
end;

//-----------------------------------------------------------------------------

function IsExcelInstalled : Boolean;
var
  Excel : OleVariant;
begin
  try
    Excel := CreateOleObject('excel.application');
    Excel.Quit;
    Result := True;
  except
    Result := False;
  end;
end;

//*****************************************************************************
// non-interfaced routines                                                 END
//*****************************************************************************

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

{
  ADataSet
  [AFileName]
  [AShowFileDialog]
  [AOpenAfterExport]
  [AHeaderFont]
  [ADetailFont]
}

procedure ExportExcel(      ADataSet         : TDataSet;
                      const AFileName        : string;
                            AShowFileDialog  : Boolean;
                            AOpenAfterExport : Boolean;
                            AHeaderFont      : TFont;
                            ADetailFont      : TFont);
var
  DTX      : TDataToXLS;
  FileName : string;
begin
  DTX := TDataToXLS.Create(Application);
  try
    DTX.DataSet := ADataSet;
    if AShowFileDialog or (Trim(AFileName) = '') then
      FileName :=
        ExecuteSaveFileDialog(SExcelDialogFilter, EXCEL_DEFAULT_EXT, AFileName)
    else
      FileName := AFileName;

    if Assigned(ADetailFont) then
      DTX.Detail.Font.Assign(ADetailFont);

    if Assigned(AHeaderFont) then
      DTX.Header.Font.Assign(AHeaderFont);

    if Trim(FileName) <> '' then
    begin
      Screen.Cursor := crHourGlass;
      try
        DTX.SaveToFile(FileName);

        if AOpenAfterExport and IsExcelInstalled then
          ShellExecEx(FileName);

      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    DTX.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure ExportText(      ADataSet         : TDataSet;
                     const AFileName        : string  = '';
                           AShowFileDialog  : Boolean = True;
                           AOpenAfterExport : Boolean = True);
var
  DTA      : TDataToAscii;
  FileName : string;
begin
  DTA := TDataToAscii.Create(Application);
  try
    DTA.DataSet := ADataSet;
    if AShowFileDialog or (Trim(AFileName) = '') then
      FileName :=
        ExecuteSaveFileDialog(STextDialogFilter, TEXT_DEFAULT_EXT, AFileName)
    else
      FileName := AFileName;

    if Trim(FileName) <> '' then
    begin
      Screen.Cursor := crHourGlass;
      try
        DTA.Quote := #0;
        DTA.Separator := '|';
        DTA.SaveToFile(FileName);

        if AOpenAfterExport then
          ShellExecEx(FileName);

      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    DTA.Free;
  end;

end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

end.

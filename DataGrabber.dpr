{$SetPEFlags 1} // strip relocation info

program DataGrabber;

uses
  Vcl.Forms,
  System.SysUtils,
  DDuce.Logger,
  DDuce.Logger.Channels.WinIPC,
  DataGrabber.Resources in 'DataGrabber.Resources.pas',
  DataGrabber.DataInspector in 'DataGrabber.DataInspector.pas' {frmDataInspector},
  DataGrabber.Utils in 'DataGrabber.Utils.pas',
  DataGrabber.Settings in 'DataGrabber.Settings.pas',
  DataGrabber.Settings.Dialog in 'DataGrabber.Settings.Dialog.pas' {frmSettingsDialog},
  DataGrabber.FieldInspector in 'DataGrabber.FieldInspector.pas' {frmFieldInspector},
  DataGrabber.DataView.GridView in 'DataView\DataGrabber.DataView.GridView.pas' {frmGridView},
  DataGrabber.ConnectionProfiles in 'DataGrabber.ConnectionProfiles.pas',
  DataGrabber.ConnectionView in 'DataGrabber.ConnectionView.pas' {frmConnectionView},
  DataGrabber.ConnectionViewManager in 'DataGrabber.ConnectionViewManager.pas' {dmConnectionViewManager: TDataModule},
  DataGrabber.EditorView in 'DataGrabber.EditorView.pas' {frmEditorView},
  DataGrabber.FormSettings in 'DataGrabber.FormSettings.pas',
  DataGrabber.Factories in 'DataGrabber.Factories.pas',
  DataGrabber.Interfaces in 'DataGrabber.Interfaces.pas',
  DataGrabber.MainForm in 'DataGrabber.MainForm.pas' {frmMain},
  DataGrabber.ConnectionProfileValueManager in 'DataGrabber.ConnectionProfileValueManager.pas',
  DataGrabber.DataView.KGrid in 'DataView\DataGrabber.DataView.KGrid.pas' {frmKGrid},
  DataGrabber.Data in 'DataGrabber.Data.pas' {dmData: TDataModule},
  DataGrabber.ConnectionSettings in 'DataGrabber.ConnectionSettings.pas',
  DataGrabber.DataView.cxGrid in 'DataView\DataGrabber.DataView.cxGrid.pas' {frmcxGrid},
  DataGrabber.MetaData.Dialog in 'DataGrabber.MetaData.Dialog.pas' {frmMetaData},
  DataGrabber.DataView.Base in 'DataView\DataGrabber.DataView.Base.pas' {BaseDataView},
  DataGrabber.Data.ResultSet in 'DataGrabber.Data.ResultSet.pas';

//Vcl.Styles.Hooks,
//  Vcl.Themes,
//  Vcl.Styles;

{$R *.res}

begin
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  {$WARNINGS ON}
  Logger.Channels.Add(TWinIPCChannel.Create);
  if CheckWin32Version(6) then // at least Vista
  begin
   Application.DefaultFont.Name := 'Segoe UI';
   Application.DefaultFont.Size := 9;
  end;
  Application.Initialize;
  Application.Title := 'DataGrabber';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

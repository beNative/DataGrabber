{$SetPEFlags 1} // strip relocation info

program DataGrabber;

uses
  Forms,
  DataGrabber.Resources in 'DataGrabber.Resources.pas',
  ts.Modules.DataInspector in 'Modules\ts.Modules.DataInspector.pas' {frmDataInspector},
  ts.Data in 'Modules\ts.Data.pas' {dmCustomModule: TDataModule},
  ts.Interfaces in 'Modules\ts.Interfaces.pas',
  DataGrabber.Utils in 'DataGrabber.Utils.pas',
  ts.Connection.ADO in 'Modules\ts.Connection.ADO.pas' {dmADOConnection: TDataModule},
  ts.Connection.DBX in 'Modules\ts.Connection.DBX.pas' {dmDBXConnection: TDataModule},
  ts.Classes.SQL.Statement in 'Modules\ts.Classes.SQL.Statement.pas',
  ts.Classes.ConnectionSettings in 'Modules\ts.Classes.ConnectionSettings.pas',
  DataGrabber.Settings in 'DataGrabber.Settings.pas',
  DataGrabber.SQLTemplates in 'DataGrabber.SQLTemplates.pas',
  DataGrabber.SettingsDialog in 'DataGrabber.SettingsDialog.pas' {frmSettingsDialog},
  ts.Classes.ModuleManager in 'Modules\ts.Classes.ModuleManager.pas',
  ts.Modules.FieldInspector in 'Modules\ts.Modules.FieldInspector.pas' {frmFieldInspector},
  ts.DataView in 'Modules\ts.DataView.pas' {frmDataView},
  DataGrabber.PropertyEditors in 'DataGrabber.PropertyEditors.pas',
  ts.Data.Selection in 'Modules\ts.Data.Selection.pas',
  ts.Data.Report in 'Modules\ts.Data.Report.pas',
  ts.Data.NativeADO in 'Modules\ts.Data.NativeADO.pas',
  ts.Data.Native in 'Modules\ts.Data.Native.pas',
  ts.Data.NativeDBX in 'Modules\ts.Data.NativeDBX.pas',
  ts.Modules.ComponentInspector in 'Modules\ts.Modules.ComponentInspector.pas' {tsComponentInspector},
  ts.Classes.SQL.CompoundCondition in 'Modules\ts.Classes.SQL.CompoundCondition.pas',
  ts.Classes.SQL.ComparisonCondition in 'Modules\ts.Classes.SQL.ComparisonCondition.pas',
  ts.Classes.SQL.Condition in 'Modules\ts.Classes.SQL.Condition.pas',
  ts.Classes.SQL.Params in 'Modules\ts.Classes.SQL.Params.pas',
  ts.Utils in 'Modules\ts.Utils.pas',
  ts.Classes.Alignment in 'Modules\ts.Classes.Alignment.pas',
  ts.Classes.ListReport.Columns in 'Modules\ts.Classes.ListReport.Columns.pas',
  ts.Classes.ListReport in 'Modules\ts.Classes.ListReport.pas',
  ts.Modules.List.Columns in 'Modules\ts.Modules.List.Columns.pas',
  ts.DBUtils in 'Modules\ts.DBUtils.pas',
  ts.Modules.Memo in 'Modules\ts.Modules.Memo.pas' {frmMemo},
  ts.Components.DBGridViewSort in 'Modules\ts.Components.DBGridViewSort.pas',
  ts.Utils.Actions in 'Modules\ts.Utils.Actions.pas',
  ts.Modules.RTTEye in 'Modules\ts.Modules.RTTEye.pas' {frmRTTEye},
  ts.Connection.ADOConnectionAdaptor in 'Modules\ts.Connection.ADOConnectionAdaptor.pas',
  ts.Connection.DBXConnectionAdaptor in 'Modules\ts.Connection.DBXConnectionAdaptor.pas',
  DataGrabber.DataView.cxGrid in 'DataView\DataGrabber.DataView.cxGrid.pas' {frmcxGrid: TFrame},
  DataGrabber.DataView.GridView in 'DataView\DataGrabber.DataView.GridView.pas' {frmGridView},
  DataGrabber.DataView.VirtualDBGrid in 'DataView\DataGrabber.DataView.VirtualDBGrid.pas',
  ts.Connection.CustomConnectionAdaptor in 'Modules\ts.Connection.CustomConnectionAdaptor.pas',
  ts.Connection in 'Modules\ts.Connection.pas' {dmConnection: TDataModule},
  DataGrabber.RegisterServices in 'DataGrabber.RegisterServices.pas',
  DataGrabber.ConnectionProfiles in 'DataGrabber.ConnectionProfiles.pas',
  DataGrabber.ConnectionView in 'DataGrabber.ConnectionView.pas' {frmConnectionView},
  DataGrabber.ConnectionViewManager in 'DataGrabber.ConnectionViewManager.pas' {dmConnectionViewManager: TDataModule},
  DataGrabber.Data in 'DataGrabber.Data.pas' {dmData: TDataModule},
  DataGrabber.EditorView in 'DataGrabber.EditorView.pas' {frmEditorView},
  DataGrabber.FormSettings in 'DataGrabber.FormSettings.pas',
  DataGrabber.Factories in 'DataGrabber.Factories.pas',
  DataGrabber.Interfaces in 'DataGrabber.Interfaces.pas',
  DataGrabber.MainForm in 'DataGrabber.MainForm.pas' {frmMain},
  ts.Classes.KeyValues in 'Modules\ts.Classes.KeyValues.pas',
  ts.Connection.FireDAC in 'Modules\ts.Connection.FireDAC.pas' {dmFireDACConnection: TDataModule},
  ts.Data.NativeFireDAC in 'Modules\ts.Data.NativeFireDAC.pas',
  Unit1 in 'Unit1.pas' {Form1},
  ts.Connection.FireDACConnectionAdaptor in 'Modules\ts.Connection.FireDACConnectionAdaptor.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.Title := 'DataGrabber';
  RegisterServices;
  //Application.CreateForm(TdmRepositoryData, dmRepositoryData);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmFireDACConnection, dmFireDACConnection);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

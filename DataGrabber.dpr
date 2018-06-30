{$SetPEFlags 1} // strip relocation info

program DataGrabber;

uses
  Forms,
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
  DDuce.DynamicRecord in '..\..\libraries\dduce\Source\DDuce.DynamicRecord.pas',
  DDuce.RandomData in '..\..\libraries\dduce\Source\DDuce.RandomData.pas',
  DDuce.Reflect in '..\..\libraries\dduce\Source\DDuce.Reflect.pas',
  DDuce.ScopedReference in '..\..\libraries\dduce\Source\DDuce.ScopedReference.pas',
  DDuce.WinIPC.Client in '..\..\libraries\dduce\Source\DDuce.WinIPC.Client.pas',
  DDuce.WinIPC.Server in '..\..\libraries\dduce\Source\DDuce.WinIPC.Server.pas',
  DDuce.Components.DBGridView in '..\..\libraries\dduce\Source\Components\DDuce.Components.DBGridView.pas',
  DDuce.Components.Factories in '..\..\libraries\dduce\Source\Components\DDuce.Components.Factories.pas',
  DDuce.Components.GridView in '..\..\libraries\dduce\Source\Components\DDuce.Components.GridView.pas',
  DDuce.Components.Inspector in '..\..\libraries\dduce\Source\Components\DDuce.Components.Inspector.pas',
  DDuce.Components.LogTree in '..\..\libraries\dduce\Source\Components\DDuce.Components.LogTree.pas',
  DDuce.Components.PropertyInspector.CollectionEditor in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.CollectionEditor.pas' {frmCollectionEditor},
  DDuce.Components.PropertyInspector in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.pas',
  DDuce.Components.PropertyInspector.StringsEditor in '..\..\libraries\dduce\Source\Components\DDuce.Components.PropertyInspector.StringsEditor.pas' {StringsEditorDialog},
  DDuce.Logger.Base in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Base.pas',
  DDuce.Logger.Interfaces in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.Interfaces.pas',
  DDuce.Logger in '..\..\libraries\dduce\Source\Modules\Logger\DDuce.Logger.pas',
  DDuce.ObjectInspector.zObjectInspector in '..\..\libraries\dduce\Source\Modules\ObjectInspector\DDuce.ObjectInspector.zObjectInspector.pas' {frmComponentInspectorzObjectInspector},
  DDuce.Settings.Form in '..\..\libraries\dduce\Source\Settings\DDuce.Settings.Form.pas',
  uCustomImageDrawHook in 'uCustomImageDrawHook.pas',
  DataGrabber.ConnectionProfileValueManager in 'DataGrabber.ConnectionProfileValueManager.pas',
  zBase in '..\..\components\zcontrols\Source\zBase.pas',
  zCanvasStack in '..\..\components\zcontrols\Source\zCanvasStack.pas',
  zCollectionEditor in '..\..\components\zcontrols\Source\zCollectionEditor.pas' {zCollectionEditorDialog},
  zControlsReg in '..\..\components\zcontrols\Source\zControlsReg.pas',
  zGraphicDialog in '..\..\components\zcontrols\Source\zGraphicDialog.pas' {GraphicDialog},
  zObjInspDialogs in '..\..\components\zcontrols\Source\zObjInspDialogs.pas',
  zObjInspector in '..\..\components\zcontrols\Source\zObjInspector.pas',
  zObjInspList in '..\..\components\zcontrols\Source\zObjInspList.pas',
  zRecList in '..\..\components\zcontrols\Source\zRecList.pas',
  zStringsDialog in '..\..\components\zcontrols\Source\zStringsDialog.pas' {StringsDialog},
  zUtils in '..\..\components\zcontrols\Source\zUtils.pas',
  FloatConv.Double in '..\..\components\zcontrols\Source\FloatConv\FloatConv.Double.pas',
  FloatConv.Extended in '..\..\components\zcontrols\Source\FloatConv\FloatConv.Extended.pas',
  FloatConv in '..\..\components\zcontrols\Source\FloatConv\FloatConv.pas',
  FloatConv.Single in '..\..\components\zcontrols\Source\FloatConv\FloatConv.Single.pas',
  DataGrabber.DataView.KGrid in 'DataView\DataGrabber.DataView.KGrid.pas',
  DDuce.ObjectInspector in '..\..\libraries\dduce\Source\Modules\ObjectInspector\DDuce.ObjectInspector.pas' {frmComponentInspector},
  zValueManager in '..\..\components\zcontrols\Source\zValueManager.pas',
  zObjInspTypes in '..\..\components\zcontrols\Source\zObjInspTypes.pas',
  DDuce.Factories.VirtualTrees in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.VirtualTrees.pas',
  DDuce.Factories.zObjInspector in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.zObjInspector.pas',
  DataGrabber.Data in 'DataGrabber.Data.pas' {dmData: TDataModule},
  DataGrabber.ConnectionSettings in 'DataGrabber.ConnectionSettings.pas',
  DataGrabber.DataView.cxGrid in 'DataView\DataGrabber.DataView.cxGrid.pas' {frmcxGrid},
  ZeroMQ.API in '..\..\libraries\dduce\Source\Dependencies\ZeroMQ\ZeroMQ.API.pas',
  ZeroMQ in '..\..\libraries\dduce\Source\Dependencies\ZeroMQ\ZeroMQ.pas',
  DataGrabber.About.Dialog in 'DataGrabber.About.Dialog.pas' {frmAboutDialog},
  DataGrabber.MetaData.Dialog in 'DataGrabber.MetaData.Dialog.pas' {frmMetaData},
  DDuce.Factories.Toolbar in '..\..\libraries\dduce\Source\Factories\DDuce.Factories.Toolbar.pas',
  DataGrabber.DataView.Base in 'DataView\DataGrabber.DataView.Base.pas' {BaseDataView},
  DataGrabber.Data.ResultSet in 'DataGrabber.Data.ResultSet.pas',
  JsonDataObjects in '..\..\libraries\JsonDataObjects\Source\JsonDataObjects.pas';

{$R *.res}

begin
  {$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
  {$WARNINGS ON}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

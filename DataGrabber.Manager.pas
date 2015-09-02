unit DataGrabber.Manager;

//*****************************************************************************

interface

uses
  System.SysUtils, System.Classes, Vcl.ActnList,

  DataGrabber.Interfaces, DataGrabber.Settings;

{
  The ConnectionViewManager is a singleton instance which manages:
    - the application settings (TDGSettings)
    - the ConnectionView instances
    -

}

type
  TdmConnectionViewManager = class(TDataModule, IConnectionViewManager)
    {$REGION 'designer controls'}
    aclActions                    : TActionList;
    actExecute                    : TAction;
    actHideEmptyColumns           : TAction;
    actShowAllColumns             : TAction;
    actToggleStayOnTop            : TAction;
    actExecuteLimited             : TAction;
    actHideSelectedColumns        : TAction;
    actHideConstantColumns        : TAction;
    actSelectionAsWiki            : TAction;
    actSelectionAsText            : TAction;
    actMergeAllColumnCells        : TAction;
    actGridMode                   : TAction;
    actAutoSizeCols               : TAction;
    actFormatSQL                  : TAction;
    actToggleFullScreen           : TAction;
    actCopy                       : TAction;
    actGroupBySelection           : TAction;
    actSelectionAsCommaText       : TAction;
    actSelectionAsQuotedCommaText : TAction;
    actMergeCells                 : TAction;
    actSelectionAsTextTable       : TAction;
    actStartTransaction           : TAction;
    actCommitTransaction          : TAction;
    actRollbackTransaction        : TAction;
    actProviderMode               : TAction;
    actInspect                    : TAction;
    actDataInspector              : TAction;
    actcxGrid                     : TAction;
    actGridView                   : TAction;
    actKGrid                      : TAction;
    actADO                        : TAction;
    actZEOS                       : TAction;
    actDBX                        : TAction;
    actSettings                   : TAction;
    actVirtualDBGrid              : TAction;
    actInspectDataSet             : TAction;
    actInspectConnection          : TAction;
    actInspectGrid                : TAction;
    actUNI                        : TAction;
    actInspectFields              : TAction;
    actToggleRepositoryTree       : TAction;
    actSyncEditorWithRepository   : TAction;
    actPreview                    : TAction;
    actDesigner                   : TAction;
    actPrint                      : TAction;
    actDebug                      : TAction;
    actSelectionAsFields          : TAction;
    actSelectionAsQuotedFields    : TAction;
    actFavoriteFieldsOnly         : TAction;
    actRtti                       : TAction;
    actSelectionAsWhereIn         : TAction;
    actCreateModel                : TAction;
    {$ENDREGION}
  private
    FSettings       : TDGSettings;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;


  end;

function ConnectionViewManager: IConnectionViewManager;


//*****************************************************************************

implementation

{$R *.dfm}

uses
  Forms;

var
  FConnectionViewManager: TdmConnectionViewManager;

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

function ConnectionViewManager: IConnectionViewManager;
begin
  Result := FConnectionViewManager;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TdmConnectionViewManager.AfterConstruction;
begin
  inherited;
  FSettings := TDGSettings.Create(Self);
  FSettings.Load;
end;

procedure TdmConnectionViewManager.BeforeDestruction;
begin
  inherited;
  FSettings.FormSettings.Assign(Self);
//  FSettings.FormSettings.VSplitterPos := pnlConnectionProfiles.Width;
//  FSettings.FormSettings.HSplitterPos := pnlTop.Height;
  FSettings.Save;
//  if Assigned(FView) then
//  begin
//    (FView as TComponent).Free;
//  end;
//  FView := nil;
//  if Assigned(FData) then
//  begin
//    (FData as TComponent).Free;
//  end;
//  FData := nil;
//  FreeAndNil(FDataInspector);
//  FreeAndNil(FSettings);
//  FreeAndNil(FTables);
//  FreeAndNil(FFields);
//  FreeAndNil(FScriptParser);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************



initialization
  FConnectionViewManager := TdmConnectionViewManager.Create(Application);

end.

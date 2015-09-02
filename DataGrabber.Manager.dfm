object dmConnectionViewManager: TdmConnectionViewManager
  OldCreateOrder = False
  Height = 536
  Width = 656
  object aclActions: TActionList
    Left = 384
    Top = 24
    object actExecute: TAction
      Category = 'Data'
      Caption = 'Execute'
      ShortCut = 120
      SecondaryShortCuts.Strings = (
        'F5')
    end
    object actHideEmptyColumns: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Hide empty'
      ShortCut = 16453
    end
    object actShowAllColumns: TAction
      Category = 'Grid'
      Caption = 'Show All'
    end
    object actToggleStayOnTop: TAction
      AutoCheck = True
      Caption = 'Stay on Top'
      ShortCut = 16464
    end
    object actExecuteLimited: TAction
      Category = 'Data'
      Caption = 'Execute Limited'
      ShortCut = 16500
    end
    object actHideSelectedColumns: TAction
      Category = 'Grid'
      Caption = 'Hide selected'
      ShortCut = 16456
    end
    object actHideConstantColumns: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Hide constant'
      ShortCut = 16459
    end
    object actSelectionAsWiki: TAction
      Category = 'Grid'
      Caption = 'Selection as Wiki'
    end
    object actSelectionAsText: TAction
      Category = 'Grid'
      Caption = 'Selection as text'
    end
    object actMergeAllColumnCells: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Merge column cells'
      ShortCut = 16461
    end
    object actGridMode: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Gridmode'
    end
    object actAutoSizeCols: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Autosize cols'
    end
    object actFormatSQL: TAction
      Category = 'Editor'
      Caption = 'Format SQL'
      ShortCut = 119
    end
    object actToggleFullScreen: TAction
      AutoCheck = True
      Caption = 'Toggle full screen'
      ShortCut = 122
    end
    object actCopy: TAction
      Category = 'Grid'
      Caption = 'Copy'
      ShortCut = 16451
    end
    object actGroupBySelection: TAction
      Category = 'Grid'
      Caption = 'Group by selected columns'
      ShortCut = 16455
    end
    object actSelectionAsCommaText: TAction
      Category = 'Grid'
      Caption = 'Selection as commatext'
    end
    object actSelectionAsQuotedCommaText: TAction
      Category = 'Grid'
      Caption = 'Selection as quoted commatext'
    end
    object actMergeCells: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'Merge cells'
      ShortCut = 49229
    end
    object actSelectionAsTextTable: TAction
      Category = 'Grid'
      Caption = 'Selection as texttable'
    end
    object actStartTransaction: TAction
      Category = 'Data'
      Caption = 'Start'
    end
    object actCommitTransaction: TAction
      Category = 'Data'
      Caption = 'Commit'
    end
    object actRollbackTransaction: TAction
      Category = 'Data'
      Caption = 'Rollback'
    end
    object actProviderMode: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'ProviderMode'
    end
    object actInspect: TAction
      Category = 'Inspect'
      Caption = 'actInspect'
      ShortCut = 16496
    end
    object actDataInspector: TAction
      Category = 'Inspect'
      AutoCheck = True
      Caption = 'Toggle data inspector'
    end
    object actcxGrid: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'TcxGrid'
      GroupIndex = 1
    end
    object actGridView: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'TGridView'
      GroupIndex = 1
    end
    object actKGrid: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'TKGrid'
      GroupIndex = 1
    end
    object actADO: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'ADO'
      GroupIndex = 2
    end
    object actZEOS: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'ZEOS'
      GroupIndex = 2
    end
    object actDBX: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'DBX'
      GroupIndex = 2
    end
    object actSettings: TAction
      Caption = 'Settings'
    end
    object actVirtualDBGrid: TAction
      Category = 'Grid'
      AutoCheck = True
      Caption = 'VirtualDBGrid'
      GroupIndex = 1
    end
    object actInspectDataSet: TAction
      Category = 'Inspect'
      Caption = 'Inspect dataset'
    end
    object actInspectConnection: TAction
      Category = 'Inspect'
      Caption = 'Inspect connection'
    end
    object actInspectGrid: TAction
      Category = 'Inspect'
      Caption = 'Inspect grid'
    end
    object actUNI: TAction
      Category = 'Data'
      AutoCheck = True
      Caption = 'UNIDAC'
      GroupIndex = 2
    end
    object actInspectFields: TAction
      Category = 'Inspect'
      Caption = 'Inspect fields'
    end
    object actToggleRepositoryTree: TAction
      AutoCheck = True
      Caption = 'Toggle repository tree'
    end
    object actSyncEditorWithRepository: TAction
      AutoCheck = True
      Caption = 'Synchronize editor with repository view'
    end
    object actPreview: TAction
      Category = 'Print'
      Caption = 'Print preview'
    end
    object actDesigner: TAction
      Category = 'Print'
      Caption = 'Report designer'
    end
    object actPrint: TAction
      Category = 'Print'
      Caption = 'Print'
    end
    object actDebug: TAction
      Caption = 'actDebug'
      ShortCut = 49220
    end
    object actSelectionAsFields: TAction
      Category = 'Grid'
      Caption = 'Selection as fields'
    end
    object actSelectionAsQuotedFields: TAction
      Category = 'Grid'
      Caption = 'Selection as quoted fields'
    end
    object actFavoriteFieldsOnly: TAction
      Category = 'Grid'
      Caption = 'Favorites only'
      Hint = 'Show favorite fields only'
    end
    object actRtti: TAction
      Caption = 'RTT-Eye'
    end
    object actSelectionAsWhereIn: TAction
      Category = 'Grid'
      Caption = 'Selection as WHERE ... IN (...)'
    end
    object actCreateModel: TAction
      Caption = 'Create Model'
    end
  end
end

object frmcxGrid: TfrmcxGrid
  Left = 0
  Top = 0
  Width = 589
  Height = 358
  DoubleBuffered = True
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Se'
  Font.Style = []
  ParentCtl3D = False
  ParentDoubleBuffered = False
  ParentFont = False
  TabOrder = 0
  object grdMain: TcxGrid
    Left = 0
    Top = 0
    Width = 589
    Height = 358
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Calibri'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    LevelTabs.Slants.Positions = []
    LevelTabs.Style = 5
    LookAndFeel.Kind = lfStandard
    LookAndFeel.NativeStyle = False
    object tvwMain: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Visible = True
      FilterBox.MRUItemsListDropDownCount = 10
      OnCustomDrawCell = tvwMainCustomDrawCell
      DataController.DataSource = dscMain
      DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText, dcoFocusTopRowAfterSorting, dcoImmediatePost, dcoInsertOnNewItemRowFocusing]
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Format = '0'
          Kind = skCount
          VisibleForCustomization = False
        end>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Filtering.ColumnFilteredItemsList = True
      Filtering.ColumnMRUItemsListCount = 10
      Filtering.ColumnPopup.MaxDropDownItemCount = 30
      OptionsBehavior.CellHints = True
      OptionsBehavior.FocusCellOnTab = True
      OptionsBehavior.FocusFirstCellOnNewRecord = True
      OptionsBehavior.GoToNextCellOnEnter = True
      OptionsBehavior.IncSearch = True
      OptionsBehavior.NavigatorHints = True
      OptionsBehavior.BestFitMaxRecordCount = 100
      OptionsBehavior.ColumnMergedGrouping = True
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.PullFocusing = True
      OptionsCustomize.ColumnHiding = True
      OptionsCustomize.ColumnsQuickCustomization = True
      OptionsCustomize.DataRowSizing = True
      OptionsCustomize.GroupRowSizing = True
      OptionsData.Appending = True
      OptionsSelection.MultiSelect = True
      OptionsSelection.CellMultiSelect = True
      OptionsSelection.InvertSelect = False
      OptionsView.CellEndEllipsis = True
      OptionsView.ExpandButtonsForEmptyDetails = False
      OptionsView.Footer = True
      OptionsView.FooterAutoHeight = True
      OptionsView.FooterMultiSummaries = True
      OptionsView.GridLineColor = clSilver
      OptionsView.GroupFooterMultiSummaries = True
      OptionsView.HeaderEndEllipsis = True
      OnCustomDrawGroupSummaryCell = tvwMainCustomDrawGroupSummaryCell
    end
    object grlGrid1Level1: TcxGridLevel
      GridView = tvwMain
      Options.TabsForEmptyDetails = False
    end
  end
  object dscMain: TDataSource
    Left = 72
    Top = 48
  end
  object ppmMain: TcxGridPopupMenu
    Grid = grdMain
    PopupMenus = <>
    AlwaysFireOnPopup = True
    Left = 240
    Top = 56
  end
end

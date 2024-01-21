inherited frmKGrid: TfrmKGrid
  ClientHeight = 257
  ClientWidth = 430
  ShowHint = True
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 430
  ExplicitHeight = 257
  TextHeight = 13
  object grdMain: TKDBGrid [0]
    Left = 0
    Top = 0
    Width = 430
    Height = 257
    Align = alClient
    BorderStyle = bsNone
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboAutoSizeImageCells, dboColNamesToHeader, dboDontClearFixedCells, dboEntireTable, dboImageHint, dboIndicateActiveRecord]
    ColCount = 2
    Colors.BrightRangeBkGnd = False
    Colors.CellLines = clSilver
    Colors.FixedCellBkGnd = clWindow
    Colors.FixedCellIndication = clBtnFace
    Colors.FixedCellLines = clWindow
    Colors.FixedThemedCellLines = clSilver
    Colors.FixedThemedCellHighlight = clYellow
    Colors.FixedThemedCellShadow = clNone
    Colors.SelectedCellBkGnd = clHighlight
    Colors.SelectedRangeBkGnd = clSilver
    Colors.SelectedFixedCellBkGnd = clGray
    Colors.ActiveRecord = clBtnFace
    Columns = <
      item
        Extent = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Callibri'
        Font.Style = []
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Calibri'
        TitleFont.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end>
    DataSource = dscMain
    DragStyle = dsLayeredConst
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Callibri'
    Font.Style = []
    MinRowHeight = 18
    Options = [goEraseBackground, goColSizing, goDoubleBufferedCells, goEditing, goEnterMoves, goHeader, goHeaderAlignment, goHorzLine, goIndicateSelection, goMouseOverCells, goRangeSelect, goRowSizing, goRowSorting, goTabs, goThemes, goVertLine]
    OptionsEx = [gxFixedCellClickSelect, gxFixedCellClickToggle, gxFixedThemedCells, gxTabWraps, gxMouseWheelScroll, gxMouseWheelKey]
    ParentFont = False
    RangeSelectStyle = rsMultiSelect
    RowCount = 2
    TabOrder = 0
    OnCustomSortRows = grdMainCustomSortRows
    OnDrawCell = grdMainDrawCell
    ColWidths = (
      16
      0)
    RowHeights = (
      0
      0)
  end
  inherited dscMain: TDataSource
    Left = 48
    Top = 80
  end
end

inherited frmKGrid: TfrmKGrid
  ClientHeight = 257
  ClientWidth = 430
  ShowHint = True
  ExplicitWidth = 446
  ExplicitHeight = 296
  PixelsPerInch = 96
  TextHeight = 13
  object grdMain: TKDBGrid [0]
    Left = 0
    Top = 0
    Width = 430
    Height = 257
    Align = alClient
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboAutoSizeImageCells, dboColNamesToHeader, dboEntireTable, dboIndicateActiveRecord]
    ColCount = 2
    Colors.CellLines = clSilver
    Columns = <
      item
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Callibri'
    Font.Style = []
    MinRowHeight = 18
    Options = [goColSizing, goDoubleBufferedCells, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goRangeSelect, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
    OptionsEx = [gxEnterWraps, gxTabWraps, gxMouseWheelScroll, gxMouseWheelKey]
    ParentFont = False
    RangeSelectStyle = rsMultiSelect
    RowCount = 2
    TabOrder = 0
    OnCustomSortRows = grdMainCustomSortRows
    OnDrawCell = grdMainDrawCell
    ColWidths = (
      0
      0)
    RowHeights = (
      0
      0)
  end
end

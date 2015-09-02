object frmKGrid: TfrmKGrid
  Left = 0
  Top = 0
  ClientHeight = 257
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object grdMain: TKDBGrid
    Left = 0
    Top = 0
    Width = 430
    Height = 257
    Align = alClient
    DBOptions = [dboAutoMoveRecord, dboColNamesToHeader, dboIndicateActiveRecord]
    ColCount = 1
    DataSource = dscMain
    DefaultRowHeight = 18
    FixedCols = 0
    MinRowHeight = 18
    Options = [goEraseBackground, goClippedCells, goColMoving, goColSizing, goColSorting, goDoubleBufferedCells, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goMouseCanHideCells, goRangeSelect, goRowSizing, goThemes, goThemedCells, goVertLine]
    RangeSelectStyle = rsMS_Excel
    RowCount = 2
    TabOrder = 0
    OnCellSpan = grdMainCellSpan
    OnDrawCell = grdMainDrawCell
    ColWidths = (
      64)
    RowHeights = (
      18
      18)
  end
  object dscMain: TDataSource
    Left = 40
    Top = 8
  end
end

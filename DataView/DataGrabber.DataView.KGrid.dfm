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
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object grdMain: TKDBGrid
    Left = 0
    Top = 0
    Width = 430
    Height = 257
    Align = alClient
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboColNamesToHeader, dboEntireTable, dboIndicateActiveRecord]
    ColCount = 1
    Columns = <
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
    DefaultRowHeight = 18
    FixedCols = 0
    MinRowHeight = 18
    Options = [goEraseBackground, goColMoving, goColSizing, goColSorting, goDoubleBufferedCells, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goRangeSelect, goRowSizing, goThemes, goThemedCells, goVertLine]
    RangeSelectStyle = rsMultiSelect
    RowCount = 2
    TabOrder = 0
    OnDrawCell = grdMainDrawCell
    ColWidths = (
      0)
    RowHeights = (
      0
      0)
  end
  object dscMain: TDataSource
    Left = 40
    Top = 8
  end
end

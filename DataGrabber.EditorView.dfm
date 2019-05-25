object frmEditorView: TfrmEditorView
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 324
  ClientWidth = 506
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object synSQL: TSynSQLSyn
    Options.AutoDetectEnabled = True
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMedGray
    ConditionalCommentAttri.Foreground = clMedGray
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clRed
    NumberAttri.Style = [fsBold, fsItalic]
    StringAttri.Foreground = clGreen
    StringAttri.Style = [fsBold, fsItalic]
    SymbolAttri.Foreground = clRed
    SymbolAttri.Style = [fsBold]
    VariableAttri.Foreground = clPurple
    VariableAttri.Style = [fsBold]
    Left = 40
    Top = 8
  end
  object scpMain: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    ItemList.Strings = (
      'select,keyword'
      'from'
      'where'
      'group by'
      'order by'
      'inner join'
      'left outer join')
    InsertList.Strings = (
      'select'
      'from'
      'where'
      'group by'
      'order by'
      'inner join'
      'left outer join')
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Left = 120
    Top = 8
  end
end

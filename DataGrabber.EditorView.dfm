object frmEditorView: TfrmEditorView
  Left = 0
  Top = 0
  ClientHeight = 286
  ClientWidth = 490
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
    Left = 48
    Top = 40
  end
  object scpMain: TSynCompletionProposal
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
    Left = 128
    Top = 40
  end
end

object frmMemo: TfrmMemo
  Left = 334
  Top = 219
  ClientHeight = 315
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    638
    315)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 555
    Top = 282
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object mmoMain: TMemo
    Left = 8
    Top = 8
    Width = 622
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyPress = mmoMainKeyPress
  end
  object btnSaveAs: TButton
    Left = 472
    Top = 282
    Width = 75
    Height = 25
    Action = actFileSaveAs
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 429
    Top = 280
    object actFileSaveAs: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      OnAccept = actFileSaveAsAccept
    end
  end
end

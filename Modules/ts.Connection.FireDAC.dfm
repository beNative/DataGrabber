object dmFireDACConnection: TdmFireDACConnection
  Left = 0
  Top = 0
  ClientHeight = 112
  ClientWidth = 199
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object conFireDAC: TFDConnection
    FetchOptions.AssignedValues = [evMode, evRecordCountMode]
    FetchOptions.Mode = fmAll
    FetchOptions.RecordCountMode = cmTotal
    ResourceOptions.AssignedValues = [rvCmdExecMode, rvDirectExecute]
    ResourceOptions.CmdExecMode = amCancelDialog
    ResourceOptions.DirectExecute = True
    Left = 24
    Top = 16
  end
end

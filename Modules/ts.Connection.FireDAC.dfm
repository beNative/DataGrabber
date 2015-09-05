object dmFireDACConnection: TdmFireDACConnection
  OldCreateOrder = False
  Height = 150
  Width = 215
  object conFireDAC: TFDConnection
    Left = 88
    Top = 56
  end
  object FDManager1: TFDManager
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <>
    Active = True
    Left = 144
    Top = 24
  end
end

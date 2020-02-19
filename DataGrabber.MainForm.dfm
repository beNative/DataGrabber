object frmMain: TfrmMain
  Left = 731
  Top = 411
  ClientHeight = 593
  ClientWidth = 1124
  Color = clBtnFace
  TransparentColorValue = clGray
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  ScreenSnap = True
  ShowHint = True
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStatusBar: TPanel
    Left = 0
    Top = 572
    Width = 1124
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object shpLine: TShape
      Left = 0
      Top = 0
      Width = 1124
      Height = 1
      Align = alTop
      Pen.Color = clScrollBar
    end
    object pnlEditMode: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Data mode'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlElapsedTime: TPanel
      AlignWithMargins = True
      Left = 105
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Elapsed execution time'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlConnectionStatus: TPanel
      AlignWithMargins = True
      Left = 53
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Connection status'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
    object pnlRecordCount: TPanel
      AlignWithMargins = True
      Left = 157
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Recordcount'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 3
    end
    object pnlFieldCount: TPanel
      AlignWithMargins = True
      Left = 209
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Field count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 4
    end
    object pnlConstantFieldsCount: TPanel
      AlignWithMargins = True
      Left = 313
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Constant fields count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 5
    end
    object pnlEmptyFieldsCount: TPanel
      AlignWithMargins = True
      Left = 261
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Empty fields count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 6
    end
    object pnlHiddenFieldsCount: TPanel
      AlignWithMargins = True
      Left = 365
      Top = 2
      Width = 50
      Height = 18
      Hint = 'Hidden fields count'
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alLeft
      BevelEdges = [beRight]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 7
    end
  end
  object pnlConnectionViews: TPanel
    Left = 0
    Top = 23
    Width = 1124
    Height = 549
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object ctMain: TChromeTabs
      Left = 0
      Top = 0
      Width = 1124
      Height = 29
      OnActiveTabChanged = ctMainActiveTabChanged
      OnButtonAddClick = ctMainButtonAddClick
      OnButtonCloseTabClick = ctMainButtonCloseTabClick
      OnNeedDragImageControl = ctMainNeedDragImageControl
      ActiveTabIndex = -1
      ImagesSpinnerDownload = imlSpinner
      Options.Display.CloseButton.Offsets.Vertical = 2
      Options.Display.CloseButton.Offsets.Horizontal = -10
      Options.Display.CloseButton.Height = 15
      Options.Display.CloseButton.Width = 14
      Options.Display.CloseButton.AutoHide = True
      Options.Display.CloseButton.Visibility = bvActive
      Options.Display.CloseButton.AutoHideWidth = 20
      Options.Display.CloseButton.CrossRadialOffset = 4
      Options.Display.AddButton.Offsets.Vertical = 10
      Options.Display.AddButton.Offsets.Horizontal = 3
      Options.Display.AddButton.Height = 14
      Options.Display.AddButton.Width = 40
      Options.Display.AddButton.ShowPlusSign = True
      Options.Display.AddButton.Visibility = avRightFloating
      Options.Display.AddButton.HorizontalOffsetFloating = -3
      Options.Display.ScrollButtonLeft.Offsets.Vertical = 10
      Options.Display.ScrollButtonLeft.Offsets.Horizontal = 1
      Options.Display.ScrollButtonLeft.Height = 15
      Options.Display.ScrollButtonLeft.Width = 15
      Options.Display.ScrollButtonRight.Offsets.Vertical = 10
      Options.Display.ScrollButtonRight.Offsets.Horizontal = 1
      Options.Display.ScrollButtonRight.Height = 15
      Options.Display.ScrollButtonRight.Width = 15
      Options.Display.TabModifiedGlow.Style = msKnightRider
      Options.Display.TabModifiedGlow.VerticalOffset = -6
      Options.Display.TabModifiedGlow.Height = 30
      Options.Display.TabModifiedGlow.Width = 100
      Options.Display.TabModifiedGlow.AnimationPeriodMS = 100
      Options.Display.TabModifiedGlow.EaseType = ttEaseInOutSine
      Options.Display.TabModifiedGlow.AnimationUpdateMS = 50
      Options.Display.Tabs.SeeThroughTabs = False
      Options.Display.Tabs.TabOverlap = 15
      Options.Display.Tabs.ContentOffsetLeft = 15
      Options.Display.Tabs.ContentOffsetRight = 22
      Options.Display.Tabs.OffsetLeft = 5
      Options.Display.Tabs.OffsetTop = 6
      Options.Display.Tabs.OffsetRight = 5
      Options.Display.Tabs.OffsetBottom = 0
      Options.Display.Tabs.MinWidth = 120
      Options.Display.Tabs.MaxWidth = 400
      Options.Display.Tabs.TabWidthFromContent = True
      Options.Display.Tabs.PinnedWidth = 150
      Options.Display.Tabs.ImageOffsetLeft = 15
      Options.Display.Tabs.TextTrimType = tttNone
      Options.Display.Tabs.Orientation = toTop
      Options.Display.Tabs.BaseLineTabRegionOnly = False
      Options.Display.Tabs.WordWrap = False
      Options.Display.Tabs.TextAlignmentHorizontal = taLeftJustify
      Options.Display.Tabs.TextAlignmentVertical = taVerticalCenter
      Options.Display.Tabs.ShowImages = True
      Options.Display.Tabs.ShowPinnedTabText = True
      Options.Display.TabContainer.TransparentBackground = True
      Options.Display.TabContainer.OverlayButtons = False
      Options.Display.TabContainer.PaddingLeft = 0
      Options.Display.TabContainer.PaddingRight = 0
      Options.Display.TabMouseGlow.Offsets.Vertical = 0
      Options.Display.TabMouseGlow.Offsets.Horizontal = 0
      Options.Display.TabMouseGlow.Height = 200
      Options.Display.TabMouseGlow.Width = 200
      Options.Display.TabMouseGlow.Visible = True
      Options.Display.TabSpinners.Upload.ReverseDirection = True
      Options.Display.TabSpinners.Upload.RenderedAnimationStep = 2
      Options.Display.TabSpinners.Upload.Position.Offsets.Vertical = 0
      Options.Display.TabSpinners.Upload.Position.Offsets.Horizontal = 0
      Options.Display.TabSpinners.Upload.Position.Height = 16
      Options.Display.TabSpinners.Upload.Position.Width = 16
      Options.Display.TabSpinners.Upload.SweepAngle = 135
      Options.Display.TabSpinners.Download.ReverseDirection = False
      Options.Display.TabSpinners.Download.RenderedAnimationStep = 5
      Options.Display.TabSpinners.Download.Position.Offsets.Vertical = 0
      Options.Display.TabSpinners.Download.Position.Offsets.Horizontal = 0
      Options.Display.TabSpinners.Download.Position.Height = 16
      Options.Display.TabSpinners.Download.Position.Width = 16
      Options.Display.TabSpinners.Download.SweepAngle = 135
      Options.Display.TabSpinners.AnimationUpdateMS = 50
      Options.Display.TabSpinners.HideImagesWhenSpinnerVisible = True
      Options.DragDrop.DragType = dtWithinContainer
      Options.DragDrop.DragOutsideImageAlpha = 220
      Options.DragDrop.DragOutsideDistancePixels = 30
      Options.DragDrop.DragStartPixels = 2
      Options.DragDrop.DragControlImageResizeFactor = 0.500000000000000000
      Options.DragDrop.DragCursor = crDefault
      Options.DragDrop.DragDisplay = ddTabAndControl
      Options.DragDrop.DragFormBorderWidth = 2
      Options.DragDrop.DragFormBorderColor = 8421504
      Options.DragDrop.ContrainDraggedTabWithinContainer = True
      Options.Animation.DefaultMovementAnimationTimeMS = 50
      Options.Animation.DefaultStyleAnimationTimeMS = 10
      Options.Animation.AnimationTimerInterval = 5
      Options.Animation.MinimumTabAnimationWidth = 40
      Options.Animation.DefaultMovementEaseType = ttLinearTween
      Options.Animation.DefaultStyleEaseType = ttLinearTween
      Options.Animation.MovementAnimations.TabAdd.UseDefaultEaseType = True
      Options.Animation.MovementAnimations.TabAdd.UseDefaultAnimationTime = True
      Options.Animation.MovementAnimations.TabAdd.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabAdd.AnimationTimeMS = 500
      Options.Animation.MovementAnimations.TabDelete.UseDefaultEaseType = True
      Options.Animation.MovementAnimations.TabDelete.UseDefaultAnimationTime = True
      Options.Animation.MovementAnimations.TabDelete.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabDelete.AnimationTimeMS = 500
      Options.Animation.MovementAnimations.TabMove.UseDefaultEaseType = False
      Options.Animation.MovementAnimations.TabMove.UseDefaultAnimationTime = False
      Options.Animation.MovementAnimations.TabMove.EaseType = ttEaseOutExpo
      Options.Animation.MovementAnimations.TabMove.AnimationTimeMS = 500
      Options.Behaviour.BackgroundDblClickMaximiseRestoreForm = False
      Options.Behaviour.BackgroundDragMovesForm = False
      Options.Behaviour.TabSmartDeleteResizing = True
      Options.Behaviour.TabSmartDeleteResizeCancelDelay = 700
      Options.Behaviour.UseBuiltInPopupMenu = False
      Options.Behaviour.TabRightClickSelect = True
      Options.Behaviour.ActivateNewTab = True
      Options.Behaviour.DebugMode = False
      Options.Behaviour.IgnoreDoubleClicksWhileAnimatingMovement = True
      Options.Scrolling.Enabled = True
      Options.Scrolling.ScrollButtons = csbRight
      Options.Scrolling.ScrollStep = 20
      Options.Scrolling.ScrollRepeatDelay = 20
      Options.Scrolling.AutoHideButtons = True
      Options.Scrolling.DragScroll = True
      Options.Scrolling.DragScrollOffset = 50
      Options.Scrolling.MouseWheelScroll = True
      Tabs = <>
      LookAndFeel.TabsContainer.StartColor = clWhite
      LookAndFeel.TabsContainer.StopColor = clWhite
      LookAndFeel.TabsContainer.StartAlpha = 255
      LookAndFeel.TabsContainer.StopAlpha = 255
      LookAndFeel.TabsContainer.OutlineColor = clSilver
      LookAndFeel.TabsContainer.OutlineAlpha = 0
      LookAndFeel.Tabs.BaseLine.Color = clScrollBar
      LookAndFeel.Tabs.BaseLine.Thickness = 1.000000000000000000
      LookAndFeel.Tabs.BaseLine.Alpha = 255
      LookAndFeel.Tabs.Modified.CentreColor = clBlue
      LookAndFeel.Tabs.Modified.OutsideColor = clWhite
      LookAndFeel.Tabs.Modified.CentreAlpha = 130
      LookAndFeel.Tabs.Modified.OutsideAlpha = 0
      LookAndFeel.Tabs.DefaultFont.Name = 'Segoe UI'
      LookAndFeel.Tabs.DefaultFont.Color = clBlack
      LookAndFeel.Tabs.DefaultFont.Size = 8
      LookAndFeel.Tabs.DefaultFont.Alpha = 255
      LookAndFeel.Tabs.DefaultFont.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.MouseGlow.CentreColor = clWhite
      LookAndFeel.Tabs.MouseGlow.OutsideColor = clWhite
      LookAndFeel.Tabs.MouseGlow.CentreAlpha = 120
      LookAndFeel.Tabs.MouseGlow.OutsideAlpha = 0
      LookAndFeel.Tabs.Spinners.Upload.Color = 12759975
      LookAndFeel.Tabs.Spinners.Upload.Thickness = 2.500000000000000000
      LookAndFeel.Tabs.Spinners.Upload.Alpha = 255
      LookAndFeel.Tabs.Spinners.Download.Color = 14388040
      LookAndFeel.Tabs.Spinners.Download.Thickness = 2.500000000000000000
      LookAndFeel.Tabs.Spinners.Download.Alpha = 255
      LookAndFeel.Tabs.Active.Font.Name = 'Tahoma'
      LookAndFeel.Tabs.Active.Font.Color = clBlack
      LookAndFeel.Tabs.Active.Font.Size = 8
      LookAndFeel.Tabs.Active.Font.Alpha = 100
      LookAndFeel.Tabs.Active.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.Active.Font.UseDefaultFont = True
      LookAndFeel.Tabs.Active.Style.StartColor = clWhite
      LookAndFeel.Tabs.Active.Style.StopColor = clWhite
      LookAndFeel.Tabs.Active.Style.StartAlpha = 255
      LookAndFeel.Tabs.Active.Style.StopAlpha = 255
      LookAndFeel.Tabs.Active.Style.OutlineColor = clGray
      LookAndFeel.Tabs.Active.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.Active.Style.OutlineAlpha = 255
      LookAndFeel.Tabs.NotActive.Font.Name = 'Tahoma'
      LookAndFeel.Tabs.NotActive.Font.Color = clBlack
      LookAndFeel.Tabs.NotActive.Font.Size = 8
      LookAndFeel.Tabs.NotActive.Font.Alpha = 255
      LookAndFeel.Tabs.NotActive.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.NotActive.Font.UseDefaultFont = True
      LookAndFeel.Tabs.NotActive.Style.StartColor = clSilver
      LookAndFeel.Tabs.NotActive.Style.StopColor = clSilver
      LookAndFeel.Tabs.NotActive.Style.StartAlpha = 255
      LookAndFeel.Tabs.NotActive.Style.StopAlpha = 255
      LookAndFeel.Tabs.NotActive.Style.OutlineColor = clGray
      LookAndFeel.Tabs.NotActive.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.NotActive.Style.OutlineAlpha = 255
      LookAndFeel.Tabs.Hot.Font.Name = 'Tahoma'
      LookAndFeel.Tabs.Hot.Font.Color = clBlack
      LookAndFeel.Tabs.Hot.Font.Size = 8
      LookAndFeel.Tabs.Hot.Font.Alpha = 255
      LookAndFeel.Tabs.Hot.Font.TextRenderingMode = TextRenderingHintClearTypeGridFit
      LookAndFeel.Tabs.Hot.Font.UseDefaultFont = True
      LookAndFeel.Tabs.Hot.Style.StartColor = clSilver
      LookAndFeel.Tabs.Hot.Style.StopColor = clSilver
      LookAndFeel.Tabs.Hot.Style.StartAlpha = 255
      LookAndFeel.Tabs.Hot.Style.StopAlpha = 255
      LookAndFeel.Tabs.Hot.Style.OutlineColor = clGray
      LookAndFeel.Tabs.Hot.Style.OutlineSize = 1.000000000000000000
      LookAndFeel.Tabs.Hot.Style.OutlineAlpha = 255
      LookAndFeel.CloseButton.Cross.Normal.Color = clBlack
      LookAndFeel.CloseButton.Cross.Normal.Thickness = 1.500000000000000000
      LookAndFeel.CloseButton.Cross.Normal.Alpha = 255
      LookAndFeel.CloseButton.Cross.Down.Color = clWhite
      LookAndFeel.CloseButton.Cross.Down.Thickness = 2.000000000000000000
      LookAndFeel.CloseButton.Cross.Down.Alpha = 255
      LookAndFeel.CloseButton.Cross.Hot.Color = clWhite
      LookAndFeel.CloseButton.Cross.Hot.Thickness = 2.000000000000000000
      LookAndFeel.CloseButton.Cross.Hot.Alpha = 255
      LookAndFeel.CloseButton.Circle.Normal.StartColor = clGradientActiveCaption
      LookAndFeel.CloseButton.Circle.Normal.StopColor = clNone
      LookAndFeel.CloseButton.Circle.Normal.StartAlpha = 0
      LookAndFeel.CloseButton.Circle.Normal.StopAlpha = 0
      LookAndFeel.CloseButton.Circle.Normal.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Normal.OutlineAlpha = 0
      LookAndFeel.CloseButton.Circle.Down.StartColor = 3487169
      LookAndFeel.CloseButton.Circle.Down.StopColor = 3487169
      LookAndFeel.CloseButton.Circle.Down.StartAlpha = 255
      LookAndFeel.CloseButton.Circle.Down.StopAlpha = 255
      LookAndFeel.CloseButton.Circle.Down.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Down.OutlineAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.StartColor = clRed
      LookAndFeel.CloseButton.Circle.Hot.StopColor = clRed
      LookAndFeel.CloseButton.Circle.Hot.StartAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.StopAlpha = 255
      LookAndFeel.CloseButton.Circle.Hot.OutlineColor = clGray
      LookAndFeel.CloseButton.Circle.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.CloseButton.Circle.Hot.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Normal.StartColor = clSilver
      LookAndFeel.AddButton.Button.Normal.StopColor = clSilver
      LookAndFeel.AddButton.Button.Normal.StartAlpha = 255
      LookAndFeel.AddButton.Button.Normal.StopAlpha = 255
      LookAndFeel.AddButton.Button.Normal.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Normal.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Down.StartColor = clSilver
      LookAndFeel.AddButton.Button.Down.StopColor = clSilver
      LookAndFeel.AddButton.Button.Down.StartAlpha = 255
      LookAndFeel.AddButton.Button.Down.StopAlpha = 255
      LookAndFeel.AddButton.Button.Down.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Down.OutlineAlpha = 255
      LookAndFeel.AddButton.Button.Hot.StartColor = clBlack
      LookAndFeel.AddButton.Button.Hot.StopColor = clBlack
      LookAndFeel.AddButton.Button.Hot.StartAlpha = 255
      LookAndFeel.AddButton.Button.Hot.StopAlpha = 255
      LookAndFeel.AddButton.Button.Hot.OutlineColor = clGray
      LookAndFeel.AddButton.Button.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.Button.Hot.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Normal.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Normal.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Normal.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Normal.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Down.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Down.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Down.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Down.OutlineAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.StartColor = clWhite
      LookAndFeel.AddButton.PlusSign.Hot.StopColor = clWhite
      LookAndFeel.AddButton.PlusSign.Hot.StartAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.StopAlpha = 255
      LookAndFeel.AddButton.PlusSign.Hot.OutlineColor = clGray
      LookAndFeel.AddButton.PlusSign.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.AddButton.PlusSign.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Normal.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Normal.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Normal.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Normal.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Down.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Down.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Down.OutlineColor = clBlack
      LookAndFeel.ScrollButtons.Button.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Down.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.StartColor = clBlack
      LookAndFeel.ScrollButtons.Button.Hot.StopColor = clBlack
      LookAndFeel.ScrollButtons.Button.Hot.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Hot.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.StartColor = clSilver
      LookAndFeel.ScrollButtons.Button.Disabled.StopColor = clSilver
      LookAndFeel.ScrollButtons.Button.Disabled.StartAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.StopAlpha = 255
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Button.Disabled.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Normal.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Normal.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Normal.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Down.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Down.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Down.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Hot.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Hot.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Hot.OutlineAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.StartColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Disabled.StopColor = clWhite
      LookAndFeel.ScrollButtons.Arrow.Disabled.StartAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.StopAlpha = 255
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineColor = clGray
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineSize = 1.000000000000000000
      LookAndFeel.ScrollButtons.Arrow.Disabled.OutlineAlpha = 255
      Align = alTop
      PopupMenu = ppmCVTabs
      TabOrder = 0
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1124
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object tlbMain: TToolBar
      Left = 0
      Top = 0
      Width = 1081
      Height = 23
      Align = alLeft
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = True
      ButtonHeight = 20
      ButtonWidth = 20
      Customizable = True
      EdgeInner = esNone
      EdgeOuter = esNone
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      HideClippedButtons = True
      List = True
      GradientDirection = gdHorizontal
      AllowTextButtons = True
      TabOrder = 0
      Transparent = True
      Wrapable = False
      OnCustomDraw = tlbMainCustomDraw
    end
    object tlbTopRight: TToolBar
      Left = 1074
      Top = 0
      Width = 50
      Height = 23
      Align = alRight
      Anchors = [akLeft, akTop, akBottom]
      ButtonHeight = 20
      Customizable = True
      EdgeInner = esNone
      EdgeOuter = esNone
      GradientEndColor = clBtnFace
      GradientStartColor = clBtnFace
      GradientDirection = gdHorizontal
      TabOrder = 1
      Transparent = True
      Wrapable = False
      OnCustomDraw = tlbMainCustomDraw
    end
  end
  object aclMain: TActionList
    Left = 104
    Top = 72
    object actAddConnectionView: TAction
      Caption = 'Add connectionview'
      ShortCut = 16462
      OnExecute = actAddConnectionViewExecute
    end
    object actInspectChromeTab: TAction
      Caption = 'actInspectChromeTab'
      ShortCut = 49225
      OnExecute = actInspectChromeTabExecute
    end
    object actCloseAllOtherTabs: TAction
      Caption = 'Close all other tabs'
      OnExecute = actCloseAllOtherTabsExecute
    end
    object actCloseTab: TAction
      Caption = 'Close this tab'
      OnExecute = actCloseTabExecute
    end
  end
  object imlSpinner: TImageList
    DrawingStyle = dsTransparent
    Left = 32
    Top = 72
    Bitmap = {
      494C01010B000D00100010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000B0B0B0C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000050505060B0B0B0C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000121212140101010200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001919191C2C2C2C380000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002D2D2D3A1414141600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010101023434344800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000343434483232324400000000000000000000
      0000000000000707070800000000000000000000000000000000000000000000
      0000000000000000000000000000000000004343438300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000313131422B2B2B38000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003A3A3AB500000000000000000000
      0000000000002222222800000000000000000000000000000000000000000000
      000000000000000000000000000000000000404040702E2E2E3C000000000000
      0000000000001414141600000000000000000000000000000000000000000000
      000000000000000000000000000000000000070707083E3E3E9F000000000000
      0000000000000000000005050506000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      030403030304000000000000000000000000363636B715151518000000000000
      0000202020262525252E000000000000000000000000030303042B2B2B383D3D
      3D643B3B3B5C1717171A00000000000000003D3D3D623F3F3F6C000000000000
      0000000000002F2F2F3E000000000000000000000000141414162D2D2D3A3F3F
      3F664040409B4040407205050506000000000B0B0B0C333333BF000000000000
      0000000000001010101214141416000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000050505062424242C393939544242
      4281393939B5383838B32323232A000000002A2A2ACF1717171A000000000101
      010241414195010101020000000000000000030303041A1A1A1E0F0F0F101010
      10123636364E2C2C2CCD4040406E00000000404040913B3B3B5C000000000000
      00002525252E3535354A00000000000000000000000000000000000000000000
      00000101010241414178333333C10909090A3636364E3E3E3E9F000000000000
      0000000000003C3C3C5C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000101010200000000000000000000
      000000000000323232441D1D1DE13D3D3D60161616ED010101022525252E3737
      37B5373737500000000000000000000000000000000000000000000000000000
      0000000000000D0D0D0E2E2E2EC74141417C1C1C1CE31C1C1C20010101023636
      364E3C3C3CAB0101010200000000000000000000000000000000000000000000
      000000000000000000004040408D3C3C3CA7323232C13636364E000000000303
      03044141417C3131314200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003B3B3B5A000000FF0F0F0FF1242424D92C2C2CCD3232
      3244000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F103E3E3E64000000FF0B0B0BF5383838B1212121DF3E3E
      3E9F0F0F0F100000000000000000000000000000000000000000000000000000
      00000909090A3333334641414178010101FD030303FB4040408F3B3B3BA52A2A
      2AD33B3B3B5C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000313131422D2D2DCB232323DB0F0F0FF1000000FF3D3D3D60000000000000
      0000000000000000000000000000000000000000000000000000000000000F0F
      0F103E3E3E9F212121DF373737B30B0B0BF5000000FF3F3F3F68101010120000
      0000000000000000000000000000000000000000000000000000000000003B3B
      3B5C2A2A2AD33B3B3BA54040408F040404FB010101FD4141417A343434480909
      090A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003737
      3750373737B52525252E01010102161616EB3C3C3C5E1E1E1EE1333333460000
      0000000000000000000000000000010101020000000000000000030303043C3C
      3CAB3636364C010101021A1A1A1E1A1A1AE5424242782D2D2DC90D0D0D0E0000
      0000000000000000000000000000000000000000000000000000323232444141
      417803030304000000003636364C313131C33E3E3EA140404091000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101024141
      4195010101020000000015151518292929D10000000021212128393939B13939
      39B54141417E393939542424242C0505050600000000000000003636364C2424
      242C00000000000000003A3A3A5840404093000000003F3F3F6A2C2C2CCD3737
      37500F0F0F100D0D0D0E1A1A1A1E0303030400000000000000003C3C3C5A0000
      000000000000000000003E3E3E9D373737500909090A343434BD4141417C0101
      0102000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000272727301E1E
      1E24000000000000000014141416353535B90000000000000000000000000303
      03040303030400000000000000000000000000000000000000002F2F2F3E0000
      000000000000000000003E3E3E6A3D3D3D640000000000000000151515183B3B
      3B5C3D3D3D642D2D2D3A030303040000000000000000151515180F0F0F100000
      00000000000000000000343434BD0D0D0D0E0000000005050506404040704040
      409B3E3E3E642D2D2D3A14141416000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000222222280000
      00000000000000000000000000003B3B3BB30000000000000000000000000000
      0000000000000000000000000000000000000000000000000000141414160000
      000000000000000000002E2E2E3C404040700000000000000000000000000000
      0000000000000000000000000000000000000000000003030304000000000000
      000000000000000000003E3E3E9F070707080000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000070707080000
      0000000000000000000000000000333333463434344800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000434343830000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002D2D2D3A303030400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002D2D2D3A1717171A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000141414162D2D2D3A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000343434480101010200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0B0B0C010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B0B0B0C05050506000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010101021010101200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000505050600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000707
      0708000000000000000000000000000000000000000000000000000000001919
      191C000000000000000000000000000000000000000000000000000000000101
      01021A1A1A1E0D0D0D0E00000000000000000000000000000000000000000F0F
      0F10000000000000000000000000000000000000000000000000000000000000
      000007070708272727300F0F0F10000000000000000000000000000000000000
      0000050505060000000000000000000000000000000000000000000000000000
      0000000000000D0D0D0E31313142030303040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000505
      0506292929343D3D3D6014141416000000000000000000000000000000003131
      3142000000000000000000000000000000000000000000000000000000000000
      000000000000303030403F3F3F68010101020000000000000000000000002929
      2934000000000000000000000000000000000000000000000000000000000000
      000000000000000000003F3F3F662D2D2D3A0000000000000000000000000000
      0000202020260000000000000000000000000000000000000000000000000000
      000000000000000000000B0B0B0C424242870303030400000000000000000000
      0000151515180000000000000000000000000000000000000000000000000000
      0000000000000B0B0B0C3C3C3CAB2424242C0000000000000000000000004040
      406C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000030303040414141910000000000000000000000003B3B
      3B5A030303040000000000000000000000000000000000000000000000000000
      00000000000000000000000000003A3A3AAF1414141600000000000000000000
      00003636364C0000000000000000000000000000000000000000000000000000
      00000000000000000000000000003636364C4040407000000000000000000000
      00002D2D2D3A0000000000000000000000000000000000000000000000000000
      000000000000000000001D1D1D22313131C500000000000000001818181C4141
      4189000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000343434BD1D1D1D2200000000000000004242
      4293000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003D3D3D644040407200000000000000002323
      232A393939560000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F0F0F10303030C500000000000000000000
      00004040406C0000000000000000000000000000000000000000000000000000
      00000000000000000000000000002E2E2EC721212128010101023A3A3AAF3030
      3040000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000404040893D3D3D6400000000323232443E3E
      3EA1000000000000000000000000000000000000000000000000010101022929
      29343131314210101012000000003535354A3D3D3D9F00000000070707083838
      38B5121212140000000000000000000000000000000003030304383838524242
      428F3D3D3D9F3C3C3C5E050505060D0D0D0E232323DB00000000000000003333
      33464040407200000000000000000000000000000000000000000F0F0F103F3F
      3F6C404040934141417A2A2A2A363E3E3E9F4040406E3A3A3AAD3F3F3F990000
      00000000000000000000000000000000000000000000030303043E3E3E663F3F
      3FA3313131CB323232C13E3E3E6642424281414141874040406E252525D71515
      151800000000000000000000000000000000000000002C2C2C383F3F3F683E3E
      3E6640404085252525D93E3E3E9D4040406C3A3A3AA92B2B2B382D2D2DC93E3E
      3E64000000000000000000000000000000000D0D0D0E272727300D0D0D0E0000
      00001D1D1D223F3F3F99282828D33D3D3D602B2B2BCD0F0F0F104141417A3232
      32C30909090A0000000000000000000000000000000028282832404040702F2F
      2F3E2D2D2D3A40404072262626D5070707F9010101FD41414191010101020000
      000000000000000000001717171A121212140D0D0D0E313131420F0F0F100000
      0000000000002525252E3A3A3AAF010101FD040404FB363636B9151515180000
      0000000000000000000000000000050505060F0F0F1005050506000000000000
      0000000000000505050641414189000000FF0B0B0BF5222222DB343434480000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003E3E3E66000000FF0F0F0FF1161616EB4040408D0B0B
      0B0C00000000000000000000000000000000121212141818181C000000000000
      000000000000010101024141418F010101FD090909F7252525D7404040742E2E
      2E3C2F2F2F3E4040407028282832000000000505050600000000000000000000
      00000000000014141416373737B7040404FB010101FD373737B3282828320000
      0000000000000D0D0D0E313131420D0D0D0E0000000000000000000000000000
      00000000000033333346232323D90B0B0BF5000000FF4040408D070707080000
      0000000000000000000003030304101010120000000000000000000000000000
      00000B0B0B0C4141418B161616EB0F0F0FF1000000FF3F3F3F6A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003F3F3F97393939AF4040406C3D3D3DA128282832414141784040
      40913F3F3F6C0F0F0F1000000000000000000000000000000000000000000000
      000015151518272727D54040407042424283424242833D3D3D62333333BF3131
      31CB3F3F3FA33F3F3F6803030304000000000000000000000000000000000000
      00003E3E3E642E2E2EC92B2B2B383B3B3BA54040406C3F3F3F99232323DB4040
      40853E3E3E663F3F3F682D2D2D3A000000000000000000000000000000000909
      090A323232C34141417A0F0F0F102D2D2DC93D3D3D602A2A2ACF3E3E3E9B1E1E
      1E24000000000D0D0D0E272727300B0B0B0C0000000000000000000000000000
      0000303030403A3A3AAF01010102202020262D2D2DC900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003E3E3EA132323244000000003C3C3C604040408B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000001414
      1416383838B507070708000000003D3D3D9D3636364C000000000F0F0F103131
      3142292929340303030400000000000000000000000000000000000000004141
      4174323232440000000000000000242424D90F0F0F10030303043B3B3B5A3D3D
      3D9D4242428D3838385203030304000000000000000000000000000000000000
      00004141418B1818181C0000000000000000323232C31D1D1D22000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      01024242429300000000000000001C1C1C20343434BD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000003A3A
      3A58212121280000000000000000404040723E3E3E6600000000000000000000
      0000000000000000000000000000000000000000000000000000000000004040
      406A000000000000000000000000313131C30F0F0F1000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004040406C0000000000000000000000002323232A3C3C3CA90909090A0000
      0000000000000000000000000000000000000000000000000000000000000505
      05063A3A3A58000000000000000000000000414141912F2F2F3E000000000000
      0000000000000000000000000000000000000000000000000000000000003636
      364C000000000000000000000000141414163B3B3BAD00000000000000000000
      0000000000000000000000000000000000000000000000000000000000002E2E
      2E3C000000000000000000000000404040703636364C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003131314200000000000000000000000000000000141414163D3D3D602929
      2934030303040000000000000000000000000000000000000000000000000000
      000029292934000000000000000000000000010101023F3F3F682F2F2F3E0000
      0000000000000000000000000000000000000000000000000000000000002020
      2026000000000000000000000000000000002D2D2D3A3E3E3E64000000000000
      0000000000000000000000000000000000000000000000000000000000001515
      151800000000000000000000000003030304424242870B0B0B0C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001919191C0000000000000000000000000000000000000000000000000000
      0000070707080000000000000000000000000000000000000000000000000000
      00000F0F0F1000000000000000000000000000000000000000000D0D0D0E1919
      191C000000000000000000000000000000000000000000000000000000000505
      050600000000000000000000000000000000000000000F0F0F10272727300707
      0708000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000003030304313131420D0D0D0E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010101020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000050505060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001010101200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000030303040B0B0B0C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000909090A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000050505060000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001F1F1F241A1A1A1E000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D2D2D3A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000F0F0F101C1C1C200000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000202020260000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000041414170000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003535354A1A1A1A1E0000
      00000000000000000000000000000000000000000000000000000F0F0F101212
      12140101010200000000000000000000000000000000000000003B3B3B5A0000
      0000000000000000000000000000000000000000000000000000030303041C1C
      1C20333333461717171A00000000000000000000000000000000333333460707
      07080000000000000000000000000000000000000000030303041C1C1C202727
      27301C1C1C200000000000000000000000000000000040404095070707080000
      00000000000000000000000000000000000000000000010101021A1A1A1E3232
      3244414141702E2E2E3C000000000000000000000000272727303D3D3D640000
      0000000000000000000000000000000000000000000000000000000000001A1A
      1A1E3C3C3C5E4141418F1D1D1D22000000000000000000000000424242890000
      0000000000000000000000000000000000000000000000000000000000000000
      00000303030440404070424242830101010200000000000000003636364E2525
      252E0000000000000000000000000000000000000000070707080D0D0D0E1E1E
      1E243F3F3F6C363636BB292929340000000000000000373737B5121212140000
      0000000000000000000010101012000000000000000000000000000000000000
      0000070707084141418B3F3F3F970101010200000000393939544040406E0000
      0000000000000000000000000000010101020000000000000000000000000000
      0000000000001818181C2C2C2CCD1C1C1C200000000005050506383838B70000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000414141874040406E0000000000000000404040892323
      232A000000000000000000000000000000000000000000000000000000000000
      0000000000001C1C1C20232323DB2323232A14141416242424DB010101020000
      0000000000001414141621212128000000000000000000000000000000000000
      000000000000000000003C3C3CA93E3E3E6600000000323232C1292929340000
      000000000000000000001D1D1D22000000000000000000000000000000000000
      00000000000000000000393939563A3A3AAB0000000040404070404040870000
      0000000000000000000000000000070707080000000000000000000000000000
      00000000000000000000101010121F1F1FE1030303041E1E1E242C2C2CCF0101
      0102000000000000000000000000000000000000000000000000000000000000
      00000000000000000000393939542C2C2CCB3E3E3E9D41414181000000000000
      0000121212144242427C00000000000000000000000000000000000000000000
      000000000000000000001D1D1D22191919E741414178363636B9000000000000
      0000000000002A2A2A362424242C000000000000000000000000000000000000
      0000000000000000000003030304181818E93C3C3C5E202020DF121212140000
      000000000000000000002D2D2D3A010101020000000000000000000000000F0F
      0F10303030402B2B2B380D0D0D0E2F2F2FC53D3D3D60222222DB333333460000
      00000000000000000000050505060F0F0F100000000000000000000000000101
      0102343434484141418140404095070707F9010101FD4141417A3F3F3F6C3F3F
      3F973A3A3AB31212121400000000000000000000000000000000000000002D2D
      2D3A3E3E3E9B343434BB363636B70D0D0DF3000000FF3F3F3F68272727302F2F
      2F3E414141873E3E3E6600000000000000000000000000000000151515183E3E
      3E9F2B2B2BD51E1E1EE7202020DF0F0F0FF1000000FF3D3D3D60000000000000
      00001717171A424242781717171A000000000000000001010102404040703E3E
      3EA140404091373737B3151515ED0D0D0DF3000000FF3F3F3F6C000000000000
      000000000000151515183636364C000000000000000000000000101010123A3A
      3AB33F3F3F953E3E3E6A41414176010101FD060606F9414141914040407C3232
      32440101010200000000000000000000000000000000000000003E3E3E644141
      41872E2E2E3C2525252E3E3E3E64000000FF0D0D0DF3373737B5363636B73F3F
      3F992B2B2B38000000000000000000000000000000001717171A4242427A1717
      171A00000000000000003B3B3B5A000000FF0F0F0FF1222222DD1F1F1FE72C2C
      2CD33E3E3E9F1717171A0000000000000000000000003535354A151515180000
      000000000000000000003F3F3F68000000FF0D0D0DF3151515ED373737B54040
      40913E3E3EA140404070010101020000000000000000000000004242427A1212
      121400000000000000004141417C3E3E3E9F2E2E2EC739393956000000000000
      000000000000000000000000000000000000000000002424242C2A2A2A360000
      00000000000000000000373737B7414141781A1A1AE51E1E1E24000000000000
      000000000000000000000000000000000000010101022D2D2D3A000000000000
      00000000000010101012202020DF3C3C3C5E181818E905050506000000000000
      0000000000000000000000000000000000000F0F0F1005050506000000000000
      00000000000032323244222222DB3C3C3C5E2D2D2DC90B0B0B0C2A2A2A362E2E
      2E3C0F0F0F100000000000000000000000000000000021212128151515180000
      00000000000000000000262626D91414141621212128242424D91D1D1D220000
      000000000000000000000000000000000000000000001F1F1F24000000000000
      00000000000028282832313131C3010101023E3E3E643B3B3BA9000000000000
      0000000000000000000000000000000000000707070800000000000000000000
      0000000000004040408540404072000000003B3B3BA93A3A3A58000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010101022C2C2CCF1E1E1E2403030304212121DF12121214000000000000
      0000000000000000000000000000000000000000000010101012000000000000
      00000000000012121214373737B5000000000000000029292934363636BB3E3E
      3E6A1D1D1D220B0B0B0C05050506000000000101010200000000000000000000
      0000000000004040406E393939540000000000000000404040954141418B0505
      0506000000000000000000000000000000000000000000000000000000000000
      000000000000383838B705050506000000001A1A1A1E2C2C2CCD1717171A0000
      0000000000000000000000000000000000000000000000000000000000000000
      00002424242C4040408700000000000000003F3F3F6C41414187000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000707070840404095000000000000000000000000000000001D1D
      1D22282828321D1D1D2205050506000000000000000000000000000000000000
      0000000000003E3E3E662525252E0000000000000000000000002E2E2E3C4141
      4170323232441A1A1A1E01010102000000000000000000000000000000000000
      000000000000424242890000000000000000000000001D1D1D22414141913C3C
      3C5E1717171A0000000000000000000000000000000000000000000000000000
      0000272727303636364C000000000000000001010102424242834040406E0303
      0304000000000000000000000000000000000000000000000000000000000000
      0000000000000000000041414170000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C1C1C2033333346000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C3C3C5C00000000000000000000000000000000000000000101
      0102151515180F0F0F1000000000000000000000000000000000000000000000
      00000909090A33333346000000000000000000000000000000001818181C3333
      33461C1C1C200101010200000000000000000000000000000000000000000000
      000000000000000000001C1C1C201D1D1D220000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002D2D2D3A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001D1D1D220D0D0D0E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002020202600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000101010120000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000B0B0B0C030303040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000909090A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000505050600000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FDFFFCFFFE7F0000FCFFFE7FFE7F0000
      FE7BFF7FFF3F0000FF7BFF3BFF3D0000E733833B8139000001230133F03B0000
      7807F803FC230000FC0FF807F0070000F03FE01FE00F0000E01EC01FC43F0000
      C480CC80DC0F0000CCE7DCC19C810000DEFFDCFFBCFF0000DE7FFEFFFCFF0000
      FF3FFE7FFE7F0000FF9FFF3FFE7F0000FFDFFFFFFFFFFBFFEFEFE3EFF1F7F8FF
      E1EFF8EFFCF7FC77F8EFFCE7FE77FE77FCCFFE6FFE67FE77FE0FFE4FC2478067
      C01F800F800F1007801C181E381FFC0F38017818F81CF03FF803F001F001E008
      F07FF27FE243E601F33FE67FE67FEE7FF71FE73FEE7FEE7FF787F71FEF3FEE3F
      F7F7F7CFEF8FFF1FF7FFFFFFFFFFFFDFFF7FFF3FFFBFFFDFFF3FFFBFFF9FFFDF
      FFBFFF9FC7DFC3CF879F839FE1DFF0CF819DF09EF89FFCCFF819FC9DFC9EFC0F
      FC33FC39FC1CE01CE003E003C0318039C007C0078C039C01CC3F9C3F383F3807
      9C1FB83F793FF03FB981798FF91FF33FF9E1F9C1FB87F30FFDFFF9FFFBE3F3C3
      FCFFFDFFF9FFFBFFFEFFFCFFFDFFFBFF00000000000000000000000000000000
      000000000000}
  end
  object ppmCVTabs: TPopupMenu
    Left = 32
    Top = 128
    object mniCloseAllOtherTabs: TMenuItem
      Action = actCloseAllOtherTabs
    end
    object mniCloseTab: TMenuItem
      Action = actCloseTab
    end
  end
end

object TreeViewObjects: TTreeViewObjects
  Left = 0
  Top = 0
  Width = 354
  Height = 467
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 314
    Width = 354
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Tree: TTreeView
    Left = 0
    Top = 0
    Width = 354
    Height = 314
    Align = alClient
    HideSelection = False
    Images = ImageList
    Indent = 19
    PopupMenu = Menu_Objects
    TabOrder = 0
    OnAddition = TreeAddition
    OnChange = TreeChange
    OnDblClick = TreeDblClick
    OnDeletion = TreeDeletion
    OnDragOver = TreeDragOver
    OnEdited = TreeEdited
    OnEditing = TreeEditing
    OnEndDrag = TreeEndDrag
    OnGetImageIndex = TreeGetImageIndex
    OnGetSelectedIndex = TreeGetSelectedIndex
  end
  object Browser: TEmbeddedWB
    Left = 0
    Top = 317
    Width = 354
    Height = 150
    TabStop = False
    Align = alBottom
    TabOrder = 1
    DownloadOptions = [DLCTL_DLIMAGES, DLCTL_VIDEOS, DLCTL_BGSOUNDS]
    UserInterfaceOptions = []
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bP'#225'gina &p de &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    ReplaceCaption = False
    EnableDDE = False
    fpExceptions = True
    ControlData = {
      4C00000096240000810F00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Menu_Objects: TPopupMenu
    OnPopup = Menu_ObjectsPopup
    Left = 33
    Top = 27
  end
  object ImageList: TImageList
    Left = 118
    Top = 28
  end
end

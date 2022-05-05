object FrameCheckListBox: TFrameCheckListBox
  Left = 0
  Top = 0
  Width = 152
  Height = 160
  TabOrder = 0
  object lbItems: TCheckListBox
    Left = 0
    Top = 16
    Width = 152
    Height = 144
    OnClickCheck = lbItemsClickCheck
    Align = alClient
    ItemHeight = 13
    PopupMenu = Menu
    TabOrder = 0
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 152
    Height = 16
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Items:'
    TabOrder = 1
  end
  object Menu: TPopupMenu
    Left = 44
    Top = 44
    object MenuST: TMenuItem
      Caption = 'Selecionar &Todos'
      OnClick = MenuSTClick
    end
    object MenuSN: TMenuItem
      Caption = 'Selecionar &Nenhum'
      OnClick = MenuSNClick
    end
  end
end

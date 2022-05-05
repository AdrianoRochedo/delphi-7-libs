object Frame_moLayer: TFrame_moLayer
  Left = 0
  Top = 0
  Width = 160
  Height = 20
  Color = clBtnFace
  ParentColor = False
  PopupMenu = Menu
  TabOrder = 0
  OnDblClick = FrameDblClick
  object cbVis: TCheckBox
    Left = 13
    Top = 2
    Width = 15
    Height = 17
    TabOrder = 0
    OnClick = cbVisClick
  end
  object Panel1: TPanel
    Left = 31
    Top = 1
    Width = 49
    Height = 19
    BevelWidth = 2
    TabOrder = 1
    object Box: TPaintBox
      Left = 2
      Top = 2
      Width = 45
      Height = 15
      Align = alClient
      OnDblClick = BoxDblClick
      OnPaint = BoxPaint
    end
  end
  object ColorDialog: TColorDialog
    Left = 94
    Top = 65531
  end
  object Menu: TPopupMenu
    OnPopup = MenuPopup
    Left = 123
    object Menu_MostrarRegistros: TMenuItem
      Caption = 'Mostrar Registros ...'
      OnClick = Menu_MostrarRegistrosClick
    end
  end
end

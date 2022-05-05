object MainForm: TMainForm
  Left = 285
  Top = 131
  Width = 631
  Height = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Dock_Left: TTBXDock
    Left = 0
    Top = 9
    Width = 25
    Height = 349
    Position = dpLeft
    object Toolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Toolbar'
      CloseButton = False
      TabOrder = 0
      object btnSelect: TTBXItem
        AutoCheck = True
        Caption = 'Selecionar'
        Checked = True
        GroupIndex = 1
      end
      object btnDrag: TTBXItem
        AutoCheck = True
        Caption = 'Arrastar'
        GroupIndex = 1
      end
      object btnConnect: TTBXItem
        AutoCheck = True
        Caption = 'Conectar'
        GroupIndex = 1
      end
      object btnPC: TTBXItem
        AutoCheck = True
        Caption = 'PC'
        GroupIndex = 1
      end
    end
  end
  object Dock_Top: TTBXDock
    Left = 0
    Top = 0
    Width = 623
    Height = 9
  end
  object MainMenu: TMainMenu
    Left = 550
    Top = 14
    object Menu_Arquivo: TMenuItem
      Caption = 'Arquivo'
      object Menu_Novo: TMenuItem
        Action = actNovo
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
    object Menu_Save: TMenuItem
      Caption = 'Save'
      OnClick = Menu_SaveClick
    end
    object Manu_Load: TMenuItem
      Caption = 'Load'
      OnClick = Manu_LoadClick
    end
  end
  object ActionList: TActionList
    Left = 494
    Top = 14
    object actNovo: TAction
      Category = 'Arquivo'
      Caption = '&Novo'
      OnExecute = actNovoExecute
    end
  end
end

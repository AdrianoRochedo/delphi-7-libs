object SpreadSheetFrame: TSpreadSheetFrame
  Left = 0
  Top = 0
  Width = 503
  Height = 240
  TabOrder = 0
  object SS: TcxSpreadSheet
    Left = 0
    Top = 0
    Width = 503
    Height = 240
    Align = alClient
    DefaultStyle.Font.Name = 'MS Sans Serif'
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'MS Sans Serif'
    HeaderFont.Style = []
  end
  object Menu: TPopupMenu
    Left = 100
    Top = 35
    object Menu_Copiar: TMenuItem
      Caption = '&Copiar'
      OnClick = Menu_CopiarClick
    end
    object Menu_Colar: TMenuItem
      Caption = 'C&olar'
      OnClick = Menu_ColarClick
    end
    object Menu_Recortar: TMenuItem
      Caption = '&Recortar'
      OnClick = Menu_RecortarClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Menu_Abrir: TMenuItem
      Caption = '&Abrir ...'
      OnClick = Menu_AbrirClick
    end
    object Menu_Salvar: TMenuItem
      Caption = '&Salvar ...'
      OnClick = Menu_SalvarClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Menu_FormatCells: TMenuItem
      Caption = 'Formatar C'#233'lulas ...'
      OnClick = Menu_FormatCellsClick
    end
  end
  object Save: TSaveDialog
    DefaultExt = 'XLS'
    Filter = 'Arquivos Excel (*.XLS)|*.XLS'
    Title = 'Salvamento'
    Left = 129
    Top = 36
  end
  object Load: TOpenDialog
    DefaultExt = 'XLS'
    Filter = 'Arquivos Excel (*.XLS)|*.XLS'
    Title = 'Leitura'
    Left = 158
    Top = 36
  end
end

object SpreadSheetBookFrame: TSpreadSheetBookFrame
  Left = 0
  Top = 0
  Width = 486
  Height = 236
  TabOrder = 0
  object SS: TcxSpreadSheetBook
    Left = 0
    Top = 0
    Width = 486
    Height = 236
    Align = alClient
    DefaultStyle.Font.Name = 'MS Sans Serif'
    DefaultColWidth = 75
    DefaultRowHeight = 18
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'MS Sans Serif'
    HeaderFont.Style = []
    PopupMenu = Menu
    OnActiveSheetChanging = SSActiveSheetChanging
  end
  object Menu: TPopupMenu
    Left = 176
    Top = 42
    object Menu_Arquivo: TMenuItem
      Caption = 'Arquivo'
      object Menu_Abrir: TMenuItem
        Caption = '&Abrir ...'
        OnClick = Menu_AbrirClick
      end
      object Menu_Salvar: TMenuItem
        Caption = '&Salvar ...'
        OnClick = Menu_SalvarClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Menu_AjustarTexto: TMenuItem
      Caption = 'Alinhamento do texto'
      object Menu_Esquerda: TMenuItem
        Caption = 'Esquerda'
        OnClick = Menu_EsquerdaClick
      end
      object Menu_Centro: TMenuItem
        Caption = 'Centro'
        OnClick = Menu_CentroClick
      end
      object Menu_Direita: TMenuItem
        Caption = 'Direita'
        OnClick = Menu_DireitaClick
      end
    end
    object Menu_Estilo: TMenuItem
      Caption = 'Estilo do texto'
      object Menu_Negrito: TMenuItem
        Caption = 'Negrito'
        OnClick = Menu_NegritoClick
      end
      object Menu_Italico: TMenuItem
        Caption = 'It'#225'lico'
        OnClick = Menu_ItalicoClick
      end
      object Menu_Sublinhado: TMenuItem
        Caption = 'Sublinhado'
        OnClick = Menu_SublinhadoClick
      end
      object Menu_NenhumEstilo: TMenuItem
        Caption = 'Nenhum'
        OnClick = Menu_NenhumEstiloClick
      end
    end
    object Menu_FormatCells: TMenuItem
      Caption = 'Formatar C'#233'lulas ...'
      OnClick = Menu_FormatCellsClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Menu_Editar: TMenuItem
      Caption = 'Editar'
      object Menu_Copiar: TMenuItem
        Caption = '&Copiar Sele'#231#227'o'
        OnClick = Menu_CopiarClick
      end
      object Menu_CopiarTudo: TMenuItem
        Caption = 'Copiar T&udo'
        OnClick = Menu_CopiarTudoClick
      end
      object Menu_Recortar: TMenuItem
        Caption = '&Recortar'
        OnClick = Menu_RecortarClick
      end
      object Menu_Colar: TMenuItem
        Caption = 'C&olar'
        OnClick = Menu_ColarClick
      end
    end
    object Menu_Transpor: TMenuItem
      Caption = 'Transpor Linhas e Colunas'
      OnClick = Menu_TransporClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Menu_Destruir: TMenuItem
      Caption = 'Destruir Planilha ao Fechar'
      Checked = True
      OnClick = Menu_DestruirClick
    end
    object Menu_SempreNoTopo: TMenuItem
      Caption = 'Mostrar Sempre no Topo'
      OnClick = Menu_SempreNoTopoClick
    end
  end
  object Save: TSaveDialog
    DefaultExt = 'XLS'
    Filter = 'Arquivos Excel (*.XLS)|*.XLS'
    Title = 'Salvamento'
    Left = 175
    Top = 99
  end
  object Load: TOpenDialog
    DefaultExt = 'XLS'
    Filter = 'Arquivos Excel (*.XLS)|*.XLS'
    Title = 'Leitura'
    Left = 176
    Top = 156
  end
end

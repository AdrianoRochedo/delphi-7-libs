inherited grDialogo_OpcoesSeriePontos: TgrDialogo_OpcoesSeriePontos
  Left = 190
  Top = 159
  Caption = ' Op'#231#245'es '
  ClientHeight = 227
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited GroupBox1: TGroupBox
    Top = 8
  end
  inherited btnOk: TBitBtn
    Top = 195
  end
  object GroupBox2: TGroupBox [2]
    Left = 8
    Top = 129
    Width = 431
    Height = 57
    Caption = ' Pontos '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label3: TLabel
      Left = 12
      Top = 26
      Width = 39
      Height = 13
      Caption = 'Largura:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 105
      Top = 26
      Width = 30
      Height = 13
      Caption = 'Altura:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 193
      Top = 11
      Width = 50
      Height = 13
      Caption = 'Formato:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object se_Largura: TSpinEdit
      Left = 55
      Top = 21
      Width = 41
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 1
      MinValue = 1
      ParentFont = False
      TabOrder = 0
      Value = 4
      OnChange = se_LarguraChange
    end
    object se_Altura: TSpinEdit
      Left = 139
      Top = 21
      Width = 41
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxValue = 1
      MinValue = 1
      ParentFont = False
      TabOrder = 1
      Value = 4
      OnChange = se_AlturaChange
    end
    object cb_Formato: TComboBox
      Left = 193
      Top = 25
      Width = 224
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 2
      Text = 'cb_Formato'
      OnChange = cb_FormatoChange
      Items.Strings = (
        'Quadrado'
        'C'#237'rculo'
        'Tri'#226'ngulo para Cima'
        'Tri'#226'ngulo para Baixo'
        'Cruz'
        'X'
        'Aster'#237'sco'
        'Diamante'
        'Ponto pequeno')
    end
  end
  inherited ColorDLG: TColorDialog
    Left = 388
    Top = 61
  end
end

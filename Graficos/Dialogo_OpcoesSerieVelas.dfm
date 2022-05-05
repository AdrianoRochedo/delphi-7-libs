inherited grDialogo_OpcoesSerieVelas: TgrDialogo_OpcoesSerieVelas
  Caption = ' Op'#231#245'es '
  ClientHeight = 223
  ClientWidth = 445
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel [0]
    Left = 368
    Top = 143
    Width = 41
    Height = 13
    Caption = 'Largura:'
  end
  inherited GroupBox1: TGroupBox
    Top = 8
  end
  inherited btnOk: TBitBtn
    Top = 191
  end
  object se_Largura: TSpinEdit [3]
    Left = 368
    Top = 157
    Width = 70
    Height = 24
    MaxValue = 1
    MinValue = 1
    TabOrder = 2
    Value = 4
    OnChange = se_LarguraChange
  end
  object GroupBox2: TGroupBox [4]
    Left = 8
    Top = 129
    Width = 350
    Height = 53
    Caption = ' Contorno '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Label2: TLabel
      Left = 11
      Top = 24
      Width = 52
      Height = 13
      Caption = 'Espessura:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 117
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Estilo:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object se_Espessura: TSpinEdit
      Left = 67
      Top = 19
      Width = 41
      Height = 22
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
      OnChange = se_EspessuraChange
    end
    object cb_Estilo: TComboBox
      Left = 149
      Top = 19
      Width = 187
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
      OnChange = cb_EstiloChange
      Items.Strings = (
        'S'#243'lido'
        'Tracejado'
        'Pontos'
        'Tra'#231'o-Ponto'
        'Tra'#231'o-Ponto-Ponto'
        'Nenhum')
    end
  end
  inherited ColorDLG: TColorDialog
    Left = 389
    Top = 60
  end
end

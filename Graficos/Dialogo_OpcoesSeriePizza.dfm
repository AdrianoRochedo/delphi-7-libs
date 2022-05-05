inherited grDialogo_OpcoesSeriePizza: TgrDialogo_OpcoesSeriePizza
  Left = 257
  Top = 134
  Caption = ' Op'#231#245'es '
  ClientHeight = 259
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel [0]
    Left = 216
    Top = 147
    Width = 64
    Height = 13
    Caption = 'Afastar maior:'
  end
  object Label5: TLabel [1]
    Left = 255
    Top = 173
    Width = 25
    Height = 13
    Caption = 'Girar:'
  end
  inherited GroupBox1: TGroupBox
    Top = 8
  end
  inherited btnOk: TBitBtn
    Top = 227
  end
  object GroupBox2: TGroupBox [4]
    Left = 8
    Top = 129
    Width = 193
    Height = 89
    Caption = ' Borda '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 42
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
    object Label3: TLabel
      Left = 80
      Top = 20
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
    object cb_Visivel: TCheckBox
      Left = 13
      Top = 19
      Width = 57
      Height = 17
      Caption = 'Vis'#237'vel'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
      OnClick = cb_VisivelClick
    end
    object cb_Estilo: TComboBox
      Left = 8
      Top = 56
      Width = 105
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
        'Tra'#231'o'
        'Ponto'
        'Tra'#231'o-Ponto'
        'Tra'#231'o-Ponto-Ponto'
        'Nenhum')
    end
    object se_Largura: TSpinEdit
      Left = 136
      Top = 16
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
      TabOrder = 2
      Value = 1
      OnChange = se_LarguraChange
    end
    object btn_Cor: TBitBtn
      Left = 121
      Top = 56
      Width = 56
      Height = 21
      Caption = 'Cor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnCorClick
      Margin = 4
    end
    object PainelCor2: TPanel
      Left = 150
      Top = 59
      Width = 23
      Height = 15
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnCorClick
    end
  end
  object se_AfastarMaior: TSpinEdit [5]
    Left = 288
    Top = 142
    Width = 41
    Height = 25
    Increment = 5
    MaxValue = 100
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = se_AfastarMaiorChange
  end
  object cb_Padrao: TCheckBox [6]
    Left = 344
    Top = 144
    Width = 65
    Height = 17
    Caption = 'Padr'#227'o'
    TabOrder = 4
    OnClick = cb_PadraoClick
  end
  object se_Girar: TSpinEdit [7]
    Left = 288
    Top = 168
    Width = 41
    Height = 25
    Increment = 5
    MaxValue = 100
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = se_GirarChange
  end
  inherited ColorDLG: TColorDialog
    Left = 394
  end
end

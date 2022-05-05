object frParManager: TfrParManager
  Left = 0
  Top = 0
  Width = 427
  Height = 18
  TabOrder = 0
  object paName: TPanel
    Left = 0
    Top = 0
    Width = 134
    Height = 18
    Align = alLeft
    Alignment = taLeftJustify
    Caption = ' Nome'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnDblClick = paNameDblClick
  end
  object paValue: TPanel
    Left = 291
    Top = 0
    Width = 73
    Height = 18
    Hint = ' Valor atual '
    Align = alRight
    Caption = 'Valor'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
  end
  object paMarca: TPanel
    Left = 404
    Top = 0
    Width = 23
    Height = 18
    Hint = ' Status da Fun'#231#227'o Objetivo '
    Align = alRight
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
  end
  object Panel4: TPanel
    Left = 134
    Top = 0
    Width = 157
    Height = 18
    Align = alClient
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    DesignSize = (
      157
      18)
    object Gauge: TGauge
      Left = 5
      Top = 6
      Width = 148
      Height = 7
      Anchors = [akLeft, akTop, akRight]
      BackColor = clSilver
      BorderStyle = bsNone
      ForeColor = clBlue
      ParentShowHint = False
      Progress = 70
      ShowHint = True
      ShowText = False
    end
  end
  object paTP: TPanel
    Left = 364
    Top = 0
    Width = 40
    Height = 18
    Hint = ' Tamanho do Passo '
    Align = alRight
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 4
  end
end

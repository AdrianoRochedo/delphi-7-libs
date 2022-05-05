inherited foParsManager_Genetic: TfoParsManager_Genetic
  Left = 245
  Top = 104
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Height = 22
    inherited paSim: TPanel
      Height = 20
    end
    inherited Panel5: TPanel
      Height = 20
      Alignment = taRightJustify
      Caption = 'Simula'#231#227'o:  '
    end
    object Panel3: TPanel
      Left = 123
      Top = 1
      Width = 132
      Height = 20
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Complexo Atual:  '
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object paCA: TPanel
      Left = 255
      Top = 1
      Width = 166
      Height = 20
      Align = alClient
      Alignment = taLeftJustify
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  inherited paFrames: TScrollBox
    Top = 61
    Height = 52
  end
  inherited Panel4: TPanel
    Top = 44
  end
  object Panel2: TPanel
    Left = 0
    Top = 22
    Width = 422
    Height = 22
    Align = alTop
    Color = clMedGray
    TabOrder = 3
    object paECA: TPanel
      Left = 255
      Top = 1
      Width = 166
      Height = 20
      Align = alClient
      Alignment = taLeftJustify
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Panel12: TPanel
      Left = 123
      Top = 1
      Width = 132
      Height = 20
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Evolu'#231#227'o do Complexo:  '
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object paE: TPanel
      Left = 71
      Top = 1
      Width = 52
      Height = 20
      Align = alLeft
      Alignment = taLeftJustify
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object Panel14: TPanel
      Left = 1
      Top = 1
      Width = 70
      Height = 20
      Align = alLeft
      Alignment = taRightJustify
      Caption = ' Gera'#231#227'o:  '
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
end

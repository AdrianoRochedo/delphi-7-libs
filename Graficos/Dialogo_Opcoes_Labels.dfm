object foDialogo_Opcoes_Labels: TfoDialogo_Opcoes_Labels
  Left = 252
  Top = 152
  Width = 448
  Height = 302
  Caption = ' T'#237'tulos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 244
    Width = 440
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 1
      Top = 4
      Width = 86
      Height = 25
      Caption = 'Ok'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 91
      Top = 4
      Width = 86
      Height = 25
      Caption = 'Cancelar'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Labels: TMemo
    Left = 0
    Top = 0
    Width = 440
    Height = 244
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
end

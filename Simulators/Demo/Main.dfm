object foMain: TfoMain
  Left = 0
  Top = 5
  Width = 272
  Height = 277
  Caption = ' Main'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnSel: TSpeedButton
    Left = 11
    Top = 11
    Width = 80
    Height = 23
    GroupIndex = 1
    Down = True
    Caption = 'Selecionar'
  end
  object btnDrag: TSpeedButton
    Left = 11
    Top = 37
    Width = 80
    Height = 23
    GroupIndex = 1
    Caption = 'Arrastar'
  end
  object btnNC: TSpeedButton
    Left = 11
    Top = 63
    Width = 80
    Height = 23
    GroupIndex = 1
    Caption = 'Novo Comp.'
  end
  object btnConnect: TSpeedButton
    Left = 97
    Top = 12
    Width = 80
    Height = 23
    GroupIndex = 1
    Caption = 'Conectar'
  end
  object Memo: TMemo
    Left = 10
    Top = 90
    Width = 242
    Height = 146
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
end

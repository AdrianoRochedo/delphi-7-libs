object Form_moCoordinateSystem: TForm_moCoordinateSystem
  Left = 245
  Top = 155
  BorderStyle = bsDialog
  Caption = ' Escolha um Sistema de Coordenadas'
  ClientHeight = 102
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object rbGeo: TRadioButton
    Left = 10
    Top = 7
    Width = 218
    Height = 17
    Caption = 'Sistema de Coordenadas Geogr'#225'ficas'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = Radio_Click
  end
  object rbProj: TRadioButton
    Left = 10
    Top = 55
    Width = 218
    Height = 17
    Caption = 'Sistema de Coordenadas Projetadas'
    TabOrder = 1
    OnClick = Radio_Click
  end
  object cbGeo: TComboBox
    Left = 29
    Top = 23
    Width = 301
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object cbProj: TComboBox
    Left = 29
    Top = 71
    Width = 301
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
  end
  object Button1: TButton
    Left = 343
    Top = 25
    Width = 114
    Height = 25
    Caption = '&Ok'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 343
    Top = 53
    Width = 114
    Height = 25
    Caption = '&Cancelar'
    ModalResult = 2
    TabOrder = 5
  end
end

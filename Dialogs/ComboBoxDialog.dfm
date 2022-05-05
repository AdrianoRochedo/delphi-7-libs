object ComboBoxDialog: TComboBoxDialog
  Left = 265
  Top = 270
  Width = 272
  Height = 100
  BorderStyle = bsSizeToolWin
  Caption = ' Selecione um item'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 7
    Top = 37
    Width = 122
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 133
    Top = 37
    Width = 122
    Height = 25
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 1
  end
  object ComboBox: TComboBox
    Left = 7
    Top = 9
    Width = 250
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
end

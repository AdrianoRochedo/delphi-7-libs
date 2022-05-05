object MonthDialog: TMonthDialog
  Left = 248
  Top = 142
  BorderStyle = bsDialog
  Caption = ' Selecione um m'#234's'
  ClientHeight = 76
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 15
    Width = 23
    Height = 13
    Caption = 'M'#234's:'
  end
  object cbMonth: TComboBox
    Left = 11
    Top = 29
    Width = 145
    Height = 21
    Style = csDropDownList
    DropDownCount = 12
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 169
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 169
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 2
  end
end

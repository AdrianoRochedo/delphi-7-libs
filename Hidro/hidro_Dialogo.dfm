object HidroDialogo: THidroDialogo
  Left = 219
  Top = 143
  ActiveControl = edNome
  BorderStyle = bsDialog
  ClientHeight = 163
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edNome: TEdit
    Left = 5
    Top = 29
    Width = 364
    Height = 21
    MaxLength = 20
    TabOrder = 0
  end
  object P1: TPanel
    Left = 5
    Top = 8
    Width = 364
    Height = 21
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = ' Nome:'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 5
    Top = 50
    Width = 364
    Height = 21
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = ' Coment'#225'rios:'
    TabOrder = 2
  end
  object mComentarios: TMemo
    Left = 4
    Top = 71
    Width = 365
    Height = 52
    TabOrder = 3
  end
  object btnOk: TBitBtn
    Left = 4
    Top = 132
    Width = 89
    Height = 25
    Caption = '&Ok'
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancelar: TBitBtn
    Left = 96
    Top = 132
    Width = 89
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 5
    OnClick = btnCancelarClick
  end
end

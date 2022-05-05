object Form_moLayerProps: TForm_moLayerProps
  Left = 343
  Top = 199
  Width = 239
  Height = 132
  Caption = ' Propriedades'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 42
    Top = 27
    Width = 72
    Height = 13
    Caption = 'Escala m'#237'nima:'
  end
  object Label3: TLabel
    Left = 132
    Top = 27
    Width = 73
    Height = 13
    Caption = 'Escala m'#225'xima:'
  end
  object cbVC: TCheckBox
    Left = 4
    Top = 8
    Width = 219
    Height = 17
    Caption = 'Utilizar visibilidade condicionada a escala'
    TabOrder = 0
  end
  object edEMin: TdrEdit
    Left = 42
    Top = 41
    Width = 86
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 1
    Text = '0'
  end
  object edEMax: TdrEdit
    Left = 132
    Top = 41
    Width = 86
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 2
    Text = '0'
  end
  object btnOk: TBitBtn
    Left = 22
    Top = 73
    Width = 89
    Height = 23
    Caption = '&Ok'
    TabOrder = 3
    OnClick = btnOkClick
  end
  object btnCancelar: TBitBtn
    Left = 114
    Top = 73
    Width = 89
    Height = 23
    Caption = '&Cancelar'
    TabOrder = 4
    OnClick = btnCancelarClick
  end
end

object CellPosDialog: TCellPosDialog
  Left = 242
  Top = 202
  BorderStyle = bsDialog
  Caption = ' Entre com a posi'#231#227'o de uma c'#233'lula'
  ClientHeight = 161
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    343
    161)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 79
    Top = 63
    Width = 29
    Height = 13
    Caption = 'Linha:'
  end
  object Label3: TLabel
    Left = 174
    Top = 63
    Width = 36
    Height = 13
    Caption = 'Coluna:'
  end
  object Bevel1: TBevel
    Left = 10
    Top = 107
    Width = 319
    Height = 8
    Shape = bsBottomLine
  end
  object Button1: TButton
    Left = 10
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akTop]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 90
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akTop]
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 9
    Top = 11
    Width = 324
    Height = 41
    Color = 13100787
    TabOrder = 0
    object laMSG: TLabel
      Left = 11
      Top = 6
      Width = 301
      Height = 27
      AutoSize = False
      Caption = 'MSG'
      WordWrap = True
    end
  end
  object edL: TdrEdit
    Left = 79
    Top = 77
    Width = 89
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 1
    Text = '1'
  end
  object edC: TdrEdit
    Left = 173
    Top = 77
    Width = 89
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 2
    Text = '1'
  end
end

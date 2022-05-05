object foStringsEditor: TfoStringsEditor
  Left = 293
  Top = 131
  Width = 350
  Height = 307
  Caption = ' Editor de Strings'
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
    Top = 247
    Width = 342
    Height = 33
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TButton
      Left = 4
      Top = 4
      Width = 114
      Height = 25
      Caption = 'Ok'
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 121
      Top = 4
      Width = 114
      Height = 25
      Caption = 'Cancelar'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 342
    Height = 247
    Align = alClient
    TabOrder = 1
  end
end

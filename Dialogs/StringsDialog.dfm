object StringsDialog: TStringsDialog
  Left = 242
  Top = 202
  Width = 482
  Height = 253
  BorderStyle = bsSizeToolWin
  Caption = ' Entre com o texto'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    474
    224)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 7
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 87
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 1
  end
  inline fr_Memo: Tfr_Memo
    Left = 7
    Top = 8
    Width = 458
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    inherited Memo: TMemo
      Width = 458
      Height = 177
      ReadOnly = False
      ScrollBars = ssNone
      WordWrap = True
    end
  end
end

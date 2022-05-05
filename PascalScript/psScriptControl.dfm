object foScriptControl: TfoScriptControl
  Left = 317
  Top = 228
  Width = 421
  Height = 139
  BorderIcons = []
  Caption = ' Script Control'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    413
    110)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 7
    Width = 34
    Height = 13
    Caption = 'Nome: '
  end
  object laName: TLabel
    Left = 43
    Top = 7
    Width = 26
    Height = 13
    Caption = 'Label'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Bevel: TBevel
    Left = 7
    Top = 26
    Width = 398
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 7
    Top = 67
    Width = 50
    Height = 13
    Caption = 'Progresso:'
  end
  object laPBMes: TLabel
    Left = 63
    Top = 67
    Width = 26
    Height = 13
    Caption = 'Label'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnExec: TBitBtn
    Left = 7
    Top = 35
    Width = 92
    Height = 25
    Caption = 'Executar'
    TabOrder = 0
    OnClick = btnExecClick
  end
  object btnStop: TBitBtn
    Left = 104
    Top = 35
    Width = 92
    Height = 25
    Caption = 'Parar'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object PB: TProgressBar
    Left = 7
    Top = 83
    Width = 398
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
end

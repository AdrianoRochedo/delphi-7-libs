object foParsManager: TfoParsManager
  Left = 227
  Top = 275
  Width = 430
  Height = 141
  BorderIcons = [biSystemMenu]
  Caption = ' Par'#226'metros e Dados Gerais'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clYellow
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 422
    Height = 21
    Align = alTop
    Color = clMedGray
    TabOrder = 0
    object paSim: TPanel
      Left = 71
      Top = 1
      Width = 52
      Height = 19
      Align = alLeft
      Alignment = taLeftJustify
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 70
      Height = 19
      Align = alLeft
      Caption = 'Simula'#231#227'o:'
      Color = clBackground
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object paFrames: TScrollBox
    Left = 0
    Top = 38
    Width = 422
    Height = 75
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
  end
  object Panel4: TPanel
    Left = 0
    Top = 21
    Width = 422
    Height = 17
    Align = alTop
    Alignment = taLeftJustify
    BevelInner = bvRaised
    BevelOuter = bvNone
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Panel6: TPanel
      Left = 397
      Top = 1
      Width = 24
      Height = 15
      Align = alRight
      BevelOuter = bvNone
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Panel7: TPanel
      Left = 1
      Top = 1
      Width = 135
      Height = 15
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Nome'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object Panel8: TPanel
      Left = 357
      Top = 1
      Width = 40
      Height = 15
      Align = alRight
      BevelOuter = bvNone
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object Panel9: TPanel
      Left = 285
      Top = 1
      Width = 72
      Height = 15
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Valor'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object Panel10: TPanel
      Left = 136
      Top = 1
      Width = 149
      Height = 15
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Lim.Inf - Valor - Lim.Sup.'
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
end

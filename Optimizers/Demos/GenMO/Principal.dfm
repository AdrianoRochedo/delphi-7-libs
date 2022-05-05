object foPrincipal: TfoPrincipal
  Left = 1
  Top = -1
  Width = 793
  Height = 471
  Caption = ' Demo - Otimizador Gen'#233'tico Multi-Objetivo - Adriano Rochedo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    785
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 4
    Width = 54
    Height = 13
    Caption = 'Popula'#231#227'o:'
  end
  object Label2: TLabel
    Left = 94
    Top = 4
    Width = 132
    Height = 13
    Caption = 'Num. M'#225'ximo de Evolu'#231#245'es'
  end
  object Label4: TLabel
    Left = 5
    Top = 44
    Width = 111
    Height = 13
    Caption = 'Par'#226'metros (Min - Max):'
  end
  object Label5: TLabel
    Left = 5
    Top = 104
    Width = 146
    Height = 13
    Caption = 'Popula'#231#227'o em cada Evolu'#231#227'o:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 396
    Top = 44
    Width = 124
    Height = 13
    Caption = 'Evolu'#231#227'o dos Par'#226'metros:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 269
    Top = 4
    Width = 35
    Height = 13
    Caption = 'Escala:'
  end
  object Label7: TLabel
    Left = 323
    Top = 4
    Width = 57
    Height = 13
    Caption = 'Simula'#231#245'es:'
  end
  object Saida: TMemo
    Left = 5
    Top = 119
    Width = 385
    Height = 319
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 6
    WordWrap = False
  end
  object edPop: TdrEdit
    Left = 5
    Top = 18
    Width = 87
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 0
    Text = '150'
  end
  object edNME: TdrEdit
    Left = 94
    Top = 18
    Width = 173
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 1
    Text = '2000'
  end
  object mmPars: TMemo
    Left = 5
    Top = 59
    Width = 385
    Height = 38
    Lines.Strings = (
      '-3 | 10'
      '-3 | 10')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnTestar: TButton
    Left = 605
    Top = 5
    Width = 115
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Testar Otimizador'
    TabOrder = 3
    OnClick = btnTestarClick
  end
  object btnPars: TButton
    Left = 605
    Top = 29
    Width = 115
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Mostrar Par'#226'metros'
    TabOrder = 4
    OnClick = btnParsClick
  end
  object btnParar: TButton
    Left = 719
    Top = 5
    Width = 60
    Height = 49
    Anchors = [akTop, akRight]
    Caption = 'Parar'
    TabOrder = 5
    OnClick = btnPararClick
  end
  object P: TExtPanel
    Left = 396
    Top = 59
    Width = 383
    Height = 378
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBackground
    TabOrder = 7
    AcceptFiles = False
    HorzRedraw = False
    VertRedraw = False
    OnPaint = PPaint
  end
  object edEscala: TdrEdit
    Left = 269
    Top = 18
    Width = 52
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 8
    Text = '40'
  end
  object edSim: TdrEdit
    Left = 323
    Top = 18
    Width = 66
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    Color = clMenu
    ReadOnly = True
    TabOrder = 9
    Text = '0'
  end
end

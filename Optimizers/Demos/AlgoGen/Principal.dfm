object foPrincipal: TfoPrincipal
  Left = 54
  Top = 1
  BorderStyle = bsDialog
  Caption = ' Demo - Otimizador Gen'#233'tico'
  ClientHeight = 360
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 4
    Width = 82
    Height = 13
    Caption = 'Num. Complexos:'
  end
  object Label2: TLabel
    Left = 95
    Top = 4
    Width = 170
    Height = 13
    Caption = 'Num. de Evolucoes dos Complexos:'
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
    Top = 135
    Width = 56
    Height = 13
    Caption = 'Resultados:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 234
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
  object Saida: TMemo
    Left = 5
    Top = 150
    Width = 224
    Height = 205
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object c1: TChart
    Left = 234
    Top = 59
    Width = 461
    Height = 296
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Maximum = 75.000000000000000000
    LeftAxis.Minimum = 5.000000000000000000
    View3D = False
    View3DWalls = False
    TabOrder = 7
  end
  object edNC: TdrEdit
    Left = 5
    Top = 18
    Width = 87
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 0
    Text = '3'
  end
  object edNEC: TdrEdit
    Left = 95
    Top = 18
    Width = 173
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 1
    Text = '25'
  end
  object mmPars: TMemo
    Left = 5
    Top = 59
    Width = 224
    Height = 72
    Lines.Strings = (
      '10 - 20'
      '20 - 40'
      '40 - 80')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnTestar: TButton
    Left = 521
    Top = 5
    Width = 115
    Height = 25
    Caption = 'Testar Otimizador'
    TabOrder = 3
    OnClick = btnTestarClick
  end
  object btnPars: TButton
    Left = 521
    Top = 29
    Width = 115
    Height = 25
    Caption = 'Mostrar Par'#226'metros'
    TabOrder = 4
    OnClick = btnParsClick
  end
  object btnParar: TButton
    Left = 635
    Top = 5
    Width = 60
    Height = 49
    Caption = 'Parar'
    TabOrder = 5
    OnClick = btnPararClick
  end
end

inherited HidroDialogo_Projeto: THidroDialogo_Projeto
  Left = 182
  Top = 33
  Caption = ' Dados do Projeto'
  ClientHeight = 205
  ClientWidth = 453
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton [0]
    Left = 424
    Top = 122
    Width = 24
    Height = 23
    Caption = '...'
    OnClick = btnProcurarClick
  end
  object SpeedButton2: TSpeedButton [1]
    Tag = 1
    Left = 424
    Top = 145
    Width = 24
    Height = 22
    Caption = '...'
    OnClick = btnProcurarClick
  end
  inherited edNome: TEdit
    Width = 443
    AutoSize = False
  end
  inherited P1: TPanel
    Width = 443
    TabOrder = 6
  end
  inherited Panel2: TPanel
    Width = 443
    TabOrder = 7
  end
  inherited mComentarios: TMemo
    Width = 444
    TabOrder = 1
  end
  inherited btnOk: TBitBtn
    Top = 174
  end
  inherited btnCancelar: TBitBtn
    Top = 174
  end
  object Panel7: TPanel
    Left = 5
    Top = 123
    Width = 163
    Height = 21
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = ' Diret'#243'rio de Sa'#237'da:'
    TabOrder = 8
  end
  object edDirSai: TEdit
    Left = 168
    Top = 123
    Width = 257
    Height = 21
    Hint = 
      ' Dir. de sa'#237'da dos arquivos gerados durante o processamento. '#13#10' ' +
      'Se vazio, o sistema utiliza o diret'#243'rio do projeto.'
    AutoSize = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object Panel8: TPanel
    Left = 5
    Top = 144
    Width = 163
    Height = 21
    Alignment = taLeftJustify
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = ' Diret'#243'rio de Trabalho:'
    TabOrder = 9
  end
  object edDirPes: TEdit
    Left = 168
    Top = 144
    Width = 257
    Height = 21
    Hint = 
      ' Dir. de pesquisa dos arquivos de dados. '#13#10' Se vazio, o sistema ' +
      'procurar'#225' no diret'#243'rio do projeto.'
    AutoSize = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
end

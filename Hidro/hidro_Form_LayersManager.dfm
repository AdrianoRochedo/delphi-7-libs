object HidroForm_LayersManager: THidroForm_LayersManager
  Left = 386
  Top = 0
  BorderStyle = bsToolWindow
  Caption = ' Gerenciador de Camadas'
  ClientHeight = 142
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 43
    Top = 101
    Width = 72
    Height = 13
    Caption = 'Escala m'#237'nima:'
  end
  object Label3: TLabel
    Left = 133
    Top = 101
    Width = 73
    Height = 13
    Caption = 'Escala m'#225'xima:'
  end
  inline frPL: TfrPL
    Left = 4
    Top = 6
    Width = 161
    Height = 72
    TabOrder = 0
    inherited L_Text: TLabel
      Width = 47
      Caption = 'Camadas:'
    end
    inherited btnUP: TSpeedButton
      Left = 133
      Top = 14
      Hint = ''
      OnClick = frPLbtnUPClick
    end
    inherited btnDown: TSpeedButton
      Left = 133
      Top = 44
      Hint = ''
    end
    inherited lbList: TListBox
      Width = 127
      Height = 57
      Hint = 
        ' Utilize o bot'#227'o de fun'#231#227'o do mouse para '#13#10'  visualizar as a'#231#245'es' +
        ' sobre uma camada'
      ParentShowHint = False
      PopupMenu = Menu
      ShowHint = True
      OnClick = Camadas_Click
    end
  end
  object gbProps: TGroupBox
    Left = 168
    Top = 14
    Width = 166
    Height = 62
    Caption = ' Propriedades: '
    TabOrder = 1
    object laCor: TLabel
      Left = 81
      Top = 29
      Width = 19
      Height = 13
      Caption = 'Cor:'
    end
    object Label2: TLabel
      Left = 18
      Top = 29
      Width = 35
      Height = 13
      Caption = 'Vis'#237'vel:'
    end
    object cbVis: TCheckBox
      Left = 58
      Top = 27
      Width = 18
      Height = 17
      TabOrder = 0
      OnClick = cbVisClick
    end
    object paCor: TPanel
      Left = 105
      Top = 29
      Width = 38
      Height = 14
      BevelInner = bvLowered
      BevelOuter = bvNone
      BevelWidth = 2
      TabOrder = 1
      OnClick = paCorClick
    end
  end
  object cbVC: TCheckBox
    Left = 4
    Top = 82
    Width = 226
    Height = 17
    Caption = 'Utilizar visibilidade condicionada a escala'
    TabOrder = 2
    OnClick = cbVCClick
  end
  object edEMin: TdrEdit
    Left = 43
    Top = 115
    Width = 86
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 3
    Text = '0'
    OnExit = edEMinExit
  end
  object edEMax: TdrEdit
    Left = 133
    Top = 115
    Width = 86
    Height = 21
    BeepOnError = False
    DataType = dtInteger
    TabOrder = 4
    Text = '0'
    OnExit = edEMaxExit
  end
  object ColorDialog: TColorDialog
    Left = 21
    Top = 25
  end
  object Menu: TPopupMenu
    Left = 90
    Top = 26
    object Menu_Remover: TMenuItem
      Caption = 'Remover'
      OnClick = Menu_RemoverClick
    end
  end
end

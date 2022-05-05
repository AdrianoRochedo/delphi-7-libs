object HidroAreaDeProjeto: THidroAreaDeProjeto
  Left = 238
  Top = 79
  Width = 308
  Height = 281
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnActivate = Form_Activate
  OnClose = Form_Close
  OnCloseQuery = Form_CloseQuery
  OnCreate = Form_Create
  OnDestroy = Form_Destroy
  OnKeyDown = Form_KeyDown
  OnResize = Form_Resize
  OnShow = Form_Show
  PixelsPerInch = 96
  TextHeight = 13
  object Map: TMapEx
    Left = 0
    Top = 0
    Width = 300
    Height = 219
    ParentColor = False
    Align = alClient
    TabOrder = 2
    OnMouseUp = Map_MouseUp
    OnMouseMove = Map_MouseMove
    OnMouseDown = Map_MouseDown
    OnDblClick = Map_Dbl_Click
    OnClick = Map_Click
    OnBeforeLayerDraw = MapBeforeLayerDraw
    OnAfterLayerDraw = Map_AfterLayerDraw
    ControlData = {
      00000200021F0000A2160000E1000000FFFFFF00000001000001102700000100
      00000000000000000000000000000000FFFFFF0001020000000000}
  end
  object Progresso: TProgressBar
    Left = 0
    Top = 219
    Width = 300
    Height = 16
    Align = alBottom
    Step = 1
    TabOrder = 0
    Visible = False
  end
  object mmMensagemInicial: TMemo
    Left = 11
    Top = 10
    Width = 277
    Height = 199
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderStyle = bsNone
    Lines.Strings = (
      ''
      '   Construa a rede hidrol'#243'gica utilizando a paleta de '
      '   componentes ao lado. '
      ''
      '   Os objeto centrais s'#227'o o PC (quadrado) e o '
      '   Reservat'#243'rio (Tri'#226'ngulo), eles s'#227'o conectados '
      '   atrav'#233's de trechos-d'#225'gua formando assim uma rede. '
      '   Voc'#234' tamb'#233'm poder'#225' conectar outros objetos a estes '
      '   componentes tais como sub-bacias e deriva'#231#245'es.'
      ''
      '   Um duplo-click na janela ou em um componente abrir'#225
      '   seu di'#225'logo de dados. Os objetos tamb'#233'm possuem '
      '   menus suspensos que s'#227'o ativados atrav'#233's do bot'#227'o'
      '   de fun'#231#227'o de seu "mouse".')
    ReadOnly = True
    TabOrder = 1
    Visible = False
    WordWrap = False
    OnClick = Map_Click
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 235
    Width = 300
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 50
      end>
  end
  object SaveDialog: TSaveDialog
    Tag = 1
    DefaultExt = 'txt'
    Filter = 'Arquivo Texto (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Salvar projeto como'
    Left = 12
    Top = 7
  end
end

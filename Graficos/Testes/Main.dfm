object Form1: TForm1
  Left = 259
  Top = 131
  Width = 696
  Height = 426
  Caption = ' Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Chart: TChart
    Left = 0
    Top = 33
    Width = 484
    Height = 364
    AllowPanning = pmNone
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    LeftAxis.LabelsSeparation = 70
    LeftAxis.MinorTickCount = 5
    LeftAxis.MinorTickLength = 9
    LeftAxis.RoundFirstLabel = False
    LeftAxis.TickLength = 5
    LeftAxis.Title.Caption = 'ddffghgh'
    Align = alLeft
    TabOrder = 0
  end
  object TeeCommander: TTeeCommander
    Left = 0
    Top = 0
    Width = 688
    Height = 33
    Panel = Chart
    Align = alTop
    TabOrder = 1
  end
  object Button1: TButton
    Left = 495
    Top = 41
    Width = 182
    Height = 25
    Caption = 'Mostrar um TfoChart'
    TabOrder = 2
    OnClick = Button1Click
  end
end

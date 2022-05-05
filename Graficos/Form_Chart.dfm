inherited foChart: TfoChart
  Left = 265
  Caption = 'foChart'
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FChart: TChart [0]
    Left = 0
    Top = 29
    Width = 442
    Height = 211
    BackWall.Brush.Color = clWhite
    BackWall.Color = clWindow
    Title.Text.Strings = (
      'TChart')
    OnClickLegend = FChartClickLegend
    BackColor = clWindow
    OnAfterDraw = FChartAfterDraw
    Align = alClient
    TabOrder = 0
    OnMouseMove = FChartMouseMove
  end
  inherited pNav: TPanel
    TabOrder = 2
  end
  inherited Menu: TPopupMenu
    Top = 37
    object N4: TMenuItem [5]
      Caption = '-'
    end
    object Menu_Visualizar: TMenuItem [6]
      Caption = 'Visualizar'
      object Menu_Nav: TMenuItem
        Caption = 'Barra de Navega'#231#227'o'
        Checked = True
        OnClick = Menu_NavClick
      end
      object Menu_3D: TMenuItem
        Caption = 'Barra 3D'
        Checked = True
        OnClick = Menu_3DClick
      end
    end
  end
  inherited Save: TSaveDialog
    Left = 31
    Top = 37
  end
end

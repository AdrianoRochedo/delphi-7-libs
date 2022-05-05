unit Frame_BaseChart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, TeeProcs, TeEngine, Chart, Menus, StdCtrls,
  CandleCh, BubbleCh, OHLChart, Series,
  ChartBaseClasses;

type
  TfrBaseChart = class(TFrame)
    pNav: TPanel;
    sbLeft: TSpeedButton;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    sbRight: TSpeedButton;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    sbZoomNormal: TSpeedButton;
    sbOpt: TSpeedButton;
    p3D: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cb3D: TCheckBox;
    sb3D: TScrollBar;
    sbRotacao: TScrollBar;
    Menu: TPopupMenu;
    Menu_OpcoesGerais: TMenuItem;
    Menu_Series: TMenuItem;
    N2: TMenuItem;
    Menu_Rastrear: TMenuItem;
    N1: TMenuItem;
    Menu_Imprimir: TMenuItem;
    Menu_Copiar: TMenuItem;
    N3: TMenuItem;
    Menu_SalvarComo: TMenuItem;
    Save: TSaveDialog;
    Chart: TChart;
    MenuVis: TMenuItem;
    N5: TMenuItem;
    Menu_3D: TMenuItem;
    Menu_Nav: TMenuItem;
    laRastrear: TLabel;
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure sbZoomNormalClick(Sender: TObject);
    procedure sbLeftClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure sbRightClick(Sender: TObject);
    procedure sbOptClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure cb3DClick(Sender: TObject);
    procedure sb3DChange(Sender: TObject);
    procedure sbRotacaoChange(Sender: TObject);
    procedure Menu_OpcoesGeraisClick(Sender: TObject);
    procedure Menu_SeriesClick(Sender: TObject);
    procedure Menu_RastrearClick(Sender: TObject);
    procedure Menu_ImprimirClick(Sender: TObject);
    procedure Menu_CopiarClick(Sender: TObject);
    procedure Menu_SalvarComoClick(Sender: TObject);
    procedure ChartAfterDraw(Sender: TObject);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Menu_NavClick(Sender: TObject);
    procedure Menu_3DClick(Sender: TObject);
  private
    FLS: TSeriesList;
    FList: TfoChartList;
    OldX, OldY: Longint;
    Procedure HorizScroll(Const Percent:Double);
    Procedure VertScroll(Const Percent:Double);
    Procedure ScrollAxis(Axis:TChartAxis; Const Percent:Double);
  protected
    destructor Destroy(); override;
    procedure DoSeriesChange(Serie: TChartSeries); virtual;
  public
    CrossHairColor: TColor;
    CrossHairStyle: TPenStyle;

    constructor Create(aOwner: TComponent); override;
    property Series : TSeriesList read FLS;
  end;

implementation
uses DialogoBase_OpcoesSeries,
     Dialogo_OpcoesBarSeries,
     Dialogo_OpcoesGeraisGrafico,
     Dialogo_OpcoesSerieLinhas,
     Dialogo_OpcoesSeriePontos,
     Dialogo_OpcoesSeriePizza,
     Dialogo_OpcoesSerieVelas;

{$R *.dfm}

procedure TfrBaseChart.sbZoomInClick(Sender: TObject);
begin
  Chart.ZoomPercent(120);
end;

procedure TfrBaseChart.sbZoomOutClick(Sender: TObject);
begin
  Chart.ZoomPercent(80);
end;

procedure TfrBaseChart.sbZoomNormalClick(Sender: TObject);
begin
  Chart.UndoZoom;
end;

procedure TfrBaseChart.sbLeftClick(Sender: TObject);
begin
  HorizScroll(10);
end;

procedure TfrBaseChart.sbUpClick(Sender: TObject);
begin
  VertScroll(-10);
end;

procedure TfrBaseChart.sbDownClick(Sender: TObject);
begin
  VertScroll(-10);
end;

procedure TfrBaseChart.sbRightClick(Sender: TObject);
begin
  HorizScroll(-10);
end;

procedure TfrBaseChart.sbOptClick(Sender: TObject);
var P: TPoint;
begin
  P := Point(sbOpt.Left, sbOpt.Top);
  P := pNav.ClientToScreen(P);
  Menu.Popup(P.x, P.y);
end;

procedure TfrBaseChart.MenuPopup(Sender: TObject);
var i: integer;
    m: TMenuItem;
begin
  while Menu_Series.Count > 0 do Menu_Series.Delete(Menu_Series.Count-1);
  for i := 0 to Chart.SeriesCount-1 do
    begin
    m := NewItem(FLS[i].Title, 0, False, True, Menu_SeriesClick, 0, 'MS_' + intToStr(i));
    m.Tag := integer(FLS[i]); // põe a referência a série i no Tag do Menu
    Menu_Series.Add(m);
    end;
end;

procedure TfrBaseChart.cb3DClick(Sender: TObject);
begin
  Chart.View3D      := cb3D.Checked;
  sb3D.Enabled      := Chart.View3D;
  sbRotacao.Enabled := Chart.View3D;
end;

procedure TfrBaseChart.sb3DChange(Sender: TObject);
begin
  Chart.Chart3DPercent := sb3D.Position;
end;

procedure TfrBaseChart.sbRotacaoChange(Sender: TObject);
var i: Integer;
begin
  if sbRotacao.Enabled then
     With Chart.View3DOptions do
       begin
       Orthogonal := False;
       for i := 0 to Chart.SeriesCount-1 do
          if FLS[i] is TPieSeries then
             Elevation := sbRotacao.Position
          else
             Rotation := sbRotacao.Position;
       end;
end;

constructor TfrBaseChart.Create(aOwner: TComponent);
begin
  inherited;
  FLS := TSeriesList.Create;
  FLS.Chart := Chart;
  Chart.Title.Text.Clear;
  Chart.Foot.Text.Clear;
  cb3D.Checked := Chart.View3D;
  Chart.Legend.LegendStyle := lsSeries;
  CrossHairColor := clYellow;
  CrossHairStyle := psSolid;
end;

procedure TfrBaseChart.HorizScroll(const Percent: Double);
begin
  ScrollAxis(Chart.TopAxis, Percent);
  ScrollAxis(Chart.BottomAxis, Percent);
end;

procedure TfrBaseChart.ScrollAxis(Axis: TChartAxis; const Percent: Double);
var Amount:Double;
begin
  With Axis do
  begin
    Amount:=-((Maximum-Minimum)/(100.0/Percent));
    SetMinMax(Minimum-Amount,Maximum-Amount);
  end;
end;

procedure TfrBaseChart.VertScroll(const Percent: Double);
begin
  ScrollAxis(Chart.LeftAxis, Percent);
  ScrollAxis(Chart.RightAxis, Percent);
end;

procedure TfrBaseChart.DoSeriesChange(Serie: TChartSeries);
begin
  // Nada
end;

destructor TfrBaseChart.Destroy();
begin
  FLS.Free;
  inherited;
end;

procedure TfrBaseChart.Menu_OpcoesGeraisClick(Sender: TObject);
var d: TgrDialogo_OpcoesGeraisGrafico;
begin
  d := TgrDialogo_OpcoesGeraisGrafico.Create(nil);
  d.Grafico := Chart;
  d.ShowModal;
  d.Free;
  cb3D.Checked := Chart.View3D;
end;

procedure TfrBaseChart.Menu_SeriesClick(Sender: TObject);
var m: TMenuItem;
    s: TChartSeries;
    d: TgrDialogo_BaseOpcoesSeries;
begin
  m := TMenuItem(Sender);
  if m.Tag <> 0 then
     begin
     s := TChartSeries(m.Tag);

     if s is TBarSeries    then d := TgrDialogo_OpcoesSerieBarras.Create(nil) else
     if s is TCandleSeries then d := TgrDialogo_OpcoesSerieVelas.Create(nil)  else
     if s is TLineSeries   then d := TgrDialogo_OpcoesSerieLinhas.Create(nil) else
     if s is TPointSeries  then d := TgrDialogo_OpcoesSeriePontos.Create(nil) else
     if s is TPieSeries    then d := TgrDialogo_OpcoesSeriePizza.Create(nil)
     else
        Exit;

     d.Serie := s;
     if d.ShowModal = mrOk then DoSeriesChange(s);
     d.Free;
     end;
end;

procedure TfrBaseChart.Menu_RastrearClick(Sender: TObject);
begin
  Menu_Rastrear.Checked := not Menu_Rastrear.Checked;
  if Menu_Rastrear.Checked then
     begin
     Chart.View3D := false;
     cb3D.Checked := false;
     Chart.AllowPanning := pmNone;
     Chart.AllowZoom := false;
     laRastrear.Visible := true;
     end
  else
     begin
     Chart.AllowPanning := pmBoth;
     Chart.AllowZoom := true;
     laRastrear.Visible := false;
     end;
end;

procedure TfrBaseChart.Menu_ImprimirClick(Sender: TObject);
begin
  Chart.Print();
end;

procedure TfrBaseChart.Menu_CopiarClick(Sender: TObject);
begin
  Chart.CopyToClipboardBitmap();
end;

procedure TfrBaseChart.Menu_SalvarComoClick(Sender: TObject);
begin
  if Save.Execute then
     Chart.SaveToBitmapFile(Save.FileName);
end;

procedure TfrBaseChart.ChartAfterDraw(Sender: TObject);
begin
  OldX := -1;  { Reset old mouse position }
end;

procedure TfrBaseChart.ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  { This procedure draws the crosshair lines }
  Procedure DrawCross(AX,AY:Integer);
  begin
    With Chart, Canvas do
    begin
      Pen.Color := CrossHairColor;
      Pen.Style := CrossHairStyle;
      Pen.Mode := pmXor;
      Pen.Width := 1;
      MoveTo(ax,ChartRect.Top-Height3D);
      LineTo(ax,ChartRect.Bottom-Height3D);
      MoveTo(ChartRect.Left+Width3D,ay);
      LineTo(ChartRect.Right+Width3D,ay);
    end;
  end;

Var tmpX,tmpY:Double;
begin
  if cb3D.Checked or not Menu_Rastrear.Checked then Exit;

  if (OldX <> -1) then
     begin
     DrawCross(OldX,OldY);  { draw old crosshair }
     OldX := -1;
     end;

  { check if mouse is inside Chart rectangle }
  if PtInRect( Chart.ChartRect, Point(X-Chart.Width3D, Y+Chart.Height3D) ) then
     begin
     DrawCross(x,y);  { draw crosshair at current position }
     { store old position }
     OldX := x;
     OldY := y;
     { set label text }
     if Chart.SeriesCount > 0 then
        With Chart.Series[0] do
          begin
          GetCursorValues(tmpX, tmpY);  { <-- get values under mouse cursor }
          laRastrear.Caption := 'X = ' + GetHorizAxis.LabelValue(tmpX) + '   ' +
                                'Y = ' + GetVertAxis.LabelValue(tmpY);
          end;
     end;
end;

procedure TfrBaseChart.Menu_NavClick(Sender: TObject);
begin
  Menu_Nav.Checked := not Menu_Nav.Checked;
  pNav.Visible := Menu_Nav.Checked;

  if pNav.Visible then
     Chart.PopupMenu := nil
  else
     Chart.PopupMenu := Menu;
end;

procedure TfrBaseChart.Menu_3DClick(Sender: TObject);
begin
  Menu_3D.Checked := not Menu_3D.Checked;
  p3D.Visible := Menu_3D.Checked;
end;

end.

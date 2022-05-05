unit Form_Chart;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, Menus, Form_BaseChart, StdCtrls,
  Buttons, Series;

type
  TfoChart = class(TfoBaseChart)
    FChart: TChart;
    N4: TMenuItem;
    Menu_Visualizar: TMenuItem;
    Menu_Nav: TMenuItem;
    Menu_3D: TMenuItem;
    procedure FChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Menu_NavClick(Sender: TObject);
    procedure Menu_3DClick(Sender: TObject);
    procedure FChartAfterDraw(Sender: TObject);
    procedure FChartClickLegend(Sender: TCustomChart; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    OldX,OldY:Longint;
    function getChart(): TChart; override;
  public
    CrossHairColor: TColor;
    CrossHairStyle: TPenStyle;
  end;

implementation

{$R *.DFM}

{ TfoChart }

function TfoChart.getChart(): TChart;
begin
  Result := FChart;
end;

procedure TfoChart.FChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  { This procedure draws the crosshair lines }
  Procedure DrawCross(AX,AY:Integer);
  begin
    With getChart(), Canvas do
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
    c: TChart;
begin
  if cb3D.Checked or not Menu_Rastrear.Checked then Exit;
  c := getChart();

  if (OldX <> -1) then
     begin
     DrawCross(OldX,OldY);  { draw old crosshair }
     OldX := -1;
     end;

  { check if mouse is inside Chart rectangle }
  if PtInRect( c.ChartRect, Point(X-c.Width3D, Y+c.Height3D) ) then
     begin
     DrawCross(x,y);  { draw crosshair at current position }
     { store old position }
     OldX := x;
     OldY := y;
     { set label text }
     if c.SeriesCount > 0 then
        With c.Series[0] do
          begin
          GetCursorValues(tmpX, tmpY);  { <-- get values under mouse cursor }
          Caption := 'X = ' + GetHorizAxis.LabelValue(tmpX) + '   ' +
                     'Y = ' + GetVertAxis.LabelValue(tmpY);
          end;
     end;
end;

procedure TfoChart.FormCreate(Sender: TObject);
begin
  inherited;
  OldX := -1;                          
  CrossHairColor := clYellow;
  CrossHairStyle := psSolid;
end;

procedure TfoChart.Menu_NavClick(Sender: TObject);
begin
  inherited;
  Menu_Nav.Checked := not Menu_Nav.Checked;
  pNav.Visible := Menu_Nav.Checked;

  if pNav.Visible then
     getChart().PopupMenu := nil
  else
     getChart().PopupMenu := Menu;
end;

procedure TfoChart.Menu_3DClick(Sender: TObject);
begin
  inherited;
  Menu_3D.Checked := not Menu_3D.Checked;
  p3D.Visible := Menu_3D.Checked;
end;

procedure TfoChart.FChartAfterDraw(Sender: TObject);
begin
  inherited;
  OldX := -1;  { Reset old mouse position }
end;

procedure TfoChart.FChartClickLegend(Sender: TCustomChart;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function FindSerie(const Title: string): TChartSeries;
  var i: Integer;
  begin
    result := nil;
    for i := 0 to FChart.SeriesCount-1 do
      if FChart.Series[i].Title = Title then
         begin
         result := FChart.Series[i];
         break;
         end;
  end;

var i: integer;
    s: TChartSeries;
begin
  inherited;
  i := FChart.Legend.Clicked(x, y);
  if (i >= 0) and (i < FChart.SeriesCount) then
     begin
     s := FindSerie(FChart.Legend.FormattedLegend(i));
     if s <> nil then
        EditSerie(s);
     end;
end;

end.

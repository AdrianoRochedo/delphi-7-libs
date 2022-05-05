unit Ubarzoom;

interface
               
uses
  Winprocs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, StdCtrls, Buttons;

{ This example shows customized Bar Series. }

{ The new Series below are simple Bar Series with a new feature:

     The Bars width is automatically resized when zooming.

  Also this demo shows how to perform " Animated Scroll " .

}

type
  { the new Series components... }
  TBarSeriesZoom=class(TBarSeries)
  protected
    Function InternalCalcBarSize:Longint; override;
  end;

  THorizBarSeriesZoom=class(THorizBarSeries)
  protected
    Function InternalCalcBarSize:Longint; override;
  end;

  TForm1 = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    ScrollBar1: TScrollBar;
    Label1: TLabel;
    Series1: TBarSeries;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Panel2: TPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TheAxis:TChartAxis;
    procedure AnimatedScroll( Const Speed:Double );
    procedure CreateSampleSeries;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses TeeShape;

{ This is the calculation necessary to change the Bar width and height 
   when the user zooms....   }
Function TBarSeriesZoom.InternalCalcBarSize:Longint;
begin
  With ParentChart do
  result:=Round(ChartWidth*(1.0*Count+1)/(1.0+GetHorizAxis.Maximum-GetHorizAxis.Minimum));
end;

{ This is the calculation necessary to change the Bar width and height 
   when the user zooms....   }
Function THorizBarSeriesZoom.InternalCalcBarSize:Longint;
begin
  With ParentChart do
  result:=Round(ChartHeight*(1.0*Count+1)/(1.0+GetVertAxis.Maximum-GetVertAxis.Minimum));
end;

{ This function simply adds a Shape Series to the Chart... }
Function AddBanner( AChart:TCustomChart;
                    const AX0,AX1,AY0,AY1:Double;
                    AColor,AFontColor:TColor;
                    Const AText:String):TChartShape;
begin
  result:=TChartShape.Create(AChart);
  With result do
  begin
    X0:=AX0;
    X1:=AX1;
    Y0:=AY0;
    Y1:=AY1;
    Style:=chasRectangle;
    Brush.Color:=AColor;
    Text.Add(AText);
    With Font do
    begin
      Size:=12;
      Style:=[fsBold];
      Name:='Arial';
      Color:=AFontColor;
    end;
    Pen.Width:=2;
    RoundRectangle:=True;
    ParentChart:=AChart;
  end;
end;

{ This procedure does "Animated Zoom"  BUT ONLY in ONE Axis, 
   instead of the normal behavior ( two axis ).
}
Procedure ZoomAxisPercent(Axis:TChartAxis; Const PercentZoom:Double);

  Procedure CalcAxisScale(Axis:TChartAxis; Var tmpA,tmpB:Double);
  Var tmpDelta,AMin,AMax:Double;
  Begin
    AMin:=Axis.Minimum;
    AMax:=Axis.Maximum;
    Axis.CalcMinMax(AMin,AMax);
    tmpDelta:=(AMax-AMin)*(PercentZoom-100.0)/100.0;
    tmpA:=AMin+tmpDelta;
    tmpB:=AMax-tmpDelta;
  end;

var A,B:Double;
    t:Longint;
Begin  { zoom a given "percent" }
  CalcAxisScale(Axis,A,B);
  With (Axis.ParentChart as TCustomChart) do
  for t:=1 to AnimatedZoomSteps do
  Begin
    with Axis do
      SetMinMax(Minimum+((A-Minimum)/AnimatedZoomFactor),
                Maximum-((Maximum-B)/AnimatedZoomFactor));
    Refresh;
  end;
end;

{ Add some random values to the Chart... }
procedure TForm1.CreateSampleSeries;
Const MaxPoints=30;
var t,num:Integer;
begin
  for num:=1 to 3 do
  With TBarSeriesZoom.Create(Self) do
  begin
    Marks.Visible:=(num=3);
    Marks.BackColor:=clNavy;
    Marks.Font.Color:=clWhite;
    MultiBar:=mbNone;
    for t:=1 to MaxPoints do Add(Random(1000),'',clTeeColor);
    ParentChart:=Chart1;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart1.FreeAllSeries;
  Chart1.AnimatedZoom:=True;
  Chart1.AnimatedZoomSteps:=3;
  Chart1.Chart3DPercent:=10;

  ComboBox1.ItemIndex:=0;

  AddBanner( Chart1,  3, 12, 1100, 1700, clBlue,clYellow, 'Winter Sales');
  AddBanner( Chart1, 15, 22, 1100, 1700, clLime,clBlue,'Summer Sales');

  TheAxis:=Chart1.BottomAxis;
  CreateSampleSeries;
  TheAxis.SetMinMax(5,12);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  ZoomAxisPercent(TheAxis,80);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  ZoomAxisPercent(TheAxis,120);
end;

{ The animated scroll routine... }
procedure TForm1.AnimatedScroll( Const Speed:Double );
var t:Integer;
begin
  for t:=1 to 5 do   { <-- 5 animation steps... }
  begin
    TheAxis.Scroll(Speed,False);
    Chart1.Refresh;
  end;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  AnimatedScroll(0.25);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  AnimatedScroll(-0.25);
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  Chart1.View3D:=ScrollBar1.Position>0;
  if Chart1.View3D then
     Chart1.Chart3DPercent:=ScrollBar1.Position;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var t:Integer;
begin
  With (Chart1[Chart1.SeriesCount-1] as TCustomBarSeries) do
  begin
    Case ComboBox1.ItemIndex of
      0: MultiBar:=mbNone;
      1: MultiBar:=mbSide;
      2: MultiBar:=mbStacked;
    end;
    for t:=0 to Chart1.SeriesCount-1 do
    if Chart1[t] is TChartShape then
       With TChartShape(Chart1[t]) do
       if MultiBar=mbStacked then
       begin
        Y0:=2100;
        Y1:=2700;
       end
       else
       begin
        Y0:=1100;
        Y1:=1700;
       end;
    end
end;

end.

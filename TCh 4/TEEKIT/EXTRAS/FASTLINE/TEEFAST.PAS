unit teefast;

interface

{ Redraw FastLine Series faster... by drawing the LAST added point only. }
uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TeEngine, Series, ExtCtrls, TeeProcs, Chart;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Series1: TFastLineSeries;
    Label3: TLabel;
    Series2: TFastLineSeries;
    Series3: TFastLineSeries;
    Series4: TFastLineSeries;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Fast:Boolean;
    Procedure RunDemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ Run the FAST demo }
procedure TForm1.Button1Click(Sender: TObject);
begin
  Fast:=True;
  RunDemo;
end;

{ Run the SLOW demo }
procedure TForm1.Button2Click(Sender: TObject);
begin
  Fast:=False;
  RunDemo;
end;

{ This procedure does the job !

    "RedrawNewPoint"

}
type TTempSeries=class(TChartSeries)
     end;

Procedure RedrawNewPoint(ASeries:TChartSeries; Const Value:Double);
begin
  With ASeries.ParentChart do
  begin
    if (Value<LeftAxis.Minimum) then
    begin
      LeftAxis.SetMinMax(Value-100,LeftAxis.Maximum);
      Repaint;
    end
    else
    if (Value>LeftAxis.Maximum) then
    begin
      LeftAxis.SetMinMax(LeftAxis.Minimum,Value+100);
      Repaint;
    end
    else
    begin
      if ASeries is TFastLineSeries then
      begin
         if ASeries.Count=1 then
            Canvas.MoveTo(ASeries.CalcXPos(0),ASeries.CalcYPos(0))
         else
            Canvas.Pen.Assign(TFastLineSeries(ASeries).LinePen);
      end;
      TTempSeries(ASeries).DrawValue(ASeries.Count-1);
    end;
  end;
end;

Procedure TForm1.RunDemo;
var old:Double;
    OldTime,t,NumberSamples:Longint;
begin
  Screen.Cursor:=crHourGlass;
  OldTime:=GetTickCount;

  NumberSamples:=StrToInt(Edit1.Text);
  Chart1.BottomAxis.SetMinMax(0,NumberSamples);
  Chart1.LeftAxis.SetMinMax(4000,16000);

  TeeDefaultCapacity:=NumberSamples;
  Series1.Clear;
  Chart1.Repaint;
  old:=10000;
  for t:=1 to NumberSamples do
  begin
    old:=old+(Random(150)-75.0);
    Series1.Add(old,'',clTeeColor);
    if Fast then ReDrawNewPoint(Series1,old)  { <-- FAST ! }
            else Chart1.Repaint;              { <-- NORMAL }
  end;

  Screen.Cursor:=crDefault;
  Label3.Caption:=IntToStr(GetTickCount-OldTime);
end;

end.

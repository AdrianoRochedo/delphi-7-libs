unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeeComma, ExtCtrls, TeeProcs, TeEngine, Chart, Series, StdCtrls,
  TeeLisB, TeeEdit;

type
  TForm1 = class(TForm)
    Chart: TChart;
    TeeCommander: TTeeCommander;
    Button1: TButton;
    Series1: TLineSeries;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses Form_Chart;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var s: TPointSeries;
    l: TLineSeries;
    c: TfoChart;
    i: integer;
begin
  c := TFoChart.Create('Demo Chart');
  c.Show(fsNormal);

  s := c.Series.AddPointSerie('serie 1', clRED);
  s.AddY(200);
  for i := 1 to 300 do
    s.AddY(sin(i) * 100);
  s.AddY(-200);

  l := c.Series.AddLineSerie('serie 2', clBLUE);
  for i := 1 to 200 do
    l.AddY(sin(i) * 50);
end;

end.

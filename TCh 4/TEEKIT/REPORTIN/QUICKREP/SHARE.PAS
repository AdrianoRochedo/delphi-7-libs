unit share;

interface

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, Chart, TeeProcs, DBChart, QrTee, quickrpt, ExtCtrls,
  StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    QuickRep1: TQuickRep;
    QRDBChart1: TQRDBChart;
    QRChart1: TQRChart;
    Chart1: TChart;
    BitBtn1: TBitBtn;
    Series1: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ This procedure will duplicate one existing Chart in a Form
  inside a QuickReport Chart.
  It can be useful to share Charts both in screen Forms and Reports. }

Procedure LinkChart(AChart:TCustomChart; AQRChart:TQRChart);
var t:Integer;
begin
  AQRChart.Chart.FreeAllSeries;  { <-- remove the existing old Series }
  AQRChart.Chart.Assign(AChart);   { <-- duplicate all Chart properties }
  { duplicate the Series and values }
  for t:=0 to AChart.SeriesCount-1 do
      CloneChartSeries(AChart[t]).ParentChart:=AQRChart.Chart;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(8);  { <-- some random values for the demo }
  LinkChart(Chart1,QRChart1);   { <-- link the Charts ! }
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  QuickRep1.Preview; 
end;

end.

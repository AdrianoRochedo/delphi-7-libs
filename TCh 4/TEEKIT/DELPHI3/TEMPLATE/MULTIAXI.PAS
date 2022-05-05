unit MultiAxi;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OHLChart, CandleCh, Series, TeEngine, TeeProcs, Chart, ExtCtrls, GanttCh;

type
  TFormMultiAxis = class(TForm)
    Panel1: TPanel;
    Chart2: TChart;
    Series3: TVolumeSeries;
    Series4: TFastLineSeries;
    Chart1: TChart;
    Series1: TCandleSeries;
    Series2: TFastLineSeries;
    Chart3: TChart;
    Series5: TGanttSeries;
    Series6: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure Chart2Scroll(Sender: TObject);
    procedure Chart1Scroll(Sender: TObject);
    procedure Chart3Scroll(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure ScrollOtherCharts(OriginAxis,DestAxis1,DestAxis2:TChartAxis);
  end;

var
  FormMultiAxis: TFormMultiAxis;

implementation

{$R *.DFM}

procedure TFormMultiAxis.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(40);
  Series2.FillSampleValues(40);
  Series3.FillSampleValues(40);
  Series4.FillSampleValues(40);
  Series5.FillSampleValues(40);
  Series6.FillSampleValues(40);

  { Important:

  To make several Charts to be aligned in their Horizontal Axis,
  set the LeftAxis.LabelsSize and RightAxis.LabelsSize properties
  to a common value:

  Chart1.LeftAxis.LabelsSize:=60;
  Chart2.LeftAxis.LabelsSize:=60;
  Chart3.LeftAxis.LabelsSize:=60;

  }
end;

{ syncronize scrolling for the 3 charts }
Procedure TFormMultiAxis.ScrollOtherCharts(OriginAxis,DestAxis1,DestAxis2:TChartAxis);
begin
  { syncronize one chart }
  DestAxis1.SetMinMax(OriginAxis.Minimum,OriginAxis.Maximum);
  DestAxis1.Repaint;
  { syncronize the other chart }
  DestAxis2.SetMinMax(OriginAxis.Minimum,OriginAxis.Maximum);
  DestAxis2.Repaint;
end;

procedure TFormMultiAxis.Chart2Scroll(Sender: TObject);
begin
  ScrollOtherCharts(Chart2.BottomAxis,Chart1.TopAxis,Chart3.BottomAxis);
end;

procedure TFormMultiAxis.Chart1Scroll(Sender: TObject);
begin
  ScrollOtherCharts(Chart1.TopAxis,Chart2.BottomAxis,Chart3.BottomAxis);
end;

procedure TFormMultiAxis.Chart3Scroll(Sender: TObject);
begin
  ScrollOtherCharts(Chart3.BottomAxis,Chart1.TopAxis,Chart2.BottomAxis);
end;

end.

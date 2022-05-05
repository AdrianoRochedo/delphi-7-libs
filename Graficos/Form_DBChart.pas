unit Form_DBChart;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Form_BaseChart, ExtCtrls, TeeProcs, TeEngine, Chart, DBChart, Menus,
  StdCtrls, Buttons;

type
  TfoDBChart = class(TfoBaseChart)
    FDBChart: TDBChart;
  private
    function getChart(): TChart; override;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

{ TgrGraficoDB }

function TfoDBChart.getChart(): TChart;
begin
  Result := TChart(FDBChart);
end;

end.

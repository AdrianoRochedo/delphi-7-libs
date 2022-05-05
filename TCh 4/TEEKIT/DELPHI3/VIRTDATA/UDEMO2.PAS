unit udemo2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, TeeData, TeEngine, Series, ExtCtrls, TeeProcs, Chart,
  StdCtrls, DBCtrls, BubbleCh;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    SeriesDataSet1: TSeriesDataSet;
    DataSource1: TDataSource;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    Series1: TBubbleSeries;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SeriesDataSet1.Close;
  Series1.FillSampleValues(8);
  SeriesDataSet1.Open;
end;

procedure TForm1.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var tmp:Integer;
begin
  if not DBGrid1.EditorMode then  { if not editing in the Grid... }
  begin
    tmp:=Series1.Clicked(x,y);   { if mouse is over a point.... }
    if tmp>=0 then
       SeriesDataSet1.RecNo:=tmp+1;   { go to record where mouse is over }
  end;
end;

procedure TForm1.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var tmp:Integer;
begin
  if Series=Series1 then
  begin
    tmp:=Series1.Clicked(x,y);
    if tmp>=0 then
       SeriesDataSet1.RecNo:=tmp+1;   { if clicked a point... }
    { go to Grid }
    DBGrid1.SetFocus;
    { go to "Y" field in the grid }
    DBGrid1.SelectedField:=SeriesDataSet1.FieldByName('Y');
    { start editing }
    DBGrid1.EditorMode:=True;
  end;
end;

end.

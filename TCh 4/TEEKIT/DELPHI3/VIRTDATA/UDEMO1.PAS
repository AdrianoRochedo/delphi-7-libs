unit udemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, Db, TeeData, StdCtrls,
  DBCtrls, Grids, DBGrids, Mask;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    SeriesDataSet1: TSeriesDataSet;
    DBEdit1: TDBEdit;
    Series1: TPieSeries;
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1DrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
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
  Series1.FillSampleValues(10);
  SeriesDataSet1.Close;
  SeriesDataSet1.Open;
end;

procedure TForm1.DBGrid1DrawDataCell(Sender: TObject; const Rect: TRect;
  Field: TField; State: TGridDrawState);
begin
  if Field.FieldName='Color' then
  With DBGrid1.Canvas do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=Field.Value;
    FillRect(Rect);
  end;
end;

end.

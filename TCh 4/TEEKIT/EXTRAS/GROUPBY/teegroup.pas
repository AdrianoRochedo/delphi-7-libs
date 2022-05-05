unit teegroup;

interface

uses
  Wintypes,winprocs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeeProcs, TeEngine, Chart, ExtCtrls, DBCtrls, Grids, DBGrids, Db,
  DBTables, Series, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    Query1: TQuery;
    DataSource1: TDataSource;
    Chart1: TChart;
    Series1: TBarSeries;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Dim1,Dim2:String;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ include TeeCross unit }

uses TeeCross,EditChar;

{ refresh the Chart showing the "sum" or the "count"... }
procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  Query1.Close;
  Query1.Open;
  Case RadioGroup1.ItemIndex of
    0: FillDataSet(Query1,Series1,Dim1,Dim2,'AmountPaid',gfCount);
    1: FillDataSet(Query1,Series1,Dim1,Dim2,'AmountPaid',gfSum);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Dim1:='Terms';
  Dim2:='ShipVia';
  RadioGroup1Click(Self);
end;

{ swap the cross-tab fields and refresh... }
procedure TForm1.Button1Click(Sender: TObject);
var tmp:String;
begin
  tmp:=Dim1;
  Dim1:=Dim2;
  Dim2:=tmp;
  RadioGroup1Click(Self);
end;

{ edit the chart as usually... }
procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

end.

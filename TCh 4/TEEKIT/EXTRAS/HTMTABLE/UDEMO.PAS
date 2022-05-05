unit Udemo;

interface

{ Demo of TImporter component, located at TEEHTM.PAS unit }
uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, StdCtrls, teeHTM;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    ComboBox1: TComboBox;
    Button2: TButton;
    Button8: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Importer1: TImporter;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses TeeSurfa,Editchar,ChartPro;

procedure TForm1.Button2Click(Sender: TObject);
begin
  EditChart(Self,Chart1);  { <-- show the Chart editor dialog }
end;

procedure TForm1.Button1Click(Sender: TObject);
var tmp:TChartSeries;
begin
  { Prepare the TImporter properties according to the kind of
    htm table we want to load....
  }
  Importer1.CloneCount:=0;       { <-- don't clone series automatically... }
  Case ComboBox1.ItemIndex of
       0: With Importer1 do     { dollar.htm }
          begin
            TitleRows:=0;       { <-- number of rows with titles... }
            XColumn:=1;         { <-- where is the "X" value in the table columns ? }
            XDateTime:=True;        { <-- are X values DateTime? }
            YColumn:=3;         { <-- where is the "Y" value in the table columns ? }
            LabelColumn:=-1;    { <-- where is the LABEL in the table columns ? }
            InvertedDate:=True;     { <-- are X Date values inverted?  ( YY/MM/DD ) }
            SwapDecimal:=True;      { <-- have to swap "." with "," ? }
          end;
       1: With Importer1 do  { world.htm }
          begin
            TitleRows:=0;
            XColumn:=-1;
            XDateTime:=False;
            YColumn:=3;
            LabelColumn:=0;
            AddCloneCols([4,5,6,7,8,9]);  { <-- duplicate Series1 for other columns }
            SwapDecimal:=True;
          end;
       2: With Importer1 do { temperat.htm }
          begin
            TitleRows:=1;
            XColumn:=-1;
            XDateTime:=False;
            LabelColumn:=0;
            YColumn:=1;
            AddCloneCols([2,3,4,5,6,7,8]);
            SwapDecimal:=True;
          end;
       3: With Importer1 do  { tv.htm }
          begin
            TitleRows:=0;
            XColumn:=-1;
            XDateTime:=False;
            LabelColumn:=2;
            YColumn:=6;
            SwapDecimal:=True;
            Chart1.Legend.Visible:=False;
            tmp:=Chart1[0];
            ChangeSeriesType(tmp,THorizBarSeries);  { <-- change to Horizontal Bar }
            { prepare some esthetic series properties... }
            Chart1[0].ColorEachPoint:=True;
            Chart1[0].Marks.Visible:=True;
            Chart1[0].Marks.Style:=smsValue;
            Chart1.LeftAxis.Inverted:=True;
            Chart1.LeftAxis.LabelsSeparation:=0;
          end;
  end;
  { Do it !!!! }
  Importer1.Chart:=Chart1;
  Importer1.ImportHTMTable(ComboBox1.Text);  { <-- load the HTM Table !!! }
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  SaveSeriesToHTM('series1.htm',Chart1[0])
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Importer1:=TImporter.Create(Self); { <-- in teeHTM.pas unit }
  ComboBox1.ItemIndex:=0;
  Chart1.AddSeries(TLineSeries.Create(Self));
end;

end.

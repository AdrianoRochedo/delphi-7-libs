unit Udemocum;

interface

{ This unit shows an "Histogram" type of Chart.

   There are two Series:

     1) The data
     2) The continous accumulated data (using a TCumulative Function)

   The Chart can be saved as "Component Template" in Delphi 3.0 for later
     reuse.
}

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, teeCumu, TeeFunci,TeeGally,
  StdCtrls;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Series1: TBarSeries;
    Button1: TButton;
    Series2: TLineSeries;
    Label1: TLabel;
    Label2: TLabel;
    TeeFunction1: TCumulative;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses EditChar;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Series1.AddArray([0,5,4,2,3,1]);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

end.

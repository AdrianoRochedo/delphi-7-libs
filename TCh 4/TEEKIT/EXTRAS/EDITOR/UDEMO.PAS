unit udemo;


{ WARNING: Please register the NOREDRAW.PAS unit BEFORE
  opening or running this project.
}

{ This demo shows the TMyChart component.
  This chart component repaints itself only after the
  Editor Dialog is closed, NOT when modifying each property
  using the dialog.

  Please see NoRedraw.pas unit for details.
}
interface

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Series, TeEngine, ExtCtrls, TeeProcs, Chart, NoRedraw, StdCtrls;

type
  TForm2 = class(TForm)
    Sales: TBarSeries;
    Expenses: TLineSeries;
    MyChart1: TMyChart;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}
Uses EditChar;

procedure TForm2.Button1Click(Sender: TObject);
begin
  MyChart1.Editing:=True;
  try
    EditChart(Self,MyChart1);
  finally
    MyChart1.Editing:=False;
    MyChart1.Repaint;        { <-- redraw the Chart when finished Editing... }
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Sales.FillSampleValues(8);
  Expenses.FillSampleValues(8);
end;

end.

unit bymonth;

interface

{ This unit shows an example of use of the TEEMONTH.PAS unit.

  It performs a "GROUP BY MONTH" or "GROUP BY WEEK" query against
  the DBDEMOS ORDERS.DB example table.

  (note: the "Year" combobox in this demo is not available in Delphi 1.0 16bit)
}
uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Grids,
  DBGrids, Db, DBTables;

type
  TFormMonthly = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    Chart1: TChart;
    Series1: TBarSeries;
    Panel1: TPanel;
    RadioGroup2: TRadioGroup;
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    LabelRemind: TLabel;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Refresh;
  end;

var
  FormMonthly: TFormMonthly;

implementation

{$R *.DFM}

Uses EditChar,   { <-- necessary to edit the chart }
     TeeMonth;   { <-- necessary to do the calculation }

procedure TFormMonthly.Refresh;
Var DatePeriod:TDateTimeStep;
    YearSt:String;
begin
  {$IFDEF WIN32}
  { only for Delphi 2 and 3... }
  { Set a YEAR filter on the table for this example... }
  if ComboBox1.ItemIndex=0 then Table1.Filtered:=False
  else
  begin
    YearSt:=ComboBox1.Text;
    Table1.Filter:='SaleDate>'+#39+'1/1/'+YearSt+#39+' and SaleDate<='+#39+'31/12/'+YearSt+#39;
    Table1.Filtered:=True;
  end;
  {$ENDIF}

  { choose a period (Month or Week) }
  if RadioGroup2.ItemIndex=0 then DatePeriod:=dtOneMonth
                             else DatePeriod:=dtOneWeek;

  { HERE IS THE IMPORTANT THING: Call the routine !!! }
  DBMonthlySeries( Series1,                   { <-- the destination Series }
                   Table1,                    { <-- the origin Table (or Query) }
                   'SaleDate',                { <-- the DATE field }
                   'AmountPaid',              { <-- the CHART field }
                   DatePeriod,                { <-- it can be dtOneMonth or dtOneWeek }
                   RadioGroup1.ItemIndex=0);  { <-- choose Sum or Count }


  { To make this demo nicer, set the Chart Titles accordingly.... }
  if DatePeriod=dtOneMonth then
     Chart1.BottomAxis.Title.Caption:='Sale Date Month'
  else
     Chart1.BottomAxis.Title.Caption:='Sale Date Week';
  if RadioGroup1.ItemIndex=0 then
     Chart1.LeftAxis.Title.Caption:='Count of Orders'
  else
     Chart1.LeftAxis.Title.Caption:='Sum of Amount Paid';
end;

procedure TFormMonthly.RadioGroup1Click(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMonthly.RadioGroup2Click(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMonthly.CheckBox1Click(Sender: TObject);
begin
  Series1.Marks.Visible:=CheckBox1.Checked;
end;

procedure TFormMonthly.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);
end;

procedure TFormMonthly.ComboBox1Change(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMonthly.FormCreate(Sender: TObject);
begin
  {$IFNDEF WIN32}
  ComboBox1.Enabled:=False;
  LabelRemind.Visible:=True;
  {$ELSE}
  LabelRemind.Visible:=False;
  {$ENDIF}
  ComboBox1.ItemIndex:=4;
  Refresh;
end;

end.

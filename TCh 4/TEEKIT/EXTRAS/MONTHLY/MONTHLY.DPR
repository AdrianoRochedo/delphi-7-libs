program monthly;

uses
  Forms,
  bymonth in 'bymonth.pas' {Form1},
  TeeMonth in 'TeeMonth.pas';

{$R *.RES}

begin
  Application.CreateForm(TFormMonthly, FormMonthly);
  Application.Run;
end.

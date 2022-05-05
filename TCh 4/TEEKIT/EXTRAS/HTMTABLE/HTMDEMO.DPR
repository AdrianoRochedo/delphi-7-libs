program Htmdemo;

uses
  Forms,
  Udemo in 'UDEMO.PAS' {Form1},
  teeHTM in 'TEEHTM.PAS';

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

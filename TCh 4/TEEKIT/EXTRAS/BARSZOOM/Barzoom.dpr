program Barzoom;

uses
  Forms,
  StdCtrls,
  Ubarzoom in 'UBARZOOM.PAS' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

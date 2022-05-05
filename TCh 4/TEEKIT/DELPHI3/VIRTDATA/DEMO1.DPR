program demo1;

uses
  Forms,
  udemo1 in 'udemo1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

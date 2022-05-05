program demo2;

uses
  Forms,
  udemo2 in 'udemo2.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

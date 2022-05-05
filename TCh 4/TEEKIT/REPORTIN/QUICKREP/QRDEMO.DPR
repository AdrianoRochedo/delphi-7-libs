program qrdemo;

uses
  Forms,
  share in 'share.pas' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

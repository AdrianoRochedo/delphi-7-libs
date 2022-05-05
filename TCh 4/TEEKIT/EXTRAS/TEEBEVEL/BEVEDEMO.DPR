program bevedemo;

uses
  Forms,
  ubevdemo in 'ubevdemo.pas' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

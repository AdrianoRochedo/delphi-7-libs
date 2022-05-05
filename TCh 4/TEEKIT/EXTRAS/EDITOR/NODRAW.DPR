program nodraw;

uses
  Forms,
  udemo in 'udemo.pas' {Form2};

{$R *.RES}

begin
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

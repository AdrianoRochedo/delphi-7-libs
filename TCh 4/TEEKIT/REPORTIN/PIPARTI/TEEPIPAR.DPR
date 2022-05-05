program teepipar;

uses
  Forms,
  uteepip in 'uteepip.pas' {FormTeePiparti};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTeePiparti, FormTeePiparti);
  Application.Run;
end.

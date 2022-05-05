program teecube;

uses
  Forms,
  teecube1 in 'teecube1.pas' {DCDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDCDemoForm, DCDemoForm);
  Application.Run;
end.

program RAI2Test;

uses
  Forms,
  fRAI2Test in 'fRAI2Test.pas' {Test};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTest, Test);
  Application.Run;
end.

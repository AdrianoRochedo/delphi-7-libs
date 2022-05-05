program teetemp;

uses
  Forms,
  MultiAxi in 'MultiAxi.pas' {FormMultiAxis};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMultiAxis, FormMultiAxis);
  Application.Run;
end.

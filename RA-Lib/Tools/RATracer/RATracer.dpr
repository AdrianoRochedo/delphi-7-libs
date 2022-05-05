program RATracer;

uses
  Forms,
  Dialogs,
  fTracer in 'fTracer.pas' {TracerMain},
  iMTracer in 'iMTracer.pas';

{$R *.RES}


begin
  Application.Initialize;
  Application.Title := 'R&A Tracer';
  Application.CreateForm(TTracerMain, TracerMain);
  Application.Run;
end.

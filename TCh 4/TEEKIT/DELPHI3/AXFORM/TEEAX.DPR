library TeeAX;

uses
  ComServ,
  TeeX_TLB in 'TeeX_TLB.pas',
  TeeAXImp in 'TeeAXImp.pas' {TeeAxForm: TActiveForm} {ChartX: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{$E ocx}

begin
end.

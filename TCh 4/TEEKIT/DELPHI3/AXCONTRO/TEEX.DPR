library TeeX;

uses
  ComServ,
  TeeXTLB in 'TeeXTLB.pas',
  TeeChaIm in 'TeeChaIm.pas' {TeeChartX: CoClass},
  About1 in 'About1.pas' {TeeChartXAbout};

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

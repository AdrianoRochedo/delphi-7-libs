library PS_Ex;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  ActiveX,
  Lib_Windows,
  Lib_wsVec,
  Lib_wsMatrix,
  Lib_Chart,
  Lib_Sheet,
  Lib_SpreadSheet,
  psBase in '..\..\psBASE.pas',
  RunTimeForm in '..\..\RunTimeForm.pas' {RunTime};

{$R *.RES}

procedure PascalScript_API(Lib: TLib); stdcall;
begin
  Lib_Windows.API(Lib);
  Lib_wsVec.API(Lib);
  Lib_wsMatrix.API(Lib);
  Lib_Chart.API(Lib);
  Lib_Sheet.API(Lib);
  Lib_SpreadSheet.API(Lib);
end;

exports
  PascalScript_API;

begin
  // Todas as bibliotecas que exportam controles ActiveX deverão chamar CoInitialize(nil)
  CoInitialize(nil);
end.


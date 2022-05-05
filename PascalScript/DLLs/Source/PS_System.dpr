library PS_System;

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
  Lib_System,
  Lib_Listas,
  Lib_String,
  Lib_StringList,
  Lib_GlobalObjects,
  psBase in '..\..\psBASE.pas';

{$R *.RES}

procedure PascalScript_API(Lib: TLib); stdcall;
begin
  Lib_System.API(Lib);
  Lib_Listas.API(Lib);
  Lib_String.API(Lib);
  Lib_StringList.API(Lib);
  Lib_GlobalObjects.API(Lib);
end;

exports
  PascalScript_API;

begin
end.


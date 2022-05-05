//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclradb53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("radbreg.pas");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEPACKAGE("ractl53.bpi");
USEPACKAGE("radb53.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------


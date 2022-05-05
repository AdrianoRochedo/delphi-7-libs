//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclractl53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("ractlreg.pas");
USEPACKAGE("ractl53.bpi");
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


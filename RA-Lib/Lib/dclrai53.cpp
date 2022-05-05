//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclrai53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("raireg.pas");
USEPACKAGE("rai53.bpi");
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


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("rai53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("RAI2Parser.pas");
USEUNIT("RAI2Fm.pas");
USEUNIT("RAI2.pas");
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


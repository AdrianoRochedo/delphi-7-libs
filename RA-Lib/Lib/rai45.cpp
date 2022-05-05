//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("rai45.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("RAI2Fm.pas");
USEUNIT("RAI2.pas");
USEUNIT("RAI2Parser.pas");
USEPACKAGE("ractl45.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------

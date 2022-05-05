//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclractl45.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("ractlreg.pas");
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

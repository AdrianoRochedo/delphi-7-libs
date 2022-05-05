//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclrai45.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("raireg.pas");
USEPACKAGE("rai45.bpi");
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

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclradb45.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("radbreg.pas");
USEPACKAGE("radb45.bpi");
USEPACKAGE("ractl45.bpi");
USEPACKAGE("vcldb40.bpi");
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

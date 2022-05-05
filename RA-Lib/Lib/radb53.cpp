//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("radb53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("RADBUtil.pas");
USEUNIT("RADBLookupTreeView.pas");
USEUNIT("RADBMove.pas");
USEUNIT("RADBTreeView.pas");
USEUNIT("RADBCt.pas");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
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


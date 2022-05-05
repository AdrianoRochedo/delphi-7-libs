//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("raia53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("RAI2_Windows.pas");
USEUNIT("RAI2_Classes.pas");
USEUNIT("RAI2_ComCtrls.pas");
USEUNIT("RAI2_Contnrs.pas");
USEUNIT("RAI2_Controls.pas");
USEUNIT("RAI2_db.pas");
USEUNIT("RAI2_DbCtrls.pas");
USEUNIT("RAI2_DbGrids.pas");
USEUNIT("RAI2_DBTables.pas");
USEUNIT("RAI2_Dialogs.pas");
USEUNIT("RAI2_ExtCtrls.pas");
USEUNIT("RAI2_Forms.pas");
USEUNIT("RAI2_Graphics.pas");
USEUNIT("RAI2_Grids.pas");
USEUNIT("RAI2_httpapp.pas");
USEUNIT("RAI2_Menus.pas");
USEUNIT("RAI2_Quickrpt.pas");
USEUNIT("RAI2_RAEditor.pas");
USEUNIT("RAI2_RAI2.pas");
USEUNIT("RAI2_RARegAuto.pas");
USEUNIT("RAI2_RAStream.pas");
USEUNIT("RAI2_RAUtils.pas");
USEUNIT("RAI2_StdCtrls.pas");
USEUNIT("RAI2_System.pas");
USEUNIT("RAI2_SysUtils.pas");
USEUNIT("RAI2_all.pas");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEPACKAGE("Qrpt50.bpi");
USEPACKAGE("Inet50.bpi");
USEPACKAGE("ractl53.bpi");
USEPACKAGE("rai53.bpi");
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


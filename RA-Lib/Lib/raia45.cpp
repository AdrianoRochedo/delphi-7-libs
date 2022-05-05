//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("raia45.res");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("ractl45.bpi");
USEPACKAGE("rai45.bpi");
USEUNIT("RAI2_all.pas");
USEUNIT("RAI2_Classes.pas");
USEUNIT("RAI2_ComCtrls.pas");
USEUNIT("RAI2_Controls.pas");
USEUNIT("RAI2_Db.pas");
USEUNIT("RAI2_DbCtrls.pas");
USEUNIT("RAI2_DbGrids.pas");
USEUNIT("RAI2_DBTables.pas");
USEUNIT("RAI2_Dialogs.pas");
USEUNIT("RAI2_Extctrls.pas");
USEUNIT("RAI2_Forms.pas");
USEUNIT("RAI2_Graphics.pas");
USEUNIT("RAI2_Grids.pas");
USEUNIT("RAI2_httpapp.pas");
USEUNIT("RAI2_Menus.pas");
USEUNIT("RAI2_Quickrpt.pas");
USEUNIT("RAI2_RAEditor.pas");
USEUNIT("RAI2_RARegAuto.pas");
USEUNIT("RAI2_RAUtils.pas");
USEUNIT("RAI2_StdCtrls.pas");
USEUNIT("RAI2_System.pas");
USEUNIT("RAI2_SysUtils.pas");
USEUNIT("RAI2_Windows.pas");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("qrpt40.bpi");
USEPACKAGE("inet40.bpi");
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

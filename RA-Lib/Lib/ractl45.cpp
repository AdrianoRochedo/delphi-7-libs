//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ractl45.res");
USEUNIT("RAUtils.pas");
USEUNIT("IParser.pas");
USEUNIT("RAButtons.pas");
USEUNIT("RACombo.pas");
USEUNIT("RAComponentPanel.pas");
USEUNIT("RAConst.pas");
USEUNIT("RACt.pas");
USEUNIT("RADlg.pas");
USEUNIT("RADoubleCombo.pas");
USEUNIT("RADsgnIntf.pas");
USEUNIT("RAHint.pas");
USEUNIT("RAHook.pas");
USEUNIT("RAImage.pas");
USEUNIT("RARegAuto.pas");
USEUNIT("RARegAutoGrid.pas");
USEUNIT("RAScrollBar.pas");
USEUNIT("RAScrollMax.pas");
USEUNIT("RAScrollText.pas");
USEUNIT("RAStream.pas");
USEUNIT("RATreeView.pas");
USEUNIT("iMTracer.pas");
USEUNIT("RAEditor.pas");
USEUNIT("RAHLEditor.pas");
USEPACKAGE("VCL40.bpi");
USEPACKAGE("VCLX40.bpi");
USEPACKAGE("bcbsmp40.bpi");
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

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ractl53.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("IParser.pas");
USEUNIT("RAButtons.pas");
USEUNIT("RACombo.pas");
USEUNIT("RAComponentPanel.pas");
USEUNIT("RAConst.pas");
USEUNIT("RACt.pas");
USEUNIT("RADlg.pas");
USEUNIT("RADoubleCombo.pas");
USEUNIT("RADsgnIntf.pas");
USEUNIT("RAEditor.pas");
USEUNIT("RAHint.pas");
USEUNIT("RAHLEditor.pas");
USEUNIT("RAHook.pas");
USEUNIT("RAImage.pas");
USEUNIT("RARegAuto.pas");
USEUNIT("RARegAutoGrid.pas");
USEUNIT("RAScrollBar.pas");
USEUNIT("RAScrollMax.pas");
USEUNIT("RAScrollText.pas");
USEUNIT("RAStream.pas");
USEUNIT("RATreeView.pas");
USEUNIT("RAUtils.pas");
USEUNIT("iMTracer.pas");
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


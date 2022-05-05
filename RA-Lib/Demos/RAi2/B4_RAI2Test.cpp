//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("B4_RAI2Test.res");
USEFORMNS("fRAI2Test.pas", Frai2test, Test);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TTest), &Test);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------

//----------------------------------------------------------------------------
// fDBMove.hpp - bcbdcc32 generated hdr (DO NOT EDIT) rev: 0
// From: fDBMove.pas
//----------------------------------------------------------------------------
#ifndef fDBMoveHPP
#define fDBMoveHPP
//----------------------------------------------------------------------------
#include <ExtCtrls.hpp>
#include <RADlg.hpp>
#include <Buttons.hpp>
#include <RADBCt.hpp>
#include <RADBMove.hpp>
#include <ComCtrls.hpp>
#include <StdCtrls.hpp>
#include <DBTables.hpp>
#include <DB.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <System.hpp>
namespace Fdbmove
{
//-- type declarations -------------------------------------------------------
class __declspec(delphiclass) TForm1;
class __declspec(pascalimplementation) TForm1 : public Forms::TForm
{
	typedef Forms::TForm inherited;
	
__published:
	Dbtables::TTable* tSource1;
	Db::TDataSource* dsSource1;
	Dbtables::TTable* tSource2;
	Db::TDataSource* dsSource2;
	Comctrls::TPageControl* PageControl1;
	Comctrls::TTabSheet* TabSheet1;
	Dbgrids::TDBGrid* DBGrid1;
	Dbgrids::TDBGrid* DBGrid2;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TLabel* Label4;
	Comctrls::TTabSheet* TabSheet2;
	Comctrls::TTabSheet* TabSheet3;
	Dbgrids::TDBGrid* DBGrid3;
	Dbgrids::TDBGrid* DBGrid4;
	Stdctrls::TLabel* Label7;
	Stdctrls::TLabel* Label9;
	Radbmove::TRADBMove* RADBMove1;
	Radbct::TRASQLScript* RASQLScript1;
	Db::TDatabase* DestinationDatabase;
	Buttons::TBitBtn* BitBtn1;
	Buttons::TBitBtn* BitBtn2;
	Dbtables::TTable* tDestination1;
	Db::TDataSource* dsDestination1;
	Dbtables::TTable* tDestination2;
	Db::TDataSource* dsDestination2;
	Stdctrls::TLabel* Label5;
	Stdctrls::TLabel* Label6;
	Stdctrls::TLabel* Label8;
	Comctrls::TTabSheet* TabSheet4;
	Buttons::TBitBtn* BitBtn3;
	Radlg::TRAProgressForm* RAProgressForm1;
	Stdctrls::TButton* Button1;
	Extctrls::TPanel* Panel1;
	Stdctrls::TLabel* Label10;
	Stdctrls::TGroupBox* GroupBox1;
	Stdctrls::TMemo* Memo1;
	Stdctrls::TLabel* Label11;
	Stdctrls::TLabel* Label12;
	Stdctrls::TMemo* Memo2;
	Stdctrls::TLabel* Label13;
	Stdctrls::TMemo* Memo3;
	Buttons::TBitBtn* BitBtn4;
	void __fastcall BitBtn1Click(System::TObject* Sender);
	void __fastcall BitBtn2Click(System::TObject* Sender);
	void __fastcall RADBMove1MoveRecord(Radbmove::TRADBMove* Sender, Dbtables::TTable* Table, Radbmove::TMoveAction 
		&Action);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall BitBtn3Click(System::TObject* Sender);
	void __fastcall RAProgressForm1Show(System::TObject* Sender);
	void __fastcall Button1Click(System::TObject* Sender);
	void __fastcall BitBtn4Click(System::TObject* Sender);
	
private:
	int Table1Uni;
	int Table2Uni;
public:
	/* TForm.Create */ __fastcall virtual TForm1(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
		
	/* TForm.CreateNew */ __fastcall TForm1(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner
		, Dummy) { }
	/* TForm.Destroy */ __fastcall virtual ~TForm1(void) { }
	
public:
	/* TWinControl.CreateParented */ __fastcall TForm1(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
		
	
};

//-- var, const, procedure ---------------------------------------------------
extern TForm1* Form1;

}	/* namespace Fdbmove */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fdbmove;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// fDBMove

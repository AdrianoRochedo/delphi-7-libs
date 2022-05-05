// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fDBTree.pas' rev: 4.00

#ifndef fDBTreeHPP
#define fDBTreeHPP

#pragma delphiheader begin
#pragma option push -w-
#include <RADBCt.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
#include <RAHint.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <RADBUtil.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <DBTables.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <RADBTreeView.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fdbtree
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TForm1;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TForm1 : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Extctrls::TPanel* Panel1;
	Dbtables::TTable* Table1;
	Radbtreeview::TRADBTreeView* RADBTreeView1;
	Db::TDataSource* DataSource1;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TMemo* Memo1;
	Dbgrids::TDBGrid* DBGrid1;
	Radbct::TRASQLScript* RASQLScript1;
	Dbtables::TDatabase* Database1;
	Dbctrls::TDBText* DBText1;
	Rahint::TRAHint* RAHint1;
	Raregauto::TRegAuto* RegAuto1;
	Stdctrls::TLabel* Label3;
	void __fastcall Table1NewRecord(Db::TDataSet* DataSet);
	void __fastcall FormCreate(System::TObject* Sender);
	
private:
	void __fastcall GridMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y);
public:
		
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TForm1(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TForm1(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TForm1(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TForm1(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TForm1* Form1;

}	/* namespace Fdbtree */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fdbtree;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fDBTree

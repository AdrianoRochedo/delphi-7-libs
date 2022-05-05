// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fRAI2Test.pas' rev: 5.00

#ifndef fRAI2TestHPP
#define fRAI2TestHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Qrctrls.hpp>	// Pascal unit
#include <QuickRpt.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBTables.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <RAHLEditor.hpp>	// Pascal unit
#include <IParser.hpp>	// Pascal unit
#include <RAEditor.hpp>	// Pascal unit
#include <RAI2Fm.hpp>	// Pascal unit
#include <RAI2.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Frai2test
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TTest;
class PASCALIMPLEMENTATION TTest : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Raregauto::TRegAuto* RegAuto1;
	Extctrls::TPanel* Panel1;
	Dialogs::TOpenDialog* OpenDialog1;
	Dbtables::TTable* Table1;
	Db::TDataSource* DataSource1;
	Rai2fm::TRAI2Fm* RAI2Program1;
	Rahleditor::TRAHLEditor* Memo1;
	Extctrls::TPanel* Panel2;
	Extctrls::TNotebook* Notebook1;
	Stdctrls::TButton* bRunReport;
	Stdctrls::TButton* bRunForm;
	Stdctrls::TButton* Button4;
	Stdctrls::TLabel* Label1;
	Stdctrls::TButton* Button1;
	Stdctrls::TButton* Button5;
	Stdctrls::TMemo* Memo2;
	Extctrls::TPanel* pnlTime;
	Stdctrls::TLabel* Label3;
	Stdctrls::TComboBox* ComboBox1;
	Raregauto::TRegAuto* RegAuto2;
	Extctrls::TPanel* Panel3;
	Extctrls::TPanel* pnlResult;
	Stdctrls::TButton* Button2;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall Button1Click(System::TObject* Sender);
	void __fastcall RegAuto1AfterSave(System::TObject* Sender);
	void __fastcall RegAuto1AfterLoad(System::TObject* Sender);
	void __fastcall Button5Click(System::TObject* Sender);
	void __fastcall bRunFormClick(System::TObject* Sender);
	void __fastcall bRunReportClick(System::TObject* Sender);
	void __fastcall RAI2Program1GetValue(System::TObject* Sender, AnsiString Identifer, Variant &Value, 
		Rai2::TArgs* Args, bool &Done);
	void __fastcall RAI2Program1GetUnitSource(AnsiString UnitName, AnsiString &Source, bool &Done);
	void __fastcall ComboBox1Change(System::TObject* Sender);
	void __fastcall Panel1Resize(System::TObject* Sender);
	void __fastcall RAI2Program1Statement(System::TObject* Sender);
	void __fastcall Memo1KeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	void __fastcall ComboBox1DropDown(System::TObject* Sender);
	
private:
	Iparser::TIParser* Parser;
	int InternalExamplesCount;
	
public:
	Variant V;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TTest(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTest(Classes::TComponent* AOwner, int Dummy)
		 : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTest(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TTest(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TTest* Test;

}	/* namespace Frai2test */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Frai2test;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fRAI2Test

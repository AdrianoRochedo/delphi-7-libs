// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fReports.pas' rev: 5.00

#ifndef fReportsHPP
#define fReportsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <QuickRpt.hpp>	// Pascal unit
#include <RAI2Fm.hpp>	// Pascal unit
#include <RAI2.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
#include <RAButtons.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <RACt.hpp>	// Pascal unit
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

namespace Freports
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TReports;
class PASCALIMPLEMENTATION TReports : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Ract::TRAhtLabel* RAhtLabel1;
	Ract::TRAhtListBox* lbReports;
	Stdctrls::TButton* bReport;
	Raregauto::TRegAuto* RegAuto2;
	Rai2fm::TRAI2Fm* RAI2Fm1;
	Stdctrls::TButton* bCancel;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall bReportClick(System::TObject* Sender);
	void __fastcall RAI2Fm1GetValue(System::TObject* Sender, AnsiString Identifer, Variant &Value, Rai2::TArgs* 
		Args, bool &Done);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TReports(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TReports(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TReports(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TReports(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Show(void);

}	/* namespace Freports */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Freports;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fReports

// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Unit1.pas' rev: 5.00

#ifndef Unit1HPP
#define Unit1HPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DBTables.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Unit1
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDataModule1;
class PASCALIMPLEMENTATION TDataModule1 : public Forms::TDataModule 
{
	typedef Forms::TDataModule inherited;
	
__published:
	Db::TDataSource* DataSource1;
	Dbtables::TTable* Table1;
	void __fastcall DataModuleCreate(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TDataModule.Create */ inline __fastcall virtual TDataModule1(Classes::TComponent* AOwner) : Forms::TDataModule(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDataModule.CreateNew */ inline __fastcall virtual TDataModule1(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TDataModule(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDataModule.Destroy */ inline __fastcall virtual ~TDataModule1(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TDataModule1* DataModule1;

}	/* namespace Unit1 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Unit1;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Unit1

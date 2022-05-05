// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fRAEditorTest.pas' rev: 5.00

#ifndef fRAEditorTestHPP
#define fRAEditorTestHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <RAHLEditor.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <IParser.hpp>	// Pascal unit
#include <RAEditor.hpp>	// Pascal unit
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

namespace Fraeditortest
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TEditor;
class PASCALIMPLEMENTATION TEditor : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Controls::TImageList* GutterImages;
	Comctrls::TPageControl* PageControl1;
	Comctrls::TTabSheet* TabSheet1;
	Rahleditor::TRAHLEditor* RAEditor;
	Comctrls::TTabSheet* TabSheet2;
	Rahleditor::TRAHLEditor* RAEditor1;
	Comctrls::TTabSheet* TabSheet3;
	Rahleditor::TRAHLEditor* RAEditor2;
	Comctrls::TTabSheet* TabSheet4;
	Comctrls::TTabSheet* TabSheet5;
	Rahleditor::TRAHLEditor* RAEditor3;
	Raeditor::TRAEditor* RAEditor4;
	Extctrls::TPanel* Panel1;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Controls::TImageList* ilCompletions;
	Raregauto::TRegAuto* RegAuto1;
	Comctrls::TTabSheet* TabSheet6;
	Rahleditor::TRAHLEditor* RAHLEditor1;
	Comctrls::TTabSheet* TabSheet7;
	Rahleditor::TRAHLEditor* RAHLEditor2;
	Comctrls::TTabSheet* TabSheet8;
	Rahleditor::TRAHLEditor* RAHLEditor3;
	Comctrls::TTabSheet* TabSheet9;
	Rahleditor::TRAHLEditor* RAHLEditor4;
	Comctrls::TStatusBar* StatusBar1;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall RAEditorPaintGutter(System::TObject* Sender, Graphics::TCanvas* Canvas);
	void __fastcall PageControl1Change(System::TObject* Sender);
	void __fastcall PageControl1Enter(System::TObject* Sender);
	void __fastcall RAEditorCompletionDrawItem(Controls::TWinControl* Control, int Index, const Windows::TRect 
		&Rect, Windows::TOwnerDrawState State);
	void __fastcall RegAuto1AfterLoad(System::TObject* Sender);
	void __fastcall RegAuto1AfterSave(System::TObject* Sender);
	void __fastcall RAEditor3ReservedWord(System::TObject* Sender, AnsiString Token, bool &Reserved);
	
private:
	Iparser::TIParser* Parser;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TEditor(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TEditor(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TEditor(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TEditor(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TEditor* Editor;

}	/* namespace Fraeditortest */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fraeditortest;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fRAEditorTest

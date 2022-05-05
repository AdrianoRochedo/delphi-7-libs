// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fMain.pas' rev: 5.00

#ifndef fMainHPP
#define fMainHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <RACt.hpp>	// Pascal unit
#include <RAI2.hpp>	// Pascal unit
#include <ShellAPI.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <RAHLEditor.hpp>	// Pascal unit
#include <RAEditor.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
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

namespace Fmain
{
//-- type declarations -------------------------------------------------------
typedef AnsiString fMain__2[10];

class DELPHICLASS TMain;
class PASCALIMPLEMENTATION TMain : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Raregauto::TRegAuto* RegAuto1;
	Rahleditor::TRAHLEditor* RAHLEditor1;
	Ract::TRAStatusBar* StatusBar;
	Menus::TMainMenu* MainMenu1;
	Menus::TMenuItem* miFile;
	Menus::TMenuItem* miFileOpen;
	Menus::TMenuItem* N1;
	Menus::TMenuItem* miExit;
	Menus::TMenuItem* miFileSave;
	Menus::TMenuItem* miFileSaveAs;
	Raregauto::TRegAuto* raCommon;
	Menus::TMenuItem* N2;
	Menus::TMenuItem* miHelpAbout;
	Dialogs::TOpenDialog* OpenDialog1;
	Dialogs::TSaveDialog* SaveDialog1;
	Dialogs::TFindDialog* FindDialog1;
	Dialogs::TReplaceDialog* ReplaceDialog1;
	Menus::TMenuItem* miSearch;
	Menus::TMenuItem* Search1;
	Menus::TMenuItem* miSearchAgain;
	Menus::TMenuItem* miSearchReplace;
	Menus::TMenuItem* N3;
	Menus::TMenuItem* miOptions;
	Menus::TPopupMenu* PopupMenu1;
	Menus::TMenuItem* miEditorProperties;
	Rai2::TRAI2Program* RAI2Program1;
	Controls::TImageList* GutterImages;
	void __fastcall RAHLEditor1ChangeStatus(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall miFileSaveClick(System::TObject* Sender);
	void __fastcall miHelpAboutClick(System::TObject* Sender);
	void __fastcall miExitClick(System::TObject* Sender);
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	void __fastcall raCommonAfterLoad(System::TObject* Sender);
	void __fastcall miFileOpenClick(System::TObject* Sender);
	void __fastcall miFileSaveAsClick(System::TObject* Sender);
	void __fastcall Search1Click(System::TObject* Sender);
	void __fastcall miSearchAgainClick(System::TObject* Sender);
	void __fastcall miSearchReplaceClick(System::TObject* Sender);
	void __fastcall miOptionsClick(System::TObject* Sender);
	void __fastcall miEditorPropertiesClick(System::TObject* Sender);
	void __fastcall raCommonAfterSave(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall RAI2Program1GetValue(System::TObject* Sender, AnsiString Identifer, Variant &Value, 
		Rai2::TArgs* Args, bool &Done);
	void __fastcall RAHLEditor1KeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
		
	void __fastcall RAHLEditor1KeyUp(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	void __fastcall RAHLEditor1KeyPress(System::TObject* Sender, char &Key);
	void __fastcall RAI2Program1GetUnitSource(AnsiString UnitName, AnsiString &Source, bool &Done);
	void __fastcall FindDialog1Find(System::TObject* Sender);
	void __fastcall RAHLEditor1PaintGutter(System::TObject* Sender, Graphics::TCanvas* Canvas);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall RAI2Program1SetValue(System::TObject* Sender, AnsiString Identifer, const Variant &
		Value, Rai2::TArgs* Args, bool &Done);
	
private:
	AnsiString FFileName;
	int FileTime;
	AnsiString Exts[10];
	AnsiString Capt;
	int BaseLine;
	void __fastcall OpenFile(AnsiString AFileName);
	void __fastcall SetHighlighter(void);
	void __fastcall LoadColors(void);
	void __fastcall UpdateCaption(void);
	void __fastcall CheckSave(void);
	void __fastcall UpdateEditorSettings(void);
	MESSAGE void __fastcall WMDropFiles(Messages::TMessage &Message);
	void __fastcall FindNext(void);
	void __fastcall CheckFileModified(void);
	void __fastcall ApplicationActivate(System::TObject* Sender);
	MESSAGE void __fastcall WMCheckFileModified(Messages::TMessage &Message);
	AnsiString RAI2FileName;
	Rai2::TArgs* Args;
	void __fastcall ErrorLogFmt(const AnsiString Message, const System::TVarRec * Args, const int Args_Size
		);
	bool __fastcall RAI2Script(void);
	void __fastcall RAI2Initialize(void);
	void __fastcall RAI2UnInitialize(void);
	Variant __fastcall RAI2SafeCall(const AnsiString FunName, Rai2::TArgs* Args, const Variant * Params
		, const int Params_Size);
	void __fastcall RAI2FileOpened(void);
	void __fastcall RAI2FileClosed(void);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TMain(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMain(Classes::TComponent* AOwner, int Dummy)
		 : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMain(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMain(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word WM_CHECKFILEMODIFIED = 0x501;
extern PACKAGE TMain* Main;

}	/* namespace Fmain */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fmain;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fMain

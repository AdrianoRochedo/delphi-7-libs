// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'fRAControls.pas' rev: 5.00

#ifndef fRAControlsHPP
#define fRAControlsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <RAScrollText.hpp>	// Pascal unit
#include <RADlg.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <RACombo.hpp>	// Pascal unit
#include <RACt.hpp>	// Pascal unit
#include <RAImage.hpp>	// Pascal unit
#include <RAScrollBar.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <RAScrollMax.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <RATreeView.hpp>	// Pascal unit
#include <RAButtons.hpp>	// Pascal unit
#include <RARegAuto.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <RAComponentPanel.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
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

namespace Fracontrols
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMainForm;
class PASCALIMPLEMENTATION TMainForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Menus::TPopupMenu* PopupMenu1;
	Menus::TMenuItem* Item1;
	Menus::TMenuItem* Item2;
	Comctrls::TTabControl* TabControl1;
	Racomponentpanel::TRAComponentPanel* RAComponentPanel1;
	Stdctrls::TMemo* Memo1;
	Raregauto::TRegAuto* RegAuto2;
	Rabuttons::TRACaptionButton* RACaptionButton1;
	Rabuttons::TRACaptionButton* RACaptionButton2;
	Rabuttons::TRACaptionButton* RACaptionButton3;
	Controls::TImageList* ImageList1;
	Raregauto::TRegAuto* RegAuto1;
	Ract::TRAStatusBar* RAStatusBar1;
	Comctrls::TProgressBar* ProgressBar1;
	Extctrls::TNotebook* Notebook1;
	Extctrls::TPanel* Panel2;
	Ratreeview::TRAComboBox4Tree* RATreeComboBox1;
	Ratreeview::TRATreeView* RATreeView1;
	Raimage::TRAImage* RAImage1;
	Stdctrls::TLabel* Label20;
	Rascrollmax::TRAScrollMax* RAScrollMax1;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand1;
	Stdctrls::TLabel* Label11;
	Stdctrls::TLabel* Label12;
	Stdctrls::TLabel* Label16;
	Stdctrls::TEdit* Edit9;
	Stdctrls::TEdit* Edit10;
	Stdctrls::TEdit* Edit14;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand2;
	Stdctrls::TLabel* Label13;
	Stdctrls::TLabel* Label14;
	Stdctrls::TLabel* Label15;
	Stdctrls::TEdit* Edit11;
	Stdctrls::TEdit* Edit12;
	Stdctrls::TEdit* Edit13;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand3;
	Stdctrls::TLabel* Label17;
	Stdctrls::TLabel* Label18;
	Stdctrls::TEdit* Edit15;
	Stdctrls::TEdit* Edit16;
	Ract::TRACheckBox* RACheckBox1;
	Stdctrls::TEdit* Edit1;
	Rabuttons::TRAColorButton* RAColorButton1;
	Rabuttons::TRANoFrameButton* RANoFrameButton1;
	Rabuttons::TRANoFrameButton* RANoFrameButton2;
	Racombo::TRACombo* RACombo1;
	Rascrollbar::TRAScrollBar* RAScrollBar1;
	Stdctrls::TLabel* Label1;
	Rascrollbar::TRAScrollBar95* RAScrollBar951;
	Rascrollbar::TRAScrollBar95* RAScrollBar952;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TLabel* Label4;
	Stdctrls::TLabel* Label5;
	Racombo::TRACombo* RACombo2;
	Stdctrls::TLabel* Label6;
	Rabuttons::TRANoFrameButton* RANoFrameButton3;
	Rabuttons::TRANoFrameButton* RANoFrameButton4;
	Rabuttons::TRANoFrameButton* RANoFrameButton5;
	Rascrollmax::TRAScrollMax* RAScrollMax2;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand4;
	Stdctrls::TLabel* Label7;
	Stdctrls::TLabel* Label8;
	Stdctrls::TLabel* Label9;
	Stdctrls::TEdit* Edit2;
	Stdctrls::TEdit* Edit3;
	Stdctrls::TEdit* Edit4;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand5;
	Stdctrls::TLabel* Label10;
	Stdctrls::TLabel* Label21;
	Stdctrls::TLabel* Label22;
	Stdctrls::TEdit* Edit5;
	Stdctrls::TEdit* Edit6;
	Stdctrls::TEdit* Edit7;
	Rascrollmax::TRAScrollMaxBand* RAScrollMaxBand6;
	Stdctrls::TLabel* Label23;
	Stdctrls::TLabel* Label24;
	Stdctrls::TEdit* Edit8;
	Stdctrls::TEdit* Edit17;
	Stdctrls::TCheckBox* CheckBox1;
	Stdctrls::TLabel* Label19;
	Stdctrls::TLabel* Label25;
	Stdctrls::TListBox* ListBox1;
	Ract::TRAhtListBox* RAhtListBox1;
	Stdctrls::TMemo* Memo2;
	Stdctrls::TButton* Button1;
	Stdctrls::TCheckBox* CheckBox2;
	Stdctrls::TButton* Button2;
	Stdctrls::TButton* Button3;
	Radlg::TRAProgressForm* RAProgressForm1;
	Stdctrls::TLabel* Label26;
	Ract::TRAhtComboBox* RAhtComboBox1;
	Rascrolltext::TRAScrollText* RAScrollText1;
	Stdctrls::TLabel* Label27;
	Ract::TRAhtLabel* RAhtLabel1;
	Rabuttons::TRAhtButton* RAhtButton1;
	Ract::TRAhtLabel* RAhtLabel2;
	void __fastcall RAComponentPanel1Click(System::TObject* Sender, int Button);
	void __fastcall TabControl1Change(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall RACaptionButton1Click(System::TObject* Sender);
	void __fastcall RANoFrameButton1Click(System::TObject* Sender);
	void __fastcall RAColorButton1Click(System::TObject* Sender);
	void __fastcall RAImage1KeyPress(System::TObject* Sender, char &Key);
	void __fastcall RAScrollBar1Scroll(System::TObject* Sender);
	void __fastcall CheckBox1Click(System::TObject* Sender);
	void __fastcall Button1Click(System::TObject* Sender);
	void __fastcall CheckBox2Click(System::TObject* Sender);
	void __fastcall Button2Click(System::TObject* Sender);
	void __fastcall RAProgressForm1Show(System::TObject* Sender);
	void __fastcall Button3Click(System::TObject* Sender);
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMainForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMainForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMainForm* MainForm;

}	/* namespace Fracontrols */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Fracontrols;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// fRAControls

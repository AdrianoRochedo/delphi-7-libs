{***********************************************************
                R&A Library
                   RAI2
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : adapter unit - converts RAI2 calls to delphi calls

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}
{$INCLUDE RA.INC}

unit RAI2_Forms;

interface

uses RAI2;

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);

implementation

uses Windows, SysUtils, Classes, Controls, Menus, Graphics, Forms;


  { TControlScrollBar }

{  procedure Assign(Source: TPersistent); }
procedure TControlScrollBar_Assign(var Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Kind: TScrollBarKind }
procedure TControlScrollBar_Read_Kind(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Kind;
end;

{ property Read ScrollPos: Integer }
procedure TControlScrollBar_Read_ScrollPos(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).ScrollPos;
end;

{ property Read Margin: Word }
procedure TControlScrollBar_Read_Margin(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Margin;
end;

{ property Write Margin(Value: Word) }
procedure TControlScrollBar_Write_Margin(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Margin := Value;
end;

{ property Read Increment: TScrollBarInc }
procedure TControlScrollBar_Read_Increment(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Increment;
end;

{ property Write Increment(Value: TScrollBarInc) }
procedure TControlScrollBar_Write_Increment(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Increment := Value;
end;

{ property Read Range: Integer }
procedure TControlScrollBar_Read_Range(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Range;
end;

{ property Write Range(Value: Integer) }
procedure TControlScrollBar_Write_Range(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Range := Value;
end;

{ property Read Position: Integer }
procedure TControlScrollBar_Read_Position(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Position;
end;

{ property Write Position(Value: Integer) }
procedure TControlScrollBar_Write_Position(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Position := Value;
end;

{ property Read Tracking: Boolean }
procedure TControlScrollBar_Read_Tracking(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Tracking;
end;

{ property Write Tracking(Value: Boolean) }
procedure TControlScrollBar_Write_Tracking(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Tracking := Value;
end;

{ property Read Visible: Boolean }
procedure TControlScrollBar_Read_Visible(var Value: Variant; Args: TArgs);
begin
  Value := TControlScrollBar(Args.Obj).Visible;
end;

{ property Write Visible(Value: Boolean) }
procedure TControlScrollBar_Write_Visible(const Value: Variant; Args: TArgs);
begin
  TControlScrollBar(Args.Obj).Visible := Value;
end;

  { TScrollingWinControl }

{ constructor Create(AOwner: TComponent) }
procedure TScrollingWinControl_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScrollingWinControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{$IFDEF RA_D3H}
{  procedure DisableAutoRange; }
procedure TScrollingWinControl_DisableAutoRange(var Value: Variant; Args: TArgs);
begin
  TScrollingWinControl(Args.Obj).DisableAutoRange;
end;

{  procedure EnableAutoRange; }
procedure TScrollingWinControl_EnableAutoRange(var Value: Variant; Args: TArgs);
begin
  TScrollingWinControl(Args.Obj).EnableAutoRange;
end;
{$ENDIF RA_D3H}

{  procedure ScrollInView(AControl: TControl); }
procedure TScrollingWinControl_ScrollInView(var Value: Variant; Args: TArgs);
begin
  TScrollingWinControl(Args.Obj).ScrollInView(V2O(Args.Values[0]) as TControl);
end;

{ property Read HorzScrollBar: TControlScrollBar }
procedure TScrollingWinControl_Read_HorzScrollBar(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScrollingWinControl(Args.Obj).HorzScrollBar);
end;

{ property Write HorzScrollBar(Value: TControlScrollBar) }
procedure TScrollingWinControl_Write_HorzScrollBar(const Value: Variant; Args: TArgs);
begin
  TScrollingWinControl(Args.Obj).HorzScrollBar := V2O(Value) as TControlScrollBar;
end;

{ property Read VertScrollBar: TControlScrollBar }
procedure TScrollingWinControl_Read_VertScrollBar(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScrollingWinControl(Args.Obj).VertScrollBar);
end;

{ property Write VertScrollBar(Value: TControlScrollBar) }
procedure TScrollingWinControl_Write_VertScrollBar(const Value: Variant; Args: TArgs);
begin
  TScrollingWinControl(Args.Obj).VertScrollBar := V2O(Value) as TControlScrollBar;
end;

  { TScrollBox }

{ constructor Create(AOwner: TComponent) }
procedure TScrollBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScrollBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read BorderStyle: TBorderStyle }
procedure TScrollBox_Read_BorderStyle(var Value: Variant; Args: TArgs);
begin
  Value := TScrollBox(Args.Obj).BorderStyle;
end;

{ property Write BorderStyle(Value: TBorderStyle) }
procedure TScrollBox_Write_BorderStyle(const Value: Variant; Args: TArgs);
begin
  TScrollBox(Args.Obj).BorderStyle := Value;
end;

  { TCustomForm }

{$IFNDEF RA_D3H}
type
  TCustomForm = TForm;
{$ENDIF RA_D3H}

{  function CloseQuery: Boolean; }
procedure TCustomForm_CloseQuery(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).CloseQuery;
end;

{  procedure Close; }
procedure TCustomForm_Close(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Close;
end;


{  procedure DefocusControl(Control: TWinControl; Removing: Boolean); }
procedure TCustomForm_DefocusControl(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).DefocusControl(V2O(Args.Values[0]) as TWinControl, Args.Values[1]);
end;

{  procedure FocusControl(Control: TWinControl); }
procedure TCustomForm_FocusControl(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).FocusControl(V2O(Args.Values[0]) as TWinControl);
end;

{  function GetFormImage: TBitmap; }
procedure TCustomForm_GetFormImage(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).GetFormImage);
end;

{  procedure Hide; }
procedure TCustomForm_Hide(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Hide;
end;

{  procedure Print; }
procedure TCustomForm_Print(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Print;
end;

{  procedure Release; }
procedure TCustomForm_Release(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Release;
end;

{  procedure SendCancelMode(Sender: TControl); }
procedure TCustomForm_SendCancelMode(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).SendCancelMode(V2O(Args.Values[0]) as TControl);
end;

{  procedure SetFocus; }
procedure TCustomForm_SetFocus(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).SetFocus;
end;

{  function SetFocusedControl(Control: TWinControl): Boolean; }
procedure TCustomForm_SetFocusedControl(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).SetFocusedControl(V2O(Args.Values[0]) as TWinControl);
end;

{  procedure Show; }
procedure TCustomForm_Show(var Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Show;
end;

{  function ShowModal: Integer; }
procedure TCustomForm_ShowModal(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).ShowModal;
end;

{ property Read Active: Boolean }
procedure TCustomForm_Read_Active(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).Active;
end;

{ property Read ActiveControl: TWinControl }
procedure TCustomForm_Read_ActiveControl(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).ActiveControl);
end;

{ property Write ActiveControl(Value: TWinControl) }
procedure TCustomForm_Write_ActiveControl(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).ActiveControl := V2O(Value) as TWinControl;
end;

{ property Read ActiveOleControl: TWinControl }
procedure TCustomForm_Read_ActiveOleControl(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).ActiveOleControl);
end;

{ property Write ActiveOleControl(Value: TWinControl) }
procedure TCustomForm_Write_ActiveOleControl(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).ActiveOleControl := V2O(Value) as TWinControl;
end;

{ property Read Canvas: TCanvas }
procedure TCustomForm_Read_Canvas(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).Canvas);
end;

{ property Read DropTarget: Boolean }
procedure TCustomForm_Read_DropTarget(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).DropTarget;
end;

{ property Write DropTarget(Value: Boolean) }
procedure TCustomForm_Write_DropTarget(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).DropTarget := Value;
end;

{$IFDEF RA_D3H}
{ property Read HelpFile: string }
procedure TCustomForm_Read_HelpFile(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).HelpFile;
end;

{ property Write HelpFile(Value: string) }
procedure TCustomForm_Write_HelpFile(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).HelpFile := Value;
end;
{$ENDIF RA_D3H}

{ property Read KeyPreview: Boolean }
procedure TCustomForm_Read_KeyPreview(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).KeyPreview;
end;

{ property Write KeyPreview(Value: Boolean) }
procedure TCustomForm_Write_KeyPreview(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).KeyPreview := Value;
end;

{ property Read Menu: TMainMenu }
procedure TCustomForm_Read_Menu(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).Menu);
end;

{ property Write Menu(Value: TMainMenu) }
procedure TCustomForm_Write_Menu(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).Menu := V2O(Value) as TMainMenu;
end;

{ property Read ModalResult: TModalResult }
procedure TCustomForm_Read_ModalResult(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).ModalResult;
end;

{ property Write ModalResult(Value: TModalResult) }
procedure TCustomForm_Write_ModalResult(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).ModalResult := Value;
end;

{ property Read WindowState: TWindowState }
procedure TCustomForm_Read_WindowState(var Value: Variant; Args: TArgs);
begin
  Value := TCustomForm(Args.Obj).WindowState;
end;

{ property Write WindowState(Value: TWindowState) }
procedure TCustomForm_Write_WindowState(const Value: Variant; Args: TArgs);
begin
  TCustomForm(Args.Obj).WindowState := Value;
end;

  { TForm }

{ constructor Create(AOwner: TComponent) }
procedure TForm_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TForm.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateNew(AOwner: TComponent) }
procedure TForm_CreateNew(var Value: Variant; Args: TArgs);
begin
{$IFDEF RA_D}
  Value := O2V(TForm.CreateNew(V2O(Args.Values[0]) as TComponent));
{$ELSE}
  Value := O2V(TForm.CreateNew(V2O(Args.Values[0]) as TComponent, 1));
{$ENDIF}
end;

{  procedure ArrangeIcons; }
procedure TForm_ArrangeIcons(var Value: Variant; Args: TArgs);
begin
  TForm(Args.Obj).ArrangeIcons;
end;

{  procedure Cascade; }
procedure TForm_Cascade(var Value: Variant; Args: TArgs);
begin
  TForm(Args.Obj).Cascade;
end;

{  procedure Next; }
procedure TForm_Next(var Value: Variant; Args: TArgs);
begin
  TForm(Args.Obj).Next;
end;

{  procedure Previous; }
procedure TForm_Previous(var Value: Variant; Args: TArgs);
begin
  TForm(Args.Obj).Previous;
end;

{  procedure Tile; }
procedure TForm_Tile(var Value: Variant; Args: TArgs);
begin
  TForm(Args.Obj).Tile;
end;

  { TDataModule }

{ constructor Create(AOwner: TComponent) }
procedure TDataModule_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TDataModule.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateNew(AOwner: TComponent) }
procedure TDataModule_CreateNew(var Value: Variant; Args: TArgs);
begin
{$IFDEF RA_D}
  Value := O2V(TDataModule.CreateNew(V2O(Args.Values[0]) as TComponent));
{$ELSE}
  Value := O2V(TDataModule.CreateNew(V2O(Args.Values[0]) as TComponent, 1));
{$ENDIF}
end;

  { TScreen }

{ constructor Create(AOwner: TComponent) }
procedure TScreen_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ActiveControl: TWinControl }
procedure TScreen_Read_ActiveControl(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveControl);
end;

{$IFDEF RA_D3H}
{ property Read ActiveCustomForm: TCustomForm }
procedure TScreen_Read_ActiveCustomForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveCustomForm);
end;
{$ENDIF RA_D3H}

{ property Read ActiveForm: TForm }
procedure TScreen_Read_ActiveForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveForm);
end;

{$IFDEF RA_D3H}
{ property Read CustomFormCount: Integer }
procedure TScreen_Read_CustomFormCount(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).CustomFormCount;
end;

{ property Read CustomForms[Integer]: TCustomForm }
procedure TScreen_Read_CustomForms(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).CustomForms[Args.Values[0]]);
end;
{$ENDIF RA_D3H}

{ property Read Cursor: TCursor }
procedure TScreen_Read_Cursor(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).Cursor;
end;

{ property Write Cursor(Value: TCursor) }
procedure TScreen_Write_Cursor(const Value: Variant; Args: TArgs);
begin
  TScreen(Args.Obj).Cursor := Value;
end;

{ property Read Cursors[Integer]: HCURSOR }
procedure TScreen_Read_Cursors(var Value: Variant; Args: TArgs);
begin
  Value := Integer(TScreen(Args.Obj).Cursors[Args.Values[0]]);
end;

{ property Write Cursors[Integer]: HCURSOR }
procedure TScreen_Write_Cursors(const Value: Variant; Args: TArgs);
begin
  TScreen(Args.Obj).Cursors[Args.Values[0]] := Value;
end;

{ property Read DataModules[Integer]: TDataModule }
procedure TScreen_Read_DataModules(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).DataModules[Args.Values[0]]);
end;

{ property Read DataModuleCount: Integer }
procedure TScreen_Read_DataModuleCount(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).DataModuleCount;
end;

{$IFDEF RA_D3H}
{ property Read IconFont: TFont }
procedure TScreen_Read_IconFont(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).IconFont);
end;

{ property Write IconFont(Value: TFont) }
procedure TScreen_Write_IconFont(const Value: Variant; Args: TArgs);
begin
  TScreen(Args.Obj).IconFont := V2O(Value) as TFont;
end;
{$ENDIF RA_D3H}

{ property Read Fonts: TStrings }
procedure TScreen_Read_Fonts(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).Fonts);
end;

{ property Read FormCount: Integer }
procedure TScreen_Read_FormCount(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).FormCount;
end;

{ property Read Forms[Integer]: TForm }
procedure TScreen_Read_Forms(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).Forms[Args.Values[0]]);
end;

{$IFDEF RA_D3H}
{ property Read Imes: TStrings }
procedure TScreen_Read_Imes(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScreen(Args.Obj).Imes);
end;

{ property Read DefaultIme: string }
procedure TScreen_Read_DefaultIme(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).DefaultIme;
end;

{ property Read DefaultKbLayout: HKL }
procedure TScreen_Read_DefaultKbLayout(var Value: Variant; Args: TArgs);
begin
  Value := Integer(TScreen(Args.Obj).DefaultKbLayout);
end;
{$ENDIF RA_D3H}

{ property Read Height: Integer }
procedure TScreen_Read_Height(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).Height;
end;

{ property Read PixelsPerInch: Integer }
procedure TScreen_Read_PixelsPerInch(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).PixelsPerInch;
end;

{ property Read Width: Integer }
procedure TScreen_Read_Width(var Value: Variant; Args: TArgs);
begin
  Value := TScreen(Args.Obj).Width;
end;

  { TApplication }

{ constructor Create(AOwner: TComponent) }
procedure TApplication_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TApplication.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure BringToFront; }
procedure TApplication_BringToFront(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).BringToFront;
end;

{  procedure ControlDestroyed(Control: TControl); }
procedure TApplication_ControlDestroyed(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).ControlDestroyed(V2O(Args.Values[0]) as TControl);
end;

{  procedure CancelHint; }
procedure TApplication_CancelHint(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).CancelHint;
end;

{  procedure CreateForm(InstanceClass: TComponentClass; var Reference); }
procedure TApplication_CreateForm(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).CreateForm(TComponentClass(V2C(Args.Values[0])), Args.Values[1]);
end;

{  procedure CreateHandle; }
procedure TApplication_CreateHandle(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).CreateHandle;
end;

{  procedure HandleException(Sender: TObject); }
procedure TApplication_HandleException(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HandleException(V2O(Args.Values[0]));
end;

{  procedure HandleMessage; }
procedure TApplication_HandleMessage(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HandleMessage;
end;

{  function HelpCommand(Command: Integer; Data: Longint): Boolean; }
procedure TApplication_HelpCommand(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HelpCommand(Args.Values[0], Args.Values[1]);
end;

{  function HelpContext(Context: THelpContext): Boolean; }
procedure TApplication_HelpContext(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HelpContext(Args.Values[0]);
end;

{  function HelpJump(const JumpID: string): Boolean; }
procedure TApplication_HelpJump(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HelpJump(Args.Values[0]);
end;

{  procedure HideHint; }
procedure TApplication_HideHint(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HideHint;
end;

{  procedure Initialize; }
procedure TApplication_Initialize(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Initialize;
end;

{  function MessageBox(Text, Caption: PChar; Flags: Longint): Integer; }
procedure TApplication_MessageBox(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).MessageBox(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{  procedure Minimize; }
procedure TApplication_Minimize(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Minimize;
end;

{$IFDEF RA_D3H}
{  procedure NormalizeAllTopMosts; }
procedure TApplication_NormalizeAllTopMosts(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).NormalizeAllTopMosts;
end;
{$ENDIF RA_D3H}

{  procedure NormalizeTopMosts; }
procedure TApplication_NormalizeTopMosts(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).NormalizeTopMosts;
end;

{  procedure ProcessMessages; }
procedure TApplication_ProcessMessages(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).ProcessMessages;
end;

{  procedure Restore; }
procedure TApplication_Restore(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Restore;
end;

{  procedure RestoreTopMosts; }
procedure TApplication_RestoreTopMosts(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).RestoreTopMosts;
end;

{  procedure Run; }
procedure TApplication_Run(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Run;
end;

{  procedure ShowException(E: Exception); }
procedure TApplication_ShowException(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).ShowException(V2O(Args.Values[0]) as Exception);
end;

{  procedure Terminate; }
procedure TApplication_Terminate(var Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Terminate;
end;

{ property Read Active: Boolean }
procedure TApplication_Read_Active(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).Active;
end;

{$IFDEF RA_D3H}
{ property Read AllowTesting: Boolean }
procedure TApplication_Read_AllowTesting(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).AllowTesting;
end;

{ property Write AllowTesting(Value: Boolean) }
procedure TApplication_Write_AllowTesting(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).AllowTesting := Value;
end;

{ property Read CurrentHelpFile: string }
procedure TApplication_Read_CurrentHelpFile(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).CurrentHelpFile;
end;
{$ENDIF RA_D3H}

{ property Read DialogHandle: HWnd }
procedure TApplication_Read_DialogHandle(var Value: Variant; Args: TArgs);
begin
  Value := Integer(TApplication(Args.Obj).DialogHandle);
end;

{ property Write DialogHandle(Value: HWnd) }
procedure TApplication_Write_DialogHandle(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).DialogHandle := Value;
end;

{ property Read ExeName: string }
procedure TApplication_Read_ExeName(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).ExeName;
end;

{ property Read Handle: HWnd }
procedure TApplication_Read_Handle(var Value: Variant; Args: TArgs);
begin
  Value := Integer(TApplication(Args.Obj).Handle);
end;

{ property Write Handle(Value: HWnd) }
procedure TApplication_Write_Handle(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Handle := Value;
end;

{ property Read HelpFile: string }
procedure TApplication_Read_HelpFile(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HelpFile;
end;

{ property Write HelpFile(Value: string) }
procedure TApplication_Write_HelpFile(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HelpFile := Value;
end;

{ property Read Hint: string }
procedure TApplication_Read_Hint(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).Hint;
end;

{ property Write Hint(Value: string) }
procedure TApplication_Write_Hint(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Hint := Value;
end;

{ property Read HintColor: TColor }
procedure TApplication_Read_HintColor(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HintColor;
end;

{ property Write HintColor(Value: TColor) }
procedure TApplication_Write_HintColor(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HintColor := Value;
end;

{ property Read HintPause: Integer }
procedure TApplication_Read_HintPause(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HintPause;
end;

{ property Write HintPause(Value: Integer) }
procedure TApplication_Write_HintPause(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HintPause := Value;
end;

{ property Read HintShortPause: Integer }
procedure TApplication_Read_HintShortPause(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HintShortPause;
end;

{ property Write HintShortPause(Value: Integer) }
procedure TApplication_Write_HintShortPause(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HintShortPause := Value;
end;

{ property Read HintHidePause: Integer }
procedure TApplication_Read_HintHidePause(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).HintHidePause;
end;

{ property Write HintHidePause(Value: Integer) }
procedure TApplication_Write_HintHidePause(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).HintHidePause := Value;
end;

{ property Read Icon: TIcon }
procedure TApplication_Read_Icon(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TApplication(Args.Obj).Icon);
end;

{ property Write Icon(Value: TIcon) }
procedure TApplication_Write_Icon(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Icon := V2O(Value) as TIcon;
end;

{ property Read MainForm: TForm }
procedure TApplication_Read_MainForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TApplication(Args.Obj).MainForm);
end;

{ property Read ShowHint: Boolean }
procedure TApplication_Read_ShowHint(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).ShowHint;
end;

{ property Write ShowHint(Value: Boolean) }
procedure TApplication_Write_ShowHint(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).ShowHint := Value;
end;

{ property Read ShowMainForm: Boolean }
procedure TApplication_Read_ShowMainForm(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).ShowMainForm;
end;

{ property Write ShowMainForm(Value: Boolean) }
procedure TApplication_Write_ShowMainForm(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).ShowMainForm := Value;
end;

{ property Read Terminated: Boolean }
procedure TApplication_Read_Terminated(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).Terminated;
end;

{ property Read Title: string }
procedure TApplication_Read_Title(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).Title;
end;

{ property Write Title(Value: string) }
procedure TApplication_Write_Title(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).Title := Value;
end;

{ property Read UpdateFormatSettings: Boolean }
procedure TApplication_Read_UpdateFormatSettings(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).UpdateFormatSettings;
end;

{ property Write UpdateFormatSettings(Value: Boolean) }
procedure TApplication_Write_UpdateFormatSettings(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).UpdateFormatSettings := Value;
end;

{$IFDEF RA_D3H}
{ property Read UpdateMetricSettings: Boolean }
procedure TApplication_Read_UpdateMetricSettings(var Value: Variant; Args: TArgs);
begin
  Value := TApplication(Args.Obj).UpdateMetricSettings;
end;

{ property Write UpdateMetricSettings(Value: Boolean) }
procedure TApplication_Write_UpdateMetricSettings(const Value: Variant; Args: TArgs);
begin
  TApplication(Args.Obj).UpdateMetricSettings := Value;
end;
{$ENDIF RA_D3H}



{ Application global variable }
procedure RAI2_Application(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Application);
end;

{ Screen global variable }
procedure RAI2_Screen(var Value: Variant; Args: TArgs);
begin
  Value := O2V(Screen);
end;


{ functions }

{ function GetParentForm(Control: TControl): TCustomForm; }
procedure RAI2_GetParentForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(GetParentForm(V2O(Args.Values[0]) as TControl));
end;

{ function ValidParentForm(Control: TControl): TCustomForm; }
procedure RAI2_ValidParentForm(var Value: Variant; Args: TArgs);
begin
  Value := O2V(ValidParentForm(V2O(Args.Values[0]) as TControl));
end;

{ function DisableTaskWindows(ActiveWindow: HWnd): Pointer; }
procedure RAI2_DisableTaskWindows(var Value: Variant; Args: TArgs);
begin
  Value := P2V(DisableTaskWindows(Args.Values[0]));
end;

{ procedure EnableTaskWindows(WindowList: Pointer); }
procedure RAI2_EnableTaskWindows(var Value: Variant; Args: TArgs);
begin
  EnableTaskWindows(V2P(Args.Values[0]));
end;

{ function IsAccel(VK: Word; const Str: string): Boolean; }
procedure RAI2_IsAccel(var Value: Variant; Args: TArgs);
begin
  Value := IsAccel(Args.Values[0], Args.Values[1]);
end;

{ function KeysToShiftState(Keys: Word): TShiftState; }
procedure RAI2_KeysToShiftState(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Byte(KeysToShiftState(Args.Values[0])));
end;

{ function KeyDataToShiftState(KeyData: Longint): TShiftState; }
procedure RAI2_KeyDataToShiftState(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Byte(KeyDataToShiftState(Args.Values[0])));
end;

{$IFDEF RA_D3H}
{ function ForegroundTask: Boolean; }
procedure RAI2_ForegroundTask(var Value: Variant; Args: TArgs);
begin
  Value := ForegroundTask;
end;
{$ENDIF RA_D3H}


type
  
  TRAI2FormsEvent = class(TRAI2Event)
  private
    procedure CloseEvent(Sender: TObject; var Action: TCloseAction);
    procedure CloseQueryEvent(Sender: TObject; var CanClose: Boolean);
    procedure MessageEvent(var Msg: TMsg; var Handled: Boolean);
    procedure ExceptionEvent(Sender: TObject; E: Exception);
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure ShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  end;

procedure TRAI2FormsEvent.CloseEvent(Sender: TObject; var Action: TCloseAction);
begin
  CallFunction(nil, [O2V(Sender), Action]);
  Action := Args.Values[1];
end;

procedure TRAI2FormsEvent.CloseQueryEvent(Sender: TObject; var CanClose: Boolean);
begin
  CallFunction(nil, [O2V(Sender), CanClose]);
  CanClose := Args.Values[1];
end;

procedure TRAI2FormsEvent.MessageEvent(var Msg: TMsg; var Handled: Boolean);
begin
//  CallFunction(nil, [O2V(Sender)]);
  NotImplemented('TMessageEvent');
end;

procedure TRAI2FormsEvent.ExceptionEvent(Sender: TObject; E: Exception);
begin
  CallFunction(nil, [O2V(Sender), O2V(E)]);
end;

procedure TRAI2FormsEvent.IdleEvent(Sender: TObject; var Done: Boolean);
begin
  CallFunction(nil, [O2V(Sender), Done]);
  Done := Args.Values[1];
end;

procedure TRAI2FormsEvent.ShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
{  CallFunction(nil, [HintStr, CanShow, HintInfo]);
  HintStr := Args.Values[0];
  CanShow := Args.Values[1];
  HintInfo := Args.Values[2]; }
  NotImplemented('TShowHintEvent');
 { need define record THintInfo } 
end;


procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
   { TScrollBarKind }
    AddConst('Forms', 'sbHorizontal', sbHorizontal);
    AddConst('Forms', 'sbVertical', sbVertical);
   { TControlScrollBar }
    AddClass('Forms', TControlScrollBar, 'TControlScrollBar');
    AddGet(TControlScrollBar, 'Assign', TControlScrollBar_Assign, 1, [varEmpty], varEmpty);
    AddGet(TControlScrollBar, 'Kind', TControlScrollBar_Read_Kind, 0, [0], varEmpty);
    AddGet(TControlScrollBar, 'ScrollPos', TControlScrollBar_Read_ScrollPos, 0, [0], varEmpty);
    AddGet(TControlScrollBar, 'Margin', TControlScrollBar_Read_Margin, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Margin', TControlScrollBar_Write_Margin, 0, [0]);
    AddGet(TControlScrollBar, 'Increment', TControlScrollBar_Read_Increment, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Increment', TControlScrollBar_Write_Increment, 0, [0]);
    AddGet(TControlScrollBar, 'Range', TControlScrollBar_Read_Range, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Range', TControlScrollBar_Write_Range, 0, [0]);
    AddGet(TControlScrollBar, 'Position', TControlScrollBar_Read_Position, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Position', TControlScrollBar_Write_Position, 0, [0]);
    AddGet(TControlScrollBar, 'Tracking', TControlScrollBar_Read_Tracking, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Tracking', TControlScrollBar_Write_Tracking, 0, [0]);
    AddGet(TControlScrollBar, 'Visible', TControlScrollBar_Read_Visible, 0, [0], varEmpty);
    AddSet(TControlScrollBar, 'Visible', TControlScrollBar_Write_Visible, 0, [0]);
   { TScrollingWinControl }
    AddClass('Forms', TScrollingWinControl, 'TScrollingWinControl');
    AddGet(TScrollingWinControl, 'Create', TScrollingWinControl_Create, 1, [varEmpty], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TScrollingWinControl, 'DisableAutoRange', TScrollingWinControl_DisableAutoRange, 0, [0], varEmpty);
    AddGet(TScrollingWinControl, 'EnableAutoRange', TScrollingWinControl_EnableAutoRange, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TScrollingWinControl, 'ScrollInView', TScrollingWinControl_ScrollInView, 1, [varEmpty], varEmpty);
    AddGet(TScrollingWinControl, 'HorzScrollBar', TScrollingWinControl_Read_HorzScrollBar, 0, [0], varEmpty);
    AddSet(TScrollingWinControl, 'HorzScrollBar', TScrollingWinControl_Write_HorzScrollBar, 0, [0]);
    AddGet(TScrollingWinControl, 'VertScrollBar', TScrollingWinControl_Read_VertScrollBar, 0, [0], varEmpty);
    AddSet(TScrollingWinControl, 'VertScrollBar', TScrollingWinControl_Write_VertScrollBar, 0, [0]);
   { TFormBorderStyle }
    AddConst('Forms', 'bsNone', bsNone);
    AddConst('Forms', 'bsSingle', bsSingle);
    AddConst('Forms', 'bsSizeable', bsSizeable);
    AddConst('Forms', 'bsDialog', bsDialog);
    AddConst('Forms', 'bsToolWindow', bsToolWindow);
    AddConst('Forms', 'bsSizeToolWin', bsSizeToolWin);
   { TScrollBox }
    AddClass('Forms', TScrollBox, 'TScrollBox');
    AddGet(TScrollBox, 'Create', TScrollBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TScrollBox, 'BorderStyle', TScrollBox_Read_BorderStyle, 0, [0], varEmpty);
    AddSet(TScrollBox, 'BorderStyle', TScrollBox_Write_BorderStyle, 0, [0]);
   { TWindowState }
    AddConst('Forms', 'wsNormal', wsNormal);
    AddConst('Forms', 'wsMinimized', wsMinimized);
    AddConst('Forms', 'wsMaximized', wsMaximized);
   { TFormStyle }
    AddConst('Forms', 'fsNormal', fsNormal);
    AddConst('Forms', 'fsMDIChild', fsMDIChild);
    AddConst('Forms', 'fsMDIForm', fsMDIForm);
    AddConst('Forms', 'fsStayOnTop', fsStayOnTop);
   { TBorderIcon }
    AddConst('Forms', 'biSystemMenu', biSystemMenu);
    AddConst('Forms', 'biMinimize', biMinimize);
    AddConst('Forms', 'biMaximize', biMaximize);
    AddConst('Forms', 'biHelp', biHelp);
   { TPosition }
    AddConst('Forms', 'poDesigned', poDesigned);
    AddConst('Forms', 'poDefault', poDefault);
    AddConst('Forms', 'poDefaultPosOnly', poDefaultPosOnly);
    AddConst('Forms', 'poDefaultSizeOnly', poDefaultSizeOnly);
    AddConst('Forms', 'poScreenCenter', poScreenCenter);
   { TPrintScale }
    AddConst('Forms', 'poNone', poNone);
    AddConst('Forms', 'poProportional', poProportional);
    AddConst('Forms', 'poPrintToFit', poPrintToFit);
   { TShowAction }
    AddConst('Forms', 'saIgnore', saIgnore);
    AddConst('Forms', 'saRestore', saRestore);
    AddConst('Forms', 'saMinimize', saMinimize);
    AddConst('Forms', 'saMaximize', saMaximize);
   { TTileMode }
    AddConst('Forms', 'tbHorizontal', tbHorizontal);
    AddConst('Forms', 'tbVertical', tbVertical);
   { TCloseAction }
    AddConst('Forms', 'caNone', caNone);
    AddConst('Forms', 'caHide', caHide);
    AddConst('Forms', 'caFree', caFree);
    AddConst('Forms', 'caMinimize', caMinimize);
   { TFormState }
    AddConst('Forms', 'fsCreating', fsCreating);
    AddConst('Forms', 'fsVisible', fsVisible);
    AddConst('Forms', 'fsShowing', fsShowing);
    AddConst('Forms', 'fsModal', fsModal);
    AddConst('Forms', 'fsCreatedMDIChild', fsCreatedMDIChild);
   { TCustomForm }
   {$IFDEF RA_D3H}
    AddClass('Forms', TCustomForm, 'TCustomForm');
   {$ENDIF RA_D3H}
    AddGet(TCustomForm, 'Close', TCustomForm_Close, 0, [0], varEmpty);
    AddGet(TCustomForm, 'CloseQuery', TCustomForm_CloseQuery, 0, [0], varEmpty);
    AddGet(TCustomForm, 'DefocusControl', TCustomForm_DefocusControl, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomForm, 'FocusControl', TCustomForm_FocusControl, 1, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'GetFormImage', TCustomForm_GetFormImage, 0, [0], varEmpty);
    AddGet(TCustomForm, 'Hide', TCustomForm_Hide, 0, [0], varEmpty);
    AddGet(TCustomForm, 'Print', TCustomForm_Print, 0, [0], varEmpty);
    AddGet(TCustomForm, 'Release', TCustomForm_Release, 0, [0], varEmpty);
    AddGet(TCustomForm, 'SendCancelMode', TCustomForm_SendCancelMode, 1, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'SetFocus', TCustomForm_SetFocus, 0, [0], varEmpty);
    AddGet(TCustomForm, 'SetFocusedControl', TCustomForm_SetFocusedControl, 1, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'Show', TCustomForm_Show, 0, [0], varEmpty);
    AddGet(TCustomForm, 'ShowModal', TCustomForm_ShowModal, 0, [0], varEmpty);
    AddGet(TCustomForm, 'Active', TCustomForm_Read_Active, 0, [0], varEmpty);
    AddGet(TCustomForm, 'ActiveControl', TCustomForm_Read_ActiveControl, 0, [0], varEmpty);
    AddSet(TCustomForm, 'ActiveControl', TCustomForm_Write_ActiveControl, 0, [0]);
    AddGet(TCustomForm, 'ActiveOleControl', TCustomForm_Read_ActiveOleControl, 0, [0], varEmpty);
    AddSet(TCustomForm, 'ActiveOleControl', TCustomForm_Write_ActiveOleControl, 0, [0]);
    AddGet(TCustomForm, 'Canvas', TCustomForm_Read_Canvas, 0, [0], varEmpty);
    AddGet(TCustomForm, 'DropTarget', TCustomForm_Read_DropTarget, 0, [0], varEmpty);
    AddSet(TCustomForm, 'DropTarget', TCustomForm_Write_DropTarget, 0, [0]);
   {$IFDEF RA_D3H}
    AddGet(TCustomForm, 'HelpFile', TCustomForm_Read_HelpFile, 0, [0], varEmpty);
    AddSet(TCustomForm, 'HelpFile', TCustomForm_Write_HelpFile, 0, [0]);
   {$ENDIF RA_D3H}
    AddGet(TCustomForm, 'KeyPreview', TCustomForm_Read_KeyPreview, 0, [0], varEmpty);
    AddSet(TCustomForm, 'KeyPreview', TCustomForm_Write_KeyPreview, 0, [0]);
    AddGet(TCustomForm, 'Menu', TCustomForm_Read_Menu, 0, [0], varEmpty);
    AddSet(TCustomForm, 'Menu', TCustomForm_Write_Menu, 0, [0]);
    AddGet(TCustomForm, 'ModalResult', TCustomForm_Read_ModalResult, 0, [0], varEmpty);
    AddSet(TCustomForm, 'ModalResult', TCustomForm_Write_ModalResult, 0, [0]);
    AddGet(TCustomForm, 'WindowState', TCustomForm_Read_WindowState, 0, [0], varEmpty);
    AddSet(TCustomForm, 'WindowState', TCustomForm_Write_WindowState, 0, [0]);
   { TForm }
    AddClass('Forms', TForm, 'TForm');
    AddGet(TForm, 'Create', TForm_Create, 1, [varEmpty], varEmpty);
   {$IFDEF RA_D}
    AddGet(TForm, 'CreateNew', TForm_CreateNew, 1, [varEmpty], varEmpty);
   {$ELSE}
    AddGet(TForm, 'CreateNew', TForm_CreateNew, 2, [varEmpty, varEmpty], varEmpty);
   {$ENDIF}
    AddGet(TForm, 'ArrangeIcons', TForm_ArrangeIcons, 0, [0], varEmpty);
    AddGet(TForm, 'Cascade', TForm_Cascade, 0, [0], varEmpty);
    AddGet(TForm, 'Next', TForm_Next, 0, [0], varEmpty);
    AddGet(TForm, 'Previous', TForm_Previous, 0, [0], varEmpty);
    AddGet(TForm, 'Tile', TForm_Tile, 0, [0], varEmpty);
   { TDataModule }
    AddClass('Forms', TDataModule, 'TDataModule');
    AddGet(TDataModule, 'Create', TDataModule_Create, 1, [varEmpty], varEmpty);
   {$IFDEF RA_D}
    AddGet(TDataModule, 'CreateNew', TDataModule_CreateNew, 1, [varEmpty], varEmpty);
   {$ELSE}
    AddGet(TDataModule, 'CreateNew', TDataModule_CreateNew, 2, [varEmpty, varEmpty], varEmpty);
   {$ENDIF}
   { TScreen }
    AddClass('Forms', TScreen, 'TScreen');
    AddGet(TScreen, 'Create', TScreen_Create, 1, [varEmpty], varEmpty);
    AddGet(TScreen, 'ActiveControl', TScreen_Read_ActiveControl, 0, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TScreen, 'ActiveCustomForm', TScreen_Read_ActiveCustomForm, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TScreen, 'ActiveForm', TScreen_Read_ActiveForm, 0, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TScreen, 'CustomFormCount', TScreen_Read_CustomFormCount, 0, [0], varEmpty);
    AddGet(TScreen, 'CustomForms', TScreen_Read_CustomForms, 1, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TScreen, 'Cursor', TScreen_Read_Cursor, 0, [0], varEmpty);
    AddSet(TScreen, 'Cursor', TScreen_Write_Cursor, 0, [0]);
    AddGet(TScreen, 'Cursors', TScreen_Read_Cursors, 1, [0], varEmpty);
    AddSet(TScreen, 'Cursors', TScreen_Write_Cursors, 1, [1]);
    AddGet(TScreen, 'DataModules', TScreen_Read_DataModules, 1, [0], varEmpty);
    AddGet(TScreen, 'DataModuleCount', TScreen_Read_DataModuleCount, 0, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TScreen, 'IconFont', TScreen_Read_IconFont, 0, [0], varEmpty);
    AddSet(TScreen, 'IconFont', TScreen_Write_IconFont, 0, [0]);
   {$ENDIF RA_D3H}
    AddGet(TScreen, 'Fonts', TScreen_Read_Fonts, 0, [0], varEmpty);
    AddGet(TScreen, 'FormCount', TScreen_Read_FormCount, 0, [0], varEmpty);
    AddGet(TScreen, 'Forms', TScreen_Read_Forms, 1, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TScreen, 'Imes', TScreen_Read_Imes, 0, [0], varEmpty);
    AddGet(TScreen, 'DefaultIme', TScreen_Read_DefaultIme, 0, [0], varEmpty);
    AddGet(TScreen, 'DefaultKbLayout', TScreen_Read_DefaultKbLayout, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TScreen, 'Height', TScreen_Read_Height, 0, [0], varEmpty);
    AddGet(TScreen, 'PixelsPerInch', TScreen_Read_PixelsPerInch, 0, [0], varEmpty);
    AddGet(TScreen, 'Width', TScreen_Read_Width, 0, [0], varEmpty);
   { TTimerMode }
    AddConst('Forms', 'tmShow', tmShow);
    AddConst('Forms', 'tmHide', tmHide);
   { TApplication }
    AddClass('Forms', TApplication, 'TApplication');
    AddGet(TApplication, 'Create', TApplication_Create, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'BringToFront', TApplication_BringToFront, 0, [0], varEmpty);
    AddGet(TApplication, 'ControlDestroyed', TApplication_ControlDestroyed, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'CancelHint', TApplication_CancelHint, 0, [0], varEmpty);
    AddGet(TApplication, 'CreateForm', TApplication_CreateForm, 2, [varEmpty, varByRef], varEmpty);
    AddGet(TApplication, 'CreateHandle', TApplication_CreateHandle, 0, [0], varEmpty);
    AddGet(TApplication, 'HandleException', TApplication_HandleException, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'HandleMessage', TApplication_HandleMessage, 0, [0], varEmpty);
    AddGet(TApplication, 'HelpCommand', TApplication_HelpCommand, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TApplication, 'HelpContext', TApplication_HelpContext, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'HelpJump', TApplication_HelpJump, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'HideHint', TApplication_HideHint, 0, [0], varEmpty);
    AddGet(TApplication, 'Initialize', TApplication_Initialize, 0, [0], varEmpty);
    AddGet(TApplication, 'MessageBox', TApplication_MessageBox, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TApplication, 'Minimize', TApplication_Minimize, 0, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TApplication, 'NormalizeAllTopMosts', TApplication_NormalizeAllTopMosts, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TApplication, 'NormalizeTopMosts', TApplication_NormalizeTopMosts, 0, [0], varEmpty);
    AddGet(TApplication, 'ProcessMessages', TApplication_ProcessMessages, 0, [0], varEmpty);
    AddGet(TApplication, 'Restore', TApplication_Restore, 0, [0], varEmpty);
    AddGet(TApplication, 'RestoreTopMosts', TApplication_RestoreTopMosts, 0, [0], varEmpty);
    AddGet(TApplication, 'Run', TApplication_Run, 0, [0], varEmpty);
    AddGet(TApplication, 'ShowException', TApplication_ShowException, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'Terminate', TApplication_Terminate, 0, [0], varEmpty);
    AddGet(TApplication, 'Active', TApplication_Read_Active, 0, [0], varEmpty);
   {$IFDEF RA_D3H}
    AddGet(TApplication, 'AllowTesting', TApplication_Read_AllowTesting, 0, [0], varEmpty);
    AddSet(TApplication, 'AllowTesting', TApplication_Write_AllowTesting, 0, [0]);
    AddGet(TApplication, 'CurrentHelpFile', TApplication_Read_CurrentHelpFile, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TApplication, 'DialogHandle', TApplication_Read_DialogHandle, 0, [0], varEmpty);
    AddSet(TApplication, 'DialogHandle', TApplication_Write_DialogHandle, 0, [0]);
    AddGet(TApplication, 'ExeName', TApplication_Read_ExeName, 0, [0], varEmpty);
    AddGet(TApplication, 'Handle', TApplication_Read_Handle, 0, [0], varEmpty);
    AddSet(TApplication, 'Handle', TApplication_Write_Handle, 0, [0]);
    AddGet(TApplication, 'HelpFile', TApplication_Read_HelpFile, 0, [0], varEmpty);
    AddSet(TApplication, 'HelpFile', TApplication_Write_HelpFile, 0, [0]);
    AddGet(TApplication, 'Hint', TApplication_Read_Hint, 0, [0], varEmpty);
    AddSet(TApplication, 'Hint', TApplication_Write_Hint, 0, [0]);
    AddGet(TApplication, 'HintColor', TApplication_Read_HintColor, 0, [0], varEmpty);
    AddSet(TApplication, 'HintColor', TApplication_Write_HintColor, 0, [0]);
    AddGet(TApplication, 'HintPause', TApplication_Read_HintPause, 0, [0], varEmpty);
    AddSet(TApplication, 'HintPause', TApplication_Write_HintPause, 0, [0]);
    AddGet(TApplication, 'HintShortPause', TApplication_Read_HintShortPause, 0, [0], varEmpty);
    AddSet(TApplication, 'HintShortPause', TApplication_Write_HintShortPause, 0, [0]);
    AddGet(TApplication, 'HintHidePause', TApplication_Read_HintHidePause, 0, [0], varEmpty);
    AddSet(TApplication, 'HintHidePause', TApplication_Write_HintHidePause, 0, [0]);
    AddGet(TApplication, 'Icon', TApplication_Read_Icon, 0, [0], varEmpty);
    AddSet(TApplication, 'Icon', TApplication_Write_Icon, 0, [0]);
    AddGet(TApplication, 'MainForm', TApplication_Read_MainForm, 0, [0], varEmpty);
    AddGet(TApplication, 'ShowHint', TApplication_Read_ShowHint, 0, [0], varEmpty);
    AddSet(TApplication, 'ShowHint', TApplication_Write_ShowHint, 0, [0]);
    AddGet(TApplication, 'ShowMainForm', TApplication_Read_ShowMainForm, 0, [0], varEmpty);
    AddSet(TApplication, 'ShowMainForm', TApplication_Write_ShowMainForm, 0, [0]);
    AddGet(TApplication, 'Terminated', TApplication_Read_Terminated, 0, [0], varEmpty);
    AddGet(TApplication, 'Title', TApplication_Read_Title, 0, [0], varEmpty);
    AddSet(TApplication, 'Title', TApplication_Write_Title, 0, [0]);
    AddGet(TApplication, 'UpdateFormatSettings', TApplication_Read_UpdateFormatSettings, 0, [0], varEmpty);
    AddSet(TApplication, 'UpdateFormatSettings', TApplication_Write_UpdateFormatSettings, 0, [0]);
   {$IFDEF RA_D3H}
    AddGet(TApplication, 'UpdateMetricSettings', TApplication_Read_UpdateMetricSettings, 0, [0], varEmpty);
    AddSet(TApplication, 'UpdateMetricSettings', TApplication_Write_UpdateMetricSettings, 0, [0]);
   {$ENDIF RA_D3H}

    AddFun('Forms', 'Application', RAI2_Application, 0, [0], varEmpty);
    AddFun('Forms', 'Screen', RAI2_Screen, 0, [0], varEmpty);

    AddFun('Forms', 'GetParentForm', RAI2_GetParentForm, 1, [varEmpty], varEmpty);
    AddFun('Forms', 'ValidParentForm', RAI2_ValidParentForm, 1, [varEmpty], varEmpty);
    AddFun('Forms', 'DisableTaskWindows', RAI2_DisableTaskWindows, 1, [varEmpty], varEmpty);
    AddFun('Forms', 'EnableTaskWindows', RAI2_EnableTaskWindows, 1, [varEmpty], varEmpty);
    AddFun('Forms', 'IsAccel', RAI2_IsAccel, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('Forms', 'KeysToShiftState', RAI2_KeysToShiftState, 1, [varEmpty], varEmpty);
    AddFun('Forms', 'KeyDataToShiftState', RAI2_KeyDataToShiftState, 1, [varEmpty], varEmpty);
   {$IFDEF RA_D3H}
    AddFun('Forms', 'ForegroundTask', RAI2_ForegroundTask, 0, [0], varEmpty);
   {$ENDIF RA_D3H}

    AddHandler('Forms', 'TCloseEvent', TRAI2FormsEvent, @TRAI2FormsEvent.CloseEvent);
    AddHandler('Forms', 'TCloseQueryEvent', TRAI2FormsEvent, @TRAI2FormsEvent.CloseQueryEvent);
    AddHandler('Forms', 'TMessageEvent', TRAI2FormsEvent, @TRAI2FormsEvent.MessageEvent);
    AddHandler('Forms', 'TExceptionEvent', TRAI2FormsEvent, @TRAI2FormsEvent.ExceptionEvent);
    AddHandler('Forms', 'TIdleEvent', TRAI2FormsEvent, @TRAI2FormsEvent.IdleEvent);
    AddHandler('Forms', 'TShowHintEvent', TRAI2FormsEvent, @TRAI2FormsEvent.ShowHintEvent);
  end;    { with }
  RegisterClasses([TScrollBox]);
end;    { RegisterRAI2Adapter }

end.

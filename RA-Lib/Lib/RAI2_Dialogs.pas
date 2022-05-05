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

unit RAI2_Dialogs;

interface

uses RAI2;

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);

implementation

uses Classes, Graphics, Controls, Dialogs, RAI2_Windows;


  { TCommonDialog }

{$IFDEF RA_D3H}
{ property Read Handle: HWnd }
procedure TCommonDialog_Read_Handle(var Value: Variant; Args: TArgs);
begin
  Value := Integer(TCommonDialog(Args.Obj).Handle);
end;
{$ENDIF RA_D3H}

{ property Read Ctl3D: Boolean }
procedure TCommonDialog_Read_Ctl3D(var Value: Variant; Args: TArgs);
begin
  Value := TCommonDialog(Args.Obj).Ctl3D;
end;

{ property Write Ctl3D(Value: Boolean) }
procedure TCommonDialog_Write_Ctl3D(const Value: Variant; Args: TArgs);
begin
  TCommonDialog(Args.Obj).Ctl3D := Value;
end;

{ property Read HelpContext: THelpContext }
procedure TCommonDialog_Read_HelpContext(var Value: Variant; Args: TArgs);
begin
  Value := TCommonDialog(Args.Obj).HelpContext;
end;

{ property Write HelpContext(Value: THelpContext) }
procedure TCommonDialog_Write_HelpContext(const Value: Variant; Args: TArgs);
begin
  TCommonDialog(Args.Obj).HelpContext := Value;
end;

  { TOpenDialog }

{ constructor Create(AOwner: TComponent) }
procedure TOpenDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TOpenDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  function Execute: Boolean; }
procedure TOpenDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).Execute;
end;

{ property Read FileEditStyle: TFileEditStyle }
procedure TOpenDialog_Read_FileEditStyle(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).FileEditStyle;
end;

{ property Write FileEditStyle(Value: TFileEditStyle) }
procedure TOpenDialog_Write_FileEditStyle(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).FileEditStyle := Value;
end;

{ property Read Files: TStrings }
procedure TOpenDialog_Read_Files(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TOpenDialog(Args.Obj).Files);
end;

{ property Read HistoryList: TStrings }
procedure TOpenDialog_Read_HistoryList(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TOpenDialog(Args.Obj).HistoryList);
end;

{ property Write HistoryList(Value: TStrings) }
procedure TOpenDialog_Write_HistoryList(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).HistoryList := V2O(Value) as TStrings;
end;

{ property Read DefaultExt: string }
procedure TOpenDialog_Read_DefaultExt(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).DefaultExt;
end;

{ property Write DefaultExt(Value: string) }
procedure TOpenDialog_Write_DefaultExt(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).DefaultExt := Value;
end;

{ property Read FileName: TFileName }
procedure TOpenDialog_Read_FileName(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).FileName;
end;

{ property Write FileName(Value: TFileName) }
procedure TOpenDialog_Write_FileName(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).FileName := Value;
end;

{ property Read Filter: string }
procedure TOpenDialog_Read_Filter(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).Filter;
end;

{ property Write Filter(Value: string) }
procedure TOpenDialog_Write_Filter(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).Filter := Value;
end;

{ property Read FilterIndex: Integer }
procedure TOpenDialog_Read_FilterIndex(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).FilterIndex;
end;

{ property Write FilterIndex(Value: Integer) }
procedure TOpenDialog_Write_FilterIndex(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).FilterIndex := Value;
end;

{ property Read InitialDir: string }
procedure TOpenDialog_Read_InitialDir(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).InitialDir;
end;

{ property Write InitialDir(Value: string) }
procedure TOpenDialog_Write_InitialDir(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).InitialDir := Value;
end;

{ property Read Options: TOpenOptions }
procedure TOpenDialog_Read_Options(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Integer(TOpenDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TOpenOptions) }
procedure TOpenDialog_Write_Options(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).Options := TOpenOptions(V2S(Value));
end;

{ property Read Title: string }
procedure TOpenDialog_Read_Title(var Value: Variant; Args: TArgs);
begin
  Value := TOpenDialog(Args.Obj).Title;
end;

{ property Write Title(Value: string) }
procedure TOpenDialog_Write_Title(const Value: Variant; Args: TArgs);
begin
  TOpenDialog(Args.Obj).Title := Value;
end;

  { TSaveDialog }

{ constructor Create(AOwner: TComponent) }
procedure TSaveDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TSaveDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function Execute: Boolean; }
procedure TSaveDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TSaveDialog(Args.Obj).Execute;
end;

  { TColorDialog }

{ constructor Create(AOwner: TComponent) }
procedure TColorDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TColorDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  function Execute: Boolean; }
procedure TColorDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TColorDialog(Args.Obj).Execute;
end;

{ property Read Color: TColor }
procedure TColorDialog_Read_Color(var Value: Variant; Args: TArgs);
begin
  Value := TColorDialog(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }
procedure TColorDialog_Write_Color(const Value: Variant; Args: TArgs);
begin
  TColorDialog(Args.Obj).Color := Value;
end;

{ property Read CustomColors: TStrings }
procedure TColorDialog_Read_CustomColors(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TColorDialog(Args.Obj).CustomColors);
end;

{ property Write CustomColors(Value: TStrings) }
procedure TColorDialog_Write_CustomColors(const Value: Variant; Args: TArgs);
begin
  TColorDialog(Args.Obj).CustomColors := V2O(Value) as TStrings;
end;

{ property Read Options: TColorDialogOptions }
procedure TColorDialog_Read_Options(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Byte(TColorDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TColorDialogOptions) }
procedure TColorDialog_Write_Options(const Value: Variant; Args: TArgs);
begin
  TColorDialog(Args.Obj).Options := TColorDialogOptions(Byte(V2S(Value)));
end;

  { TFontDialog }

{ constructor Create(AOwner: TComponent) }
procedure TFontDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TFontDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  function Execute: Boolean; }
procedure TFontDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TFontDialog(Args.Obj).Execute;
end;

{ property Read Font: TFont }
procedure TFontDialog_Read_Font(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TFontDialog(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }
procedure TFontDialog_Write_Font(const Value: Variant; Args: TArgs);
begin
  TFontDialog(Args.Obj).Font := V2O(Value) as TFont;
end;

{ property Read Device: TFontDialogDevice }
procedure TFontDialog_Read_Device(var Value: Variant; Args: TArgs);
begin
  Value := TFontDialog(Args.Obj).Device;
end;

{ property Write Device(Value: TFontDialogDevice) }
procedure TFontDialog_Write_Device(const Value: Variant; Args: TArgs);
begin
  TFontDialog(Args.Obj).Device := Value;
end;

{ property Read MinFontSize: Integer }
procedure TFontDialog_Read_MinFontSize(var Value: Variant; Args: TArgs);
begin
  Value := TFontDialog(Args.Obj).MinFontSize;
end;

{ property Write MinFontSize(Value: Integer) }
procedure TFontDialog_Write_MinFontSize(const Value: Variant; Args: TArgs);
begin
  TFontDialog(Args.Obj).MinFontSize := Value;
end;

{ property Read MaxFontSize: Integer }
procedure TFontDialog_Read_MaxFontSize(var Value: Variant; Args: TArgs);
begin
  Value := TFontDialog(Args.Obj).MaxFontSize;
end;

{ property Write MaxFontSize(Value: Integer) }
procedure TFontDialog_Write_MaxFontSize(const Value: Variant; Args: TArgs);
begin
  TFontDialog(Args.Obj).MaxFontSize := Value;
end;

{ property Read Options: TFontDialogOptions }
procedure TFontDialog_Read_Options(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Word(TFontDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TFontDialogOptions) }
procedure TFontDialog_Write_Options(const Value: Variant; Args: TArgs);
begin
  TFontDialog(Args.Obj).Options := TFontDialogOptions(Word(V2S(Value)));
end;

  { TPrinterSetupDialog }

{ constructor Create(AOwner: TComponent) }
procedure TPrinterSetupDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TPrinterSetupDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  function Execute: Boolean; }
procedure TPrinterSetupDialog_Execute(var Value: Variant; Args: TArgs);
begin
 {$IFDEF RA_D3H}
  Value := TPrinterSetupDialog(Args.Obj).Execute;
 {$ELSE}
  TPrinterSetupDialog(Args.Obj).Execute;
 {$ENDIF RA_D3H}
end;

  { TPrintDialog }

{ constructor Create(AOwner: TComponent) }
procedure TPrintDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TPrintDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  function Execute: Boolean; }
procedure TPrintDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).Execute;
end;

{ property Read Collate: Boolean }
procedure TPrintDialog_Read_Collate(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).Collate;
end;

{ property Write Collate(Value: Boolean) }
procedure TPrintDialog_Write_Collate(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).Collate := Value;
end;

{ property Read Copies: Integer }
procedure TPrintDialog_Read_Copies(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).Copies;
end;

{ property Write Copies(Value: Integer) }
procedure TPrintDialog_Write_Copies(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).Copies := Value;
end;

{ property Read FromPage: Integer }
procedure TPrintDialog_Read_FromPage(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).FromPage;
end;

{ property Write FromPage(Value: Integer) }
procedure TPrintDialog_Write_FromPage(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).FromPage := Value;
end;

{ property Read MinPage: Integer }
procedure TPrintDialog_Read_MinPage(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).MinPage;
end;

{ property Write MinPage(Value: Integer) }
procedure TPrintDialog_Write_MinPage(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).MinPage := Value;
end;

{ property Read MaxPage: Integer }
procedure TPrintDialog_Read_MaxPage(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).MaxPage;
end;

{ property Write MaxPage(Value: Integer) }
procedure TPrintDialog_Write_MaxPage(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).MaxPage := Value;
end;

{ property Read Options: TPrintDialogOptions }
procedure TPrintDialog_Read_Options(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Byte(TPrintDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TPrintDialogOptions) }
procedure TPrintDialog_Write_Options(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).Options := TPrintDialogOptions(Byte(V2S(Value)));
end;

{ property Read PrintToFile: Boolean }
procedure TPrintDialog_Read_PrintToFile(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).PrintToFile;
end;

{ property Write PrintToFile(Value: Boolean) }
procedure TPrintDialog_Write_PrintToFile(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).PrintToFile := Value;
end;

{ property Read PrintRange: TPrintRange }
procedure TPrintDialog_Read_PrintRange(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).PrintRange;
end;

{ property Write PrintRange(Value: TPrintRange) }
procedure TPrintDialog_Write_PrintRange(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).PrintRange := Value;
end;

{ property Read ToPage: Integer }
procedure TPrintDialog_Read_ToPage(var Value: Variant; Args: TArgs);
begin
  Value := TPrintDialog(Args.Obj).ToPage;
end;

{ property Write ToPage(Value: Integer) }
procedure TPrintDialog_Write_ToPage(const Value: Variant; Args: TArgs);
begin
  TPrintDialog(Args.Obj).ToPage := Value;
end;

  { TFindDialog }

{ constructor Create(AOwner: TComponent) }
procedure TFindDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TFindDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure CloseDialog; }
procedure TFindDialog_CloseDialog(var Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).CloseDialog;
end;

{  function Execute: Boolean; }
procedure TFindDialog_Execute(var Value: Variant; Args: TArgs);
begin
  Value := TFindDialog(Args.Obj).Execute;
end;

{ property Read Left: Integer }
procedure TFindDialog_Read_Left(var Value: Variant; Args: TArgs);
begin
  Value := TFindDialog(Args.Obj).Left;
end;

{ property Write Left(Value: Integer) }
procedure TFindDialog_Write_Left(const Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).Left := Value;
end;

{ property Read Position: TPoint }
procedure TFindDialog_Read_Position(var Value: Variant; Args: TArgs);
begin
  Value := Point2Var(TFindDialog(Args.Obj).Position);
end;

{ property Write Position(Value: TPoint) }
procedure TFindDialog_Write_Position(const Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).Position := Var2Point(Value);
end;

{ property Read Top: Integer }
procedure TFindDialog_Read_Top(var Value: Variant; Args: TArgs);
begin
  Value := TFindDialog(Args.Obj).Top;
end;

{ property Write Top(Value: Integer) }
procedure TFindDialog_Write_Top(const Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).Top := Value;
end;

{ property Read FindText: string }
procedure TFindDialog_Read_FindText(var Value: Variant; Args: TArgs);
begin
  Value := TFindDialog(Args.Obj).FindText;
end;

{ property Write FindText(Value: string) }
procedure TFindDialog_Write_FindText(const Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).FindText := Value;
end;

{ property Read Options: TFindOptions }
procedure TFindDialog_Read_Options(var Value: Variant; Args: TArgs);
begin
  Value := S2V(Word(TFindDialog(Args.Obj).Options));
end;

{ property Write Options(Value: TFindOptions) }
procedure TFindDialog_Write_Options(const Value: Variant; Args: TArgs);
begin
  TFindDialog(Args.Obj).Options := TFindOptions(Word(V2S(Value)));
end;

  { TReplaceDialog }

{ constructor Create(AOwner: TComponent) }
procedure TReplaceDialog_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TReplaceDialog.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ReplaceText: string }
procedure TReplaceDialog_Read_ReplaceText(var Value: Variant; Args: TArgs);
begin
  Value := TReplaceDialog(Args.Obj).ReplaceText;
end;

{ property Write ReplaceText(Value: string) }
procedure TReplaceDialog_Write_ReplaceText(const Value: Variant; Args: TArgs);
begin
  TReplaceDialog(Args.Obj).ReplaceText := Value;
end;


{ function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TForm; }
procedure RAI2_CreateMessageDialog(var Value: Variant; Args: TArgs);
begin
  Value := O2V(CreateMessageDialog(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2])))));
end;

{ function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; }
procedure RAI2_MessageDlg(var Value: Variant; Args: TArgs);
begin
  Value := MessageDlg(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3]);
end;

{ function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer; }
procedure RAI2_MessageDlgPos(var Value: Variant; Args: TArgs);
begin
  Value := MessageDlgPos(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4], Args.Values[5]);
end;

{$IFDEF RA_D3H}
{ function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer; }
procedure RAI2_MessageDlgPosHelp(var Value: Variant; Args: TArgs);
begin
  Value := MessageDlgPosHelp(Args.Values[0], Args.Values[1], TMsgDlgButtons(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4], Args.Values[5], Args.Values[6]);
end;
{$ENDIF RA_D3H}

{ procedure ShowMessage(const Msg: string); }
procedure RAI2_ShowMessage(var Value: Variant; Args: TArgs);
begin
  ShowMessage(Args.Values[0]);
end;

{$IFDEF RA_D3H}
{ procedure ShowMessageFmt(const Msg: string; Params: array of const); }
procedure RAI2_ShowMessageFmt(var Value: Variant; Args: TArgs);
begin
  Args.OpenArray(1);
  ShowMessageFmt(Args.Values[0], Slice(Args.OA^, Args.OAS));
end;
{$ENDIF RA_D3H}

{ procedure ShowMessagePos(const Msg: string; X, Y: Integer); }
procedure RAI2_ShowMessagePos(var Value: Variant; Args: TArgs);
begin
  ShowMessagePos(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function InputBox(const ACaption, APrompt, ADefault: string): string; }
procedure RAI2_InputBox(var Value: Variant; Args: TArgs);
begin
  Value := InputBox(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean; }
procedure RAI2_InputQuery(var Value: Variant; Args: TArgs);
begin
  Value := InputQuery(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;


procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
   { TCommonDialog }
    AddClass('Dialogs', TCommonDialog, 'TCommonDialog');
   {$IFDEF RA_D3H}
    AddGet(TCommonDialog, 'Handle', TCommonDialog_Read_Handle, 0, [0], varEmpty);
   {$ENDIF RA_D3H}
    AddGet(TCommonDialog, 'Ctl3D', TCommonDialog_Read_Ctl3D, 0, [0], varEmpty);
    AddSet(TCommonDialog, 'Ctl3D', TCommonDialog_Write_Ctl3D, 0, [0]);
    AddGet(TCommonDialog, 'HelpContext', TCommonDialog_Read_HelpContext, 0, [0], varEmpty);
    AddSet(TCommonDialog, 'HelpContext', TCommonDialog_Write_HelpContext, 0, [0]);
   { TOpenOption }
    AddConst('Dialogs', 'ofReadOnly', ofReadOnly);
    AddConst('Dialogs', 'ofOverwritePrompt', ofOverwritePrompt);
    AddConst('Dialogs', 'ofHideReadOnly', ofHideReadOnly);
    AddConst('Dialogs', 'ofNoChangeDir', ofNoChangeDir);
    AddConst('Dialogs', 'ofShowHelp', ofShowHelp);
    AddConst('Dialogs', 'ofNoValidate', ofNoValidate);
    AddConst('Dialogs', 'ofAllowMultiSelect', ofAllowMultiSelect);
    AddConst('Dialogs', 'ofExtensionDifferent', ofExtensionDifferent);
    AddConst('Dialogs', 'ofPathMustExist', ofPathMustExist);
    AddConst('Dialogs', 'ofFileMustExist', ofFileMustExist);
    AddConst('Dialogs', 'ofCreatePrompt', ofCreatePrompt);
    AddConst('Dialogs', 'ofShareAware', ofShareAware);
    AddConst('Dialogs', 'ofNoReadOnlyReturn', ofNoReadOnlyReturn);
    AddConst('Dialogs', 'ofNoTestFileCreate', ofNoTestFileCreate);
    AddConst('Dialogs', 'ofNoNetworkButton', ofNoNetworkButton);
    AddConst('Dialogs', 'ofNoLongNames', ofNoLongNames);
    AddConst('Dialogs', 'ofOldStyleDialog', ofOldStyleDialog);
    AddConst('Dialogs', 'ofNoDereferenceLinks', ofNoDereferenceLinks);
   { TFileEditStyle }
    AddConst('Dialogs', 'fsEdit', fsEdit);
    AddConst('Dialogs', 'fsComboBox', fsComboBox);
   { TOpenDialog }
    AddClass('Dialogs', TOpenDialog, 'TOpenDialog');
    AddGet(TOpenDialog, 'Create', TOpenDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TOpenDialog, 'Execute', TOpenDialog_Execute, 0, [0], varEmpty);
    AddGet(TOpenDialog, 'FileEditStyle', TOpenDialog_Read_FileEditStyle, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FileEditStyle', TOpenDialog_Write_FileEditStyle, 0, [0]);
    AddGet(TOpenDialog, 'Files', TOpenDialog_Read_Files, 0, [0], varEmpty);
    AddGet(TOpenDialog, 'HistoryList', TOpenDialog_Read_HistoryList, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'HistoryList', TOpenDialog_Write_HistoryList, 0, [0]);
    AddGet(TOpenDialog, 'DefaultExt', TOpenDialog_Read_DefaultExt, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'DefaultExt', TOpenDialog_Write_DefaultExt, 0, [0]);
    AddGet(TOpenDialog, 'FileName', TOpenDialog_Read_FileName, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FileName', TOpenDialog_Write_FileName, 0, [0]);
    AddGet(TOpenDialog, 'Filter', TOpenDialog_Read_Filter, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Filter', TOpenDialog_Write_Filter, 0, [0]);
    AddGet(TOpenDialog, 'FilterIndex', TOpenDialog_Read_FilterIndex, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'FilterIndex', TOpenDialog_Write_FilterIndex, 0, [0]);
    AddGet(TOpenDialog, 'InitialDir', TOpenDialog_Read_InitialDir, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'InitialDir', TOpenDialog_Write_InitialDir, 0, [0]);
    AddGet(TOpenDialog, 'Options', TOpenDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Options', TOpenDialog_Write_Options, 0, [0]);
    AddGet(TOpenDialog, 'Title', TOpenDialog_Read_Title, 0, [0], varEmpty);
    AddSet(TOpenDialog, 'Title', TOpenDialog_Write_Title, 0, [0]);
   { TSaveDialog }
    AddClass('Dialogs', TSaveDialog, 'TSaveDialog');
    AddGet(TSaveDialog, 'Create', TSaveDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TSaveDialog, 'Execute', TSaveDialog_Execute, 0, [0], varEmpty);
   { TColorDialogOption }
    AddConst('Dialogs', 'cdFullOpen', cdFullOpen);
    AddConst('Dialogs', 'cdPreventFullOpen', cdPreventFullOpen);
    AddConst('Dialogs', 'cdShowHelp', cdShowHelp);
    AddConst('Dialogs', 'cdSolidColor', cdSolidColor);
    AddConst('Dialogs', 'cdAnyColor', cdAnyColor);
   { TColorDialog }
    AddClass('Dialogs', TColorDialog, 'TColorDialog');
    AddGet(TColorDialog, 'Create', TColorDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TColorDialog, 'Execute', TColorDialog_Execute, 0, [0], varEmpty);
    AddGet(TColorDialog, 'Color', TColorDialog_Read_Color, 0, [0], varEmpty);
    AddSet(TColorDialog, 'Color', TColorDialog_Write_Color, 0, [0]);
    AddGet(TColorDialog, 'CustomColors', TColorDialog_Read_CustomColors, 0, [0], varEmpty);
    AddSet(TColorDialog, 'CustomColors', TColorDialog_Write_CustomColors, 0, [0]);
    AddGet(TColorDialog, 'Options', TColorDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TColorDialog, 'Options', TColorDialog_Write_Options, 0, [0]);
   { TFontDialogOption }
    AddConst('Dialogs', 'fdAnsiOnly', fdAnsiOnly);
    AddConst('Dialogs', 'fdTrueTypeOnly', fdTrueTypeOnly);
    AddConst('Dialogs', 'fdEffects', fdEffects);
    AddConst('Dialogs', 'fdFixedPitchOnly', fdFixedPitchOnly);
    AddConst('Dialogs', 'fdForceFontExist', fdForceFontExist);
    AddConst('Dialogs', 'fdNoFaceSel', fdNoFaceSel);
    AddConst('Dialogs', 'fdNoOEMFonts', fdNoOEMFonts);
    AddConst('Dialogs', 'fdNoSimulations', fdNoSimulations);
    AddConst('Dialogs', 'fdNoSizeSel', fdNoSizeSel);
    AddConst('Dialogs', 'fdNoStyleSel', fdNoStyleSel);
    AddConst('Dialogs', 'fdNoVectorFonts', fdNoVectorFonts);
    AddConst('Dialogs', 'fdShowHelp', fdShowHelp);
    AddConst('Dialogs', 'fdWysiwyg', fdWysiwyg);
    AddConst('Dialogs', 'fdLimitSize', fdLimitSize);
    AddConst('Dialogs', 'fdScalableOnly', fdScalableOnly);
    AddConst('Dialogs', 'fdApplyButton', fdApplyButton);
   { TFontDialogDevice }
    AddConst('Dialogs', 'fdScreen', fdScreen);
    AddConst('Dialogs', 'fdPrinter', fdPrinter);
    AddConst('Dialogs', 'fdBoth', fdBoth);
   { TFontDialog }
    AddClass('Dialogs', TFontDialog, 'TFontDialog');
    AddGet(TFontDialog, 'Create', TFontDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TFontDialog, 'Execute', TFontDialog_Execute, 0, [0], varEmpty);
    AddGet(TFontDialog, 'Font', TFontDialog_Read_Font, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Font', TFontDialog_Write_Font, 0, [0]);
    AddGet(TFontDialog, 'Device', TFontDialog_Read_Device, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Device', TFontDialog_Write_Device, 0, [0]);
    AddGet(TFontDialog, 'MinFontSize', TFontDialog_Read_MinFontSize, 0, [0], varEmpty);
    AddSet(TFontDialog, 'MinFontSize', TFontDialog_Write_MinFontSize, 0, [0]);
    AddGet(TFontDialog, 'MaxFontSize', TFontDialog_Read_MaxFontSize, 0, [0], varEmpty);
    AddSet(TFontDialog, 'MaxFontSize', TFontDialog_Write_MaxFontSize, 0, [0]);
    AddGet(TFontDialog, 'Options', TFontDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TFontDialog, 'Options', TFontDialog_Write_Options, 0, [0]);
   { TPrinterSetupDialog }
    AddClass('Dialogs', TPrinterSetupDialog, 'TPrinterSetupDialog');
    AddGet(TPrinterSetupDialog, 'Create', TPrinterSetupDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrinterSetupDialog, 'Execute', TPrinterSetupDialog_Execute, 0, [0], varEmpty);
   { TPrintRange }
    AddConst('Dialogs', 'prAllPages', prAllPages);
    AddConst('Dialogs', 'prSelection', prSelection);
    AddConst('Dialogs', 'prPageNums', prPageNums);
   { TPrintDialogOption }
    AddConst('Dialogs', 'poPrintToFile', poPrintToFile);
    AddConst('Dialogs', 'poPageNums', poPageNums);
    AddConst('Dialogs', 'poSelection', poSelection);
    AddConst('Dialogs', 'poWarning', poWarning);
    AddConst('Dialogs', 'poHelp', poHelp);
    AddConst('Dialogs', 'poDisablePrintToFile', poDisablePrintToFile);
   { TPrintDialog }
    AddClass('Dialogs', TPrintDialog, 'TPrintDialog');
    AddGet(TPrintDialog, 'Create', TPrintDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TPrintDialog, 'Execute', TPrintDialog_Execute, 0, [0], varEmpty);
    AddGet(TPrintDialog, 'Collate', TPrintDialog_Read_Collate, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Collate', TPrintDialog_Write_Collate, 0, [0]);
    AddGet(TPrintDialog, 'Copies', TPrintDialog_Read_Copies, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Copies', TPrintDialog_Write_Copies, 0, [0]);
    AddGet(TPrintDialog, 'FromPage', TPrintDialog_Read_FromPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'FromPage', TPrintDialog_Write_FromPage, 0, [0]);
    AddGet(TPrintDialog, 'MinPage', TPrintDialog_Read_MinPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'MinPage', TPrintDialog_Write_MinPage, 0, [0]);
    AddGet(TPrintDialog, 'MaxPage', TPrintDialog_Read_MaxPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'MaxPage', TPrintDialog_Write_MaxPage, 0, [0]);
    AddGet(TPrintDialog, 'Options', TPrintDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'Options', TPrintDialog_Write_Options, 0, [0]);
    AddGet(TPrintDialog, 'PrintToFile', TPrintDialog_Read_PrintToFile, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'PrintToFile', TPrintDialog_Write_PrintToFile, 0, [0]);
    AddGet(TPrintDialog, 'PrintRange', TPrintDialog_Read_PrintRange, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'PrintRange', TPrintDialog_Write_PrintRange, 0, [0]);
    AddGet(TPrintDialog, 'ToPage', TPrintDialog_Read_ToPage, 0, [0], varEmpty);
    AddSet(TPrintDialog, 'ToPage', TPrintDialog_Write_ToPage, 0, [0]);
   { TFindOption }
    AddConst('Dialogs', 'frDown', frDown);
    AddConst('Dialogs', 'frFindNext', frFindNext);
    AddConst('Dialogs', 'frHideMatchCase', frHideMatchCase);
    AddConst('Dialogs', 'frHideWholeWord', frHideWholeWord);
    AddConst('Dialogs', 'frHideUpDown', frHideUpDown);
    AddConst('Dialogs', 'frMatchCase', frMatchCase);
    AddConst('Dialogs', 'frDisableMatchCase', frDisableMatchCase);
    AddConst('Dialogs', 'frDisableUpDown', frDisableUpDown);
    AddConst('Dialogs', 'frDisableWholeWord', frDisableWholeWord);
    AddConst('Dialogs', 'frReplace', frReplace);
    AddConst('Dialogs', 'frReplaceAll', frReplaceAll);
    AddConst('Dialogs', 'frWholeWord', frWholeWord);
    AddConst('Dialogs', 'frShowHelp', frShowHelp);
   { TFindDialog }
    AddClass('Dialogs', TFindDialog, 'TFindDialog');
    AddGet(TFindDialog, 'Create', TFindDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TFindDialog, 'CloseDialog', TFindDialog_CloseDialog, 0, [0], varEmpty);
    AddGet(TFindDialog, 'Execute', TFindDialog_Execute, 0, [0], varEmpty);
    AddGet(TFindDialog, 'Left', TFindDialog_Read_Left, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Left', TFindDialog_Write_Left, 0, [0]);
    AddGet(TFindDialog, 'Position', TFindDialog_Read_Position, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Position', TFindDialog_Write_Position, 0, [0]);
    AddGet(TFindDialog, 'Top', TFindDialog_Read_Top, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Top', TFindDialog_Write_Top, 0, [0]);
    AddGet(TFindDialog, 'FindText', TFindDialog_Read_FindText, 0, [0], varEmpty);
    AddSet(TFindDialog, 'FindText', TFindDialog_Write_FindText, 0, [0]);
    AddGet(TFindDialog, 'Options', TFindDialog_Read_Options, 0, [0], varEmpty);
    AddSet(TFindDialog, 'Options', TFindDialog_Write_Options, 0, [0]);
   { TReplaceDialog }
    AddClass('Dialogs', TReplaceDialog, 'TReplaceDialog');
    AddGet(TReplaceDialog, 'Create', TReplaceDialog_Create, 1, [varEmpty], varEmpty);
    AddGet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Read_ReplaceText, 0, [0], varEmpty);
    AddSet(TReplaceDialog, 'ReplaceText', TReplaceDialog_Write_ReplaceText, 0, [0]);
   { TMsgDlgType }
    AddConst('Dialogs', 'mtWarning', mtWarning);
    AddConst('Dialogs', 'mtError', mtError);
    AddConst('Dialogs', 'mtInformation', mtInformation);
    AddConst('Dialogs', 'mtConfirmation', mtConfirmation);
    AddConst('Dialogs', 'mtCustom', mtCustom);
   { TMsgDlgBtn }
    AddConst('Dialogs', 'mbYes', mbYes);
    AddConst('Dialogs', 'mbNo', mbNo);
    AddConst('Dialogs', 'mbOK', mbOK);
    AddConst('Dialogs', 'mbCancel', mbCancel);
    AddConst('Dialogs', 'mbAbort', mbAbort);
    AddConst('Dialogs', 'mbRetry', mbRetry);
    AddConst('Dialogs', 'mbIgnore', mbIgnore);
    AddConst('Dialogs', 'mbAll', mbAll);
   {$IFDEF RA_D3H}
    AddConst('Dialogs', 'mbNoToAll', mbNoToAll);
    AddConst('Dialogs', 'mbYesToAll', mbYesToAll);
   {$ENDIF RA_D3H}
    AddConst('Dialogs', 'mbHelp', mbHelp);
    AddConst('Dialogs', 'mrNone', mrNone);
    AddConst('Dialogs', 'mrOk', mrOk);
    AddConst('Dialogs', 'mrCancel', mrCancel);
    AddConst('Dialogs', 'mrAbort', mrAbort);
    AddConst('Dialogs', 'mrRetry', mrRetry);
    AddConst('Dialogs', 'mrIgnore', mrIgnore);
    AddConst('Dialogs', 'mrYes', mrYes);
    AddConst('Dialogs', 'mrNo', mrNo);
    AddConst('Dialogs', 'mrAll', mrAll);
   {$IFDEF RA_D3H}
    AddConst('Dialogs', 'mrNoToAll', mrNoToAll);
    AddConst('Dialogs', 'mrYesToAll', mrYesToAll);
   {$ENDIF RA_D3H}
    AddFun('Dialogs', 'CreateMessageDialog', RAI2_CreateMessageDialog, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('Dialogs', 'MessageDlg', RAI2_MessageDlg, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('Dialogs', 'MessageDlgPos', RAI2_MessageDlgPos, 6, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
   {$IFDEF RA_D3H}
    AddFun('Dialogs', 'MessageDlgPosHelp', RAI2_MessageDlgPosHelp, 7, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
   {$ENDIF RA_D3H}
    AddFun('Dialogs', 'ShowMessage', RAI2_ShowMessage, 1, [varEmpty], varEmpty);
   {$IFDEF RA_D3H}
    AddFun('Dialogs', 'ShowMessageFmt', RAI2_ShowMessageFmt, 2, [varEmpty, varEmpty], varEmpty);
   {$ENDIF RA_D3H}
    AddFun('Dialogs', 'ShowMessagePos', RAI2_ShowMessagePos, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('Dialogs', 'InputBox', RAI2_InputBox, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('Dialogs', 'InputQuery', RAI2_InputQuery, 3, [varEmpty, varEmpty, varByRef], varEmpty);
  end;    { with }
  RegisterClasses([TOpenDialog, TSaveDialog, TFontDialog, TColorDialog,
  TPrintDialog, TPrinterSetupDialog, TFindDialog, TReplaceDialog]);
end;    { RegisterRAI2Adapter }

end.

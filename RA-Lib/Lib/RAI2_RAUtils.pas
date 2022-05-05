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

unit RAI2_RAUtils;

interface

uses RAI2;

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);

implementation

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  Dialogs, Menus, RAUtils, RAI2_Windows, RAI2_SysUtils;


{ function ReplaceAllSokr(S: string; Words, Frases: TStrings): string; }
procedure RAI2_ReplaceAllSokr(var Value: Variant; Args: TArgs);
begin
  Value := ReplaceAllSokr(Args.Values[0], V2O(Args.Values[1]) as TStrings, V2O(Args.Values[2]) as TStrings);
end;

{ function ReplaceSokr(S: string; PosBeg, Len: integer; Words, Frases: TStrings; var NewSelStart: integer): string; }
procedure RAI2_ReplaceSokr(var Value: Variant; Args: TArgs);
begin
  Value := ReplaceSokr(Args.Values[0], Args.Values[1], Args.Values[2], V2O(Args.Values[3]) as TStrings, V2O(Args.Values[4]) as TStrings, TVarData(Args.Values[5]).vInteger);
end;

{ function CountOfLines(const S: string): integer; }
procedure RAI2_CountOfLines(var Value: Variant; Args: TArgs);
begin
  Value := CountOfLines(Args.Values[0]);
end;

{ procedure DeleteEmptyLines(Ss: TStrings); }
procedure RAI2_DeleteEmptyLines(var Value: Variant; Args: TArgs);
begin
  DeleteEmptyLines(V2O(Args.Values[0]) as TStrings);
end;

{ procedure SQLAddWhere(SQL: TStrings; const where: string); }
procedure RAI2_SQLAddWhere(var Value: Variant; Args: TArgs);
begin
  SQLAddWhere(V2O(Args.Values[0]) as TStrings, Args.Values[1]);
end;

{ function ResSaveToFile(const Typ, Name: string; const Compressed: boolean; const FileName: string): boolean; }
procedure RAI2_ResSaveToFile(var Value: Variant; Args: TArgs);
begin
  Value := ResSaveToFile(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar; const Compressed: boolean; const FileName: string): boolean; }
procedure RAI2_ResSaveToFileEx(var Value: Variant; Args: TArgs);
begin
  Value := ResSaveToFileEx(Args.Values[0], PChar(string(Args.Values[1])), PChar(string(Args.Values[2])), Args.Values[3], Args.Values[4]);
end;

{ function ResSaveToString(Instance: HINST; const Typ, Name: string; var S: string): boolean; }
procedure RAI2_ResSaveToString(var Value: Variant; Args: TArgs);
begin
  Value := ResSaveToString(Args.Values[0], Args.Values[1], Args.Values[2], string(TVarData(Args.Values[3]).vString));
end;

{ function Execute(const CommandLine, WorkingDirectory: string): integer; }
procedure RAI2_Execute(var Value: Variant; Args: TArgs);
begin
  Value := Execute(Args.Values[0], Args.Values[1]);
end;

{ function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): boolean; }
procedure RAI2_IniReadSection(var Value: Variant; Args: TArgs);
begin
  Value := IniReadSection(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ function LoadTextFile(const FileName: TFileName): string; }
procedure RAI2_LoadTextFile(var Value: Variant; Args: TArgs);
begin
  Value := LoadTextFile(Args.Values[0]);
end;

{ procedure SaveTextFile(const FileName: TFileName; const Source: string); }
procedure RAI2_SaveTextFile(var Value: Variant; Args: TArgs);
begin
  SaveTextFile(Args.Values[0], Args.Values[1]);
end;

{ function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): integer; }
procedure RAI2_ReadFolder(var Value: Variant; Args: TArgs);
begin
  Value := ReadFolder(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

procedure RAI2_ReadFolders(var Value: Variant; Args: TArgs);
begin
  Value := ReadFolders(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{$IFDEF RA_D3H}
{ function TargetFileName(const FileName: TFileName): TFileName; }
procedure RAI2_TargetFileName(var Value: Variant; Args: TArgs);
begin
  Value := TargetFileName(Args.Values[0]);
end;

{ function ResolveLink(const hwnd: HWND; const LinkFile: TFileName; var FileName: TFileName): HRESULT; }
procedure RAI2_ResolveLink(var Value: Variant; Args: TArgs);
begin
  Value := ResolveLink(Args.Values[0], Args.Values[1], TFileName(TVarData(Args.Values[2]).vString));
end;
{$ENDIF RA_D3H}

{ procedure LoadIcoToImage(ALarge, ASmall: TImageList; const NameRes: string); }
procedure RAI2_LoadIcoToImage(var Value: Variant; Args: TArgs);
begin
  LoadIcoToImage(V2O(Args.Values[0]) as TImageList, V2O(Args.Values[1]) as TImageList, Args.Values[2]);
end;

{ procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string); }
procedure RAI2_RATextOut(var Value: Variant; Args: TArgs);
begin
  RATextOut(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]), Args.Values[3]);
end;

{ function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: boolean): integer; }
procedure RAI2_RATextOutEx(var Value: Variant; Args: TArgs);
begin
  Value := RATextOutEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Var2Rect(Args.Values[2]), Args.Values[3], Args.Values[4]);
end;

{ function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): integer; }
procedure RAI2_RATextCalcHeight(var Value: Variant; Args: TArgs);
begin
  Value := RATextCalcHeight(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;

{ procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: boolean); }
procedure RAI2_Roughed(var Value: Variant; Args: TArgs);
begin
  Roughed(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), Args.Values[2]);
end;

{ function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, index: integer): TBitmap; }
procedure RAI2_BitmapFromBitmap(var Value: Variant; Args: TArgs);
begin
  Value := O2V(BitmapFromBitmap(V2O(Args.Values[0]) as TBitmap, Args.Values[1], Args.Values[2], Args.Values[3]));
end;

{ function TextWidth(AStr: string): integer; }
procedure RAI2_TextWidth(var Value: Variant; Args: TArgs);
begin
  Value := TextWidth(Args.Values[0]);
end;

{ function DefineCursor(Identifer: PChar): TCursor; }
procedure RAI2_DefineCursor(var Value: Variant; Args: TArgs);
begin
  Value := DefineCursor(PChar(string(Args.Values[0])));
end;

{ function FindFormByClassName(FormClassName: string): TForm; }
procedure RAI2_FindFormByClassName(var Value: Variant; Args: TArgs);
begin
  Value := O2V(FindFormByClassName(Args.Values[0]));
end;

{ function FindByTag(WinControl: TWinControl; ComponentClass: TComponentClass; const Tag: integer): TComponent; }
procedure RAI2_FindByTag(var Value: Variant; Args: TArgs);
begin
  Value := O2V(FindByTag(V2O(Args.Values[0]) as TWinControl, TComponentClass(V2C(Args.Values[1])), Args.Values[2]));
end;

{ function ControlAtPos2(Parent: TWinControl; X, Y: integer): TControl; }
procedure RAI2_ControlAtPos2(var Value: Variant; Args: TArgs);
begin
  Value := O2V(ControlAtPos2(V2O(Args.Values[0]) as TWinControl, Args.Values[1], Args.Values[2]));
end;

{ function RBTag(Parent: TWinControl): integer; }
procedure RAI2_RBTag(var Value: Variant; Args: TArgs);
begin
  Value := RBTag(V2O(Args.Values[0]) as TWinControl);
end;

{ function AppMinimized: boolean; }
procedure RAI2_AppMinimized(var Value: Variant; Args: TArgs);
begin
  Value := AppMinimized;
end;

{ function MsgDlg2(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpContext: integer; Control: TWinControl): Integer; }
procedure RAI2_MsgDlg2(var Value: Variant; Args: TArgs);
begin
  Value := MsgDlg2(Args.Values[0], Args.Values[1], Args.Values[2], TMsgDlgButtons(Word(V2S(Args.Values[3]))), Args.Values[4], V2O(Args.Values[5]) as TWinControl);
end;

{ function MsgDlgDef(const Msg, ACaption: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; HelpContext: integer; Control: TWinControl): Integer; }
procedure RAI2_MsgDlgDef(var Value: Variant; Args: TArgs);
begin
  Value := MsgDlgDef(Args.Values[0], Args.Values[1], Args.Values[2], TMsgDlgButtons(Word(V2S(Args.Values[3]))), Args.Values[4], Args.Values[5], V2O(Args.Values[6]) as TWinControl);
end;

{ procedure Delay(MSec: longword); }
procedure RAI2_Delay(var Value: Variant; Args: TArgs);
begin
  Delay(Args.Values[0]);
end;

(*
{ procedure CenterHor(Parent: TControl; MinLeft: integer; Controls: array of TControl); }
procedure RAI2_CenterHor(var Value: Variant; Args: TArgs);
begin
  CenterHor(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ procedure EnableControls(Control: TWinControl; const Enable: boolean); }
procedure RAI2_EnableControls(var Value: Variant; Args: TArgs);
begin
  EnableControls(V2O(Args.Values[0]) as TWinControl, Args.Values[1]);
end;

{ procedure EnableMenuItems(MenuItem: TMenuItem; const Tag: integer; const Enable: boolean); }
procedure RAI2_EnableMenuItems(var Value: Variant; Args: TArgs);
begin
  EnableMenuItems(V2O(Args.Values[0]) as TMenuItem, Args.Values[1], Args.Values[2]);
end;

(*
{ procedure ExpandWidth(Parent: TControl; MinWidth: integer; Controls: array of TControl); }
procedure RAI2_ExpandWidth(var Value: Variant; Args: TArgs);
begin
  ExpandWidth(V2O(Args.Values[0]) as TControl, Args.Values[1], Args.Values[2]);
end;
*)

{ function PanelBorder(Panel: TCustomPanel): integer; }
procedure RAI2_PanelBorder(var Value: Variant; Args: TArgs);
begin
  Value := PanelBorder(V2O(Args.Values[0]) as TCustomPanel);
end;

{ function Pixels(Control: TControl; APixels: integer): integer; }
procedure RAI2_Pixels(var Value: Variant; Args: TArgs);
begin
  Value := Pixels(V2O(Args.Values[0]) as TControl, Args.Values[1]);
end;

{ procedure SetChildPropOrd(Owner: TComponent; PropName: string; Value: Longint); }
procedure RAI2_SetChildPropOrd(var Value: Variant; Args: TArgs);
begin
  SetChildPropOrd(V2O(Args.Values[0]) as TComponent, Args.Values[1], Args.Values[2]);
end;

{ procedure Error(const Message: string); }
procedure RAI2_Error(var Value: Variant; Args: TArgs);
begin
  Error(Args.Values[0]);
end;

{ procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean; var PlainItem: string; var Width: Integer; CalcWidth: Boolean); }
procedure RAI2_ItemHtDrawEx(var Value: Variant; Args: TArgs);
begin
 {$IFDEF RA_D5H}
  ItemHtDrawEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4], string(TVarData(Args.Values[5]).vString), TVarData(Args.Values[6]).vInteger, Args.Values[7]);
 {$ELSE}
  ItemHtDrawEx(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Byte(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4], string(TVarData(Args.Values[5]).vString), TVarData(Args.Values[6]).vInteger, Args.Values[7]);
 {$ENDIF}
end;

{ function ItemHtDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): string; }
procedure RAI2_ItemHtDraw(var Value: Variant; Args: TArgs);
begin
 {$IFDEF RA_D5H}
  Value := ItemHtDraw(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
 {$ELSE}
  Value := ItemHtDraw(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Byte(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
 {$ENDIF}
end;

{ function ItemHtWidth(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean): Integer; }
procedure RAI2_ItemHtWidth(var Value: Variant; Args: TArgs);
begin
 {$IFDEF RA_D5H}
  Value := ItemHtWidth(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Word(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
 {$ELSE}
  Value := ItemHtWidth(V2O(Args.Values[0]) as TCanvas, Var2Rect(Args.Values[1]), TOwnerDrawState(Byte(V2S(Args.Values[2]))), Args.Values[3], Args.Values[4]);
 {$ENDIF}
end;

{ function ItemHtPlain(const Text: string): string; }
procedure RAI2_ItemHtPlain(var Value: Variant; Args: TArgs);
begin
  Value := ItemHtPlain(Args.Values[0]);
end;

{ procedure ClearList(List: TList); }
procedure RAI2_ClearList(var Value: Variant; Args: TArgs);
begin
  ClearList(V2O(Args.Values[0]) as TList);
end;

{ procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: word); }
procedure RAI2_MemStreamToClipBoard(var Value: Variant; Args: TArgs);
begin
  MemStreamToClipBoard(V2O(Args.Values[0]) as TMemoryStream, Args.Values[1]);
end;

{ procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: word); }
procedure RAI2_ClipBoardToMemStream(var Value: Variant; Args: TArgs);
begin
  ClipBoardToMemStream(V2O(Args.Values[0]) as TMemoryStream, Args.Values[1]);
end;

{ function GetPropType(Obj: TObject; const PropName: string): TTypeKind; }
procedure RAI2_GetPropType(var Value: Variant; Args: TArgs);
begin
  Value := GetPropType(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function GetPropStr(Obj: TObject; const PropName: string): string; }
procedure RAI2_GetPropStr(var Value: Variant; Args: TArgs);
begin
  Value := GetPropStr(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function GetPropOrd(Obj: TObject; const PropName: string): Integer; }
procedure RAI2_GetPropOrd(var Value: Variant; Args: TArgs);
begin
  Value := GetPropOrd(V2O(Args.Values[0]), Args.Values[1]);
end;

{ function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; }
procedure RAI2_CompareMem(var Value: Variant; Args: TArgs);
begin
  Value := CompareMem(V2P(Args.Values[0]), V2P(Args.Values[1]), Args.Values[2]);
end;

{ procedure ShowMenu(Form: TForm; MenuAni: TMenuAnimation); }
procedure RAI2_ShowMenu(var Value: Variant; Args: TArgs);
begin
  ShowMenu(V2O(Args.Values[0]) as TForm, Args.Values[1]);
end;





{****************************** RAUtilsW *******************************}
{ function GetWordOnPos(const S: string; const P: integer): string; }
procedure RAI2_GetWordOnPos(var Value: Variant; Args: TArgs);
begin
  Value := GetWordOnPos(Args.Values[0], Args.Values[1]);
end;

{ function GetWordOnPosEx(const S: string; const P: integer; var iBeg, iEnd: integer): string; }
procedure RAI2_GetWordOnPosEx(var Value: Variant; Args: TArgs);
begin
  Value := GetWordOnPosEx(Args.Values[0], Args.Values[1], TVarData(Args.Values[2]).vInteger, TVarData(Args.Values[3]).vInteger);
end;

{ function SubStr(const S: string; const index: integer; const Separator: string): string; }
procedure RAI2_SubStr(var Value: Variant; Args: TArgs);
begin
  Value := SubStr(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function SubStrEnd(const S: string; const index: integer; const Separator: string): string; }
procedure RAI2_SubStrEnd(var Value: Variant; Args: TArgs);
begin
  Value := SubStrEnd(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

(*
{ function SubWord(P: PChar; var P2: PChar): string; }
procedure RAI2_SubWord(var Value: Variant; Args: TArgs);
begin
  Value := SubWord(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])));
end;
*)

{ function NumberByWord(const N: longint): string; }
procedure RAI2_NumberByWord(var Value: Variant; Args: TArgs);
begin
  Value := NumberByWord(Args.Values[0]);
end;

{ function GetLineByPos(const S: string; const Pos: integer): integer; }
procedure RAI2_GetLineByPos(var Value: Variant; Args: TArgs);
begin
  Value := GetLineByPos(Args.Values[0], Args.Values[1]);
end;

{ procedure GetXYByPos(const S: string; const Pos: integer; var X, Y: integer); }
procedure RAI2_GetXYByPos(var Value: Variant; Args: TArgs);
begin
  GetXYByPos(Args.Values[0], Args.Values[1], TVarData(Args.Values[2]).vInteger, TVarData(Args.Values[3]).vInteger);
end;

{ function ReplaceString(S: string; const OldPattern, NewPattern: string): string; }
procedure RAI2_ReplaceString(var Value: Variant; Args: TArgs);
begin
  Value := ReplaceString(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ConcatSep(const S, S2, Separator: string): string; }
procedure RAI2_ConcatSep(var Value: Variant; Args: TArgs);
begin
  Value := ConcatSep(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ConcatLeftSep(const S, S2, Separator: string): string; }
procedure RAI2_ConcatLeftSep(var Value: Variant; Args: TArgs);
begin
  Value := ConcatLeftSep(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function MinimizeString(const S: string; const MaxLen: integer): string; }
procedure RAI2_MinimizeString(var Value: Variant; Args: TArgs);
begin
  Value := MinimizeString(Args.Values[0], Args.Values[1]);
end;

{ procedure Dos2Win(var S: string); }
procedure RAI2_Dos2Win(var Value: Variant; Args: TArgs);
begin
  Dos2Win(string(TVarData(Args.Values[0]).vString));
end;

{ procedure Win2Dos(var S: string); }
procedure RAI2_Win2Dos(var Value: Variant; Args: TArgs);
begin
  Win2Dos(string(TVarData(Args.Values[0]).vString));
end;

{ function Dos2WinRes(const S: string): string; }
procedure RAI2_Dos2WinRes(var Value: Variant; Args: TArgs);
begin
  Value := Dos2WinRes(Args.Values[0]);
end;

{ function Win2DosRes(const S: string): string; }
procedure RAI2_Win2DosRes(var Value: Variant; Args: TArgs);
begin
  Value := Win2DosRes(Args.Values[0]);
end;

{ function Win2Koi(const S: string): string; }
procedure RAI2_Win2Koi(var Value: Variant; Args: TArgs);
begin
  Value := Win2Koi(Args.Values[0]);
end;

{ function Spaces(const N: integer): string; }
procedure RAI2_Spaces(var Value: Variant; Args: TArgs);
begin
  Value := Spaces(Args.Values[0]);
end;

{ function AddSpaces(const S: string; const N: integer): string; }
procedure RAI2_AddSpaces(var Value: Variant; Args: TArgs);
begin
  Value := AddSpaces(Args.Values[0], Args.Values[1]);
end;

{ function LastDate(const Dat: TDateTime): string; }
procedure RAI2_LastDate(var Value: Variant; Args: TArgs);
begin
  Value := LastDate(Args.Values[0]);
end;

{ function CurrencyToStr(const Cur: currency): string; }
procedure RAI2_CurrencyToStr(var Value: Variant; Args: TArgs);
begin
  Value := CurrencyToStr(Args.Values[0]);
end;

{ function Cmp(const S1, S2: string): boolean; }
procedure RAI2_Cmp(var Value: Variant; Args: TArgs);
begin
  Value := Cmp(Args.Values[0], Args.Values[1]);
end;

{ function StringCat(var S1: string; S2: string): string; }
procedure RAI2_StringCat(var Value: Variant; Args: TArgs);
begin
  Value := StringCat(string(TVarData(Args.Values[0]).vString), Args.Values[1]);
end;

{ function HasChar(const Ch: Char; const S: string): boolean; }
procedure RAI2_HasChar(var Value: Variant; Args: TArgs);
begin
  Value := HasChar(string(Args.Values[0])[1], Args.Values[1]);
end;

{ function HasAnyChar(const Chars: string; const S: string): boolean; }
procedure RAI2_HasAnyChar(var Value: Variant; Args: TArgs);
begin
  Value := HasAnyChar(Args.Values[0], Args.Values[1]);
end;

(*
{ function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): boolean; }
procedure RAI2_CharInSet(var Value: Variant; Args: TArgs);
begin
  Value := CharInSet(string(Args.Values[0])[1], Args.Values[1]);
end;
*)

{ function CountOfChar(const Ch: Char; const S: string): Integer; }
procedure RAI2_CountOfChar(var Value: Variant; Args: TArgs);
begin
  Value := CountOfChar(string(Args.Values[0])[1], Args.Values[1]);
end;

{ function DefStr(const S: string; Default: string): string; }
procedure RAI2_DefStr(var Value: Variant; Args: TArgs);
begin
  Value := DefStr(Args.Values[0], Args.Values[1]);
end;

{ function GetWinDir: TFileName; }
procedure RAI2_GetWinDir(var Value: Variant; Args: TArgs);
begin
  Value := GetWinDir;
end;

{ function GetTempDir: string; }
procedure RAI2_GetTempDir(var Value: Variant; Args: TArgs);
begin
  Value := GetTempDir;
end;

{ function GenTempFileName(FileName: string): string; }
procedure RAI2_GenTempFileName(var Value: Variant; Args: TArgs);
begin
  Value := GenTempFileName(Args.Values[0]);
end;

{ function GenTempFileNameExt(FileName: string; const FileExt: string): string; }
procedure RAI2_GenTempFileNameExt(var Value: Variant; Args: TArgs);
begin
  Value := GenTempFileNameExt(Args.Values[0], Args.Values[1]);
end;

{ function ClearDir(const Dir: string): boolean; }
procedure RAI2_ClearDir(var Value: Variant; Args: TArgs);
begin
  Value := ClearDir(Args.Values[0]);
end;

{ function DeleteDir(const Dir: string): boolean; }
procedure RAI2_DeleteDir(var Value: Variant; Args: TArgs);
begin
  Value := DeleteDir(Args.Values[0]);
end;

{ function FileEquMask(FileName, Mask: TFileName): boolean; }
procedure RAI2_FileEquMask(var Value: Variant; Args: TArgs);
begin
  Value := FileEquMask(Args.Values[0], Args.Values[1]);
end;

{ function FileEquMasks(FileName, Masks: TFileName): boolean; }
procedure RAI2_FileEquMasks(var Value: Variant; Args: TArgs);
begin
  Value := FileEquMasks(Args.Values[0], Args.Values[1]);
end;

{ procedure DeleteFiles(const Folder: TFileName; const Masks: string); }
procedure RAI2_DeleteFiles(var Value: Variant; Args: TArgs);
begin
  DeleteFiles(Args.Values[0], Args.Values[1]);
end;

{ function LZFileExpand(const FileSource, FileDest: string): boolean; }
procedure RAI2_LZFileExpand(var Value: Variant; Args: TArgs);
begin
  Value := LZFileExpand(Args.Values[0], Args.Values[1]);
end;

{ function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): boolean; }
procedure RAI2_FileGetInfo(var Value: Variant; Args: TArgs);
begin
  Value := FileGetInfo(Args.Values[0], TSearchRec(V2R(Args.Values[1])^));
end;

{ function HasSubFolder(APath: TFileName): boolean; }
procedure RAI2_HasSubFolder(var Value: Variant; Args: TArgs);
begin
  Value := HasSubFolder(Args.Values[0]);
end;

{ function IsEmptyFolder(APath: TFileName): boolean; }
procedure RAI2_IsEmptyFolder(var Value: Variant; Args: TArgs);
begin
  Value := IsEmptyFolder(Args.Values[0]);
end;

{ procedure AddSlash(var Dir: TFileName); }
procedure RAI2_AddSlash(var Value: Variant; Args: TArgs);
begin
  AddSlash(TFileName(TVarData(Args.Values[0]).vString));
end;

{ function AddSlash2(const Dir: TFileName): string; }
procedure RAI2_AddSlash2(var Value: Variant; Args: TArgs);
begin
  Value := AddSlash2(Args.Values[0]);
end;

{ function AddPath(const FileName, Path: TFileName): TFileName; }
procedure RAI2_AddPath(var Value: Variant; Args: TArgs);
begin
  Value := AddPath(Args.Values[0], Args.Values[1]);
end;

{$IFNDEF RA_B1}
{ function BrowseForFolder(const Handle: HWnd; const Title: string; var Folder: string): boolean; }
procedure RAI2_BrowseForFolder(var Value: Variant; Args: TArgs);
begin
  Value := BrowseForFolder(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;
{$ENDIF RA_B1}

{ function DeleteReadOnlyFile(const FileName: TFileName): boolean; }
procedure RAI2_DeleteReadOnlyFile(var Value: Variant; Args: TArgs);
begin
  Value := DeleteReadOnlyFile(Args.Values[0]);
end;

{ function HasParam(const Param: string): boolean; }
procedure RAI2_HasParam(var Value: Variant; Args: TArgs);
begin
  Value := HasParam(Args.Values[0]);
end;

{ function HasSwitch(const Param: string): boolean; }
procedure RAI2_HasSwitch(var Value: Variant; Args: TArgs);
begin
  Value := HasSwitch(Args.Values[0]);
end;

{ function Switch(const Param: string): string; }
procedure RAI2_Switch(var Value: Variant; Args: TArgs);
begin
  Value := Switch(Args.Values[0]);
end;

{ function ExePath: TFileName; }
procedure RAI2_ExePath(var Value: Variant; Args: TArgs);
begin
  Value := ExePath;
end;

{ function CopyDir(const SourceDir, DestDir: TFileName): Boolean; }
procedure RAI2_CopyDir(var Value: Variant; Args: TArgs);
begin
  Value := CopyDir(Args.Values[0], Args.Values[1]);
end;

{ function TTFontSelected(const DC: HDC): boolean; }
procedure RAI2_TTFontSelected(var Value: Variant; Args: TArgs);
begin
  Value := TTFontSelected(Args.Values[0]);
end;

{ function TrueInflateRect(const R: TRect; const I: integer): TRect; }
procedure RAI2_TrueInflateRect(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(TrueInflateRect(Var2Rect(Args.Values[0]), Args.Values[1]));
end;

{ procedure SetWindowTop(const Handle: HWND; const Top: boolean); }
procedure RAI2_SetWindowTop(var Value: Variant; Args: TArgs);
begin
  SetWindowTop(Args.Values[0], Args.Values[1]);
end;

{ function KeyPressed(VK: integer): boolean; }
procedure RAI2_KeyPressed(var Value: Variant; Args: TArgs);
begin
  Value := KeyPressed(Args.Values[0]);
end;

{ function Max(x, y: integer): integer; }
procedure RAI2_Max(var Value: Variant; Args: TArgs);
begin
  Value := Max(Args.Values[0], Args.Values[1]);
end;

{ function Min(x, y: integer): integer; }
procedure RAI2_Min(var Value: Variant; Args: TArgs);
begin
  Value := Min(Args.Values[0], Args.Values[1]);
end;

{ procedure SwapInt(var Int1, Int2: Integer); }
procedure RAI2_SwapInt(var Value: Variant; Args: TArgs);
begin
  SwapInt(TVarData(Args.Values[0]).vInteger, TVarData(Args.Values[1]).vInteger);
end;

{ function IntPower(Base, Exponent: integer): integer; }
procedure RAI2_IntPower(var Value: Variant; Args: TArgs);
begin
  Value := IntPower(Args.Values[0], Args.Values[1]);
end;

{ function ChangeTopException(E: TObject): TObject; }
procedure RAI2_ChangeTopException(var Value: Variant; Args: TArgs);
begin
  Value := O2V(ChangeTopException(V2O(Args.Values[0])));
end;

{ function MakeValidFileName(const FileName: TFileName; const ReplaceBadChar: Char): TFileName; }
procedure RAI2_MakeValidFileName(var Value: Variant; Args: TArgs);
begin
  Value := MakeValidFileName(Args.Values[0], string(Args.Values[1])[1]);
end;

{ function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer; }
procedure RAI2_AnsiStrLIComp(var Value: Variant; Args: TArgs);
begin
  Value := AnsiStrLIComp(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ function Var2Type(V: Variant; const VarType: integer): variant; }
procedure RAI2_Var2Type(var Value: Variant; Args: TArgs);
begin
  Value := Var2Type(Args.Values[0], Args.Values[1]);
end;

{ function VarToInt(V: Variant): Integer; }
procedure RAI2_VarToInt(var Value: Variant; Args: TArgs);
begin
  Value := VarToInt(Args.Values[0]);
end;

{ function GetSubStr(const S: string; const index: integer; const Separator: Char): string; }
procedure RAI2_GetSubStr(var Value: Variant; Args: TArgs);
begin
  Value := GetSubStr(Args.Values[0], Args.Values[1], string(Args.Values[2])[1]);
end;

{ function GetParameter: string; }
procedure RAI2_GetParameter(var Value: Variant; Args: TArgs);
begin
  Value := GetParameter;
end;

{ function GetLongFileName(FileName: string): string; }
procedure RAI2_GetLongFileName(var Value: Variant; Args: TArgs);
begin
  Value := GetLongFileName(Args.Values[0]);
end;

{ function DirectoryExists(const Name: string): Boolean; }
procedure RAI2_DirectoryExists(var Value: Variant; Args: TArgs);
begin
  Value := DirectoryExists(Args.Values[0]);
end;

{ procedure ForceDirectories(Dir: string); }
procedure RAI2_ForceDirectories(var Value: Variant; Args: TArgs);
begin
  ForceDirectories(Args.Values[0]);
end;

{ function FileNewExt(const FileName, NewExt: TFileName): TFileName; }
procedure RAI2_FileNewExt(var Value: Variant; Args: TArgs);
begin
  Value := FileNewExt(Args.Values[0], Args.Values[1]);
end;

{ function GetComputerID: string; }
procedure RAI2_GetComputerID(var Value: Variant; Args: TArgs);
begin
  Value := GetComputerID;
end;

{ function AddPaths(const PathList, Path: string): string; }
procedure RAI2_AddPaths(var Value: Variant; Args: TArgs);
begin
  Value := AddPaths(Args.Values[0], Args.Values[1]);
end;

{ function ParentPath(const Path: TFileName): TFileName; }
procedure RAI2_ParentPath(var Value: Variant; Args: TArgs);
begin
  Value := ParentPath(Args.Values[0]);
end;

{ function FindInPath(const FileName, PathList: string): TFileName; }
procedure RAI2_FindInPath(var Value: Variant; Args: TArgs);
begin
  Value := FindInPath(Args.Values[0], Args.Values[1]);
end;

{ procedure PrepareIniSection(SS: TStrings); }
procedure RAI2_PrepareIniSection(var Value: Variant; Args: TArgs);
begin
  PrepareIniSection(V2O(Args.Values[0]) as TStrings);
end;


procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
    AddFun('RAUtils', 'ReplaceAllSokr', RAI2_ReplaceAllSokr, 3, [varString, varObject, varObject], varEmpty);
    AddFun('RAUtils', 'ReplaceSokr', RAI2_ReplaceSokr, 6, [varString, varInteger, varInteger, varObject, varObject, varInteger or varByRef], varEmpty);
    AddFun('RAUtils', 'CountOfLines', RAI2_CountOfLines, 1, [varString], varEmpty);
    AddFun('RAUtils', 'DeleteEmptyLines', RAI2_DeleteEmptyLines, 1, [varObject], varEmpty);
    AddFun('RAUtils', 'SQLAddWhere', RAI2_SQLAddWhere, 2, [varObject, varString], varEmpty);
    AddFun('RAUtils', 'ResSaveToFile', RAI2_ResSaveToFile, 4, [varString, varString, varBoolean, varString], varEmpty);
    AddFun('RAUtils', 'ResSaveToFileEx', RAI2_ResSaveToFileEx, 5, [varEmpty, varEmpty, varEmpty, varBoolean, varString], varEmpty);
    AddFun('RAUtils', 'ResSaveToString', RAI2_ResSaveToString, 4, [varEmpty, varString, varString, varString or varByRef], varEmpty);
    AddFun('RAUtils', 'Execute', RAI2_Execute, 2, [varString, varString], varEmpty);
    AddFun('RAUtils', 'IniReadSection', RAI2_IniReadSection, 3, [varEmpty, varString, varObject], varEmpty);
    AddFun('RAUtils', 'LoadTextFile', RAI2_LoadTextFile, 1, [varEmpty], varEmpty);
    AddFun('RAUtils', 'SaveTextFile', RAI2_SaveTextFile, 2, [varEmpty, varString], varEmpty);
    AddFun('RAUtils', 'ReadFolder', RAI2_ReadFolder, 3, [varEmpty, varEmpty, varObject], varEmpty);
    AddFun('RAUtils', 'ReadFolders', RAI2_ReadFolders, 2, [varEmpty, varObject], varEmpty);
   {$IFDEF RA_D3H}
    AddFun('RAUtils', 'TargetFileName', RAI2_TargetFileName, 1, [varEmpty], varEmpty);
    AddFun('RAUtils', 'ResolveLink', RAI2_ResolveLink, 3, [varEmpty, varEmpty, varEmpty or varByRef], varEmpty);
   {$ENDIF RA_D3H}
    AddFun('RAUtils', 'LoadIcoToImage', RAI2_LoadIcoToImage, 3, [varObject, varObject, varString], varEmpty);
    AddFun('RAUtils', 'RATextOut', RAI2_RATextOut, 4, [varObject, varEmpty, varEmpty, varString], varEmpty);
    AddFun('RAUtils', 'RATextOutEx', RAI2_RATextOutEx, 5, [varObject, varEmpty, varEmpty, varString, varBoolean], varEmpty);
    AddFun('RAUtils', 'RATextCalcHeight', RAI2_RATextCalcHeight, 3, [varObject, varEmpty, varString], varEmpty);
    AddFun('RAUtils', 'Roughed', RAI2_Roughed, 3, [varObject, varEmpty, varBoolean], varEmpty);
    AddFun('RAUtils', 'BitmapFromBitmap', RAI2_BitmapFromBitmap, 4, [varObject, varInteger, varInteger, varInteger], varEmpty);
    AddFun('RAUtils', 'TextWidth', RAI2_TextWidth, 1, [varString], varEmpty);
    AddFun('RAUtils', 'DefineCursor', RAI2_DefineCursor, 1, [varEmpty], varEmpty);
    AddFun('RAUtils', 'FindFormByClassName', RAI2_FindFormByClassName, 1, [varString], varEmpty);
    AddFun('RAUtils', 'FindByTag', RAI2_FindByTag, 3, [varObject, varEmpty, varInteger], varEmpty);
    AddFun('RAUtils', 'ControlAtPos2', RAI2_ControlAtPos2, 3, [varObject, varInteger, varInteger], varEmpty);
    AddFun('RAUtils', 'RBTag', RAI2_RBTag, 1, [varObject], varEmpty);
    AddFun('RAUtils', 'AppMinimized', RAI2_AppMinimized, 0, [0], varEmpty);
    AddFun('RAUtils', 'MsgDlg2', RAI2_MsgDlg2, 6, [varString, varString, varEmpty, varEmpty, varInteger, varObject], varEmpty);
    AddFun('RAUtils', 'MsgDlgDef', RAI2_MsgDlgDef, 7, [varString, varString, varEmpty, varEmpty, varEmpty, varInteger, varObject], varEmpty);
    AddFun('RAUtils', 'Delay', RAI2_Delay, 1, [varEmpty], varEmpty);
    //AddFun('RAUtils', 'CenterHor', RAI2_CenterHor, 3, [varObject, varInteger, varEmpty], nil);
    AddFun('RAUtils', 'EnableControls', RAI2_EnableControls, 2, [varObject, varBoolean], varEmpty);
    AddFun('RAUtils', 'EnableMenuItems', RAI2_EnableMenuItems, 3, [varObject, varInteger, varBoolean], varEmpty);
    //AddFun('RAUtils', 'ExpandWidth', RAI2_ExpandWidth, 3, [varObject, varInteger, varEmpty], nil);
    AddFun('RAUtils', 'PanelBorder', RAI2_PanelBorder, 1, [varObject], varEmpty);
    AddFun('RAUtils', 'Pixels', RAI2_Pixels, 2, [varObject, varInteger], varEmpty);
    AddFun('RAUtils', 'SetChildPropOrd', RAI2_SetChildPropOrd, 3, [varObject, varString, varEmpty], varEmpty);
    AddFun('RAUtils', 'Error', RAI2_Error, 1, [varString], varEmpty);
    AddFun('RAUtils', 'ItemHtDrawEx', RAI2_ItemHtDrawEx, 8, [varObject, varEmpty, varEmpty, varString, varBoolean, varString or varByRef, varInteger or varByRef, varBoolean], varEmpty);
    AddFun('RAUtils', 'ItemHtDraw', RAI2_ItemHtDraw, 5, [varObject, varEmpty, varEmpty, varString, varBoolean], varEmpty);
    AddFun('RAUtils', 'ItemHtWidth', RAI2_ItemHtWidth, 5, [varObject, varEmpty, varEmpty, varString, varBoolean], varEmpty);
    AddFun('RAUtils', 'ItemHtPlain', RAI2_ItemHtPlain, 1, [varString], varEmpty);
    AddFun('RAUtils', 'ClearList', RAI2_ClearList, 1, [varObject], varEmpty);
    AddFun('RAUtils', 'MemStreamToClipBoard', RAI2_MemStreamToClipBoard, 2, [varObject, varSmallint], varEmpty);
    AddFun('RAUtils', 'ClipBoardToMemStream', RAI2_ClipBoardToMemStream, 2, [varObject, varSmallint], varEmpty);
    AddFun('RAUtils', 'GetPropType', RAI2_GetPropType, 2, [varObject, varString], varEmpty);
    AddFun('RAUtils', 'GetPropStr', RAI2_GetPropStr, 2, [varObject, varString], varEmpty);
    AddFun('RAUtils', 'GetPropOrd', RAI2_GetPropOrd, 2, [varObject, varString], varEmpty);
    AddFun('RAUtils', 'CompareMem', RAI2_CompareMem, 3, [varPointer, varPointer, varInteger], varEmpty);
    AddFun('RAUtils', 'ShowMenu', RAI2_ShowMenu, 2, [varObject, varEmpty], varEmpty);
    AddFun('RAUtils', 'PrepareIniSection', RAI2_PrepareIniSection, 1, [varObject], varEmpty);

    AddFun('RAUtilsW', 'GetWordOnPos', RAI2_GetWordOnPos, 2, [varString, varInteger], varEmpty);
    AddFun('RAUtilsW', 'GetWordOnPosEx', RAI2_GetWordOnPosEx, 4, [varString, varInteger, varInteger or varByRef, varInteger or varByRef], varEmpty);
    AddFun('RAUtilsW', 'SubStr', RAI2_SubStr, 3, [varString, varInteger, varString], varEmpty);
    AddFun('RAUtilsW', 'SubStrEnd', RAI2_SubStrEnd, 3, [varString, varInteger, varString], varEmpty);
    AddFun('RAUtilsW', 'NumberByWord', RAI2_NumberByWord, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'GetLineByPos', RAI2_GetLineByPos, 2, [varString, varInteger], varEmpty);
    AddFun('RAUtilsW', 'GetXYByPos', RAI2_GetXYByPos, 4, [varString, varInteger, varInteger or varByRef, varInteger or varByRef], varEmpty);
    AddFun('RAUtilsW', 'ReplaceString', RAI2_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFun('RAUtilsW', 'ReplaceSokr1', RAI2_ReplaceString, 3, [varString, varString, varString], varEmpty);
    AddFun('RAUtilsW', 'ConcatSep', RAI2_ConcatSep, 3, [varString, varString, varString], varEmpty);
    AddFun('RAUtilsW', 'ConcatLeftSep', RAI2_ConcatLeftSep, 3, [varString, varString, varString], varEmpty);
    AddFun('RAUtilsW', 'MinimizeString', RAI2_MinimizeString, 2, [varString, varInteger], varEmpty);
    AddFun('RAUtilsW', 'Dos2Win', RAI2_Dos2Win, 1, [varString or varByRef], varEmpty);
    AddFun('RAUtilsW', 'Win2Dos', RAI2_Win2Dos, 1, [varString or varByRef], varEmpty);
    AddFun('RAUtilsW', 'Dos2WinRes', RAI2_Dos2WinRes, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'Win2DosRes', RAI2_Win2DosRes, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'Win2Koi', RAI2_Win2Koi, 1, [varString], varString);
    AddFun('RAUtilsW', 'Spaces', RAI2_Spaces, 1, [varInteger], varEmpty);
    AddFun('RAUtilsW', 'AddSpaces', RAI2_AddSpaces, 2, [varString, varInteger], varEmpty);
    AddFun('RAUtilsW', 'LastDate', RAI2_LastDate, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'CurrencyToStr', RAI2_CurrencyToStr, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'Cmp', RAI2_Cmp, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'StringCat', RAI2_StringCat, 2, [varString or varByRef, varString], varEmpty);
    AddFun('RAUtilsW', 'HasChar', RAI2_HasChar, 2, [varEmpty, varString], varEmpty);
    AddFun('RAUtilsW', 'HasAnyChar', RAI2_HasAnyChar, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'CountOfChar', RAI2_CountOfChar, 2, [varEmpty, varString], varEmpty);
    AddFun('RAUtilsW', 'DefStr', RAI2_DefStr, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'GetWinDir', RAI2_GetWinDir, 0, [0], varEmpty);
    AddFun('RAUtilsW', 'GetTempDir', RAI2_GetTempDir, 0, [0], varEmpty);
    AddFun('RAUtilsW', 'GenTempFileName', RAI2_GenTempFileName, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'GenTempFileNameExt', RAI2_GenTempFileNameExt, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'ClearDir', RAI2_ClearDir, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'DeleteDir', RAI2_DeleteDir, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'FileEquMask', RAI2_FileEquMask, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'FileEquMasks', RAI2_FileEquMasks, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'DeleteFiles', RAI2_DeleteFiles, 2, [varEmpty, varString], varEmpty);
    AddFun('RAUtilsW', 'LZFileExpand', RAI2_LZFileExpand, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'FileGetInfo', RAI2_FileGetInfo, 2, [varEmpty, varEmpty or varByRef], varEmpty);
    AddFun('RAUtilsW', 'HasSubFolder', RAI2_HasSubFolder, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'IsEmptyFolder', RAI2_IsEmptyFolder, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'AddSlash', RAI2_AddSlash, 1, [varEmpty or varByRef], varEmpty);
    AddFun('RAUtilsW', 'AddSlash2', RAI2_AddSlash2, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'AddPath', RAI2_AddPath, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'AddPaths', RAI2_AddPaths, 2, [varString, varString], varEmpty);
    AddFun('RAUtilsW', 'ParentPath', RAI2_ParentPath, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'FindInPath', RAI2_FindInPath, 2, [varString, varString], varEmpty);
   {$IFNDEF RA_B1}
    AddFun('RAUtilsW', 'BrowseForFolder', RAI2_BrowseForFolder, 3, [varEmpty, varString, varString or varByRef], varEmpty);
   {$ENDIF RA_B1}
    AddFun('RAUtilsW', 'DeleteReadOnlyFile', RAI2_DeleteReadOnlyFile, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'HasParam', RAI2_HasParam, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'HasSwitch', RAI2_HasSwitch, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'Switch', RAI2_Switch, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'ExePath', RAI2_ExePath, 0, [0], varEmpty);
    AddFun('RAUtilsW', 'CopyDir', RAI2_CopyDir, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'TTFontSelected', RAI2_TTFontSelected, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'TrueInflateRect', RAI2_TrueInflateRect, 2, [varEmpty, varInteger], varEmpty);
    AddFun('RAUtilsW', 'SetWindowTop', RAI2_SetWindowTop, 2, [varEmpty, varBoolean], varEmpty);
    AddFun('RAUtilsW', 'KeyPressed', RAI2_KeyPressed, 1, [varInteger], varEmpty);
    AddFun('RAUtilsW', 'Max', RAI2_Max, 2, [varInteger, varInteger], varEmpty);
    AddFun('RAUtilsW', 'Min', RAI2_Min, 2, [varInteger, varInteger], varEmpty);
    AddFun('RAUtilsW', 'SwapInt', RAI2_SwapInt, 2, [varInteger or varByRef, varInteger or varByRef], varEmpty);
    AddFun('RAUtilsW', 'IntPower', RAI2_IntPower, 2, [varInteger, varInteger], varEmpty);
    AddFun('RAUtilsW', 'ChangeTopException', RAI2_ChangeTopException, 1, [varObject], varEmpty);
    AddFun('RAUtilsW', 'MakeValidFileName', RAI2_MakeValidFileName, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'AnsiStrLIComp', RAI2_AnsiStrLIComp, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'Var2Type', RAI2_Var2Type, 2, [varEmpty, varInteger], varEmpty);
    AddFun('RAUtilsW', 'VarToInt', RAI2_VarToInt, 1, [varEmpty], varEmpty);
    AddFun('RAUtilsW', 'GetSubStr', RAI2_GetSubStr, 3, [varString, varInteger, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'GetParameter', RAI2_GetParameter, 0, [0], varEmpty);
    AddFun('RAUtilsW', 'GetLongFileName', RAI2_GetLongFileName, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'DirectoryExists', RAI2_DirectoryExists, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'ForceDirectories', RAI2_ForceDirectories, 1, [varString], varEmpty);
    AddFun('RAUtilsW', 'FileNewExt', RAI2_FileNewExt, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('RAUtilsW', 'GetComputerID', RAI2_GetComputerID, 0, [0], varEmpty);
  end;    { with }
end;    { RegisterRAI2Adapter }

end.

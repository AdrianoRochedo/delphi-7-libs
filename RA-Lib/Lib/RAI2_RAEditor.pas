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

unit RAI2_RAEditor;

interface

uses RAI2;

  procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);

implementation

uses Classes, RAEditor, RAHLEditor, RAI2_Windows;


  { TKeyboard }

{ constructor Create }
procedure TKeyboard_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TKeyboard.Create);
end;

{  procedure Add(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState); }
procedure TKeyboard_Add(var Value: Variant; Args: TArgs);
begin
  TKeyboard(Args.Obj).Add(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))));
end;

{  procedure Add2(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState); }
procedure TKeyboard_Add2(var Value: Variant; Args: TArgs);
begin
  TKeyboard(Args.Obj).Add2(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))), Args.Values[3], TShiftState(Byte(V2S(Args.Values[4]))));
end;

{  procedure Clear; }
procedure TKeyboard_Clear(var Value: Variant; Args: TArgs);
begin
  TKeyboard(Args.Obj).Clear;
end;

{  procedure SetDefLayot; }
procedure TKeyboard_SetDefLayot(var Value: Variant; Args: TArgs);
begin
  TKeyboard(Args.Obj).SetDefLayot;
end;

  { ERAEditorError }

  { TRACustomEditor }

{ constructor Create(AOwner: TComponent) }
procedure TRACustomEditor_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TRACustomEditor.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure SetLeftTop(ALeftCol, ATopRow: integer); }
procedure TRACustomEditor_SetLeftTop(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SetLeftTop(Args.Values[0], Args.Values[1]);
end;

{  procedure ClipBoardCopy; }
procedure TRACustomEditor_ClipBoardCopy(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).ClipBoardCopy;
end;

{  procedure ClipBoardPaste; }
procedure TRACustomEditor_ClipBoardPaste(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).ClipBoardPaste;
end;

{  procedure ClipBoardCut; }
procedure TRACustomEditor_ClipBoardCut(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).ClipBoardCut;
end;

{  procedure DeleteSelected; }
procedure TRACustomEditor_DeleteSelected(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).DeleteSelected;
end;

{  function CalcCellRect(const X, Y: integer): TRect; }
procedure TRACustomEditor_CalcCellRect(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(TRACustomEditor(Args.Obj).CalcCellRect(Args.Values[0], Args.Values[1]));
end;

{  procedure SetCaret(X, Y: integer); }
procedure TRACustomEditor_SetCaret(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SetCaret(Args.Values[0], Args.Values[1]);
end;

{  procedure CaretFromPos(const Pos: integer; var X, Y: integer); }
procedure TRACustomEditor_CaretFromPos(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).CaretFromPos(Args.Values[0], TVarData(Args.Values[1]).vInteger, TVarData(Args.Values[2]).vInteger);
end;

{  function PosFromCaret(const X, Y: integer): integer; }
procedure TRACustomEditor_PosFromCaret(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).PosFromCaret(Args.Values[0], Args.Values[1]);
end;

{  procedure PaintCaret(const bShow: boolean); }
procedure TRACustomEditor_PaintCaret(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).PaintCaret(Args.Values[0]);
end;

{  function GetTextLen: Integer; }
procedure TRACustomEditor_GetTextLen(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).GetTextLen;
end;

{  function GetSelText: string; }
procedure TRACustomEditor_GetSelText(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).GetSelText;
end;

{  procedure SetSelText(const AValue: string); }
procedure TRACustomEditor_SetSelText(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SetSelText(Args.Values[0]);
end;

{  function GetWordOnCaret: string; }
procedure TRACustomEditor_GetWordOnCaret(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).GetWordOnCaret;
end;

{  procedure BeginUpdate; }
procedure TRACustomEditor_BeginUpdate(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).BeginUpdate;
end;

{  procedure EndUpdate; }
procedure TRACustomEditor_EndUpdate(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).EndUpdate;
end;

{  procedure MakeRowVisible(ARow: Integer); }
procedure TRACustomEditor_MakeRowVisible(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).MakeRowVisible(Args.Values[0]);
end;

{  procedure Command(ACommand: TEditCommand); }
procedure TRACustomEditor_Command(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).Command(Args.Values[0]);
end;

{  procedure PostCommand(ACommand: TEditCommand); }
procedure TRACustomEditor_PostCommand(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).PostCommand(Args.Values[0]);
end;

{  procedure InsertText(const Text: string); }
procedure TRACustomEditor_InsertText(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).InsertText(Args.Values[0]);
end;

{  procedure ReplaceWord(const NewString: string); }
procedure TRACustomEditor_ReplaceWord(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).ReplaceWord(Args.Values[0]);
end;

{  procedure ReplaceWord2(const NewString: string); }
procedure TRACustomEditor_ReplaceWord2(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).ReplaceWord2(Args.Values[0]);
end;

{  procedure BeginCompound; }
procedure TRACustomEditor_BeginCompound(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).BeginCompound;
end;

{  procedure EndCompound; }
procedure TRACustomEditor_EndCompound(var Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).EndCompound;
end;

{  function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint; }
procedure TRACustomEditor_GetText(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).GetText(Args.Values[0], PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ property Read LeftCol: integer }
procedure TRACustomEditor_Read_LeftCol(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).LeftCol;
end;

{ property Read TopRow: integer }
procedure TRACustomEditor_Read_TopRow(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).TopRow;
end;

{ property Read VisibleColCount: integer }
procedure TRACustomEditor_Read_VisibleColCount(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).VisibleColCount;
end;

{ property Read VisibleRowCount: integer }
procedure TRACustomEditor_Read_VisibleRowCount(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).VisibleRowCount;
end;

{ property Read LastVisibleCol: integer }
procedure TRACustomEditor_Read_LastVisibleCol(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).LastVisibleCol;
end;

{ property Read LastVisibleRow: integer }
procedure TRACustomEditor_Read_LastVisibleRow(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).LastVisibleRow;
end;

{ property Read Cols: integer }
procedure TRACustomEditor_Read_Cols(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).Cols;
end;

{ property Write Cols(Value: integer) }
procedure TRACustomEditor_Write_Cols(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).Cols := Value;
end;

{ property Read Rows: integer }
procedure TRACustomEditor_Read_Rows(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).Rows;
end;

{ property Write Rows(Value: integer) }
procedure TRACustomEditor_Write_Rows(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).Rows := Value;
end;

{ property Read CaretX: integer }
procedure TRACustomEditor_Read_CaretX(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).CaretX;
end;

{ property Write CaretX(Value: integer) }
procedure TRACustomEditor_Write_CaretX(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).CaretX := Value;
end;

{ property Read CaretY: integer }
procedure TRACustomEditor_Read_CaretY(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).CaretY;
end;

{ property Write CaretY(Value: integer) }
procedure TRACustomEditor_Write_CaretY(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).CaretY := Value;
end;

{ property Read Modified: boolean }
procedure TRACustomEditor_Read_Modified(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).Modified;
end;

{ property Write Modified(Value: boolean) }
procedure TRACustomEditor_Write_Modified(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).Modified := Value;
end;

{ property Read SelStart: integer }
procedure TRACustomEditor_Read_SelStart(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: integer) }
procedure TRACustomEditor_Write_SelStart(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SelStart := Value;
end;

{ property Read SelLength: integer }
procedure TRACustomEditor_Read_SelLength(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: integer) }
procedure TRACustomEditor_Write_SelLength(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SelLength := Value;
end;

{ property Read SelText: string }
procedure TRACustomEditor_Read_SelText(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }
procedure TRACustomEditor_Write_SelText(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).SelText := Value;
end;

(*
{ property Read BookMarks: TBookMarks }
procedure TRACustomEditor_Read_BookMarks(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).BookMarks;
end;
*)

{ property Read Keyboard: TKeyboard }
procedure TRACustomEditor_Read_Keyboard(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TRACustomEditor(Args.Obj).Keyboard);
end;

(*
{ property Read CellRect: TCellRect }
procedure TRACustomEditor_Read_CellRect(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).CellRect;
end;
*)

{ property Read UndoBuffer: TUndoBuffer }
procedure TRACustomEditor_Read_UndoBuffer(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TRACustomEditor(Args.Obj).UndoBuffer);
end;

{ property Read Recording: boolean }
procedure TRACustomEditor_Read_Recording(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).Recording;
end;

procedure TRACustomEditor_Read_BookMarkX(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X;
end;

procedure TRACustomEditor_Read_BookMarkY(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y;
end;

procedure TRACustomEditor_Read_BookMarkValid(var Value: Variant; Args: TArgs);
begin
  Value := TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid;
end;

procedure TRACustomEditor_Write_BookMarkX(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X := Value;
end;

procedure TRACustomEditor_Write_BookMarkY(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y := Value;
end;

procedure TRACustomEditor_Write_BookMarkValid(const Value: Variant; Args: TArgs);
begin
  TRACustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid := Value;
end;


procedure RegisterRAI2Adapter(RAI2Adapter: TRAI2Adapter);
begin
  with RAI2Adapter do
  begin
   { TModifiedAction }
    AddConst('RAEditor', 'maInsert', maInsert);
    AddConst('RAEditor', 'maDelete', maDelete);
   { TKeyboard }
    AddClass('RAEditor', TKeyboard, 'TKeyboard');
    AddGet(TKeyboard, 'Create', TKeyboard_Create, 0, [0], varEmpty);
    AddGet(TKeyboard, 'Add', TKeyboard_Add, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TKeyboard, 'Add2', TKeyboard_Add2, 5, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TKeyboard, 'Clear', TKeyboard_Clear, 0, [0], varEmpty);
    AddGet(TKeyboard, 'SetDefLayot', TKeyboard_SetDefLayot, 0, [0], varEmpty);
   { ERAEditorError }
    AddClass('RAEditor', ERAEditorError, 'ERAEditorError');
   { TTabStop }
    AddConst('RAEditor', 'tsTabStop', tsTabStop);
    AddConst('RAEditor', 'tsAutoIndent', tsAutoIndent);
   { TRACustomEditor }
    AddClass('RAEditor', TRACustomEditor, 'TRACustomEditor');
    AddGet(TRACustomEditor, 'Create', TRACustomEditor_Create, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'SetLeftTop', TRACustomEditor_SetLeftTop, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'ClipBoardCopy', TRACustomEditor_ClipBoardCopy, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'ClipBoardPaste', TRACustomEditor_ClipBoardPaste, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'ClipBoardCut', TRACustomEditor_ClipBoardCut, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'DeleteSelected', TRACustomEditor_DeleteSelected, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'CalcCellRect', TRACustomEditor_CalcCellRect, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'SetCaret', TRACustomEditor_SetCaret, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'CaretFromPos', TRACustomEditor_CaretFromPos, 3, [varEmpty, varByRef, varByRef], varEmpty);
    AddGet(TRACustomEditor, 'PosFromCaret', TRACustomEditor_PosFromCaret, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'PaintCaret', TRACustomEditor_PaintCaret, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'GetTextLen', TRACustomEditor_GetTextLen, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'GetSelText', TRACustomEditor_GetSelText, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'SetSelText', TRACustomEditor_SetSelText, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'GetWordOnCaret', TRACustomEditor_GetWordOnCaret, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'BeginUpdate', TRACustomEditor_BeginUpdate, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'EndUpdate', TRACustomEditor_EndUpdate, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'MakeRowVisible', TRACustomEditor_MakeRowVisible, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'Command', TRACustomEditor_Command, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'PostCommand', TRACustomEditor_PostCommand, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'InsertText', TRACustomEditor_InsertText, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'ReplaceWord', TRACustomEditor_ReplaceWord, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'ReplaceWord2', TRACustomEditor_ReplaceWord2, 1, [varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'BeginCompound', TRACustomEditor_BeginCompound, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'EndCompound', TRACustomEditor_EndCompound, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'GetText', TRACustomEditor_GetText, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TRACustomEditor, 'LeftCol', TRACustomEditor_Read_LeftCol, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'TopRow', TRACustomEditor_Read_TopRow, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'VisibleColCount', TRACustomEditor_Read_VisibleColCount, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'VisibleRowCount', TRACustomEditor_Read_VisibleRowCount, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'LastVisibleCol', TRACustomEditor_Read_LastVisibleCol, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'LastVisibleRow', TRACustomEditor_Read_LastVisibleRow, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'Cols', TRACustomEditor_Read_Cols, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'Cols', TRACustomEditor_Write_Cols, 0, [0]);
    AddGet(TRACustomEditor, 'Rows', TRACustomEditor_Read_Rows, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'Rows', TRACustomEditor_Write_Rows, 0, [0]);
    AddGet(TRACustomEditor, 'CaretX', TRACustomEditor_Read_CaretX, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'CaretX', TRACustomEditor_Write_CaretX, 0, [0]);
    AddGet(TRACustomEditor, 'CaretY', TRACustomEditor_Read_CaretY, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'CaretY', TRACustomEditor_Write_CaretY, 0, [0]);
    AddGet(TRACustomEditor, 'Modified', TRACustomEditor_Read_Modified, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'Modified', TRACustomEditor_Write_Modified, 0, [0]);
    AddGet(TRACustomEditor, 'SelStart', TRACustomEditor_Read_SelStart, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'SelStart', TRACustomEditor_Write_SelStart, 0, [0]);
    AddGet(TRACustomEditor, 'SelLength', TRACustomEditor_Read_SelLength, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'SelLength', TRACustomEditor_Write_SelLength, 0, [0]);
    AddGet(TRACustomEditor, 'SelText', TRACustomEditor_Read_SelText, 0, [0], varEmpty);
    AddSet(TRACustomEditor, 'SelText', TRACustomEditor_Write_SelText, 0, [0]);
   // AddGet(TRACustomEditor, 'BookMarks', TRACustomEditor_Read_BookMarks, 0, [0], nil);
    AddGet(TRACustomEditor, 'Keyboard', TRACustomEditor_Read_Keyboard, 0, [0], varEmpty);
   // AddGet(TRACustomEditor, 'CellRect', TRACustomEditor_Read_CellRect, 0, [0], nil);
    AddGet(TRACustomEditor, 'UndoBuffer', TRACustomEditor_Read_UndoBuffer, 0, [0], varEmpty);
    AddGet(TRACustomEditor, 'Recording', TRACustomEditor_Read_Recording, 0, [0], varEmpty);

    AddIGet(TRACustomEditor, 'BookMarkX', TRACustomEditor_Read_BookMarkX, 1, [varInteger], varEmpty);
    AddIGet(TRACustomEditor, 'BookMarkY', TRACustomEditor_Read_BookMarkY, 1, [varInteger], varEmpty);
    AddIGet(TRACustomEditor, 'BookMarkValid', TRACustomEditor_Read_BookMarkValid, 1, [varInteger], varEmpty);
    AddISet(TRACustomEditor, 'BookMarkX', TRACustomEditor_Write_BookMarkX, 1, [varInteger]);
    AddISet(TRACustomEditor, 'BookMarkY', TRACustomEditor_Write_BookMarkY, 1, [varInteger]);
    AddISet(TRACustomEditor, 'BookMarkValid', TRACustomEditor_Write_BookMarkValid, 1, [varInteger]);

   { TCompletionList }
    AddConst('RAEditor', 'cmIdentifers', cmIdentifers);
    AddConst('RAEditor', 'cmTemplates', cmTemplates);

    AddConst('RAEditor', 'ecCharFirst', ecCharFirst);
    AddConst('RAEditor', 'ecCharLast', ecCharLast);
    AddConst('RAEditor', 'ecCommandFirst', ecCommandFirst);
    AddConst('RAEditor', 'ecUser', ecUser);
    AddConst('RAEditor', 'ecLeft', ecLeft);
    AddConst('RAEditor', 'ecUp', ecUp);
    AddConst('RAEditor', 'ecRight', ecRight);
    AddConst('RAEditor', 'ecDown', ecDown);
    AddConst('RAEditor', 'ecSelLeft', ecSelLeft);
    AddConst('RAEditor', 'ecSelUp', ecSelUp);
    AddConst('RAEditor', 'ecSelRight', ecSelRight);
    AddConst('RAEditor', 'ecSelDown', ecSelDown);
    AddConst('RAEditor', 'ecPrevWord', ecPrevWord);
    AddConst('RAEditor', 'ecNextWord', ecNextWord);
    AddConst('RAEditor', 'ecSelPrevWord', ecSelPrevWord);
    AddConst('RAEditor', 'ecSelNextWord', ecSelNextWord);
    AddConst('RAEditor', 'ecSelWord', ecSelWord);
    AddConst('RAEditor', 'ecWindowTop', ecWindowTop);
    AddConst('RAEditor', 'ecWindowBottom', ecWindowBottom);
    AddConst('RAEditor', 'ecPrevPage', ecPrevPage);
    AddConst('RAEditor', 'ecNextPage', ecNextPage);
    AddConst('RAEditor', 'ecSelPrevPage', ecSelPrevPage);
    AddConst('RAEditor', 'ecSelNextPage', ecSelNextPage);
    AddConst('RAEditor', 'ecBeginLine', ecBeginLine);
    AddConst('RAEditor', 'ecEndLine', ecEndLine);
    AddConst('RAEditor', 'ecBeginDoc', ecBeginDoc);
    AddConst('RAEditor', 'ecEndDoc', ecEndDoc);
    AddConst('RAEditor', 'ecSelBeginLine', ecSelBeginLine);
    AddConst('RAEditor', 'ecSelEndLine', ecSelEndLine);
    AddConst('RAEditor', 'ecSelBeginDoc', ecSelBeginDoc);
    AddConst('RAEditor', 'ecSelEndDoc', ecSelEndDoc);
    AddConst('RAEditor', 'ecSelAll', ecSelAll);
    AddConst('RAEditor', 'ecScrollLineUp', ecScrollLineUp);
    AddConst('RAEditor', 'ecScrollLineDown', ecScrollLineDown);
    AddConst('RAEditor', 'ecInsertPara', ecInsertPara);
    AddConst('RAEditor', 'ecBackspace', ecBackspace);
    AddConst('RAEditor', 'ecDelete', ecDelete);
    AddConst('RAEditor', 'ecChangeInsertMode', ecChangeInsertMode);
    AddConst('RAEditor', 'ecTab', ecTab);
    AddConst('RAEditor', 'ecBackTab', ecBackTab);
    AddConst('RAEditor', 'ecIndent', ecIndent);
    AddConst('RAEditor', 'ecUnindent', ecUnindent);
    AddConst('RAEditor', 'ecDeleteSelected', ecDeleteSelected);
    AddConst('RAEditor', 'ecClipboardCopy', ecClipboardCopy);
    AddConst('RAEditor', 'ecClipboardCut', ecClipboardCut);
    AddConst('RAEditor', 'ecClipBoardPaste', ecClipBoardPaste);
    AddConst('RAEditor', 'ecDeleteLine', ecDeleteLine);
    AddConst('RAEditor', 'ecDeleteWord', ecDeleteWord);
    AddConst('RAEditor', 'ecToUpperCase', ecToUpperCase);
    AddConst('RAEditor', 'ecToLowerCase', ecToLowerCase);
    AddConst('RAEditor', 'ecChangeCase', ecChangeCase);
    AddConst('RAEditor', 'ecUndo', ecUndo);
    AddConst('RAEditor', 'ecRedo', ecRedo);
    AddConst('RAEditor', 'ecBeginCompound', ecBeginCompound);
    AddConst('RAEditor', 'ecEndCompound', ecEndCompound);
    AddConst('RAEditor', 'ecBeginUpdate', ecBeginUpdate);
    AddConst('RAEditor', 'ecEndUpdate', ecEndUpdate);
    AddConst('RAEditor', 'ecSetBookmark0', ecSetBookmark0);
    AddConst('RAEditor', 'ecSetBookmark1', ecSetBookmark1);
    AddConst('RAEditor', 'ecSetBookmark2', ecSetBookmark2);
    AddConst('RAEditor', 'ecSetBookmark3', ecSetBookmark3);
    AddConst('RAEditor', 'ecSetBookmark4', ecSetBookmark4);
    AddConst('RAEditor', 'ecSetBookmark5', ecSetBookmark5);
    AddConst('RAEditor', 'ecSetBookmark6', ecSetBookmark6);
    AddConst('RAEditor', 'ecSetBookmark7', ecSetBookmark7);
    AddConst('RAEditor', 'ecSetBookmark8', ecSetBookmark8);
    AddConst('RAEditor', 'ecSetBookmark9', ecSetBookmark9);
    AddConst('RAEditor', 'ecGotoBookmark0', ecGotoBookmark0);
    AddConst('RAEditor', 'ecGotoBookmark1', ecGotoBookmark1);
    AddConst('RAEditor', 'ecGotoBookmark2', ecGotoBookmark2);
    AddConst('RAEditor', 'ecGotoBookmark3', ecGotoBookmark3);
    AddConst('RAEditor', 'ecGotoBookmark4', ecGotoBookmark4);
    AddConst('RAEditor', 'ecGotoBookmark5', ecGotoBookmark5);
    AddConst('RAEditor', 'ecGotoBookmark6', ecGotoBookmark6);
    AddConst('RAEditor', 'ecGotoBookmark7', ecGotoBookmark7);
    AddConst('RAEditor', 'ecGotoBookmark8', ecGotoBookmark8);
    AddConst('RAEditor', 'ecGotoBookmark9', ecGotoBookmark9);
    AddConst('RAEditor', 'ecCompletionIdentifers', ecCompletionIdentifers);
    AddConst('RAEditor', 'ecCompletionTemplates', ecCompletionTemplates);
    AddConst('RAEditor', 'ecRecordMacro', ecRecordMacro);
    AddConst('RAEditor', 'ecPlayMacro', ecPlayMacro);
    AddConst('RAEditor', 'ecBeginRecord', ecBeginRecord);
    AddConst('RAEditor', 'ecEndRecord', ecEndRecord);
  end;    { with }
  RegisterClasses([TRAEditor, TRAHLEditor]);
end;    { RegisterRAI2Adapter }

end.

{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                    OVCNF.PAS 2.15                     *}
{*     Copyright 1995-97 (c) TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OvcNF;
  {-Numeric field visual component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, Forms, Graphics, Menus, Messages, SysUtils,
  OvcBase, OvcCaret, OvcCmd, OvcColor, OvcConst,
  OvcData, OvcEf, OvcExcpt, OvcMisc, OvcPb, OvcStr, OvcVer;

type
  {numeric field types}
  TNumericDataType   = (
    nftLongInt, nftWord, nftInteger, nftByte, nftShortInt, nftReal,
    nftExtended, nftDouble, nftSingle, nftComp);

const
  nfDefNumericDataType = nftLongInt;
  nfDefNumericMask     = '##########';
  nfDefParentColor       = False;
  nfDefTabStop           = True;

type
  TOvcCustomNumericField = class(TOvcPictureBase)
  {.Z+}
  protected {private}
    {property instance variables}
    FNumericDataType   : TNumericDataType;
    FPictureMask       : string;

    {private instance variables}
    nfMaxLen    : Word;        {maximum length of numeric string}
    nfMaxDigits : Word;        {maximum # of digits to left of decimal}
    nfPlaces    : Word;        {# of decimal places}
    nfMinus     : Boolean;     {true if number is negative}
    nfTmp       : TEditString; {temporary input string}

    function nfGetDataType(Value : TNumericDataType) : Byte;
      {-return a Byte value representing the data type of this field}
    procedure nfReloadTmp;
      {-reload Tmp from efEditSt, etc.}
    procedure nfResetFieldProperties(FT : TNumericDataType);
      {-reset field properties}
    procedure nfSetDefaultRanges;
      {-set default range values based on the field type}
    procedure nfSetMaxLength(Mask : PAnsiChar);
      {-determine and set MaxLength}

    procedure WMSetFocus(var Msg : TWMSetFocus);
      message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus);
      message WM_KILLFOCUS;

  protected
    {VCL methods}
    procedure Assign(Source : TPersistent);
      override;
    procedure CreateParams(var Params : TCreateParams);
      override;
    procedure CreateWnd;
      override;

    procedure efCaretToEnd;
      override;
      {-move the caret to the end of the field}
    procedure efCaretToStart;
      override;
      {-move the caret to the beginning of the field}
    procedure efChangeMask(Mask : PAnsiChar);
      override;
      {-change the picture mask}
    procedure efEdit(var Msg : TMessage; Cmd : Word);
      override;
      {-process the specified editing command}
    function efGetDisplayString(Dest : PAnsiChar; Size : Word) : PAnsiChar;
      override;
      {-return the display string in Dest and a pointer as the result}
    procedure efIncDecValue(Wrap : Boolean; Delta : Double);
      override;
      {-increment field by Delta}
    function efTransfer(DataPtr : Pointer; TransferFlag : Word) : Word;
      override;
      {-transfer data to/from the entry fields}
        procedure pbRemoveSemiLits;
      override;
      {-remove semi-literal mask characters from the edit string}

    {virtual property methods}
    procedure efSetCaretPos(Value : Integer);
      override;
      {-set position of caret within the field}
    procedure nfSetDataType(Value : TNumericDataType);
      virtual;
      {-set the data type for this field}
    procedure nfSetPictureMask(const Value : string);
      virtual;
      {-set the picture mask}
  {.Z-}

  public
    constructor Create(AOwner: TComponent);
      override;

  {.Z+}
    function efValidateField : Word;
      override;
      {-validate contents of field; result is error code or 0}
  {.Z-}

    {public properties}
    property DataType : TNumericDataType
      read FNumericDataType
      write nfSetDataType
      default nfDefNumericDataType;

    property PictureMask : string
      read FPictureMask
      write nfSetPictureMask;
  end;

  TOvcNumericField = class(TOvcCustomNumericField)
  published
    {inherited properties}
    property DataType;              {needs to loaded before most other properties}
    property ArrowIncDec;                                              {!!.10}
    property AutoAdvanceChar;
    property AutoAdvanceLeftRight;
    property AutoAdvanceUpDown;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CaretIns;
    property CaretOvr;
    property Color;
    property Controller;
    property Ctl3D;
    property DisabledColors;                                           {!!.12}
    property DragCursor;
    property DragMode;
    property Enabled;
    property ErrorColors;
    property Font;
    property HighlightColors;
    property InputRequired;
    property PadChar;
    property ParentColor default nfDefParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PictureMask;
    property PopupMenu;
    property RangeHi stored False;
    property RangeLo stored False;
    property ReadOnly;
    property ShowHint;
    property SoftValidation;
    property TabOrder;
    property TabStop default nfDefTabStop;
    property Tag;
    property TextMargin;
    property Uninitialized;
    property Visible;
    property ZeroDisplay;
    property ZeroDisplayValue;

    {inherited events}
    property AfterEnter;                                               {!!.12}
    property AfterExit;                                                {!!.12}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnError;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF Win32}
    property OnStartDrag;
    {$ENDIF}
    property OnUserCommand;
    property OnUserValidation;
  end;

implementation

{$IFDEF TRIALRUN} uses OrTrial; {$ENDIF}


{*** TOvcCustomNumericField ***}

procedure TOvcCustomNumericField.Assign(Source : TPersistent);
var
  NF : TOvcCustomNumericField absolute Source;
begin
  if (Source <> nil) and (Source is TOvcCustomNumericField) then begin
    DataType             := NF.DataType;
    AutoAdvanceChar      := NF.AutoAdvanceChar;
    AutoAdvanceLeftRight := NF.AutoAdvanceLeftRight;
    AutoAdvanceUpDown    := NF.AutoAdvanceUpDown;
    AutoSelect           := NF.AutoSelect;
    AutoSize             := NF.AutoSize;
    BeepOnError          := NF.BeepOnError;
    BorderStyle          := NF.BorderStyle;
    CaretToEnd           := NF.CaretToEnd;
    Color                := NF.Color;
    ErrorColors.Assign(NF.ErrorColors);
    HighlightColors.Assign(NF.HighlightColors);
    InputRequired        := NF.InputRequired;
    PadChar              := NF.PadChar;
    PictureMask          := NF.PictureMask;
    RangeHi              := NF.RangeHi;
    RangeLo              := NF.RangeLo;
    ReadOnly             := NF.ReadOnly;
    SoftValidation       := NF.SoftValidation;
    TextMargin           := NF.TextMargin;
    Uninitialized        := NF.Uninitialized;
    ZeroDisplay          := NF.ZeroDisplay;
    ZeroDisplayValue     := NF.ZeroDisplayValue;
  end else
    inherited Assign(Source);
end;

constructor TOvcCustomNumericField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNumericDataType := nfDefNumericDataType;
  FPictureMask     := nfDefNumericMask;
  efFieldClass     := fcNumeric;
  efDataType       := nfGetDataType(FNumericDataType);
  efRangeHi.rtLong := High(LongInt);
  efRangeLo.rtLong := Low(LongInt);
end;

procedure TOvcCustomNumericField.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  pfSelPos := 0;

  {get current picture string}
  StrPLCopy(efPicture, FPictureMask, MaxPicture);

  {set MaxLength based on picture mask}
  nfSetMaxLength(efPicture);

  FillChar(nfTmp, SizeOf(nfTmp), #0);
  pfSemiLits := 0;
  pbCalcWidthAndPlaces(nfMaxLen, nfPlaces);

  {adjust max length for decimal point if needed}
  nfMaxDigits := nfMaxLen;
  if nfPlaces <> 0 then
    Dec(nfMaxDigits, nfPlaces+1);
end;

procedure TOvcCustomNumericField.CreateWnd;
var
  P : array[0..MaxEditLen+1] of Byte;
begin
  {save field data}
  if efSaveData then
    efTransfer(@P, otf_GetData);

  inherited CreateWnd;

  {try to optimize InitPictureFlags}
  pbOptimizeInitPictureFlags;

  pfSemiLits := 0;
  nfSetDefaultRanges;
  efSetInitialValue;

  {if we saved the field data, restore it}
  if efSaveData then
    efTransfer(@P, otf_SetData);

  {set save data flag}
  efSaveData := True;
end;

procedure TOvcCustomNumericField.efCaretToEnd;
  {-move the caret to the end of the field}
begin
  efHPos := efEditEnd + 1;
end;

procedure TOvcCustomNumericField.efCaretToStart;
  {-move the caret to the beginning of the field}
begin
  efHPos := efEditEnd + 1;
end;

procedure TOvcCustomNumericField.efChangeMask(Mask : PAnsiChar);
  {-change the picture mask}
begin
  inherited efChangeMask(Mask);

  pfSemiLits := 0;
  pbCalcWidthAndPlaces(nfMaxLen, nfPlaces);

  {set MaxLength based on picture mask}
  nfSetMaxLength(Mask);
  nfMaxDigits := nfMaxLen;
  if nfPlaces <> 0 then
    Dec(nfMaxDigits, nfPlaces+1);
end;

procedure TOvcCustomNumericField.efEdit(var Msg : TMessage; Cmd : Word);
  {-process the specified editing command}
label
  ExitPoint;
var
  MF         : Byte;
  Ch         : AnsiChar;
  HaveSel    : Boolean;
  PicChar    : AnsiChar;
  StLen      : Word;
  StBgn      : Word;
  StEnd      : Word;
  DotPos     : Cardinal;
  Found      : Boolean;

  function MinusVal : Byte;
  begin
    if nfMinus then
      Result := 1
    else
      Result := 0;
  end;

  procedure ClearString;
    {-clear the string being edited}
  begin
    nfTmp[0] := #0;
    nfMinus := False;
    StLen := 0;
  end;

  function CharIsOK : Boolean;
    {-return true if Ch can be added to the string}
  begin
    Result := (Ch >= ' ');
  end;

  function CheckAutoAdvance(SP : Integer) : Boolean;
    {-see if we need to auto-advance to next/previous field}
  begin
    Result := False;
    if (SP < 0) and AutoAdvanceLeftRight then begin
      efMoveFocusToPrevField;
      Result := True;
    end else if (SP > 0) then
      if (Cmd = ccChar) and AutoAdvanceChar then begin
        efMoveFocusToNextField;
        Result := True;
      end else if (Cmd <> ccChar) and AutoAdvanceLeftRight then begin
        efMoveFocusToNextField;
        Result := True;
      end;
  end;

  procedure DeleteChar;
    {-delete char at end of string}
  begin
    if (StLen = 0) then
      if not nfMinus then
        Exit
      else
        nfMinus := False
    else begin
      {remove the last character}
      nfTmp[StLen-1] := #0;
      Dec(StLen);

      {if all that's left is a 0, remove it}
      if (StLen = 1) and (nfTmp[0] = '0') then
        nfTmp[0] := #0;
    end;
    MF := 10;
  end;

  procedure DeleteSel;
  begin
    ClearString;
    efSelStart := 0;
    efSelEnd := 0;
    MF := 10;
  end;

  function InsertChar : Boolean;
    {-insert Ch}
  var
    tDotPos : Cardinal;
    tFound  : Boolean;

    function DigitCount : Word;
      {-return number of digits to left of decimal place in St}
    begin
      if tFound then
        Result := tDotPos + MinusVal
      else
        Result := StLen + MinusVal;
    end;

  begin
    Result := False;

    {reject spaces}
    if (Ch = ' ') then
      Exit;

    {ok to add decimal point?}
    tFound := StrChPos(nfTmp, pmDecimalPt, tDotPos);
    if (Ch = pmDecimalPt) then
      if not Found or tFound then
        Exit;

    if (Ch = '-') then begin
      {minus sign treated as toggle}
      if nfMinus then
        nfMinus := False
      else begin
        nfMinus := (DigitCount < nfMaxDigits) and (StLen < nfMaxLen);
        if not nfMinus then
          Exit;
      end
    end else if (StLen+MinusVal < nfMaxLen) then begin
      {don't allow initial zeros}
      if (Ch = '0') and (StLen = 0) then begin
        Result := True;
        Exit;
      end;

      {check for too many digits to left of decimal point}
      if Found and (Ch <> pmDecimalPt) then
        if not tFound and (DigitCount >= nfMaxDigits) then
          Exit;

      {append the character}
      nfTmp[StLen] := Ch;
      Inc(StLen);
      nfTmp[StLen] := #0;
    end else if (nfMaxLen = 1) then
      if (Ch = pmDecimalPt) then
        Exit
      else
        {overwrite the character}
        nfTmp[0] := Ch
    else
      Exit;

    Result := True;
  end;

  procedure Adjust;
    {-adjust display string to show correct number of decimal places}
  var
    Delta     : Integer;
    ActPlaces : Integer;
    DP        : Cardinal;
    Len       : Word;
    ExDec     : TEditString;
  begin
    Len := StrLen(nfTmp);
    if not StrChPos(nfTmp, pmDecimalPt, DP) then
      Delta := nfPlaces+1
    else begin
      ActPlaces := Len-Succ(DP);
      Delta := nfPlaces-ActPlaces;
    end;

    if Delta = 0 then
      Exit;

    if Delta > 0 then begin
      StrStDeletePrim(efEditSt, StEnd-Pred(Delta), Delta);
      StrStInsertPrim(efEditSt, CharStrPChar(ExDec, ' ', Delta), StBgn);
    end else begin
      Delta := -Delta;
      StrStCopy(ExDec, nfTmp, DP+nfPlaces+1, Delta);
      StrStDeletePrim(efEditSt, StBgn, Delta);
      StrStInsertPrim(efEditSt, ExDec, StEnd-Pred(Delta));
    end;
  end;

  procedure UpdateEditSt;
    {-update efEditSt}
  begin
    StrCopy(efEditSt, nfTmp);
    case efEditSt[0] of
      #0 :
        begin
          {string is empty, put in a 0}
          efEditSt[0] := '0';
          efEditSt[1] := #0;
        end;
      '.' :
        StrChInsertPrim(efEditSt, '0', 0);
    end;

    {prepend the minus sign}
    if nfMinus then
      StrChInsertPrim(efEditSt, '-', 0);

    pbMergePicture(efEditSt, efEditSt);
    if Found then
      Adjust;
  end;

  procedure UpdateSel(Delta : Integer);
  begin
    if Delta <> 0 then begin
      efSelStart := 0;
      efSelEnd := MaxEditLen;
    end else begin
      efSelStart := 0;
      efSelEnd := 0;
    end;
  end;

  procedure PastePrim(P : PAnsiChar);
  begin
    if HaveSel then
      DeleteSel;
    while P^ <> #0 do begin
      Ch := P^;
      if (Ch = '(') then
        if StrScan(efPicture, pmNegParens) <> nil then
          if StrScan(P, ')') <> nil then
            Ch := '-';
      if (Ch <> '-') or not nfMinus then
        if (StLen+MinusVal <= nfMaxLen) then begin
          if Ch = IntlSupport.DecimalChar then
            Ch := pmDecimalPt
          else if Ch = pmDecimalPt then
            Ch := #0;
          if efCharOK(PicChar, Ch, #255, True) then
            if InsertChar then
              MF := 10
        end;
      Inc(P);
    end;
  end;

begin
       {edit}
  HaveSel := efSelStart <> efSelEnd;
  MF := Ord(HaveSel);

  case Cmd of
    ccAccept : ;
  else
    if not (sefFixSemiLits in sefOptions) then
      pbRemoveSemiLits;

    Exclude(sefOptions, sefLiteral);
  end;

  StBgn := efEditBegin;
  StEnd := efEditEnd;
  StLen := StrLen(nfTmp);
  PicChar := efNthMaskChar(efHPos-1);
  Found := StrChPos(efPicture, pmDecimalPt, DotPos);

  Exclude(sefOptions, sefCharOK);
  case Cmd of
    ccChar :
      begin
        Ch := AnsiChar(Lo(Msg.wParam));
        if not (sefAcceptChar in sefOptions) then
          Exit
        else begin
          Exclude(sefOptions, sefAcceptChar);
          if HaveSel and CharIsOk then
            DeleteSel;
          if StLen+MinusVal <= nfMaxLen then begin
            if Ch = IntlSupport.DecimalChar then
              Ch := pmDecimalPt
            else if Ch = pmDecimalPt then
              Ch := #0;
            if not efCharOK(PicChar, Ch, #255, True) then
                efConditionalBeep
            else begin
              if InsertChar then begin
                if (Ch <> '-') and (StLen+MinusVal = nfMaxLen) then
                  CheckAutoAdvance(1);
                MF := 10;
              end else
                efConditionalBeep;
            end;
          end else if not CheckAutoAdvance(1) then
            efConditionalBeep;
        end;
      end;
    ccLeft, ccWordLeft :
      CheckAutoAdvance(-1);
    ccRight, ccWordRight :
      CheckAutoAdvance(1);
    ccUp :
      if AutoAdvanceUpDown then
        efMoveFocusToPrevField
      else if ArrowIncDec and not ReadOnly then                {!!.10}{!!.14}
        IncreaseValue(True, 1)                                        {!!.10}
      else
        CheckAutoAdvance(-1);
    ccDown :
      if AutoAdvanceUpDown then
        efMoveFocusToNextField
      else if ArrowIncDec and not ReadOnly then                {!!.10}{!!.14}
        DecreaseValue(True, 1)                                        {!!.10}
      else
        CheckAutoAdvance(1);
    ccMouse :
      begin
        efSelStart := 0;
        efSelEnd := 0;
      end;
    ccDblClk :
      SetSelection(0, MaxEditLen);
    ccHome, ccEnd : {do nothing};
    ccBack, ccDel :
      if HaveSel then
        DeleteSel
      else
        DeleteChar;
    ccDelWord :
      if HaveSel then
        DeleteSel;
    ccExtendLeft :
      UpdateSel(-1);
    ccExtendRight :
      UpdateSel(+1);
    ccExtWordLeft, ccExtendHome :
      UpdateSel(-MaxEditLen);
    ccExtWordRight, ccExtendEnd :
      UpdateSel(+MaxEditLen);
    ccCut :
      if HaveSel then
        DeleteSel;
    ccCopy : {} ;
    ccPaste :
      PastePrim(PAnsiChar(Msg.lParam));
    ccDelLine :
      begin
        ClearString;
        MF := 10;
      end;
    ccIns :
      begin
        if sefInsert in sefOptions then
          Exclude(sefOptions, sefInsert)
        else
          Include(sefOptions, sefInsert);
        efCaret.InsertMode := (sefInsert in sefOptions);
      end;
    ccRestore :
      begin
        Restore;
        nfReloadTmp;
      end;
    ccAccept :
      begin
        Include(sefOptions, sefCharOK);
        Include(sefOptions, sefAcceptChar);
        Exit;
      end;
    ccCtrlChar : {};
    ccDec :
      DecreaseValue(True, 1);
    ccInc :
      IncreaseValue(True, 1);
    ccSuppress, ccPartial :
      goto ExitPoint;
  else
    Include(sefOptions, sefCharOK);
  end;
  Exclude(sefOptions, sefAcceptChar);

  case Cmd of
    ccMouse : {};
    ccRestore, ccDblClk,
    ccExtendLeft, ccExtendRight, ccExtendEnd,
    ccExtendHome, ccExtWordLeft, ccExtWordRight :
      Inc(MF);
    else
      efSelStart := efHPos;
      efSelEnd := efHPos;
  end;

ExitPoint:
  if MF >= 10 then begin
    UpdateEditSt;
    efFieldModified;
  end;
  if efPositionCaret(True) then
    Inc(MF);
  if MF > 0 then
    Invalidate;
end;

function TOvcCustomNumericField.efGetDisplayString(Dest : PAnsiChar; Size : Word) : PAnsiChar;
  {-return the display string in Dest and a pointer as the result}
var
  I, J  : Cardinal;
  Found : Boolean;
begin
  Result := inherited efGetDisplayString(Dest, Size);

  if Uninitialized and not (sefHaveFocus in sefOptions) then
    Exit;

  Found := StrChPos(Dest, '-', I);
  if StrChPos(efPicture, pmNegParens, J) then
    if not Found then
      Dest[J] := ' '
    else begin
      Dest[I] := '(';
      Dest[J] := ')';
    end;

  if StrChPos(efPicture, pmNegHere, J) then
    if not Found then
      Dest[J] := ' '
    else begin
      Dest[J] := '-';
      J := efEditBegin;
      if J = I then
        Dest[I] := ' '
      else begin
        StrChDeletePrim(Dest, I);
        StrChInsertPrim(Dest, ' ', J);
      end;
    end;

  TrimAllSpacesPChar(Dest);
end;

procedure TOvcCustomNumericField.efIncDecValue(Wrap : Boolean; Delta : Double);
  {-increment field by Delta}
var
  Code : Integer;
  S    : TEditString;

  procedure IncDecValueLongInt;
  var
    L : LongInt;
  begin
    pbStripPicture(S, efEditSt);

    if efStr2Long(S, L) then begin
      if (Delta < 0) and (L <= efRangeLo.rtLong) then
        if Wrap then
          L := efRangeHi.rtLong
        else
          Exit
      else if (Delta > 0) and (L >= efRangeHi.rtLong) then
        if Wrap then
          L := efRangeLo.rtLong
        else
          Exit
      else
        Inc(L, Trunc(Delta));

      {!!.11}
      {insure valid value}
      if L < efRangeLo.rtLong then
        L := efRangeLo.rtLong;
      if L > efRangeHi.rtLong then
        L := efRangeHi.rtLong;

      efTransfer(@L, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

  procedure IncDecValueReal;
  var
    Re : Real;
  begin
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, Re, Code);

    if Code = 0 then begin
      if (Delta < 0) and (Re <= efRangeLo.rtReal) then
        if Wrap then
          Re := efRangeHi.rtReal
        else
          Exit
      else if (Delta > 0) and (Re >= efRangeHi.rtReal) then
        if Wrap then
          Re := efRangeLo.rtReal
        else
          Exit
      else
        Re := Re + Delta;

      {!!.11}
      {insure valid value}
      if Re < efRangeLo.rtReal then
        Re := efRangeLo.rtReal;
      if Re > efRangeHi.rtReal then
        Re := efRangeHi.rtReal;

      efTransfer(@Re, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

  procedure IncDecValueExtended;
  var
    Ex : Extended;
  begin
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, Ex, Code);

    if Code = 0 then begin
      if (Delta < 0) and (Ex <= efRangeLo.rtExt) then
        if Wrap then
          Ex := efRangeHi.rtExt
        else
          Exit
      else if (Delta > 0) and (Ex >= efRangeHi.rtExt) then
        if Wrap then
          Ex := efRangeLo.rtExt
        else
          Exit
      else
        Ex := Ex + Delta;

      {!!.11}
      {insure valid value}
      if Ex < efRangeLo.rtExt then
        Ex := efRangeLo.rtExt;
      if Ex > efRangeHi.rtExt then
        Ex := efRangeHi.rtExt;

      efTransfer(@Ex, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

  procedure IncDecValueDouble;
  var
    Db : Double;
  begin
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, Db, Code);

    if Code = 0 then begin
      if (Delta < 0) and (Db <= efRangeLo.rtExt) then
        if Wrap then
          Db := efRangeHi.rtExt
        else
          Exit
      else if (Delta > 0) and (Db >= efRangeHi.rtExt) then
        if Wrap then
          Db := efRangeLo.rtExt
        else
          Exit
      else
        Db := Db + Delta;

      {!!.11}
      {insure valid value}
      if Db < efRangeLo.rtExt then
        Db := efRangeLo.rtExt;
      if Db > efRangeHi.rtExt then
        Db := efRangeHi.rtExt;

      efTransfer(@Db, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

  procedure IncDecValueSingle;
  var
    Si : Single;
  begin
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, Si, Code);

    if Code = 0 then begin
      if (Delta < 0) and (Si <= efRangeLo.rtExt) then
        if Wrap then
          Si := efRangeHi.rtExt
        else
          Exit
      else if (Delta > 0) and (Si >= efRangeHi.rtExt) then
        if Wrap then
          Si := efRangeLo.rtExt
        else
          Exit
      else
        Si := Si + Delta;

      {!!.11}
      {insure valid value}
      if Si < efRangeLo.rtExt then
        Si := efRangeLo.rtExt;
      if Si > efRangeHi.rtExt then
        Si := efRangeHi.rtExt;

      efTransfer(@Si, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

  procedure IncDecValueComp;
  var
    Co : Comp;
  begin
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, Co, Code);

    if Code = 0 then begin
      if (Delta < 0) and (Co <= efRangeLo.rtExt) then
        if Wrap then
          Co := efRangeHi.rtExt
        else
          Exit
      else if (Delta > 0) and (Co >= efRangeHi.rtExt) then
        if Wrap then
          Co := efRangeLo.rtExt
        else
          Exit
      else
        Co := Co + Delta;

      {!!.11}
      {insure valid value}
      if Co < efRangeLo.rtExt then
        Co := efRangeLo.rtExt;
      if Co > efRangeHi.rtExt then
        Co := efRangeHi.rtExt;

      efTransfer(@Co, otf_SetData);
      nfReloadTmp;
      efPerformRepaint(True);
    end;
  end;

begin
  if not (sefHaveFocus in sefOptions) then
    Exit;

  case FNumericDataType of
    nftLongInt,
    nftWord,
    nftInteger,
    nftByte,
    nftShortInt  : IncDecValueLongInt;
    nftReal      : IncDecValueReal;
    nftExtended  : IncDecValueExtended;
    nftDouble    : IncDecValueDouble;
    nftSingle    : IncDecValueSingle;
    nftComp      : IncDecValueComp;
  end;
  efPositionCaret(False);
end;

procedure TOvcCustomNumericField.efSetCaretPos(Value : Integer);
  {-set position of caret within the field}
begin
  {do nothing}
end;

function TOvcCustomNumericField.efTransfer(DataPtr : Pointer; TransferFlag : Word) : Word;
  {-transfer data to/from the entry fields}
var
  L      : LongInt;
  R      : Real;
  Code   : Integer;
  Width  : Word;
  Places : Word;
  E      : Extended;
  D      : Double;
  G      : Single;
  C      : Comp;
  S      : TEditString;

  procedure TransferLongInt;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      if not efStr2Long(S, LongInt(DataPtr^)) then
        LongInt(DataPtr^) := 0;
    end else begin
      efLong2Str(S, LongInt(DataPtr^));
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferWord;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      if efStr2Long(S, L) then
        Word(DataPtr^) := L
      else
        Word(DataPtr^) := 0;
    end else begin
      efLong2Str(S, Word(DataPtr^));
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferInteger;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      if efStr2Long(S, L) then
        SmallInt(DataPtr^) := L
      else
        SmallInt(DataPtr^) := 0;
    end else begin
      efLong2Str(S, SmallInt(DataPtr^));
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferByte;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      if efStr2Long(S, L) then
        Byte(DataPtr^) := L
      else
        Byte(DataPtr^) := 0;
    end else begin
      efLong2Str(S, Byte(DataPtr^));
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferShortInt;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      if efStr2Long(S, L) then
        ShortInt(DataPtr^) := L
      else
        ShortInt(DataPtr^) := 0;
    end else begin
      efLong2Str(S, ShortInt(DataPtr^));
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferReal;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      FixRealPrim(S, False, IntlSupport.DecimalChar);                  {!!.12}
      Val(S, R, Code);

      if Code <> 0 then
        R := 0;
      Real(DataPtr^) := R;
    end else begin
      pbCalcWidthAndPlaces(Width, Places);
      Str(Real(DataPtr^):Width:Places, S);
      if DecimalPlaces <> 0 then
        TrimTrailingZerosPChar(S)
      else
        TrimAllSpacesPChar(S);
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferExtended;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      FixRealPrim(S, False, IntlSupport.DecimalChar);                  {!!.12}
      Val(S, E, Code);

      if Code <> 0 then
        E := 0;
      Extended(DataPtr^) := E;
    end else begin
      pbCalcWidthAndPlaces(Width, Places);
      Str(Extended(DataPtr^):Width:Places, S);
      if DecimalPlaces <> 0 then
        TrimTrailingZerosPChar(S)
      else
        TrimAllSpacesPChar(S);
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferDouble;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      FixRealPrim(S, False, IntlSupport.DecimalChar);                  {!!.12}
      Val(S, D, Code);

      if Code <> 0 then
        D := 0;
      Double(DataPtr^) := D;
    end else begin
      pbCalcWidthAndPlaces(Width, Places);
      Str(Double(DataPtr^):Width:Places, S);
      if DecimalPlaces <> 0 then
        TrimTrailingZerosPChar(S)
      else
        TrimAllSpacesPChar(S);
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferSingle;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      FixRealPrim(S, False, IntlSupport.DecimalChar);                  {!!.12}
      Val(S, G, Code);

      if Code <> 0 then
        G := 0;
      Single(DataPtr^) := G;
    end else begin
      pbCalcWidthAndPlaces(Width, Places);
      Str(Single(DataPtr^):Width:Places, S);
      if DecimalPlaces <> 0 then
        TrimTrailingZerosPChar(S)
      else
        TrimAllSpacesPChar(S);
      pbMergePicture(efEditSt, S);
    end;
  end;

  procedure TransferComp;
  begin
    if TransferFlag = otf_GetData then begin
      pbStripPicture(S, efEditSt);

      FixRealPrim(S, False, IntlSupport.DecimalChar);                  {!!.12}
      Val(S, C, Code);

      if Code <> 0 then
        C := 0;
      Comp(DataPtr^) := C;
    end else begin
      pbCalcWidthAndPlaces(Width, Places);
      Str(Comp(DataPtr^):Width:Places, S);
      if DecimalPlaces <> 0 then
        TrimTrailingZerosPChar(S)
      else
        TrimAllSpacesPChar(S);
      pbMergePicture(efEditSt, S);
    end;
  end;

begin
       {transfer}
  {!!.13} {added}
  if DataPtr = nil then begin
    Result := 0;
    Exit;
  end;

  case FNumericDataType of
    nftLongInt  : TransferLongInt;
    nftWord     : TransferWord;
    nftInteger  : TransferInteger;
    nftByte     : TransferByte;
    nftShortInt : TransferShortInt;
    nftReal     : TransferReal;
    nftExtended : TransferExtended;
    nftDouble   : TransferDouble;
    nftSingle   : TransferSingle;
    nftComp     : TransferComp;
  end;
  Result := inherited efTransfer(DataPtr, TransferFlag);
end;

function TOvcCustomNumericField.efValidateField : Word;
  {-validate contents of field; result is error code or 0}
var
  Code : Integer;
  S    : TEditString;

  procedure ValidateLongInt;
  var
    L : LongInt;
  begin
    pbStripPicture(S, efEditSt);

    if not efStr2Long(S, L) then
      Result := oeInvalidNumber
    else if (L < efRangeLo.rtLong) or (L > efRangeHi.rtLong) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          efTransfer(@L, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateWord;
  var
    L : LongInt;
    W : Word;
  begin
    pbStripPicture(S, efEditSt);

    if not efStr2Long(S, L) then
      Result := oeInvalidNumber
    else if (L < efRangeLo.rtLong) or (L > efRangeHi.rtLong) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          W := L;
          efTransfer(@W, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateInteger;
  var
    L : LongInt;
    I : Integer;
  begin
    pbStripPicture(S, efEditSt);

    if not efStr2Long(S, L) then
      Result := oeInvalidNumber
    else if (L < efRangeLo.rtLong) or (L > efRangeHi.rtLong) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          I := L;
          efTransfer(@I, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateByte;
  var
    L : LongInt;
    B : Byte;
  begin
    pbStripPicture(S, efEditSt);

    if not efStr2Long(S, L) then
      Result := oeInvalidNumber
    else if (L < efRangeLo.rtLong) or (L > efRangeHi.rtLong) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          B := L;
          efTransfer(@B, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateShortInt;
  var
    L  : LongInt;
    Si : Byte;
  begin
    pbStripPicture(S, efEditSt);

    if not efStr2Long(S, L) then
      Result := oeInvalidNumber
    else if (L < efRangeLo.rtLong) or (L > efRangeHi.rtLong) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          Si := L;
          efTransfer(@Si, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateReal;
  var
    R : Real;
  begin
    {convert efEditSt to a real}
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, R, Code);

    if Code <> 0 then
      Result := oeInvalidNumber
    else if (R < efRangeLo.rtReal) or (R > efRangeHi.rtReal) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          efTransfer(@R, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateExtended;
  var
    E : Extended;
  begin
    {convert efEditSt to an extended}
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, E, Code);

    if Code <> 0 then
      Result := oeInvalidNumber
    else if (E < efRangeLo.rtExt) or (E > efRangeHi.rtExt) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          efTransfer(@E, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateDouble;
  var
    E : Extended;
    D : Double;
  begin
    {convert efEditSt to an extended}
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, E, Code);

    if Code <> 0 then
      Result := oeInvalidNumber
    else if (E < efRangeLo.rtExt) or (E > efRangeHi.rtExt) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          D := E;
          efTransfer(@D, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateSingle;
  var
    E  : Extended;
    Si : Single;
  begin
    {convert efEditSt to an extended}
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, E, Code);

    if Code <> 0 then
      Result := oeInvalidNumber
    else if (E < efRangeLo.rtExt) or (E > efRangeHi.rtExt) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          Si := E;
          efTransfer(@Si, otf_SetData);
          Invalidate;
        end;
    end;
  end;

  procedure ValidateComp;
  var
    E : Extended;
    C : Comp;
  begin
    {convert efEditSt to an comp}
    pbStripPicture(S, efEditSt);

    FixRealPrim(S, True, IntlSupport.DecimalChar);                     {!!.12}
    Val(S, C, Code);

    E := C;
    if Code <> 0 then
      Result := oeInvalidNumber
    else if (E < efRangeLo.rtExt) or (E > efRangeHi.rtExt) then
      Result := oeRangeError
    else begin
      if sefHaveFocus in sefOptions then
        if not (sefGettingValue in sefOptions) then begin
          efTransfer(@C, otf_SetData);
          Invalidate;
        end;
    end;
  end;

begin
       {validate}
  Result := 0;
  case FNumericDataType of
    nftLongInt  : ValidateLongInt;
    nftWord     : ValidateWord;
    nftInteger  : ValidateInteger;
    nftByte     : ValidateByte;
    nftShortInt : ValidateShortInt;
    nftReal     : ValidateReal;
    nftExtended : ValidateExtended;
    nftDouble   : ValidateDouble;
    nftSingle   : ValidateSingle;
    nftComp     : ValidateComp;
  end;

  if not (sefUserValidating in sefOptions) then begin
    {user may retrieve data from field. flag that we are doing}
    {user validation to avoid calling this routine recursively}
    Include(sefOptions, sefUserValidating);
    DoOnUserValidation(Result);
    Exclude(sefOptions, sefUserValidating);
  end;
end;

function TOvcCustomNumericField.nfGetDataType(Value: TNumericDataType) : Byte;
  {-return a Byte value representing the type of this field}
begin
  case Value of
    nftLongInt   : Result := fidNumericLongInt;
    nftWord      : Result := fidNumericWord;
    nftInteger   : Result := fidNumericInteger;
    nftByte      : Result := fidNumericByte;
    nftShortInt  : Result := fidNumericShortInt;
    nftReal      : Result := fidNumericReal;
    nftExtended  : Result := fidNumericExtended;
    nftDouble    : Result := fidNumericDouble;
    nftSingle    : Result := fidNumericSingle;
    nftComp      : Result := fidNumericComp;
  else
    raise EOvcException.Create(GetOrphStr(SCInvalidParamValue));
  end;
end;

procedure TOvcCustomNumericField.nfReloadTmp;
  {-reload Tmp from efEditSt, etc.}
begin
  {load nfTmp}
  pbStripPicture(nfTmp, efEditSt);

  TrimAllSpacesPChar(nfTmp);

  {remove the minus sign if there is one}
  nfMinus := (nfTmp[0] = '-');
  if nfMinus then
    StrChDeletePrim(nfTmp, 0);

  {want a blank string if it's a zero}
  if (nfTmp[0] = '0') and (nfTmp[1] = #0) then
    nfTmp[0] := #0;
end;

{!!.10} {revised}
procedure TOvcCustomNumericField.nfResetFieldProperties(FT: TNumericDataType);
  {-reset field properties}
begin
  DecimalPlaces := 0;
  case FT of
    nftLongInt   : PictureMask := 'iiiiiiiiii';
    nftWord      : PictureMask := '99999';
    nftInteger   : PictureMask := 'iiiiii';
    nftByte      : PictureMask := '999';
    nftShortInt  : PictureMask := 'iiii';
    nftReal      : PictureMask := '##########';
    nftExtended  : PictureMask := '##########';
    nftDouble    : PictureMask := '##########';
    nftSingle    : PictureMask := '##########';
    nftComp      : PictureMask := 'iiiiiiiiii';
  else
    raise EOvcException.Create(GetOrphStr(SCInvalidParamValue));
  end;
end;

procedure TOvcCustomNumericField.nfSetDataType(Value: TNumericDataType);
  {-set the data type for this field}
begin
  if FNumericDataType <> Value then begin
    FNumericDataType := Value;
    efDataType := nfGetDataType(FNumericDataType);
    efSetDefaultRange(efDataType);

    {set defaults for this field type}
    nfResetFieldProperties(FNumericDataType);
    if HandleAllocated then begin
      {don't save data through create window}
      efSaveData := False;
      RecreateWnd;
    end;
  end;
end;

procedure TOvcCustomNumericField.nfSetDefaultRanges;
  {-set default range values based on the field type}
begin
  case FNumericDataType of
    nftLongInt, nftWord, nftInteger, nftByte, nftShortInt :
      if efRangeLo.rtLong = efRangeHi.rtLong then
        efSetDefaultRange(efDataType);
    nftReal :
      if efRangeLo.rtReal = efRangeHi.rtReal then
        efSetDefaultRange(efDataType);
    nftExtended, nftDouble, nftSingle, nftComp :
      if efRangeLo.rtExt = efRangeHi.rtExt then
        efSetDefaultRange(efDataType);
  else
    efSetDefaultRange(efDataType);
  end;
end;

procedure TOvcCustomNumericField.nfSetMaxLength(Mask : PChar);
  {-determine and set MaxLength}
var
  C : Cardinal;
begin
  FMaxLength := StrLen(Mask);

  {decrease this if Mask has special characters that}
  {should not be considered part of the display string}
  if StrChPos(Mask, pmNegParens, C) then
    Dec(FMaxLength);
  if StrChPos(Mask, pmNegHere, C) then
    Dec(FMaxLength);
end;

procedure TOvcCustomNumericField.nfSetPictureMask(const Value: string);
  {-set the picture mask}
var
  Buf : TPictureMask;
begin
  if (FPictureMask <> Value) and (Value <> '') then begin

    {test for blatantly invalid masks}
    if csDesigning in ComponentState then begin
      {check for masks like "999.99" or "iii.ii" in fields editing floating data types}
      if (efDataType mod fcpDivisor) in [fsubReal, fsubExtended, fsubDouble, fsubSingle] then
        if (Pos(pmDecimalPt, Value) > 0) and
         ((Pos(pmPositive, Value) > 0) or (Pos(pmWhole, Value) > 0)) then
          raise EInvalidPictureMask.Create(Value);
    end;

    FPictureMask := Value;
    if csDesigning in ComponentState then begin
      StrPLCopy(efPicture, FPictureMask, MaxPicture);
      efPicLen := StrLen(efPicture);
      {set MaxLength based on picture mask}
      nfSetMaxLength(efPicture);
      pbOptimizeInitPictureFlags;
      efInitializeDataSize;
      Repaint;
    end else begin
      StrPLCopy(Buf, FPictureMask, MaxPicture);
      efChangeMask(Buf);
      RecreateWnd;
    end;
  end;
end;

procedure TOvcCustomNumericField.pbRemoveSemiLits;
  {-remove semi-literal mask characters from the edit string}
begin
  if (sefHexadecimal in sefOptions) or (sefOctal in sefOptions) or
     (sefBinary in sefOptions) then
    Include(sefOptions, sefFixSemiLits)
  else
    Exclude(sefOptions, sefFixSemiLits);
end;

procedure TOvcCustomNumericField.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;

  {are we giving up the focus?}
  if not (sefRetainPos in sefOptions) then
    FillChar(nfTmp, SizeOf(nfTmp), #0);
end;

procedure TOvcCustomNumericField.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;
  nfReloadTmp;
  efResetCaret;
end;


{$IFDEF TRIALRUN}
initialization
  _CC_; _VC_;
{$ENDIF}

end.

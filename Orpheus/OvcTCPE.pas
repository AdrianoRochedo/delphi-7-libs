{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFDEF Win32}
{$H+} {Long strings}
{$ENDIF}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                  OVCTCPE.PAS 2.15                     *}
{*     Copyright 1995-97 (c) TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OvcTCPE;
  {-Table cell property editor}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Graphics, Forms, Controls, Buttons, DsgnIntf,
  StdCtrls, ExtCtrls,
  OvcTCBEF, OvcStr, OvcEF, OvcBase, OvcSf, OvcPb, OvcPf, OvcHelp;

type
  TOvcfrmTCRange = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblRange: TLabel;
    lblLower: TLabel;
    lblUpper: TLabel;
    pfRange: TOvcPictureField;
    DefaultController: TOvcController;
    sfRange: TOvcSimpleField;
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  {local class to allow access to protected data and methods}
  TLocalEF = class(TOvcBaseEntryField);
  TLocalTC = class(TOvcTCBaseEntryField);

type
  {property editor for ranges}
  TTCRangeProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    function GetValue : string;
      override;
    procedure Edit;
      override;
  end;

implementation

{$R *.DFM}

uses
  OvcConst, OvcData, OvcIntl, SysUtils;

{*** TTCRangeProperty ***}

function TTCRangeProperty.GetAttributes: TPropertyAttributes;
begin
  case TLocalEF(TLocalTC(GetComponent(0)).FEdit).efDataType mod fcpDivisor of
    fSubString,
    fsubBoolean,
    fsubYesNo    : Result := [paDialog, paReadOnly]
  else
    Result := [paDialog];
  end;
end;

function TTCRangeProperty.GetValue : string;
begin
  {return string that is displayed in the object inspector}
  case TLocalEF(TLocalTC(GetComponent(0)).FEdit).efDataType mod fcpDivisor of
    fSubString,
    fsubBoolean,
    fsubYesNo    : Result := '(None)';
  else
    Result := inherited GetValue;
  end;
end;

procedure TTCRangeProperty.Edit;
var
  RangePE  : TOvcfrmTCRange;
  TC       : TLocalTC;
  Field    : TOvcBaseEntryField;
  pfDT     : TPictureDataType;
  sfDT     : TSimpleDataType;
  R        : TRangeType;
  S        : string;
  PropName : string;

  procedure ChangeFromRange(var Data; DataType : Byte);
    {-alter how data is stored based on the field data type}
  begin
    case DataType mod fcpDivisor of
      fsubChar     : {no change needed};
      fsubLongInt  : {no change needed};
      fsubWord     : Word(Data) := TRangeType(Data).rtLong;
      fsubInteger  : Integer(Data) := TRangeType(Data).rtLong;
      fsubByte     : Byte(Data) := TRangeType(Data).rtLong;
      fsubShortInt : ShortInt(Data) := TRangeType(Data).rtLong;
      fsubReal     : {no change needed};
      fsubExtended : {no change needed};
      fsubDouble   : Double(Data) := TRangeType(Data).rtExt;
      fsubSingle   : Single(Data) := TRangeType(Data).rtExt;
      fsubComp     : Comp(Data) := TRangeType(Data).rtExt;
      fsubDate     : {no change needed};
      fsubTime     : {no change needed};
    end;
  end;

  procedure ChangeToRange(var Data; DataType : Byte);
    {-alter how data is stored based on the field data type}
  begin
    case DataType mod fcpDivisor of
      fsubChar     : {no change needed};
      fsubLongInt  : {no change needed};
      fsubWord     : TRangeType(Data).rtLong := Word(Data);
      fsubInteger  : TRangeType(Data).rtLong := Integer(Data);
      fsubByte     : TRangeType(Data).rtLong := Byte(Data);
      fsubShortInt : TRangeType(Data).rtLong := ShortInt(Data);
      fsubReal     : {no change needed};
      fsubExtended : {no change needed};
      fsubDouble   : TRangeType(Data).rtExt := Double(Data);
      fsubSingle   : TRangeType(Data).rtExt := Single(Data);
      fsubComp     : TRangeType(Data).rtExt := Comp(Data);
      fsubDate     : {no change needed};
      fsubTime     : {no change needed};
    end;
  end;

  function GetDecimalPlaces(EF : TLocalEF) : Byte;
  var
    I      : Word;
    DotPos : Cardinal;
  begin
    {calculate decimal places}
    if not StrChPos(EF.efPicture, pmDecimalPt, DotPos) then
      Result := EF.DecimalPlaces
    else begin
      Result := 0;
      for I := DotPos+1 to EF.MaxLength-1 do
        if EF.efNthMaskChar(I) in PictureChars then
          Inc(Result)
        else
          Break;
    end;
  end;

begin
  RangePE := TOvcfrmTCRange.Create(Application);
  try
    with RangePE do begin
      TC := TLocalTC(GetComponent(0));

      {initialize to unused data types}
      sfDT := sftString;
      pfDT := pftString;

      {set our edit field to the fields data type}
      case TLocalEF(TC.FEdit).efDataType mod fcpDivisor of
        fsubChar     : sfDT := sftChar;
        fsubLongInt  : sfDT := sftLongInt;
        fsubWord     : sfDT := sftWord;
        fsubInteger  : sfDT := sftInteger;
        fsubByte     : sfDT := sftByte;
        fsubShortInt : sfDT := sftShortInt;
        fsubReal     : sfDT := sftReal;
        fsubExtended : sfDT := sftExtended;
        fsubDouble   : sfDT := sftDouble;
        fsubSingle   : sfDT := sftSingle;
        fsubComp     : sfDT := sftComp;
        fsubDate     : pfDT := pftDate;
        fsubTime     : pfDT := pftTime;
      else
        {don't allow editing ranges for string, boolean, yesno fields}
        raise Exception.Create(GetOrphStr(SCRangeNotSupported));
      end;

      if (pfDT in [pftDate, pftTime]) then begin
        pfRange.HandleNeeded;
        pfRange.DataType := pfDT;
        pfRange.Visible := True;
        if pfDT = pftDate then
          pfRange.PictureMask := OvcIntlSup.InternationalDate(True)
        else
          pfRange.PictureMask := OvcIntlSup.InternationalTime(False);
        pfRange.MaxLength := Length(pfRange.PictureMask);
        Field := pfRange
      end else begin
        sfRange.HandleNeeded;
        sfRange.DataType := sfDT;
        sfRange.Visible := True;
        sfRange.DecimalPlaces := GetDecimalPlaces(TLocalEF(TC.FEdit));
        sfRange.MaxLength := TLocalEF(TC.FEdit).MaxLength;
        Field := sfRange;
      end;

      lblLower.Caption := Field.RangeLo;
      lblUpper.Caption := Field.RangeHi;

      {set default value based on current field range value}
      PropName := GetName;
      if CompareText(PropName, 'RangeHi') = 0 then
        R := TLocalEF(TC.FEdit).efRangeHi
      else
        R := TLocalEF(TC.FEdit).efRangeLo;

      ChangeFromRange(R, TLocalEF(TC.FEdit).efDataType);
      Field.SetValue(R);

      {show the form}
      ShowModal;
      if ModalResult = idOK then begin
        {transfer values}
        Field.GetValue(R);
        ChangeToRange(R, TLocalEF(TC.FEdit).efDataType);
        if CompareText(PropName, 'RangeHi') = 0 then begin
          {let edit cell convert it to a string}
          TLocalEF(TC.FEdit).SetRangeHi(R);
          S := TC.RangeHi;
          TC.RangeHi := S;
        end else begin
          {let edit cell convert it to a string}
          TLocalEF(TC.FEdit).SetRangeLo(R);
          S := TC.RangeLo;
          TC.RangeLo := S;
        end;
        Modified;
      end;
    end;
  finally
    RangePE.Free;
  end;
end;


{*** TfrmRange ***}

procedure TOvcfrmTCRange.btnHelpClick(Sender: TObject);
begin
  ShowHelpContext(hcEntryFieldRange);
end;

end.

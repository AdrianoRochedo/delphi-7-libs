{*********************************************************}
{*                  UBCDCAL1.PAS 1.05                    *}
{*     Copyright (c) TurboPower Software Co 1995-97      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit Ubcdcal1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, ExtCtrls, Buttons, Menus;

type
  TBCDCalcDlg = class(TForm)
    Edit1: TEdit;
    ZeroBtn: TButton;
    DecKey: TButton;
    ThreeKey: TButton;
    OneKey: TButton;
    TwoKey: TButton;
    SixKey: TButton;
    FourKey: TButton;
    FiveKey: TButton;
    NineKey: TButton;
    SevenKey: TButton;
    EightKey: TButton;
    SqrtBtn: TButton;
    LnBtn: TButton;
    ExpBtn: TButton;
    XtoYBtn: TButton;
    AddBtn: TButton;
    SubBtn: TButton;
    MulBtn: TButton;
    DivBtn: TButton;
    PlusMinusBtn: TButton;
    ClearBtn: TButton;
    EqualBtn: TButton;
    ClearEntryBtn: TButton;
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    Bevel1: TBevel;
    gb1: TGroupBox;
    BCDString: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CloseBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ZeroBtnClick(Sender: TObject);
    procedure DecKeyClick(Sender: TObject);
    procedure OneKeyClick(Sender: TObject);
    procedure TwoKeyClick(Sender: TObject);
    procedure ThreeKeyClick(Sender: TObject);
    procedure FourKeyClick(Sender: TObject);
    procedure FiveKeyClick(Sender: TObject);
    procedure SixKeyClick(Sender: TObject);
    procedure SevenKeyClick(Sender: TObject);
    procedure EightKeyClick(Sender: TObject);
    procedure NineKeyClick(Sender: TObject);
    procedure PlusMinusBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure SubBtnClick(Sender: TObject);
    procedure MulBtnClick(Sender: TObject);
    procedure DivBtnClick(Sender: TObject);
    procedure SqrtBtnClick(Sender: TObject);
    procedure ExpBtnClick(Sender: TObject);
    procedure LnBtnClick(Sender: TObject);
    procedure XtoYBtnClick(Sender: TObject);
    procedure EqualBtnClick(Sender: TObject);
    procedure ClearEntryBtnClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure SendKeyPress(Sender : TObject; C : Char);
  end;

  BCDCharSet = set of Char;
  BCDOperSet = set of Char;

var
  BCDCalcDlg: TBCDCalcDlg;

implementation

{$R *.DFM}

uses
  StConst,
  StBase,
 {$IFDEF Win32}
  StStrL,
 {$ENDIF}
  StStrS,
  StBCD,

  UBCDCal2;


var
  BCDChar   : BCDCharSet;
  BCDOper   : BCDOperSet;
  PendOp    : Char;
  DFHold    : Integer;

  XBuffer   : string[20];

  ClearOnNext : Boolean;


procedure TBCDCalcDlg.FormCreate(Sender: TObject);
begin
  BCDChar := ['0'..'9', SysUtils.DecimalSeparator,'p'];
  BCDOper := ['+','-','/','*','^','e','l','s','='];
  Edit1.Text := '';
  PendOp := #0;
  DFHold := 0;
  XBuffer := '0';
  ClearOnNext := False;
end;


function BytesToString(Value : PByte; Size : Cardinal) : string;
  {-convert byte array to string, no spaces or hex enunciators, e.g., '$'}
var
  I,
  Index  : Cardinal;
  S      : String[3];
begin
{$IFDEF WIN32}
  {$IFOPT H+}
  SetLength(Result,2*Size);
  {$ELSE}
  Result[0] := AnsiChar(Size*2);
  {$ENDIF}
{$ELSE}
  Result[0] := AnsiChar(Size*2);
{$ENDIF}

  for I := 1 to Size do
  begin
    Index := I*2;
{$IFDEF WIN32}
  {$IFOPT H+}
    S := HexBL(Byte(PAnsiChar(Value)[I-1]));
  {$ELSE}
    S := HexBS(Byte(PAnsiChar(Value)[I-1]);
  {$ENDIF}
{$ELSE}
    S := HexBS(Byte(PAnsiChar(Value)[I-1]));
{$ENDIF}
    Result[(Index)-1] := S[1];
    Result[Index] := S[2];
  end;
end;

function StringToBytes(IString : string; var Value; Size : Cardinal) : Boolean;
  {-convert string (by groups of 2 char) to byte values}
var
  Code,
  Index,
  I     : Integer;
  Q     : TBcd;
  S     : array[1..3] of AnsiChar;
begin
  if ((Length(IString) div 2) <> Size) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  for I := 1 to Size do
  begin
    Index := (2*(I-1))+1;
    S[1] := '$';
    S[2] := IString[Index];
    S[3] := IString[Index+1];
    Val(S,Q[I-1],Code);
    if (Code <> 0) then
    begin
      Result := False;
      Exit;
    end;
 end;
  Move(Q,Value,Size);
end;

procedure TBCDCalcDlg.FormKeyPress(Sender: TObject; var Key: Char);
var
  HldOp : Char;
  L     : Integer;
  BCD1  : TBcd;
  S     : string[21];
begin
  if Key = #13 then
  begin
    if XBuffer = '0' then
      XBuffer := Edit1.Text
    else begin
      EqualBtnClick(Sender);
      XBuffer := '0';
    end;
    Key := #0;
    ClearOnNext := True;
  end;

  if Key IN BCDChar then
  begin
    if (Key = 'p') then
    begin
      S := Edit1.Text;
      if (S[1] <> '-') then
        Insert('-',S,1)
      else
        Delete(S,1,1);
      Edit1.Text := S;
      BCD1 := ValBcd(S);
      BCDString.Text := BytesToString(@BCD1,SizeOf(BCD1));
      Key := #0;
    end else
    begin
      if ClearOnNext then
      begin
        Edit1.Text := '';
        ClearOnNext := False;
      end;
    end;
  end;

  if Key IN BCDOper then
  begin
    if NOT (Key in ['s','e','l']) then
    begin
      if Edit1.Text = '' then
        Edit1.Text := '0';
      if (XBuffer <> '0') then
        EqualBtnClick(Sender);

      XBuffer := Edit1.Text;
      BCD1 := ValBcd(XBuffer);
      BCDString.Text := BytesToString(@BCD1,SizeOf(BCD1));
      PendOp := Key;
      Key := #0;
      ClearOnNext := True;
    end else
    begin
      HldOp := PendOp;
      PendOp := Key;
      EqualBtnClick(Sender);
      PendOp := HldOp;
      Key := #0;
    end;
  end;

  if (Key IN BCDChar) then
  begin
    S := Edit1.Text;
    L := Length(S);
    if (L < Edit1.MaxLength) then
    begin
      Edit1.Text := S + Key;
    end;
    Key := #0
  end;
  Edit1.SetFocus;
  Edit1.SelStart := Length(Edit1.Text);
  Edit1.SelLength := 0;
end;


procedure TBCDCalcDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TBCDCalcDlg.ClearBtnClick(Sender: TObject);
begin
  XBuffer := '0';
  Edit1.Text := '0';
  BCDString.Text := '';
  PendOp := #0;
  ClearOnNext := True;
end;

procedure TBCDCalcDlg.ClearEntryBtnClick(Sender: TObject);
begin
  Edit1.Text := '0';
  ClearOnNext := True;
end;

procedure TBCDCalcDlg.SendKeyPress(Sender : TObject; C : Char);
var
  KP : Char;
begin
  KP := C;
  FormKeyPress(Sender,KP);
end;

procedure TBCDCalcDlg.ZeroBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'0');
end;

procedure TBCDCalcDlg.DecKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender, SysUtils.DecimalSeparator);
end;

procedure TBCDCalcDlg.OneKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'1');
end;

procedure TBCDCalcDlg.TwoKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'2');
end;

procedure TBCDCalcDlg.ThreeKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'3');
end;

procedure TBCDCalcDlg.FourKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'4');
end;

procedure TBCDCalcDlg.FiveKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'5');
end;

procedure TBCDCalcDlg.SixKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'6');
end;

procedure TBCDCalcDlg.SevenKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'7');
end;

procedure TBCDCalcDlg.EightKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'8');
end;

procedure TBCDCalcDlg.NineKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'9');
end;

procedure TBCDCalcDlg.PlusMinusBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'p');
end;

procedure TBCDCalcDlg.AddBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'+');
end;

procedure TBCDCalcDlg.SubBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'-');
end;

procedure TBCDCalcDlg.MulBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'*');
end;

procedure TBCDCalcDlg.DivBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'/');
end;

procedure TBCDCalcDlg.SqrtBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'s');
end;

procedure TBCDCalcDlg.ExpBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'e');
end;

procedure TBCDCalcDlg.LnBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'l');
end;

procedure TBCDCalcDlg.XtoYBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'^');
end;

procedure TBCDCalcDlg.EqualBtnClick(Sender: TObject);
var
  RV    : Extended;
  S     : String[21];
  BCD   : TBcd;

begin
  if PendOp <> #0 then
  begin
    S := Edit1.Text;
    if S = '' then
    begin
      MessageBeep(0);
      Exit;
    end;
    case PendOp of
      '+' : begin
              RV := StrToFloat(XBuffer) + StrToFloat(S);
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '-' : begin
              RV := StrToFloat(XBuffer) - StrToFloat(S);
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '*' : begin
              RV := StrToFloat(XBuffer) * StrToFloat(S);
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '/' : begin
              RV := StrToFloat(S);
              if RV = 0 then
              begin
                Edit1.Text := 'Divide by zero error';
                PendOp := #0;
                ClearOnNext := False;
              end else
              begin
                RV := StrToFloat(XBuffer) / StrToFloat(S);
                Edit1.Text := FloatToStr(RV);
                BCD := ValBcd(Edit1.Text);
                BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
              end;
            end;
      's' : begin
              RV := Sqrt(StrToFloat(S));
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      'e' : begin
              RV := Exp(StrToFloat(S));
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      'l' : begin
              RV := ln(StrToFloat(S));
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '^' : begin
              RV := exp(ln(StrToFloat(XBuffer)) * StrToFloat(S));
              Edit1.Text := FloatToStr(RV);
              BCD := ValBcd(Edit1.Text);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;

    end;
  end;
  PendOp := #0;
  ClearOnNext := True;
  Edit1.SetFocus;
  Edit1.SelStart := 0;
  Edit1.SelLength := 0;
end;


procedure TBCDCalcDlg.About1Click(Sender: TObject);
begin
  BCDCalcAbout.ShowModal;
end;

end.

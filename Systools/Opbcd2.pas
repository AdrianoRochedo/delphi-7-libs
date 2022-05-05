{*********************************************************}
{*                   OpBCD2.PAS 1.05                     *}
{*                BCD compatibility unit                 *}
{*   Copyright (c) TurboPower Software Co., 1996, 1997   *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpBCD2;
  {-Compatibility unit to make StBcd look more like OpBcd}

interface

uses
  StBcd;

type
  BCD = TBcd;

procedure LongintToBCD(L : LongInt; var B : BCD);
  {-Convert a Longint to a BCD}

procedure ValBCD(S : string; var B : BCD; var Code : Word);
  {-Convert a string to a BCD}

procedure AbsBCD(B1 : BCD; var B2 : BCD);
  {-Returns absolute value of B1 in B2}

procedure FracBCD(B1 : BCD; var B2 : BCD);
  {-Returns the fractional part of B1 in B2}

procedure IntBCD(B1 : BCD; var B2 : BCD);
  {-Returns the integer part of B1 in B2}

procedure AddBCD(B1, B2 : BCD; var B3 : BCD);
  {-Add B1 to B2 and put result in B3}

procedure SubBCD(B1, B2 : BCD; var B3 : BCD);
  {-Subtract B2 from B1 and put result in B3}

procedure MultBCD(B1, B2 : BCD; var B3 : BCD);
  {-Multiply B1 by B2 and put result in B3}

procedure DivBCD(B1, B2 : BCD; var B3 : BCD);
  {-Divide B1 by B2 and put result in B3}

function EqualBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 = B2}

function NotEqualBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 <> B2}

function GreaterBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 > B2}

function GreaterEqualBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 >= B2}

function LessBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 < B2}

function LessEqualBCD(B1, B2 : BCD) : Boolean;
  {-Returns true if B1 <= B2}

procedure ExpBCD(B1 : BCD; var B2 : BCD);
  {-Returns the exponential of B1 in B2}

procedure LnBCD(B1 : BCD; var B2 : BCD);
  {-Returns the natural log of B1 in B2}

procedure SqrBCD(B1 : BCD; var B2 : BCD);
  {-Returns the square of B1 in B2}

procedure SqrtBCD(B1 : BCD; var B2 : BCD);
  {-Returns the square root of B1 in B2}

  {==========================================================================}

implementation

{$IFDEF SYSDEMO} uses SysUtil; {$ENDIF}

procedure LongintToBCD(L : LongInt; var B : BCD);
begin
  B := StBcd.LongBcd(L);
end;

procedure ValBCD(S : string; var B : BCD; var Code : Word);
begin
  try
    B := StBcd.ValBcd(S);
    Code := 0;
  except
    Code := 1;
  end;
end;

procedure AbsBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.AbsBcd(B1);
end;

procedure FracBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.FracBcd(B1);
end;

procedure IntBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.IntBcd(B1);
end;

procedure AddBCD(B1, B2 : BCD; var B3 : BCD);
begin
  B3 := StBcd.AddBcd(B1, B2);
end;

procedure SubBCD(B1, B2 : BCD; var B3 : BCD);
begin
  B3 := StBcd.SubBcd(B1, B2);
end;

procedure MultBCD(B1, B2 : BCD; var B3 : BCD);
begin
  B3 := StBcd.MulBcd(B1, B2);
end;

procedure DivBCD(B1, B2 : BCD; var B3 : BCD);
begin
  B3 := StBcd.DivBcd(B1, B2);
end;

function EqualBCD(B1, B2 : BCD) : Boolean;
begin
  EqualBcd := StBcd.CmpBcd(B1, B2) = 0;
end;

function NotEqualBCD(B1, B2 : BCD) : Boolean;
begin
  NotEqualBcd := StBcd.CmpBcd(B1, B2) <> 0;
end;

function GreaterBCD(B1, B2 : BCD) : Boolean;
begin
  GreaterBcd := StBcd.CmpBcd(B1, B2) > 0;
end;

function GreaterEqualBCD(B1, B2 : BCD) : Boolean;
begin
  GreaterEqualBcd := StBcd.CmpBcd(B1, B2) >= 0;
end;

function LessBCD(B1, B2 : BCD) : Boolean;
begin
  LessBcd := StBcd.CmpBcd(B1, B2) < 0;
end;

function LessEqualBCD(B1, B2 : BCD) : Boolean;
begin
  LessEqualBcd := StBcd.CmpBcd(B1, B2) <= 0;
end;

procedure ExpBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.ExpBcd(B1);
end;

procedure LnBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.LnBcd(B1);
end;

procedure SqrBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.MulBcd(B1, B1);
end;

procedure SqrtBCD(B1 : BCD; var B2 : BCD);
begin
  B2 := StBcd.SqrtBcd(B1);
end;

{$IFDEF SYSDEMO}
initialization
  _CC_; _VC_;
{$ENDIF}

end.

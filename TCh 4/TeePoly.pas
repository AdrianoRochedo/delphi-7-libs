{****************************************************}
{   TeeChart Pro 4.0				     }
{   Polynomic routines                               }
{   Copyright (c) 1995-1998 by David Berneda         }
{****************************************************}
Unit TeePoly;

interface

Uses Classes;

Const MaxPolyDegree=20;    { maximum number of polynomial degree }
      MaxPolyPoints=16300; { maximum number of points }

Type Float         = Extended;
     PFloat        = ^Float;
     TDegreeVector = Array[1..MaxPolyDegree] of Float;
     TMatrix       = Array[1..MaxPolyDegree,1..MaxPolyDegree] of Float;

     PVector       = ^TVector;
     TVector       = Array[0..MaxPolyPoints] of PFloat;

Function CalcFitting(PolyDegree:Integer; Const Answer:TDegreeVector; Const XWert:Float):Float;
Procedure PolyFitting( NumPoints:Longint; PolyDegree:Integer;
                       X,Y:PVector; var Answer:TDegreeVector);

Procedure SetVectorValue(Const V:TVector; Index:Longint; Const Value:Float);
Function GetVectorValue(Const V:TVector; Index:Longint):Float;

implementation

Uses SysUtils,TeeProCo;

Function GetVectorValue(Const V:TVector; Index:Longint):Float;
Begin
  result:=V[Index]^;
end;

Procedure SetVectorValue(Const V:TVector; Index:Longint; Const Value:Float);
begin
  V[Index]^:=Value;
end;

Function GaussianFitting( NumDegree:Integer;
                          Var M:TMatrix;
                          Var Y,X:TDegreeVector;
                          Const Error:Float):Float;
var i        : Integer;
    j        : Integer;
    k        : Integer;
    MaxIndex : Integer;
    Change   : Integer;
    MaxEl    : Float;
    Temp     : Float;
    Wert     : Float;
begin
  Change:=0;
  for i:=1 to NumDegree-1 do
  begin
    MaxIndex:=i;
    MaxEl:=Abs(M[i,i]);
    for j:=i+1 to NumDegree do
    if Abs(M[j,i]) > MaxEl then
    begin
      MaxEl:=Abs(M[j,i]);
      MaxIndex:=j;
    end;
    if MaxIndex <> i then
    begin
       for j:=i to NumDegree do
       begin
         Temp:=M[i,j];
         M[i,j]:=M[MaxIndex,j];
         M[MaxIndex,j]:=Temp;
       end;
       Temp:=Y[i];
       Y[i]:=Y[MaxIndex];
       Y[MaxIndex]:=Temp;
       Inc(Change);
    end;
    if Abs(M[i,i]) < Error then
    Begin
      result:=0;
      exit;
    end;
    for j:=i+1 to NumDegree do
    begin
      Wert:=M[j,i]/M[i,i];
      for k:=i+1 to NumDegree do M[j,k]:=M[j,k]-Wert*M[i,k];
      Y[j]:=Y[j]-Wert*Y[i];
    end;
  end;
  if Abs(M[NumDegree,NumDegree])< Error then
  Begin
    Result:=0;
    Exit;
  end;
  Result:=1;
  for i:=NumDegree DownTo 1 do
  begin
    Wert:=0;
    for j:=i+1 to NumDegree do Wert:=Wert + M[i,j]*X[j];
    X[i]:=(Y[i]-Wert)/M[i,i];
    result:=result*M[i,i];
  end;
  if Odd(Change) then result:=-result;
end;

Procedure FKT(PolyDegree:Integer; Const xarg:Float; Var PHI:TDegreeVector);
var t:Integer;
begin
  PHI[1]:=1;
  for t:=2 to PolyDegree do PHI[t]:=PHI[t-1]*xarg;
end;

Function CalcFitting( PolyDegree:Integer;
                      Const Answer:TDegreeVector;
                      Const xwert:Float):Float;
var PHI : TDegreeVector;
    t   : Integer;
begin
  result:=0;
  FKT(PolyDegree,xwert,PHI);
  for t:=1 to PolyDegree do result:=result+Answer[t]*PHI[t];
end;

Procedure PolyFitting( NumPoints:Longint; PolyDegree:Integer; X,Y:PVector;
                       var Answer:TDegreeVector);
var  t     : Integer;
     tt    : Integer;
     l     : Integer;
     PHI   : TDegreeVector;
     B     : TDegreeVector;
     F     : Array[1..MaxPolyDegree] of PVector;
     M     : TMatrix;
begin
  for t:=1 to PolyDegree do F[t]:=nil;
  for t:=1 to PolyDegree do
  Begin
    New(F[t]);
    for tt:=0 to NumPoints do F[t]^[tt]:=New(PFloat);
  end;
  try
    for t:=1 to NumPoints do         { Prepare the approximation }
    begin
      FKT(PolyDegree,GetVectorValue(X^,t),PHI);
      for tt:=1 to PolyDegree do SetVectorValue(F[tt]^,t,PHI[tt]);
    end;
    for tt:=1 to PolyDegree  do         { Build the matrix of the LinEqu. }
    for t:=1 to PolyDegree  do
    begin
      M[t,tt]:=0;
      for l:=1 to NumPoints do
          M[t,tt]:=M[t,tt]+GetVectorValue(F[t]^,l)*GetVectorValue(F[tt]^,l);
      M[tt,t]:=M[t,tt];
    end;
    for t:=1 to PolyDegree  do
    begin
      B[t]:=0;
      for l:=1 to NumPoints do
          B[t]:=B[t]+GetVectorValue(F[t]^,l)*GetVectorValue(Y^,l);
    end;

    if GaussianFitting(PolyDegree,M,B,Answer,1.0e-15)=0 then
       Raise Exception.Create(TeeMsg_FittingError);
  finally
    for t:=1 to PolyDegree do
    if Assigned(F[t]) then
    Begin
      for tt:=0 to NumPoints do Dispose(PFloat(F[t]^[tt]));
      Dispose(F[t]);
    end;
  end;
end;

end.



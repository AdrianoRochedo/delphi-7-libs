{**********************************************}
{   TBar3DSeries Component                     }
{   Copyright (c) 1996-98 by David Berneda     }
{      Series Developer Kit Example            }
{**********************************************}
{$I teedefs.inc}
unit Bar3D;

{ This Series component is derived from TBarSeries.
  It has a new property:  OffsetValues

  This new property allows to specify a different ORIGIN value
  for EACH bar point.
  This can be used with standard TBarSeries components to
  make a "Stacked-3D" chart type.
}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Teengine, Series;

type
  TBar3DSeries = class(TBarSeries)
  private
    { Private declarations }
    FOffsetValues:TChartValueList;
  protected
    { Protected declarations }
    Procedure SetOffsetValues(Value:TChartValueList);
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Function AddBar( Const AX,AY,AOffset:Double;
                     Const AXLabel:String{$IFDEF D5}=''{$ENDIF};
                     AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF}):Longint;
    Procedure FillSampleValues(NumValues:Longint); override;
    Function GetOriginValue(ValueIndex:Longint):Double; override;
    Function MaxYValue:Double; override;
    Function MinYValue:Double; override;
    Function PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double; override;
  published
    { Published declarations }
    property OffsetValues:TChartValueList read FOffsetValues
                                          write SetOffsetValues;
  end;

implementation

Uses Chart,TeeProcs,TeCanvas,
     TeeProco;  { <-- needed only for the "Samples" constant }

Constructor TBar3DSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOffsetValues :=TChartValueList.Create(Self,TeeMsg_ValuesOffset); { <-- "offset" storage }
end;

Procedure TBar3DSeries.SetOffsetValues(Value:TChartValueList);
begin
  SetChartValueList(FOffsetValues,Value); { standard method }
end;

{ calculate maximum Y value }
Function TBar3DSeries.MaxYValue:Double;
begin
  result:=inherited MaxYValue;
  if (MultiBar=mbNone) or (MultiBar=mbSide) then
     result:=MaxDouble(result,FOffsetValues.MaxValue);
end;

{ calculate minimum Y value ( YValues and negative Offsets supported ) }
Function TBar3DSeries.MinYValue:Double;
var t:Longint;
begin
  result:=inherited MinYValue;
  if (MultiBar=mbNone) or (MultiBar=mbSide) then
     for t:=0 to Count-1 do
         if FOffsetValues[t]<0 then result:=MinDouble(result,YValues[t]+FOffsetValues[t]);
end;

Function TBar3DSeries.AddBar( Const AX,AY,AOffset:Double;
                              Const AXLabel:String{$IFDEF D5}=''{$ENDIF};
                              AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF}):Longint;
begin
  result:=AddXY(AX,AY,AXLabel,AColor); { standard add X,Y }
  FOffsetValues.TempValue:=AOffset;
  AddValue(result);
end;

Procedure TBar3DSeries.FillSampleValues(NumValues:Longint);
Var t:Longint;
    tmpX,tmpY,StepX,MinY,DifY:Double;
Begin
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  for t:=1 to NumValues do { some sample values to see something in design mode }
  Begin
    tmpY:=Random(Round(DifY));
    AddBar( tmpX,
            10+Abs(tmpY),
            Abs(DifY/(1+Random(5)))
            {$IFNDEF D5},'',clTeeColor{$ENDIF});
    tmpX:=tmpX+StepX;
  end;
  RefreshSeries;
end;

{ this overrides default bottom origin calculation }
Function TBar3DSeries.PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double;
begin
  result:=FOffsetValues.Value[ValueIndex];
end;

{ this makes this bar heigth to be: "offset" + "heigth" }
Function TBar3DSeries.GetOriginValue(ValueIndex:Longint):Double;
begin
  result:=inherited GetOriginValue(ValueIndex)+FOffsetValues.Value[ValueIndex];
end;

{ Un-register the Series }
Procedure TeeBar3DExitProc; far;
begin
  UnRegisterTeeSeries([TBar3DSeries]);
end;

{ Register the Series at Chart gallery }
initialization
  RegisterTeeSeries( TBar3DSeries, 'Bar3D', TeeMsg_GallerySamples, 1 );
{$IFDEF D1}
  AddExitProc(TeeBar3DExitProc);
{$ELSE}
finalization
  TeeBar3DExitProc;
{$ENDIF}
end.

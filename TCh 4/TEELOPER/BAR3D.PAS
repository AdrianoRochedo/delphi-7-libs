{**********************************************}
{   TBar3DSeries Component                     }
{   Copyright (c) 1996 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
unit Bar3d;

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
    Function GetOriginValue(ValueIndex:Longint):Double; override;
    Function PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double; override;
    Procedure SetOffsetValues(Value:TChartValueList);
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Function AddBar( Const AX,AY,AOffset:Double;
                     Const AXLabel:String;
                     AColor:TColor):Longint;
    Procedure FillSampleValues(NumValues:Longint); override;
  published
    { Published declarations }
    property OffsetValues:TChartValueList read FOffsetValues
                                          write SetOffsetValues;
  end;

implementation

Uses Chart;

Constructor TBar3DSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOffsetValues :=TChartValueList.Create(Self,'Offset'); { <-- "offset" storage }
end;

Procedure TBar3DSeries.SetOffsetValues(Value:TChartValueList);
begin
  SetChartValueList(FOffsetValues,Value); { standard method }
end;

Function TBar3DSeries.AddBar( Const AX,AY,AOffset:Double;
                              Const AXLabel:String;
                              AColor:TColor):Longint;
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
            Abs(DifY/(1+Random(5))),
            '',
            clTeeColor);
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
  RegisterTeeSeries( TBar3DSeries, 'Bar3D', 'Samples', 1 );
{$IFNDEF WIN32}
  AddExitProc(TeeBar3DExitProc);
{$ENDIF}
{$IFDEF WIN32}
finalization
  TeeBar3DExitProc;
{$ENDIF}
end.

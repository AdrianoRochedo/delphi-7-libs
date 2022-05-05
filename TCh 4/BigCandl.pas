{**********************************************}
{   TBigCandleSeries (TCandleSeries)           }
{   Copyright (c) 1996 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
{$I teedefs.inc}
unit BigCandl;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Teengine, Series, OHLChart, CandleCh;

type
  TBigCandleSeries = class(TCandleSeries)
  private
    { Private declarations }
    FHorizGap,FVertGap:Longint;
  protected
    { Protected declarations }
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
    Procedure SetHorizGap(Value:Longint);
    Procedure SetVertGap(Value:Longint);
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property HorizGap:Longint read FHorizGap write SetHorizGap;
    property VertGap:Longint read FVertGap write SetVertGap;
  end;

implementation

Uses Chart,TeCanvas,
     TeeProco;  { <-- needed only for the "Samples" constant }

Constructor TBigCandleSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorizGap:=20;
  FVertGap:=6;
  Marks.Visible:=True;
end;

Procedure TBigCandleSeries.SetHorizGap(Value:Longint);
begin
  SetLongintProperty(FHorizGap,Value);
end;

Procedure TBigCandleSeries.SetVertGap(Value:Longint);
begin
  SetLongintProperty(FVertGap,Value);
end;

Procedure TBigCandleSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                     APosition:TSeriesMarkPosition);
Var tmp           : String;
    tmpX          : Integer;
    tmpHorizOffset: Longint;
    tmpVertOffset : Longint;
    tmpOpen       : Longint;
    tmpHigh       : Longint;
    tmpLow        : Longint;
    tmpClose      : Longint;
begin
  { the four prices screen Y coordinates }
  { remember:  Y coordinates are inverted }
  tmpOpen :=CalcYPosValue(OpenValues.Value[ValueIndex]);
  tmpClose:=CalcYPosValue(CloseValues.Value[ValueIndex]);
  tmpHigh :=CalcYPosValue(HighValues.Value[ValueIndex]);
  tmpLow  :=CalcYPosValue(LowValues.Value[ValueIndex]);

  With APosition do
  begin
    tmpHorizOffset:=(Width div 2)+ FHorizGap ;  { <-- custom horiz "gap" }
    tmpVertOffset :=Height + FVertGap;       { <-- custom vert "gap" }

    tmpX:=LeftTop.X;

    { Open Price Mark }
    With LeftTop do
    begin
      Y:=tmpOpen-(Height div 2);
      X:=tmpX-tmpHorizOffset;
    end;
    tmp:=FormatFloat(ValueFormat,OpenValues.Value[ValueIndex]);
    inherited DrawMark(ValueIndex,tmp,APosition);

    { Close Price Mark }
    With LeftTop do
    begin
      Y:=tmpClose-(Height div 2);
      X:=tmpX+tmpHorizOffset;
    end;
    tmp:=FormatFloat(ValueFormat,CloseValues.Value[ValueIndex]);
    inherited DrawMark(ValueIndex,tmp,APosition);

    { High Price Mark }
    LeftTop.Y:=tmpHigh-tmpVertOffset;
    tmp:=FormatFloat(ValueFormat,HighValues.Value[ValueIndex]);
    inherited DrawMark(ValueIndex,tmp,APosition);

    { Low Price Mark }
    LeftTop.Y:=tmpLow+tmpVertOffset-Height;
    tmp:=FormatFloat(ValueFormat,LowValues.Value[ValueIndex]);
    inherited DrawMark(ValueIndex,tmp,APosition);
  end;
end;

Procedure TeeBigCandleExitProc; far;
begin
  UnRegisterTeeSeries([TBigCandleSeries]);
end;

initialization
  RegisterTeeSeries( TBigCandleSeries, 'Big Candle', TeeMsg_GallerySamples, 1 );
{$IFDEF D1}
  AddExitProc(TeeBigCandleExitProc);
{$ELSE}
finalization
  TeeBigCandleExitProc;
{$ENDIF}
end.

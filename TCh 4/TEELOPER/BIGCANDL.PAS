{**********************************************}
{   TBigCandleSeries (TCandleSeries)           }
{   Copyright (c) 1996 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
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
    Procedure DrawMark(ValueIndex:Longint; Const St:String; R:TRect;
                                  iMaX,iMaY,fMaX,fMay:Integer); override;
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

Uses Chart;

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

Procedure TBigCandleSeries.DrawMark(ValueIndex:Longint; Const St:String; R:TRect;
                                  iMaX,iMaY,fMaX,fMay:Integer);
Var tmp:String;
    tmpR:TRect;
    tmpWidth,tmpHeight,
    tmpHorizOffset,tmpVertOffset,
    tmpOpen,tmpHigh,tmpLow,tmpClose:Longint;
begin
  { the four prices screen Y coordinates }
  { remember:  Y coordinates are inverted }
  tmpOpen :=CalcYPosValue(OpenValues.Value[ValueIndex]);
  tmpClose:=CalcYPosValue(CloseValues.Value[ValueIndex]);
  tmpHigh :=CalcYPosValue(HighValues.Value[ValueIndex]);
  tmpLow  :=CalcYPosValue(LowValues.Value[ValueIndex]);

  tmpHeight:=R.Bottom-R.Top;
  tmpWidth:=R.Right-R.Left;

  tmpHorizOffset:=(tmpWidth div 2)+ FHorizGap ;  { <-- custom horiz "gap" }
  tmpVertOffset :=tmpHeight + FVertGap;       { <-- custom vert "gap" }

  { Open Price Mark }
  tmpR:=R;
  tmpR.Top:=tmpOpen-(tmpHeight div 2);
  tmpR.Bottom:=tmpR.Top+tmpHeight;
  Dec(tmpR.Left,tmpHorizOffset);
  Dec(tmpR.Right,tmpHorizOffset);
  tmp:=FormatFloat(ValueFormat,OpenValues.Value[ValueIndex]);
  inherited DrawMark(ValueIndex,tmp,tmpR,iMaX,iMaY,fMaX,fMay);

  { Close Price Mark }
  tmpR:=R;
  tmpR.Top:=tmpClose-(tmpHeight div 2);
  tmpR.Bottom:=tmpR.Top+tmpHeight;
  Inc(tmpR.Left,tmpHorizOffset);
  Inc(tmpR.Right,tmpHorizOffset);
  tmp:=FormatFloat(ValueFormat,CloseValues.Value[ValueIndex]);
  inherited DrawMark(ValueIndex,tmp,tmpR,iMaX,iMaY,fMaX,fMay);

  { High Price Mark }
  tmpR:=R;
  tmpR.Top:=tmpHigh-tmpVertOffset;
  tmpR.Bottom:=tmpR.Top+tmpHeight;
  tmp:=FormatFloat(ValueFormat,HighValues.Value[ValueIndex]);
  inherited DrawMark(ValueIndex,tmp,tmpR,iMaX,iMaY,fMaX,fMay);

  { Low Price Mark }
  tmpR:=R;
  tmpR.Top:=tmpLow+tmpVertOffset-tmpHeight;
  tmpR.Bottom:=tmpR.Top+tmpHeight;
  tmp:=FormatFloat(ValueFormat,LowValues.Value[ValueIndex]);
  inherited DrawMark(ValueIndex,tmp,tmpR,iMaX,iMaY,fMaX,fMay);
end;

Procedure TeeBigCandleExitProc; far;
begin
  UnRegisterTeeSeries([TBigCandleSeries]);
end;

initialization
  RegisterTeeSeries( TBigCandleSeries, 'Big Candle', 'Samples', 1 );
{$IFNDEF WIN32}
  AddExitProc(TeeBigCandleExitProc);
{$ENDIF}
{$IFDEF WIN32}
finalization
  TeeBigCandleExitProc;
{$ENDIF}
end.

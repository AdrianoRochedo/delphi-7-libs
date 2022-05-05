{**********************************************}
{   TErrorBarSeries (derived from TBarSeries)  }
{   Copyright (c) 1995-1998 by David Berneda   }
{**********************************************}
{$I teedefs.inc}
unit ErrorBar;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, TeEngine, Series,
  TeCanvas;

type
  TErrorSeriesStyle=( essLeft,essRight,essLeftRight,
                      essTop,essBottom,essTopBottom);

  TErrorWidthUnits=(ewuPercent,ewuPixels);

  TCustomErrorSeries = class(TBarSeries)
  private
    { Private declarations }
    FErrorPen        : TChartPen;
    FErrorStyle      : TErrorSeriesStyle;
    FErrorValues     : TChartValueList;
    FErrorWidth      : Integer;
    FErrorWidthUnits : TErrorWidthUnits;
    { internal }
    IDrawBar         : Boolean;
    Function GetErrorValue(Index:Longint):Double;
    Procedure PrepareErrorPen(ValueIndex:Integer);
    Procedure SetErrorStyle(Value:TErrorSeriesStyle);
    Procedure SetErrorValue(Index:Longint; Const Value:Double);
    Procedure SetErrorValues(Value:TChartValueList);
    Procedure SetErrorWidthUnits(Value:TErrorWidthUnits);
    Procedure SetErrorPen(Value:TChartPen);
    Procedure SetErrorWidth(Value:Integer);
  protected
    { Protected declarations }
    Procedure DrawError(X,Y,AWidth,AHeight:Longint; Draw3D:Boolean);
    Procedure SetSeriesColor(AColor:TColor); override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Function AddErrorBar(Const AX,AY,AError:Double;
                         Const AXLabel:String{$IFDEF D5}=''{$ENDIF};
                         AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF}):Longint;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); override;
    Procedure DrawBar(BarIndex,StartPos,EndPos:Longint); override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Procedure FillSampleValues(NumValues:Longint); override; { <-- to add random error values }
    Function GetEditorClass:String; override;
    Function MinYValue:Double; override;
    Function MaxYValue:Double; override;
    property ErrorValue[Index:Longint]:Double read GetErrorValue
                                              write SetErrorValue;
    Procedure Assign(Source:TPersistent); override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
    { To be published declarations }
    property ErrorPen:TChartPen read FErrorPen write SetErrorPen;
    property ErrorStyle:TErrorSeriesStyle read FErrorStyle write SetErrorStyle
                                          default essTopBottom;
    property ErrorValues:TChartValueList read FErrorValues write SetErrorValues;
    property ErrorWidth:Integer read FErrorWidth write SetErrorWidth default 100;
    property ErrorWidthUnits:TErrorWidthUnits read FErrorWidthUnits
                                              write SetErrorWidthUnits default ewuPercent;
  end;

  TErrorSeries=class(TCustomErrorSeries)
  published
    property ErrorPen;
    property ErrorStyle;
    property ErrorValues;
    property ErrorWidth;
    property ErrorWidthUnits;
  end;

  TErrorBarSeries=class(TCustomErrorSeries)
  public
    Constructor Create(AOwner:TComponent); override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    property ErrorPen;
    property ErrorValues;
    property ErrorWidth;
    property ErrorWidthUnits;
  end;

implementation

Uses Chart,TeeProCo,TeeConst;

Constructor TCustomErrorSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  IDrawBar:=False;
  FErrorValues :=TChartValueList.Create(Self,TeeMsg_ValuesStdError); { <-- Std Error storage }
  FErrorPen:=CreateChartPen;
  FErrorStyle:=essTopBottom;
  FErrorWidth:=100;
  FErrorWidthUnits:=ewuPercent;
  Marks.Visible:=False;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Destructor TCustomErrorSeries.Destroy;
begin
  FErrorPen.Free;
  inherited Destroy;
end;

Procedure TCustomErrorSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin);
  if (FErrorStyle=essLeft) or (FErrorStyle=essLeftRight) then
     LeftMargin  :=MaxLong(LeftMargin, FErrorPen.Width);
  if (FErrorStyle=essRight) or (FErrorStyle=essLeftRight) then
     RightMargin :=MaxLong(RightMargin, FErrorPen.Width);
end;

Procedure TCustomErrorSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  inherited CalcVerticalMargins(TopMargin,BottomMargin);
  if (FErrorStyle=essTop) or (FErrorStyle=essTopBottom) then
     TopMargin    :=MaxLong(TopMargin, FErrorPen.Width);
  if (FErrorStyle=essBottom) or (FErrorStyle=essTopBottom) then
     BottomMargin :=MaxLong(BottomMargin, FErrorPen.Width);
end;

Procedure TCustomErrorSeries.SetErrorPen(Value:TChartPen);
Begin
  FErrorPen.Assign(Value);
  if not IDrawBar then SeriesColor:=FErrorPen.Color;
End;

Procedure TCustomErrorSeries.PrepareErrorPen(ValueIndex:Integer);
begin
  With ParentChart.Canvas do
  begin
    AssignVisiblePen(FErrorPen);
    if (ValueIndex<>TeeAllValues) and (not IDrawBar) then
       Pen.Color:=ValueColor[ValueIndex];
    BackMode:=cbmTransparent;
  end;
end;

Procedure TCustomErrorSeries.SetErrorWidth(Value:Integer);
Begin
  SetIntegerProperty(FErrorWidth,Value);
End;

Procedure TCustomErrorSeries.DrawError(X,Y,AWidth,AHeight:Longint; Draw3D:Boolean);

  Procedure DrawHoriz(XPos:Longint);
  begin
    With ParentChart.Canvas do
    begin
      if Draw3D then
      begin
        HorizLine3D(X,XPos,Y,MiddleZ);
        VertLine3D(XPos,Y-AHeight,Y+AHeight,MiddleZ);
      end
      else
      begin
        DoHorizLine(X,XPos,Y);
        DoVertLine(XPos,Y-AHeight,Y+AHeight);
      end;
    end;
  end;

  Procedure DrawVert(YPos:Longint);
  begin
    With ParentChart.Canvas do
    begin
      if Draw3D then
      begin
        VertLine3D(X,Y,YPos,MiddleZ);
        HorizLine3D(X-(AWidth div 2),X+(AWidth div 2),YPos,MiddleZ);
      end
      else
      begin
        DoVertLine(X,Y,YPos);
        DoHorizLine(X-(AWidth div 2),X+(AWidth div 2),YPos);
      end;
    end;
  end;

begin
  Case FErrorStyle of
    essLeft     : DrawHoriz(X-(AWidth div 2));
    essRight    : DrawHoriz(X+(AWidth div 2));
    essLeftRight: begin
                    DrawHoriz(X-(AWidth div 2));
                    DrawHoriz(X+(AWidth div 2));
                  end;
    essTop      : DrawVert(Y-AHeight);
    essBottom   : DrawVert(Y+AHeight);
    essTopBottom: begin
                    DrawVert(Y-AHeight);
                    DrawVert(Y+AHeight);
                  end;
  end;
end;

Procedure TCustomErrorSeries.DrawBar(BarIndex,StartPos,EndPos:Longint);
Var tmp         : Longint;
    tmpWidth    : Longint;
    tmpBarWidth : Longint;
    ErrorTop    : Longint;
    tmpError    : Double;
Begin
  if IDrawBar then inherited DrawBar(BarIndex,StartPos,EndPos);
  if FErrorPen.Visible then
  Begin
    tmpError:=FErrorValues.Value[BarIndex];
    if tmpError<>0 then
    Begin
      if IDrawBar and (YValue[BarIndex]<YOrigin) then tmpError:=-tmpError;
      ErrorTop:=CalcYPosValue(YValue[BarIndex]+tmpError);
      tmpBarWidth:=BarBounds.Right-BarBounds.Left;

      if FErrorWidth=0 then tmpWidth:=tmpBarWidth
      else
      if FErrorWidthUnits=ewuPercent then
         tmpWidth:=Round(1.0*FErrorWidth*tmpBarWidth/100.0)
      else
         tmpWidth:=FErrorWidth;

      if (not IDrawBar) or (YValue[BarIndex]>0) then tmp:=StartPos
                                                else tmp:=EndPos;

      PrepareErrorPen(BarIndex);
      DrawError((BarBounds.Right+BarBounds.Left) div 2,tmp,
                 tmpWidth,tmp-ErrorTop,ParentChart.View3D);
    end;
  end;
End;

Procedure TCustomErrorSeries.SetErrorWidthUnits(Value:TErrorWidthUnits);
Begin
  if FErrorWidthUnits<>Value then
  Begin
    FErrorWidthUnits:=Value;
    Repaint;
  end;
end;

Procedure TCustomErrorSeries.SetErrorStyle(Value:TErrorSeriesStyle);
begin
  if FErrorStyle<>Value then
  begin
    FErrorStyle:=Value;
    Repaint;
  end;
end;

Procedure TCustomErrorSeries.SetErrorValues(Value:TChartValueList);
Begin
  SetChartValueList(FErrorValues,Value); { standard method }
End;

Function TCustomErrorSeries.AddErrorBar( Const AX,AY,AError:Double;
                                         Const AXLabel:String;
                                         AColor:TColor):Longint;
Begin
  result:=AddXY(AX,AY,AXLabel,AColor); { standard add X,Y }
  FErrorValues.TempValue:=AError;
  AddValue(result);
End;

Procedure TCustomErrorSeries.FillSampleValues(NumValues:Longint);
Var t:Longint;
    tmpX,tmpY,StepX,MinY,DifY:Double;
Begin
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  for t:=1 to NumValues do { some sample values to see something in design mode }
  Begin
    AddErrorBar( tmpX,
                 Random(Round(DifY)),
                 DifY/(20+Random(4))
                 {$IFNDEF D5},'', clTeeColor{$ENDIF});
    tmpX:=tmpX+StepX;
  end;
  RefreshSeries;
end;

Function TCustomErrorSeries.MaxYValue:Double;
Var t:Longint;
Begin
  result:=inherited MaxYValue;
  for t:=0 to Count-1 do result:=MaxDouble(result,YValue[t]+FErrorValues.Value[t]);
End;

Function TCustomErrorSeries.MinYValue:Double;
Var t      : Longint;
    tmp    : Double;
    tmpErr : Double;
Begin
  if IDrawBar then result:=inherited MinYValue else result:=0;
  for t:=0 to Count-1 do
  if IDrawBar then
  Begin
    tmpErr:=FErrorValues.Value[t];
    tmp:=YValue[t];
    if tmp<0 then tmp:=tmp-tmpErr else tmp:=tmp+tmpErr;
    if tmp<result then result:=tmp;
  end
  else
  begin
    tmp:=YValue[t]-FErrorValues.Value[t];
    if t=0 then result:=tmp else result:=MinDouble(result,tmp);
  end;
End;

Function TCustomErrorSeries.GetErrorValue(Index:Longint):Double;
Begin
  result:=FErrorValues.Value[Index];
End;

Procedure TCustomErrorSeries.SetErrorValue(Index:Longint; Const Value:Double);
Begin
  FErrorValues.Value[Index]:=Value;
End;

Function TCustomErrorSeries.GetEditorClass:String;
Begin
  result:='TErrorSeriesEditor';
end;

Procedure TCustomErrorSeries.Assign(Source:TPersistent);
begin
  if Source is TCustomErrorSeries then
  With TCustomErrorSeries(Source) do
  begin
    Self.FErrorPen.Assign(FErrorPen);
    Self.FErrorStyle:=FErrorStyle;
    Self.FErrorWidth:=FErrorWidth;
    Self.FErrorWidthUnits:=FErrorWidthUnits;
  end;
  inherited Assign(Source);
end;

Procedure TCustomErrorSeries.PrepareForGallery(IsEnabled:Boolean);
Const Colors:Array[Boolean] of TColor=(clSilver,clBlue);
      ErrorColors:Array[Boolean] of TColor=(clWhite,clRed);
begin
  inherited PrepareForGallery(IsEnabled);
  FErrorPen.Color:=ErrorColors[IsEnabled];
  SeriesColor:=Colors[IsEnabled];
end;

Procedure TCustomErrorSeries.SetSeriesColor(AColor:TColor);
begin
  inherited SetSeriesColor(AColor);
  if not IDrawBar then FErrorPen.Color:=AColor;
end;

Procedure TCustomErrorSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
begin
  PrepareErrorPen(ValueIndex);
  With Rect do
    DrawError( (Left+Right) shr 1,(Top+Bottom) shr 1,
               Right-Left,(Bottom-Top) div 2,False);
end;

{ TErrorBarSeries }
Constructor TErrorBarSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  IDrawBar:=True;
  FErrorStyle:=essTop;
end;

Procedure TErrorBarSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  FErrorPen.Width:=2;
  inherited PrepareForGallery(IsEnabled);
end;

Procedure TeeErrorSeriesExitProc; far;
begin
  UnRegisterTeeSeries( [TErrorBarSeries,TErrorSeries]);
end;

initialization
RegisterTeeSeries(TErrorBarSeries,TeeMsg_GalleryErrorBar,TeeMsg_GalleryExtended,1);
RegisterTeeSeries(TErrorSeries,TeeMsg_GalleryError,TeeMsg_GalleryExtended,1);
{$IFDEF D1}
  AddExitProc(TeeErrorSeriesExitProc);
{$ELSE}
finalization
  TeeErrorSeriesExitProc;
{$ENDIF}
end.

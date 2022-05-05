{*****************************************}
{   TeeChart-Pro 4.0                      }
{   Copyright (c) 1995-98 David Berneda   }
{     TPoint3DSeries                      }
{*****************************************}
{$I teedefs.inc}
unit TeePoin3;

interface

Uses WinTypes, WinProcs, Classes, SysUtils, Graphics, Teengine, TeeSurfa,
     Series, TeCanvas;

type
     TPoint3DSeries=class;

     TSeriesClickPointer3DEvent=Procedure( Sender:TPoint3DSeries;
                                           ValueIndex:Longint;
                                           X, Y: Integer) of object;

     TPoint3DSeries = class(TCustom3DSeries)
     private
       FDepthSize : Double;
       FLinePen   : TChartPen;
       FPointer   : TSeriesPointer;
       { events }
       FOnClickPointer     : TSeriesClickPointer3DEvent;
       FOnGetPointerStyle  : TOnGetPointerStyle;

       { internal }
       IOldX     : Integer;
       IOldY     : Integer;
       IOldZ     : Integer;
       Procedure SetDepthSize(Const Value:Double);
       Procedure CalcZPositions(ValueIndex:Longint);
       Procedure SetLinePen(Value:TChartPen);
       Procedure SetPointer(Value:TSeriesPointer);
     protected
     public
       Constructor Create(AOwner: TComponent); override;
       Destructor Destroy; override;

       Procedure Assign(Source:TPersistent); override;
       Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
       Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); override;
       Function Clicked(x,y:Integer):Longint; override;
       Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
       Procedure DrawMark( ValueIndex:Longint; Const St:String;
                           APosition:TSeriesMarkPosition); override;
       Procedure DrawValue(ValueIndex:Longint); override;
       Procedure FillSampleValues(NumValues:Longint); override;
       Function  GetEditorClass:String; override;
       Function MaxZValue:Double; override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
     published
       property DepthSize:Double read FDepthSize write SetDepthSize;
       property LinePen:TChartPen read FLinePen write SetLinePen;
       property Pointer:TSeriesPointer read FPointer write SetPointer;
       property TimesZOrder;
       property XValues;
       property YValues;
       property ZValues;
     { events }
       property OnClickPointer:TSeriesClickPointer3DEvent read FOnClickPointer
                                                          write FOnClickPointer;
     end;

implementation

Uses Chart,TeeConst,TeeProCo;

Constructor TPoint3DSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointer:=TSeriesPointer.Create(Self);
  FDepthSize:=4;
  FLinePen:=CreateChartPen;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Destructor TPoint3DSeries.Destroy;
begin
  FLinePen.Free;
  FPointer.Free;
  inherited Destroy;
end;

Procedure TPoint3DSeries.SetPointer(Value:TSeriesPointer);
Begin
  FPointer.Assign(Value);
end;

Procedure TPoint3DSeries.CalcZPositions(ValueIndex:Longint);
begin
  With ParentChart.DepthAxis do
  begin
    StartZ:=CalcYPosValue(ZValues[ValueIndex]);
    EndZ:=StartZ+CalcYSizeValue(FDepthSize);
  end;
end;

Procedure TPoint3DSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin);
  FPointer.CalcHorizMargins(LeftMargin,RightMargin);
end;

Procedure TPoint3DSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  inherited CalcVerticalMargins(TopMargin,BottomMargin);
  FPointer.CalcVerticalMargins(TopMargin,BottomMargin);
end;

Procedure TPoint3DSeries.DrawValue(ValueIndex:Longint);
var tmpColor : TColor;
    tmpStyle : TSeriesPointerStyle;
    tmpX     : Integer;
    tmpY     : Integer;
begin
  CalcZPositions(ValueIndex);
  With Pointer do
  begin
    tmpX:=CalcXPos(ValueIndex);
    tmpY:=CalcYPos(ValueIndex);
    if Visible then
    begin { emulate TCustomSeries.DrawPointer method }
      tmpColor:=ValueColor[ValueIndex];
      PrepareCanvas(tmpColor);
      if Assigned(FOnGetPointerStyle) then
         tmpStyle:=FOnGetPointerStyle(Self,ValueIndex)
      else
         tmpStyle:=FPointer.Style;
      DrawPointer( ParentChart.View3D,tmpX,tmpY,HorizSize,VertSize,tmpColor,tmpStyle);
    end;

    if ValueIndex>FirstValueIndex then
    if FLinePen.Visible then
    With ParentChart.Canvas do
    begin
      Pen.Assign(FLinePen);
      BackMode:=cbmTransparent;
      MoveTo3D(IOldX,IOldY,IOldZ);
      LineTo3D(tmpX,tmpY,StartZ);
    end;
    IOldX:=tmpX;
    IOldY:=tmpY;
    IOldZ:=StartZ;
  end;
end;

Procedure TPoint3DSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
var tmpColor:TColor;
begin
  if FPointer.Visible then
  begin
    if ValueIndex=-1 then tmpColor:=SeriesColor
                     else tmpColor:=ValueColor[ValueIndex];
    FPointer.DrawLegendShape(tmpColor,Rect,FLinePen.Visible);
  end
  else inherited DrawLegendShape(ValueIndex,Rect)
end;

Procedure TPoint3DSeries.FillSampleValues(NumValues:Longint);
var t:Longint;
Begin
  for t:=1 to NumValues do
    AddXYZ( Random(100), Random(100), Random(100){$IFNDEF D5},'', clTeeColor{$ENDIF});
end;

Procedure TPoint3DSeries.SetDepthSize(Const Value:Double);
begin
  SetDoubleProperty(FDepthSize,Value);
end;

Function TPoint3DSeries.MaxZValue:Double;
begin
  result:=ZValues.MaxValue+FDepthSize;
end;

Procedure TPoint3DSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                   APosition:TSeriesMarkPosition);
var tmp:Integer;
begin
  CalcZPositions(ValueIndex);
  if FPointer.Visible then Marks.ZPosition:=(StartZ+EndZ) div 2
                      else Marks.ZPosition:=StartZ;
  tmp:=Marks.ArrowLength;
  With APosition do
  begin
    Dec(LeftTop.Y,tmp);
    Dec(ArrowTo.Y,tmp);
  end;
  inherited DrawMark(ValueIndex,St,APosition);
end;

Procedure TPoint3DSeries.SetLinePen(Value:TChartPen);
Begin
  FLinePen.Assign(Value);
end;

Procedure TPoint3DSeries.Assign(Source:TPersistent);
begin
  if Source is TPoint3DSeries then
  With TPoint3DSeries(Source) do
  begin
    Self.FPointer.Assign(FPointer);
    Self.FDepthSize:=FDepthSize;
    Self.FLinePen.Assign(FLinePen);
  end;
  inherited Assign(Source);
end;

Function TPoint3DSeries.GetEditorClass:String;
Begin
  result:='TPoint3DSeriesEditor'; { <-- dont translate ! }
end;

Procedure TPoint3DSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  LinePen.Color:=clNavy;
end;

Function TPoint3DSeries.Clicked(x,y:Integer):Longint;
var t    : Longint;
    tmpX : Integer;
    tmpY : Integer;
    OldX : Integer;
    OldY : Integer;
begin
  OldX:=X;
  OldY:=Y;
  result:=inherited Clicked(x,y);
  if result=TeeNoPointClicked then
  if FPointer.Visible then
  for t:=0 to Count-1  do
  begin
    tmpX:=CalcXPos(t);
    tmpY:=CalcYPos(t);
    X:=OldX;
    Y:=OldY;
    if ParentChart<>nil then
    With ParentChart do
      Canvas.Calculate2DPosition(X,Y,DepthAxis.CalcYPosValue(ZValues[t]));
    if (Abs(tmpX-X)<FPointer.HorizSize) and { <-- Canvas.Zoom? }
       (Abs(tmpY-Y)<FPointer.VertSize) then
    begin
      if Assigned(FOnClickPointer) then FOnClickPointer(Self,t,OldX,OldY);
      result:=t;
      break;
    end;
  end;
end;

{ Un-register the Series }
Procedure TeePoint3DExitProc; far;
begin
  UnRegisterTeeSeries([TPoint3DSeries]);
end;

{ Register the Series at Chart gallery }
initialization
  RegisterTeeSeries( TPoint3DSeries, TeeMsg_GalleryPoint3D, TeeMsg_GalleryExtended, 1 );
{$IFDEF D1}
  AddExitProc(TeePoint3DExitProc);
{$ELSE}
finalization
  TeePoint3DExitProc;
{$ENDIF}
end.

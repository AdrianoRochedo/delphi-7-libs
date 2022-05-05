{****************************************}
{    TeeChart. TChartShape Component     }
{ Copyright (c) 1995,98 by David Berneda }
{    All Rights Reserved                 }
{****************************************}
{$I teedefs.inc}
unit TeeShape;

interface

Uses WinProcs, WinTypes, Chart, Graphics, Classes, Series, Teengine, TeCanvas;

Type
  TChartShapeXYStyle=( xysPixels, xysAxis, xysAxisOrigin );

  TChartShapeStyle=( chasRectangle,
                     chasCircle,
                     chasVertLine,
                     chasHorizLine,
                     chasTriangle,
                     chasInvertTriangle,
                     chasLine,
                     chasDiamond,
                     chasCube,
                     chasCross,
                     chasDiagCross,
                     chasStar,
                     chasPyramid,
                     chasInvertPyramid );

  TChartShape = class(TChartSeries)
  private
     FAlignment      : TAlignment;
     FBrush          : TChartBrush;
     FFont           : TFont;
     FPen            : TChartPen;
     FRoundRectangle : Boolean;
     FStyle          : TChartShapeStyle;
     FText           : TStrings;
     FTransparent    : Boolean;
     FXYStyle        : TChartShapeXYStyle;
     Procedure AddDefaultPoints;
     procedure SetAlignment(Value: TAlignment);
     procedure SetBrush(Value : TChartBrush);
     procedure SetFont(Value: TFont);
     procedure SetPen(Value : TChartPen);
     procedure SetRoundRectangle(Value: Boolean);
     procedure SetStyle(Value : TChartShapeStyle);
     procedure SetTransparent(Value: Boolean);
     Procedure SetX0(Const Value:Double);
     Procedure SetX1(Const Value:Double);
     Procedure SetY0(Const Value:Double);
     Procedure SetY1(Const Value:Double);
     Function GetX0:Double;
     Function GetX1:Double;
     Function GetY0:Double;
     Function GetY1:Double;
     procedure SetXYStyle(Value: TChartShapeXYStyle);
  protected
     procedure DrawShape(Is3D:Boolean; Const R:TRect);
     procedure DrawText(Const R:TRect);
     Procedure SetSeriesColor(AColor:TColor); override;
     Function GetShapeRectangle:TRect; virtual;
     Procedure SetShapeRectangle(Const ARect:TRect);
     procedure SetText(Value : TStrings); virtual;
     Function MoreSameZOrder:Boolean; override;
  public
     Constructor Create(AOwner : TComponent); override;
     Destructor Destroy; override;
     Procedure Assign(Source:TPersistent); override;
     Procedure CalcZOrder; override;
     Function Clicked(x,y:Integer):Longint; override;
     Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
     procedure DrawValue(ValueIndex:Longint); override;
     Procedure FillSampleValues(NumValues:Longint); override;
     Procedure PrepareForGallery(IsEnabled:Boolean); override;
     Function GetEditorClass:String; override;
     Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
     Function UseAxis:Boolean; override;
     property Bounds:TRect read GetShapeRectangle write SetShapeRectangle;
  published
     property Alignment: TAlignment read FAlignment write SetAlignment
                                    default taCenter;
     property Brush : TChartBrush read FBrush write SetBrush;
     property Font:TFont read FFont write SetFont;
     property Text:TStrings read FText write SetText;
     property Pen : TChartPen read FPen write SetPen;
     property RoundRectangle: Boolean read FRoundRectangle
                                    write SetRoundRectangle default False;
     property Style : TChartShapeStyle Read FStyle write SetStyle
                                       default chasCircle;
     property Transparent:Boolean read FTransparent
                                  write SetTransparent default False;
     property XYStyle:TChartShapeXYStyle read FXYStyle
                                         write SetXYStyle default xysAxis;
     property X0:Double read GetX0 write SetX0;
     property X1:Double read GetX1 write SetX1;
     property Y0:Double read GetY0 write SetY0;
     property Y1:Double read GetY1 write SetY1;
     property XValues;
     property YValues;
  end;

implementation

Uses SysUtils,TeeProcs,TeeConst;

Constructor TChartShape.Create(AOwner : TComponent);
Begin
  inherited Create(AOwner);
  FAlignment:=taCenter;
  CalcVisiblePoints:=False;
  FBrush:=TChartBrush.Create(CanvasChanged);
  FBrush.Color:=clWhite;
  FStyle:=chasCircle;
  FFont:=CreateDefaultFont(CanvasChanged);
  FText:=TStringList.Create;
  (FText as TStringList).OnChange:=CanvasChanged;
  FPen:=CreateChartPen;
  SeriesColor:=FBrush.Color;
  FXYStyle:=xysAxis;
  AddDefaultPoints;
End;

Destructor TChartShape.Destroy;
Begin
  FText.Free;
  FFont.Free;
  FBrush.Free;
  FPen.Free;
  inherited Destroy;
End;

Procedure TChartShape.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
begin
  DrawShape(False,Rect);
end;

procedure TChartShape.SetBrush(Value : TChartBrush);
Begin
  FBrush:=Value;
End;

procedure TChartShape.SetPen(Value : TChartPen);
Begin
  FPen:=Value;
End;

Function TChartShape.GetX0:Double;
Begin
  result:=XValue[0]
End;

Procedure TChartShape.SetX0(Const Value:Double);
Begin
  XValue[0]:=Value;
End;

Function TChartShape.GetY0:Double;
Begin
  result:=YValue[0]
End;

Procedure TChartShape.SetY0(Const Value:Double);
Begin
  YValue[0]:=Value;
End;

Function TChartShape.GetX1:Double;
Begin
  result:=XValue[1]
End;

Procedure TChartShape.SetX1(Const Value:Double);
Begin
  XValue[1]:=Value;
End;

Function TChartShape.GetY1:Double;
Begin
  result:=YValue[1]
End;

Procedure TChartShape.SetY1(Const Value:Double);
Begin
  YValue[1]:=Value;
End;

procedure TChartShape.SetStyle(Value : TChartShapeStyle);
Begin
  if Value<>FStyle then
  begin
    FStyle:=Value;
    Repaint;
  end;
End;

Procedure TChartShape.SetSeriesColor(AColor:TColor);
Begin
  inherited SetSeriesColor(AColor);
  FBrush.Color:=SeriesColor;
end;

procedure TChartShape.DrawShape(Is3D:Boolean; Const R:TRect);

  Procedure DrawDiagonalCross2D;
  begin
    With ParentChart.Canvas,R do
    Begin
      Line(Left,Top,Right,Bottom);
      Line(Left,Bottom,Right,Top);
    end;
  end;

  Procedure DrawDiagonalCross3D;
  begin
    With ParentChart.Canvas,R do
    Begin
      LineWithZ(Left,Top,Right,Bottom,MiddleZ);
      LineWithZ(Left,Bottom,Right,Top,MiddleZ);
    end;
  end;

var tmpMidX : Longint;
    tmpMidY : Longint;

  Procedure DrawCross3D;
  begin
    With ParentChart.Canvas,R do
    Begin
      VertLine3D(tmpMidX,Top,Bottom,MiddleZ);
      HorizLine3D(Left,Right,tmpMidY,MiddleZ);
    end;
  end;

  Procedure DrawCross2D;
  begin
    With ParentChart.Canvas,R do
    Begin
      DoVertLine(tmpMidX,Top,Bottom);
      DoHorizLine(Left,Right,tmpMidY);
    end;
  end;

begin
  With ParentChart.Canvas do
  Begin
    AssignVisiblePen(Self.FPen);
    if FTransparent then Brush.Style:=bsClear
    else
    begin
      Brush.Assign(Self.FBrush);
      if Self.FBrush.Bitmap<>nil then Brush.Bitmap:=Self.FBrush.Bitmap;
    end;
    BackMode:=cbmTransparent;

    RectCenter(R,tmpMidX,tmpMidY);

    With R do
    if Is3D then
    Case Self.FStyle of
     chasRectangle      : RectangleWithZ(R,MiddleZ);
     chasCircle         : EllipseWithZ(Left,Top,Right,Bottom,MiddleZ);
     chasVertLine       : VertLine3D(tmpMidX,Top,Bottom,MiddleZ);
     chasHorizLine      : HorizLine3D(Left,Right,tmpMidY,MiddleZ);
     chasTriangle       : TriangleWithZ( Classes.Point(Left,Bottom),
                                         Classes.Point(tmpMidX,Top),
                                         BottomRight, MiddleZ );
     chasInvertTriangle : TriangleWithZ( TopLeft,
                                         Classes.Point(tmpMidX,Bottom),
                                         Classes.Point(Right,Top), MiddleZ);
     chasLine           : LineWithZ(Left,Top,Right,Bottom,MiddleZ);
     chasDiamond        : PlaneWithZ( Classes.Point(Left,tmpMidY),
                                      Classes.Point(tmpMidX,Top),
                                      Classes.Point(Right,tmpMidY),
                                      Classes.Point(tmpMidX,Bottom), MiddleZ );
     chasCube           : Cube(Left,Right,Top,Bottom,StartZ,EndZ,not FTransparent);
     chasCross          : DrawCross3D;
     chasDiagCross      : DrawDiagonalCross3D;
     chasStar           : begin DrawCross3D; DrawDiagonalCross3D; end;
     chasPyramid        : Pyramid(True,Left,Top,Right,Bottom,StartZ,EndZ,not FTransparent);
     chasInvertPyramid  : Pyramid(True,Left,Bottom,Right,Top,StartZ,EndZ,not FTransparent);
    end
    else
    Case Self.FStyle of
     chasRectangle      : if FRoundRectangle then
                             RoundRect(Left,Top,Right,Bottom,12,12)
                          else
                             DoRectangle(R);
     chasCircle         : Ellipse(Left,Top,Right,Bottom);
     chasVertLine       : DoVertLine(tmpMidX,Top,Bottom);
     chasHorizLine      : DoHorizLine(Left,Right,tmpMidY);
     chasTriangle,
     chasPyramid        : Polygon( [Classes.Point(Left,Bottom),
                                    Classes.Point(tmpMidX,Top),
                                    BottomRight] );
     chasInvertTriangle,
     chasInvertPyramid  : Polygon( [TopLeft,
                                    Classes.Point(tmpMidX,Bottom),
                                    Classes.Point(Right,Top)]);
     chasLine           : Line(Left,Top,Right,Bottom);
     chasDiamond        : Polygon( [Classes.Point(Left,tmpMidY),
                                    Classes.Point(tmpMidX,R.Top),
                                    Classes.Point(Right,tmpMidY),
                                    Classes.Point(tmpMidX,Bottom)] );
     chasCube           : DoRectangle(R);
     chasCross          : DrawCross2D;
     chasDiagCross      : DrawDiagonalCross2D;
     chasStar           : begin DrawCross2D; DrawDiagonalCross2D; end;
    end;
  end;
end;

procedure TChartShape.DrawText(Const R:TRect);
Const ShapeHorizMargin=4;
      BrushColors:Array[Boolean] of TColor=(clBlack,clWhite);
var t        : Integer;
    tmpPosX  : Integer;
    tmpH     : Integer;
    tmpMidX  : Longint;
    tmpMidY  : Longint;
    tmpPosY  : Integer;
    tmpWidth : Integer;
begin
  With ParentChart,Canvas do
  if Self.FText.Count>0 then
  begin
    FontCanvas(Self.Font);
    With Font do
         if Brush.Color=Color then Color:=BrushColors[Color=clBlack];
    tmpH:=FontHeight;
    RectCenter(R,tmpMidX,tmpMidY);
    tmpPosY:=tmpMidY-Round(tmpH*Self.FText.Count/2.0);
    BackMode:=cbmTransparent;
    for t:=0 to Self.FText.Count-1 do
    begin
      tmpWidth:=TextWidth(FText[t]);
      Case FAlignment of
        taCenter       : tmpPosX:=tmpMidX-(tmpWidth div 2);
        taLeftJustify  : tmpPosX:=R.Left+Pen.Width+ShapeHorizMargin;
      else
        tmpPosX:=R.Right-Pen.Width-tmpWidth-ShapeHorizMargin;
      end;
      TextOut3D(tmpPosX,tmpPosY,StartZ,FText[t]);
      Inc(tmpPosY,tmpH);
    end;
  end;
end;

Procedure TChartShape.SetShapeRectangle(Const ARect:TRect);
begin
  FXYStyle:=xysPixels;
  With ARect do
  begin
    X0:=Left;
    Y0:=Top;
    X1:=Right;
    Y1:=Bottom;
  end;
end;

Function TChartShape.GetShapeRectangle:TRect;
begin
  Case FXYStyle of
    xysPixels: result:=Rect( Trunc(X0), Trunc(Y0), Trunc(X1), Trunc(Y1) );
    xysAxis  : result:=Rect( CalcXPos(0),CalcYPos(0),CalcXPos(1),CalcYPos(1) );
  else
    With Result do
    begin
      Left:=CalcXPos(0);
      Top :=CalcYPos(0);
      Right:=Left+Trunc(X1);
      Bottom:=Top+Trunc(Y1);
    end;
  end;
  With Result do
  begin
    if Top=Bottom then Bottom:=Top+1 else
    if Top>Bottom then SwapInteger(Top,Bottom);
    if Left=Right then Right:=Left+1 else
    if Left>Right then SwapInteger(Left,Right);
  end;
end;

procedure TChartShape.DrawValue(ValueIndex:Longint);
Var R        : TRect;
    DestRect : TRect;
Begin
  if (Count=2) and (ValueIndex=0) then
  begin
    R:=GetShapeRectangle;
    if IntersectRect(DestRect,R,ParentChart.ChartRect){$IFDEF D1}<>0{$ENDIF} then
    begin
      DrawShape(ParentChart.View3D,R);
      DrawText(R);
    end;
  end;
End;

Procedure TChartShape.AddDefaultPoints;
begin
  AddXY(   0,   0{$IFNDEF D5},'', clTeeColor{$ENDIF});
  AddXY( 100, 100{$IFNDEF D5},'', clTeeColor{$ENDIF});
end;

Procedure TChartShape.FillSampleValues(NumValues:Longint);
var tmpX,StepX,tmpY,MinY,DifY:Double;
Begin
  Clear;
  CalcRandomBounds(1,tmpX,StepX,tmpY,MinY,DifY);
  if StepX=0 then AddDefaultPoints
  else
  begin
    AddXY( tmpX+(StepX/8.0), tmpY/2{$IFNDEF D5},'', clTeeColor{$ENDIF} );
    AddXY( tmpX+StepX-(StepX/8.0),tmpY+Random(Round(DifY)){$IFNDEF D5},'', clTeeColor{$ENDIF} );
  end;
  RefreshSeries;
End;

Function TChartShape.Clicked(x,y:Integer):Longint;
var R       : TRect;
    tmp     : Boolean;
    tmpMidX : Longint;
    tmpMidY : Longint;
    P       : TPoint;
Begin
  if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(X,Y,StartZ);
  P.X:=X;
  P.Y:=Y;
  R:=GetShapeRectangle;
  RectCenter(R,tmpMidX,tmpMidY);
  Case FStyle of
     chasVertLine: tmp:=PointInLine(P,tmpMidX,R.Top,tmpMidX,R.Bottom);
    chasHorizLine: tmp:=PointInLine(P,R.Left,tmpMidY,R.Right,tmpMidY);
         chasLine: tmp:=PointInLine(P,R.Left,R.Top,R.Right,R.Bottom);
      chasDiamond: tmp:=PointInPolygon( P,[ Classes.Point(tmpMidX,R.Top),
                                            Classes.Point(R.Right,tmpMidY),
                                            Classes.Point(tmpMidX,R.Bottom),
                                            Classes.Point(R.Left,tmpMidY)] );
     chasTriangle,
     chasPyramid : tmp:=PointInTriangle( P,R.Left,R.Right,R.Bottom,R.Top);
chasInvertTriangle,
chasInvertPyramid: tmp:=PointInTriangle( P,R.Left,R.Right,R.Top,R.Bottom);
       chasCircle: tmp:=PointInEllipse(P,R);
  else
    tmp:=PtInRect(R,P);
  end;
  if tmp then result:=0 else result:=-1;
End;

Procedure TChartShape.PrepareForGallery(IsEnabled:Boolean);
Const EnabledColor1:Array[Boolean] of TColor=(clSilver,clBlue);
      EnabledColor2:Array[Boolean] of TColor=(clSilver,clRed);
Begin
  inherited PrepareForGallery(IsEnabled);
  Font.Color:=clWhite;
  Font.Size:=14;
  Text.Clear;
  if ParentChart.SeriesList.IndexOf(Self)=1 then
  begin
    Style:=chasCircle;
    Brush.Color:=EnabledColor1[IsEnabled];
    Text.Add(TeeMsg_ShapeGallery1);
  end
  else
  begin
    Style:=chasTriangle;
    Brush.Color:=EnabledColor2[IsEnabled];
    Text.Add(TeeMsg_ShapeGallery2);
  end
end;

Function TChartShape.GetEditorClass:String;
Begin
  result:='TChartShapeEditor';  { <-- dont translate }
end;

Procedure TChartShape.Assign(Source:TPersistent);
begin
  if Source is TChartShape then
  With TChartShape(Source) do
  begin
    Self.FAlignment:=FAlignment;
    Self.FBrush.Assign(FBrush);
    Self.FFont.Assign(FFont);
    Self.FPen.Assign(FPen);
    Self.FRoundRectangle:=FRoundRectangle;
    Self.FStyle :=FStyle;
    Self.FText.Assign(FText);
    Self.FTransparent:=FTransparent;
    Self.FXYStyle:=FXYStyle;
  end;
  inherited Assign(Source);
end;

Function TChartShape.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TChartShape;
end;

procedure TChartShape.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TChartShape.SetAlignment(Value: TAlignment);
begin
  if FAlignment<>Value then
  begin
    FAlignment:=Value;
    Repaint;
  end;
end;

procedure TChartShape.SetText(Value : TStrings);
begin
  FText.Assign(Value);
  Repaint;
end;

procedure TChartShape.SetTransparent(Value: Boolean);
begin
  SetBooleanProperty(FTransparent,Value);
end;

procedure TChartShape.SetRoundRectangle(Value: Boolean);
begin
  SetBooleanProperty(FRoundRectangle,Value);
end;

procedure TChartShape.SetXYStyle(Value: TChartShapeXYStyle);
begin
  if FXYStyle<>Value then
  begin
    FXYStyle:=Value;
    Repaint;
  end;
end;

Function TChartShape.UseAxis:Boolean;
begin
  result:=FXYStyle<>xysPixels;
end;

Procedure TChartShape.CalcZOrder;
begin
  if UseAxis then inherited CalcZOrder;
end;

Function TChartShape.MoreSameZOrder:Boolean;
begin
  result:=False;
end;

initialization
  RegisterTeeSeries(TChartShape, TeeMsg_GalleryShape, TeeMsg_GalleryStandard, 2);
end.

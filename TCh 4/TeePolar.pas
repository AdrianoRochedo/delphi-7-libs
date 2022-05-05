{*********************************************}
{ TeeChart Delphi Component Library           }
{    Polar and Radar Series Components        }
{ Copyright (c) 1995-1998 by David Berneda    }
{ All rights reserved                         }
{*********************************************}
{$I teedefs.inc}
unit TeePolar;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, TeEngine, Chart, Series, TeCanvas;

Const TeePolarDegreeSymbol:Char='º';

type
  TCustomPolarSeries = class(TCircledSeries)
  private
    FBrush            : TChartBrush;
    FCircleLabels     : Boolean;
    FCircleLabelsFont : TFont;
    FCircleLabelsRot  : Boolean;
    FCirclePen        : TChartPen;
    FCloseCircle      : Boolean;
    FPen              : TChartPen;
    FPointer          : TSeriesPointer;

    { Private declarations }
    OldX              : Longint;
    OldY              : Longint;
    IMaxValuesCount   : Longint;
    Procedure CalcXYPos( ValueIndex:Longint;
                         Const ARadius:Double; Var X,Y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
    Procedure FillTriangle(Const A,B:TPoint; Z:Integer);
    Function GetAngleIncrement:Double;
    Function GetRadiusIncrement:Double;
    Function GetRadiusValues:TChartValueList;
    Function GetAngleValues:TChartValueList;
    Function IsFontStored:Boolean;
    Function IsRadiusStored:Boolean;
    Procedure SetAngleIncrement(Const Value:Double);
    Procedure SetAngleValues(Value:TChartValueList);
    Procedure SetBrush(Value:TChartBrush);
    Procedure SetCircleLabels(Value:Boolean);
    Procedure SetCircleLabelsFont(Value:TFont);
    procedure SetCirclePen(Value:TChartPen);
    Procedure SetCloseCircle(Value:Boolean);
    Procedure SetLabelsRotated(Value:Boolean);
    Procedure SetPen(Value:TChartPen);
    Procedure SetPointer(Value:TSeriesPointer);
    Procedure SetRadiusIncrement(Const Value:Double);
    Procedure SetRadiusValues(Value:TChartValueList);
  protected
    { Protected declarations }
    Procedure SetParentChart(Value:TCustomAxisPanel); override;
    Procedure DoBeforeDrawValues; override;
    Procedure DrawAllValues; override;
    Procedure DrawPolarCircle(HalfWidth,HalfHeight,Z:Integer);
    procedure DrawValue(ValueIndex:Longint); override;
    Function GetCircleLabel(Const AngleOrIndex:Double):String; virtual;
    procedure LinePrepareCanvas(ValueIndex:Longint);
    Procedure SetSeriesColor(AColor:TColor); override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function CalcXPos(ValueIndex:Longint):Integer; override;
    Function CalcYPos(ValueIndex:Longint):Integer; override;
    Function Clicked(x,y:Integer):Longint; override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Procedure DrawRing(Const Value:Double; Z:Integer);
    Function  GetEditorClass:String; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;

    { to be Published declarations }
    property AngleIncrement:Double read GetAngleIncrement write SetAngleIncrement;
    property AngleValues:TChartValueList read GetAngleValues write SetAngleValues;
    property Brush:TChartBrush read FBrush write SetBrush;
    property CircleLabels:Boolean read FCircleLabels write SetCircleLabels default False;
    property CircleLabelsFont:TFont read FCircleLabelsFont write SetCircleLabelsFont stored IsFontStored;
    property CircleLabelsRotated:Boolean read FCircleLabelsRot
                                         write SetLabelsRotated default False;
    property CirclePen:TChartPen read FCirclePen write SetCirclePen;
    property CloseCircle:Boolean read FCloseCircle
                                 write SetCloseCircle default True;
    property Pen:TChartPen read FPen write SetPen;
    property Pointer:TSeriesPointer read FPointer write SetPointer;
    property RadiusIncrement:Double read GetRadiusIncrement write SetRadiusIncrement;
    property RadiusValues:TChartValueList read GetRadiusValues write SetRadiusValues
                                          stored IsRadiusStored;
  end;

  TPolarSeries = class(TCustomPolarSeries)
  public
    Function AddPolar( Const Angle,Value:Double;
                       Const ALabel:String{$IFDEF D5}=''{$ENDIF};
                       AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF}):Longint;
    Procedure FillSampleValues(NumValues:Longint); override;
  published
    { Published declarations }
    property AngleIncrement;
    property AngleValues;
    property Brush;
    property CircleBackColor;
    property CircleLabels;
    property CircleLabelsFont;
    property CircleLabelsRotated;
    property CirclePen;
    property CloseCircle;
    property Pen;
    property Pointer;
    property RadiusIncrement;
    property RadiusValues;
    property RotationAngle;
  end;

  TRadarSeries=class(TCustomPolarSeries)
  protected
    Procedure DoBeforeDrawChart; override;
    Function GetCircleLabel(Const AngleOrIndex:Double):String; override;
  public
    Procedure FillSampleValues(NumValues:Longint); override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    { Published declarations }
    property Brush;
    property CircleBackColor;
    property CircleLabels;
    property CircleLabelsFont;
    property CircleLabelsRotated;
    property CirclePen;
    property CloseCircle;
    property Pen;
    property Pointer;
    property RadiusIncrement;
    property RadiusValues;
  end;

implementation

Uses TeeProCo,TeeConst,TeeProcs;

Const TeeMsg_RadiusValues='Radius'; { <-- dont translate ! }

{ TCustomPolarSeries }
Constructor TCustomPolarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IMaxValuesCount:=0;
  FCloseCircle := True;
  FPointer     := TSeriesPointer.Create(Self);
  FPen         := CreateChartPen;
  YValues.Name := TeeMsg_RadiusValues;
  FCirclePen   := CreateChartPen;
  FBrush       := TChartBrush.Create(CanvasChanged);
  FBrush.Style:=bsClear;
  FCircleLabels:= False;
  FCircleLabelsRot:=False;
  FCircleLabelsFont:=CreateDefaultFont(CanvasChanged);
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Destructor TCustomPolarSeries.Destroy;
Begin
  FCircleLabelsFont.Free;
  FBrush.Free;
  FPen.Free;
  FPointer.Free;
  FCirclePen.Free;
  inherited Destroy;
end;

Procedure TCustomPolarSeries.SetParentChart(Value:TCustomAxisPanel);
Begin
  inherited SetParentChart(Value);
  if (ParentChart<>nil) and (csDesigning in ComponentState) then
     ParentChart.View3D:=False;
end;

Procedure TCustomPolarSeries.SetCloseCircle(Value:Boolean);
begin
  SetBooleanProperty(FCloseCircle,Value);
end;

Procedure TCustomPolarSeries.SetLabelsRotated(Value:Boolean);
begin
  SetBooleanProperty(FCircleLabelsRot,Value);
end;

procedure TCustomPolarSeries.SetBrush(Value:TChartBrush);
begin
  FBrush.Assign(Value);
end;

procedure TCustomPolarSeries.SetCirclePen(Value:TChartPen);
begin
  FCirclePen.Assign(Value);
end;

Procedure TCustomPolarSeries.DrawPolarCircle(HalfWidth,HalfHeight,Z:Integer);
var OldP : TPoint;
    P    : TPoint;
    tmp  : Double;
    t    : Integer;
begin
  With ParentChart.Canvas do
  begin
    if IMaxValuesCount=0 then
       EllipseWithZ( CircleXCenter-HalfWidth,CircleYCenter-HalfHeight,
                     CircleXCenter+HalfWidth,CircleYCenter+HalfHeight, Z)
    else
    begin
      tmp:=piDegree*360.0/IMaxValuesCount;
      AngleToPos(0,HalfWidth,HalfHeight,OldP.X,OldP.Y);
      MoveTo3D(OldP.X,OldP.Y,Z);
      for t:=0 to IMaxValuesCount do
      begin
        AngleToPos(t*tmp,HalfWidth,HalfHeight,P.X,P.Y);
        if Brush.Style<>bsClear then FillTriangle(OldP,P,Z);
        LineTo3D(P.X,P.Y,Z);
        OldP:=P;
      end;
    end;
  end;
end;

Function TCustomPolarSeries.GetCircleLabel(Const AngleOrIndex:Double):String;
begin
  result:=FloatToStr(AngleOrIndex)+TeePolarDegreeSymbol;
end;

Procedure TCustomPolarSeries.DoBeforeDrawValues;

    Procedure SetGridCanvas(Axis:TChartAxis);
    Begin
      with ParentChart,Canvas do
      begin
        Brush.Style:=bsClear;
        BackMode:=cbmTransparent;
        Pen.Assign(Axis.Grid);
        CheckPenWidth(Pen);
        if Pen.Color=clTeeColor then Pen.Color:=clGray;
      end;
    end;

    Procedure DrawAngleLabel(Angle:Double);
    var X,Y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
    var tmpHeight : Integer;
        tmpWidth  : Integer;
        tmp       : Double;
        tmp2      : Double;
    begin
      ParentChart.FontCanvas(FCircleLabelsFont);
      With ParentChart.Canvas do
      begin
        tmpHeight:=FontHeight;
        if Angle>=360 then Angle:=Angle-360;
        tmp:=Angle;
        if FCircleLabelsRot then
        begin
          if (tmp>90) and (tmp<270) then tmp2:=Angle-3
                                    else tmp2:=Angle+2;
        end
        else tmp2:=Angle;
        AngleToPos((tmp2)*PiDegree,XRadius+2,YRadius+2,X,Y);
        Angle:=Angle+RotationAngle;
        if Angle>=360 then Angle:=Angle-360;
        if FCircleLabelsRot then
        begin
          if (Angle>90) and (Angle<270) then
          begin
            TextAlign:=ta_Right;
            Angle:=Angle+180;
          end
          else TextAlign:=ta_Left;
          RotateLabel3D(X,Y,EndZ,GetCircleLabel(tmp),Round(Angle));
        end
        else
        begin
          if (Angle=0) or (Angle=180) then Dec(Y,tmpHeight div 2)
          else
          if (Angle>0) and (Angle<180) then Dec(Y,tmpHeight);
          if (Angle=90) or (Angle=270) then TextAlign:=ta_Center
          else
          if (Angle>90) and (Angle<270) then TextAlign:=ta_Right
                                        else TextAlign:=ta_Left;
          tmpWidth:=TextWidth('0') div 2;
          if Angle=0 then Inc(x,tmpWidth) else
          if Angle=180 then Dec(x,tmpWidth);
          TextOut3D(X,Y,EndZ,GetCircleLabel(tmp));
        end;
      end;
    end;

var tmpValue     : Double;
    tmpIncrement : Double;

    Procedure DrawYGrid;

      Procedure InternalDrawGrid;
      begin
        DrawRing(tmpValue,EndZ);
        tmpValue:=tmpValue-tmpIncrement;
      end;

    Begin
      With GetVertAxis do
      if Grid.Visible then
      begin
        tmpIncrement:=CalcIncrement;
        if tmpIncrement>0 then
        begin
          SetGridCanvas(GetVertAxis);
          tmpValue:=Maximum/tmpIncrement;
          if (Abs(tmpValue)<MaxLongint) and
             (Abs((Maximum-Minimum)/tmpIncrement)<10000) then
          Begin
            if RoundFirstLabel then tmpValue:=tmpIncrement*Trunc(tmpValue)
                               else tmpValue:=Maximum;
            if LabelsOnAxis then
            begin
              While tmpValue>Maximum do tmpValue:=tmpValue-tmpIncrement;
              While tmpValue>=Minimum do InternalDrawGrid;
            end
            else
            begin
              While tmpValue>=Maximum do tmpValue:=tmpValue-tmpIncrement;
              While tmpValue>Minimum do InternalDrawGrid;
            end;
          end;
        end;
      end;
    end;

    Procedure DrawXGrid;
    var tmpX,tmpY:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
    Begin
      With GetHorizAxis do
      if Grid.Visible or Self.FCircleLabels then
      begin
        tmpIncrement:=Increment;
        if tmpIncrement<=0 then tmpIncrement:=10.0;
        SetGridCanvas(GetHorizAxis);
        tmpValue:=360;
        While tmpValue>=0 do
        begin
          if FCircleLabels then DrawAngleLabel(tmpValue);
          if Grid.Visible then
          begin
            AngleToPos(piDegree*tmpValue,XRadius,YRadius,tmpX,tmpY);
            ParentChart.Canvas.LineWithZ(CircleXCenter,CircleYCenter,tmpX,tmpY,EndZ);
          end;
          tmpValue:=tmpValue-tmpIncrement;
        end;
      end;
    end;

  Procedure DrawAxis;
  Var tmp : Integer;
  begin
    DrawXGrid;
    DrawYGrid;
    With ParentChart do
    begin
      With RightAxis do
      if Visible then
      begin
        tmp:=CircleXCenter+SizeTickAxis;
        CustomDrawMinMaxStartEnd( tmp,tmp+SizeLabels,CircleXCenter,False,
                                  LeftAxis.Minimum,LeftAxis.Maximum,LeftAxis.Increment,
                                  CircleYCenter-YRadius,CircleYCenter);
      end;
      if IMaxValuesCount=0 then
      begin
        With LeftAxis do
        if Visible then
        begin
          InternalSetInverted(True);
          tmp:=CircleXCenter-SizeTickAxis;
          CustomDrawStartEnd( tmp,tmp-SizeLabels,CircleXCenter,False,
                              CircleYCenter,CircleYCenter+YRadius);
          InternalSetInverted(False);
        end;
        With TopAxis do
        if Visible then
        begin
          InternalSetInverted(True);
          tmp:=CircleYCenter-SizeTickAxis;
          CustomDrawMinMaxStartEnd( tmp,tmp-SizeLabels,CircleYCenter,False,
                                    LeftAxis.Minimum,LeftAxis.Maximum,LeftAxis.Increment,
                                    CircleXCenter-XRadius,CircleXCenter);
          InternalSetInverted(False);
        end;
        With BottomAxis do
        if Visible then
        begin
          tmp:=CircleYCenter+SizeTickAxis;
          CustomDrawMinMaxStartEnd( tmp,tmp+SizeLabels,CircleYCenter,False,
                                    LeftAxis.Minimum,LeftAxis.Maximum,LeftAxis.Increment,
                                    CircleXCenter,CircleXCenter+XRadius);
        end;
      end;
    end;
  end;

  Procedure DrawCircle;
  begin
    With ParentChart.Canvas do
    Begin
      if CircleBackColor=clTeeColor then Brush.Style:=bsClear
      else
      begin
        Brush.Style:=bsSolid;
        Brush.Color:=CalcCircleBackColor;
      end;
      AssignVisiblePen(FCirclePen);
      DrawPolarCircle(CircleWidth div 2,CircleHeight div 2,EndZ);
    end;
  end;

var t   : Integer;
    tmp : Integer;
Begin
  With ParentChart do
  for t:=0 to SeriesCount-1 do
  if (Series[t].Active) and (Series[t] is Self.ClassType) then
     if Series[t]=Self then
     begin
       if FCircleLabels then
       begin
         FontCanvas(FCircleLabelsFont);
         With ChartRect do
         begin
           tmp:=Canvas.FontHeight+2;
           Inc(Top,tmp);
           Dec(Bottom,tmp);
           tmp:=Canvas.TextWidth('360');
           Inc(Left,tmp);
           Dec(Right,tmp);
         end;
       end;
       break;
     end;
  inherited DoBeforeDrawValues;
  With ParentChart do
  for t:=0 to SeriesCount-1 do
  if (Series[t].Active) and (Series[t] is Self.ClassType) then
     if Series[t]=Self then
     begin
       DrawCircle;
       DrawAxis;
     end
     else
     With TCustomPolarSeries(Series[t]) do
     begin
       CustomXRadius:=Self.XRadius;
       CustomYRadius:=Self.YRadius;
     end;
end;

Procedure TCustomPolarSeries.CalcXYPos( ValueIndex:Longint;
                                        Const ARadius:Double;
                                        Var X,Y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var tmp       : Double;
    tmpRadius : Double;
    {$IFDEF D1}
    tmpX      : Integer;
    tmpY      : Integer;
    {$ENDIF}
begin
  With GetVertAxis do tmp:=Maximum-Minimum;
  if tmp=0 then
  begin
    X:=0;
    Y:=0;
  end
  else
  begin
    tmpRadius:=(YValue[ValueIndex]-GetVertAxis.Minimum)*ARadius/tmp;
    {$IFNDEF D1}
    AngleToPos( piDegree*XValue[ValueIndex],tmpRadius,tmpRadius,X,Y );
    {$ELSE}
    AngleToPos( piDegree*XValue[ValueIndex],tmpRadius,tmpRadius,tmpX,tmpY );
    X:=tmpX;
    Y:=tmpY;
    {$ENDIF}
  end;
end;

Function TCustomPolarSeries.CalcYPos(ValueIndex:Longint):Integer;
var tmpX:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
begin
  CalcXYPos(ValueIndex,YRadius,tmpX,{$IFDEF D2C1}Longint(result){$ELSE}result{$ENDIF});
end;

Function TCustomPolarSeries.CalcXPos(ValueIndex:Longint):Integer;
var tmpY:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
begin
  CalcXYPos(ValueIndex,XRadius,{$IFDEF D2C1}Longint(result){$ELSE}result{$ENDIF},tmpY);
end;

procedure TCustomPolarSeries.LinePrepareCanvas(ValueIndex:Longint);
begin
  With ParentChart.Canvas do
  begin
    AssignVisiblePen(Self.FPen);
    if Self.FPen.Visible then
       if ValueIndex=-1 then Pen.Color:=SeriesColor
                        else Pen.Color:=ValueColor[ValueIndex];
    BackMode:=cbmTransparent;
  end;
end;

Procedure TCustomPolarSeries.FillTriangle(Const A,B:TPoint; Z:Integer);
var tmpStyle:TPenStyle;
begin
  With ParentChart.Canvas do
  begin
    tmpStyle:=Pen.Style;
    Pen.Style:=psClear;
    TriangleWithZ(A,B,Point(CircleXCenter,CircleYCenter),Z);
    Pen.Style:=tmpStyle;
  end;
end;

procedure TCustomPolarSeries.DrawValue(ValueIndex:Longint);
var X        : LongInt;
    Y        : LongInt;

  Procedure TryFillTriangle;
  begin
    if FBrush.Style<>bsClear then
    begin
      ParentChart.Canvas.Brush.Assign(FBrush);
      FillTriangle(Point(OldX,OldY),Point(X,Y),StartZ);
    end;
  end;

begin
  X:=CalcXPos(ValueIndex);
  Y:=CalcYPos(ValueIndex);
  LinePrepareCanvas(ValueIndex);
  With ParentChart.Canvas do
  if ValueIndex=FirstValueIndex then MoveTo3D(X,Y,StartZ)  { <-- first point }
  else
  begin
    if (X<>OldX) or (Y<>OldY) then
    begin
      TryFillTriangle;
      LineTo3D(X,Y,StartZ);
    end;
    if (ValueIndex=LastValueIndex) and FCloseCircle then
    begin
      Pen.Color:=ValueColor[0];
      OldX:=X;
      OldY:=Y;
      X:=CalcXPos(0);
      Y:=CalcYPos(0);
      TryFillTriangle;
      LineTo3D(X,Y,StartZ);
      X:=OldX;
      Y:=OldY;
    end;
  end;
  OldX:=X;
  OldY:=Y;
end;

Procedure TCustomPolarSeries.DrawAllValues;
var t        : LongInt;
    tmpColor : TColor;
begin
  inherited DrawAllValues;
  With FPointer do
  if Visible then
    for t:=FirstValueIndex to LastValueIndex do
    begin
      tmpColor:=ValueColor[t];
      PrepareCanvas(tmpColor);
      Draw(CalcXPos(t),CalcYPos(t),tmpColor,Style);
    end;
end;

Procedure TCustomPolarSeries.SetSeriesColor(AColor:TColor);
begin
  inherited SetSeriesColor(AColor);
  FPen.Color:=AColor;
end;

Procedure TCustomPolarSeries.SetPen(Value:TChartPen);
Begin
  FPen.Assign(Value);
end;

Procedure TCustomPolarSeries.SetPointer(Value:TSeriesPointer);
Begin
  FPointer.Assign(Value);
end;

Procedure TCustomPolarSeries.SetCircleLabels(Value:Boolean);
Begin
  SetBooleanProperty(FCircleLabels,Value);
end;

Procedure TCustomPolarSeries.SetCircleLabelsFont(Value:TFont);
begin
  FCircleLabelsFont.Assign(Value);
end;

Function TCustomPolarSeries.IsFontStored:Boolean;
begin
  result:=not IsDefaultFont(FCircleLabelsFont);
end;

Function TCustomPolarSeries.GetEditorClass:String;
Begin
  result:='TPolarSeriesEditor'; { <-- dont translate ! }
end;

{ The BottomAxis is used in Angle axis }
Function TCustomPolarSeries.GetAngleIncrement:Double;
Const MinAngle=10;
begin
  if ParentChart=nil then result:=MinAngle
  else
  begin
    result:=GetHorizAxis.Increment;
    if result=0 then result:=MinAngle;
  end;
end;

{ The BottomAxis is used in Angle axis }
Procedure TCustomPolarSeries.SetAngleIncrement(Const Value:Double);
begin
  if ParentChart<>nil then GetHorizAxis.Increment:=Value;
end;

{ The LeftAxis is used in Radius axis }
Function TCustomPolarSeries.GetRadiusIncrement:Double;
begin
  if ParentChart=nil then result:=0
                     else result:=GetVertAxis.Increment;
end;

{ The LeftAxis is used in Radius axis }
Procedure TCustomPolarSeries.SetRadiusIncrement(Const Value:Double);
begin
  if ParentChart<>nil then GetVertAxis.Increment:=Value;
end;

Function TCustomPolarSeries.Clicked(x,y:Integer):Longint;
var t:Longint;
begin
  if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(x,y,StartZ);
  result:=inherited Clicked(x,y);
  if (result=-1) and (FirstValueIndex>-1) and (LastValueIndex>-1) then
    if FPointer.Visible then
    for t:=FirstValueIndex to LastValueIndex do
        if (Abs(CalcXPos(t)-X)<FPointer.HorizSize) and
           (Abs(CalcYPos(t)-Y)<FPointer.VertSize) then
        begin
          result:=t;
          break;
        end;
end;

Procedure TCustomPolarSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
var tmpColor : TColor;
begin
  if FPen.Visible then
  begin
    LinePrepareCanvas(ValueIndex);
    With Rect do ParentChart.Canvas.DoHorizLine(Left,Right,(Top+Bottom) div 2);
  end;
  if FPointer.Visible then
  begin
    if ValueIndex=-1 then tmpColor:=SeriesColor
                     else tmpColor:=ValueColor[ValueIndex];
    FPointer.DrawLegendShape(tmpColor,Rect,FPen.Visible);
  end
  else
  if not FPen.Visible then
     inherited DrawLegendShape(ValueIndex,Rect)
end;

{ Used to draw inside circles (grid) (or radar grid lines) }
{ Can be used also to custom draw circles at specific values }
Procedure TCustomPolarSeries.DrawRing(Const Value:Double; Z:Integer);
Var tmp  : Double;
begin
  with GetVertAxis do
  begin
    tmp:=Maximum-Minimum;
    if tmp<>0 then
    begin
      tmp :=(Value-Minimum)/tmp;
      DrawPolarCircle(Round(tmp*XRadius),Round(tmp*YRadius),Z);
    end;
  end;
end;

Procedure TCustomPolarSeries.Assign(Source:TPersistent);
begin
  if Source is TCustomPolarSeries then
  With TCustomPolarSeries(Source) do
  begin
    Self.FCircleLabels  := FCircleLabels;
    Self.FCircleLabelsFont.Assign(FCircleLabelsFont);
    Self.FCircleLabelsRot:=FCircleLabelsRot;
    Self.FBrush.Assign(FBrush);
    Self.FCirclePen.Assign(FCirclePen);
    Self.FCloseCircle  := FCloseCircle;
    Self.FPen.Assign(FPen);
    Self.FPointer.Assign(FPointer);
  end;
  inherited Assign(Source);
end;

Function TCustomPolarSeries.GetRadiusValues:TChartValueList;
begin
  result:=YValues;
end;

Procedure TCustomPolarSeries.SetRadiusValues(Value:TChartValueList);
begin
  SetYValues(Value); { overrides the default YValues }
end;

Function TCustomPolarSeries.IsRadiusStored:Boolean;
begin
  With YValues do
  result:=(Name<>TeeMsg_RadiusValues) or DateTime or (Multiplier<>1) or
          (Order<>loNone) or (ValueSource<>'');
end;

Function TCustomPolarSeries.GetAngleValues:TChartValueList;
begin
  result:=XValues;
end;

Procedure TCustomPolarSeries.SetAngleValues(Value:TChartValueList);
begin
  SetXValues(Value); { overrides the default XValues }
end;

Procedure TCustomPolarSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  Circled:=True;
  With ParentChart do
  begin
    Chart3DPercent:=5;
    RightAxis.Labels:=False;
    TopAxis.Labels:=False;
    With View3DOptions do
    begin
      Orthogonal:=False;
      Elevation:=360;
      Zoom:=90;
    end;
  end;
end;

{ TPolarSeries }
Function TPolarSeries.AddPolar( Const Angle,Value:Double;
                                Const ALabel:String;
                                AColor:TColor):Longint;
begin
  result:=AddXY(Angle,Value,ALabel,AColor);
end;

Procedure TPolarSeries.FillSampleValues(NumValues:Longint);
var t   : Longint;
    tmp : Double;
Begin
  Clear;
  tmp:=360.0/NumValues;
  for t:=1 to NumValues do
      AddPolar( t*tmp,                      { <-- Angle }
                1+Random(ChartSamplesMax)  { <-- Value (Radius) }
                {$IFNDEF D5},'', clTeeColor{$ENDIF});
  RefreshSeries;
end;

{ TRadarSeries }
Procedure TRadarSeries.DoBeforeDrawChart;
var t   : Integer;
    tmp : Double;
begin
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
  inherited DoBeforeDrawChart;
  RotationAngle:=90;
  IMaxValuesCount:=ParentChart.GetMaxValuesCount;
  if IMaxValuesCount>0 then
  begin
    tmp:=360.0/IMaxValuesCount;
    AngleIncrement:=tmp;
    for t:=0 to Count-1 do SetXValue(t,t*tmp);
  end;
end;

Procedure TRadarSeries.FillSampleValues(NumValues:Longint);
Const RadarSampleStr:Array[0..4] of String=
         ( TeeMsg_PieSample1,TeeMsg_PieSample2,TeeMsg_PieSample3,
           TeeMsg_PieSample4,TeeMsg_PieSample5);
var t:Integer;
begin
  Randomize;
  Clear;
  for t:=0 to 4 do Add(Random(1000),RadarSampleStr[t]{$IFNDEF D5},clTeeColor{$ENDIF});
  RefreshSeries;
end;

Procedure TRadarSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(NumSampleValues);
end;

Function TRadarSeries.GetCircleLabel(Const AngleOrIndex:Double):String;
begin
  result:=GetXLabel(XValues.Locate(AngleOrIndex));
end;

Procedure TeePolarRadarExitProc; far;
begin
  UnRegisterTeeSeries([TPolarSeries,TRadarSeries]);
end;

initialization
  RegisterTeeSeries(TPolarSeries,TeeMsg_GalleryPolar,TeeMsg_GalleryExtended,2);
  RegisterTeeSeries(TRadarSeries,TeeMsg_GalleryRadar,TeeMsg_GalleryExtended,2);
{$IFDEF D1}
  AddExitProc(TeePolarRadarExitProc);
{$ELSE}
finalization
  TeePolarRadarExitProc;
{$ENDIF}
end.

{**********************************************}
{  TeeChart Pro 4.0                            }
{                                              }
{  TCustom3DSeries                             }
{     TSurfaceSeries                           }
{     TContourSeries                           }
{  Copyright (c) 1995-1998 by David Berneda    }
{**********************************************}
{$I teedefs.inc}
unit TeeSurfa;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics,
  Teengine, TeCanvas, Chart;

Const MaxAllowedCells={$IFDEF D1}127{$ELSE}500{$ENDIF}; { max 500 x 500 cells }
      TeeSurfaceTriangles:Boolean=False;  { <-- True to draw triangles }

type
  TChartSurfaceGetY=Function(Sender:TChartSeries; X,Z:Longint):Double of object;
  TChartSurfaceGetColor=Procedure( Sender:TChartSeries;
                                   ValueIndex:Longint;
                                   Var Color:TColor) of object;

  TCustom3DSeries=class(TChartSeries)
  private
    FTimesZOrder : Longint;
    FZValues     : TChartValueList;
    Procedure SetTimesZOrder(Value:Longint);
    Procedure SetZValues(Value:TChartValueList);
    Procedure SetZValue(Index:Longint; Const Value:Double);
    Function GetZValue(Index:Longint):Double;
    Function IsZValuesStored:Boolean;
  protected
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure Assign(Source:TPersistent); override;
    Function AddXYZ( AX:Longint; Const AY:Double; AZ:Longint;
                     Const AXLabel:String{$IFDEF D5}=''{$ENDIF};
                     AColor:TColor{$IFDEF D5}=clTeeColor{$ENDIF}):Longint; virtual;
    Procedure CalcZOrder; override;
    Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
    Function MaxZValue:Double; override;
    Function MinZValue:Double; override;
    property ZValue[Index:Longint]:Double read GetZValue write SetZValue;
    { to be published }
    property TimesZOrder:Longint read FTimesZOrder write SetTimesZOrder default 3;
    property ZValues:TChartValueList read FZValues write SetZValues stored IsZValuesStored;
  end;

  TTeeCellsRow=Array[1..MaxAllowedCells] of Integer;
  PTeeCellsRow=^TTeeCellsRow;

  TCustom3DGridSeries=class(TCustom3DSeries)
  private
    FEndColor     : TColor;
    FGridIndex    : Array[1..MaxAllowedCells] of PTeeCellsRow;
    FNumXValues   : Longint;
    FNumZValues   : Longint;
    FPalette      : TList;
    FPaletteSteps : Longint;
    FStartColor   : TColor;
    FUseColorRange: Boolean;
    FUsePalette   : Boolean;

    FOnGetYValue  : TChartSurfaceGetY;
    FOnGetColor   : TChartSurfaceGetColor;

    { internal }
    IInGallery   : Boolean;
    IRangeRed    : Longint;
    IEndRed      : Longint;
    IRangeGreen  : Longint;
    IEndGreen    : Longint;
    IRangeBlue   : Longint;
    IEndBlue     : Longint;
    Procedure SetNumXValues(Value:Longint);
    Procedure SetNumZValues(Value:Longint);
    Function GetGridIndex(X,Z:Integer):Integer;
    Procedure InternalSetGridIndex(X,Z,Value:Integer);
    Procedure SetGridIndex(X,Z,Value:Integer);
    Procedure CalcColorRange;
    Procedure ClearGridIndex;
  protected
    Procedure SetPaletteSteps(Value:Longint);
    Procedure SetStartColor(Value:TColor);
    Procedure SetEndColor(Value:TColor);
    Procedure SetUsePalette(Value:Boolean);
    Procedure SetUseColorRange(Value:Boolean);
    Procedure AddValues(Source:TChartSeries); override;
    Function GetValueColor(ValueIndex:Longint):TColor; override;
    Function GetValueColorValue(Const AValue:Double):TColor;
    Procedure DoBeforeDrawChart; override;
  public
    RedFactor    : Double;
    GreenFactor  : Double;
    BlueFactor   : Double;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Function AddPalette(Const Value:Double; Color:TColor):Longint;
    Procedure Assign(Source:TPersistent); override;
    Function CanCreateValues:Boolean;
    Procedure Clear; override;
    Procedure ClearPalette;
    Procedure CreateDefaultPalette(NumSteps:Longint);
    Procedure CreateValues(NumX,NumZ:Longint); virtual;
    Function CountLegendItems:Integer; override;
    Procedure FillGridIndex;
    Procedure FillSampleValues(NumValues:Longint); override;
    Procedure GalleryChanged3D(Is3D:Boolean); override;
    Function GetSurfacePaletteColor(Const Y:Double):TColor;
    Function GetXZValue(X,Z:Longint):Double; virtual;
    Function LegendItemColor(LegendIndex:Longint):TColor; override;
    Function LegendString( LegendIndex:Integer;
                           LegendTextStyle:TLegendTextStyle ):String; override;
    Function NumSampleValues:Longint; override;
    Procedure ReCreateValues;

    property EndColor:TColor read FEndColor write SetEndColor default clWhite;
    property GridIndex[X,Z:Integer]:Integer read GetGridIndex write SetGridIndex;

    property NumXValues:Longint read FNumXValues write SetNumXValues default 10;
    property NumZValues:Longint read FNumZValues write SetNumZValues default 10;
    property Palette:TList read FPalette;
    property PaletteSteps:Longint read FPaletteSteps write SetPaletteSteps default 32;
    property StartColor:TColor read FStartColor write SetStartColor default clNavy;
    property UseColorRange:Boolean read FUseColorRange write SetUseColorRange default True;
    property UsePalette:Boolean read FUsePalette write SetUsePalette default False;
    { events }
    property OnGetYValue:TChartSurfaceGetY read FOnGetYValue write FOnGetYValue;
    property OnGetColor:TChartSurfaceGetColor read FOnGetColor write FOnGetColor;
  end;

type
  TSurfaceSeries=class(TCustom3DGridSeries)
  private
    { Private declarations }
    FWireFrame      : Boolean;
    FDotFrame       : Boolean;
    FPen            : TChartPen;
    FBrush          : TChartBrush;
    { internal }
    FSameBrush      : Boolean;
    Points          : TFourPoints;
    ValueIndex0     : Integer;
    ValueIndex1     : Integer;
    ValueIndex2     : Integer;
    ValueIndex3     : Integer;
    INextXCell      : Integer;
    INextZCell      : Integer;
    Procedure SetPen(Value:TChartPen);
    Procedure SetBrush(Value:TChartBrush);
  protected
    { Protected declarations }
    Function ExistFourGridIndex(X,Z:Integer):Boolean;
    Procedure SetDotFrame(Value:Boolean);
    Procedure SetWireFrame(Value:Boolean);
    Procedure DrawAllValues; override;
    Procedure DrawCell(x,z:Longint); virtual;
    Function CalcPointPos(Index:Longint):TPoint;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function Clicked(x,y:Integer):Longint; override;
    Function GetEditorClass:String; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    { Published declarations }
    property Brush:TChartBrush read FBrush write SetBrush;
    property DotFrame:Boolean read FDotFrame write SetDotFrame default False;
    property EndColor;
    property NumXValues;
    property NumZValues;
    property Pen:TChartPen read FPen write SetPen;
    property PaletteSteps;
    property StartColor;
    property UseColorRange;
    property UsePalette;
    property WireFrame:Boolean read FWireFrame write SetWireFrame default False;
    property TimesZOrder;
    property XValues;
    property YValues;
    property ZValues;
  { events }
    property OnGetYValue;
    property OnGetColor;
  end;

Const
  MaxLevels={$IFDEF D1}100{$ELSE}250{$ENDIF};

type
  TContourSeries=class;
  
  TOnGetLevelEvent=procedure( Sender:TContourSeries; LevelIndex:Integer;
                              Var Value:Double; Var Color:TColor) of object;

  TContourSeries=class(TCustom3DGridSeries)
  private
    FNumLevels      : Integer;
    FLevels         : Array[0..MaxLevels] of Double;
    FLevelColors    : Array[0..MaxLevels] of TColor;
    FPen            : TChartPen;
    FYPosition      : Double;
    FYPositionLevel : Boolean;
    FOnGetLevel     : TOnGetLevelEvent;
    Procedure SetNumLevels(Value:Integer);
    Procedure SetPen(Value:TChartPen);
    Procedure SetYPosition(Const Value:Double);
    Procedure SetYPositionLevel(Value:Boolean);
  protected
    procedure DrawAllValues; override;
    Procedure DoBeforeDrawChart; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    procedure FillSampleValues(NumValues:Longint); override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
    Function GetEditorClass:String; override;
    Function CountLegendItems:Integer; override;
    Function LegendItemColor(LegendIndex:Longint):TColor; override;
    Function LegendString( LegendIndex:Integer;
                           LegendTextStyle:TLegendTextStyle ):String; override;
  published
    property ColorEachPoint default True;
    property EndColor;
    property NumLevels:Integer read FNumLevels write SetNumLevels default 10;
    property NumXValues;
    property NumZValues;
    property PaletteSteps;
    property Pen:TChartPen read FPen write SetPen;
    property StartColor;
    property TimesZOrder;
    property UseColorRange;
    property UsePalette;
    property XValues;
    property YPosition:Double read FYPosition write SetYPosition;
    property YPositionLevel:Boolean read FYPositionLevel write SetYPositionLevel default False;
    property YValues;
    property ZValues;
  { events }
    property OnGetYValue;
    property OnGetLevel:TOnGetLevelEvent read FOnGetLevel write FOnGetLevel;
  end;

implementation

Uses TeeProcs,TeeProCo,TeeConst;

{ TCustom3DSeries }
Constructor TCustom3DSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  HasZValues:=True;
  CalcVisiblePoints:=False;
  FZValues :=TChartValueList.Create(Self,'Z'); { <-- dont translate ! }
  XValues.Order:=loNone;
  FTimesZOrder:=3;
end;

Procedure TCustom3DSeries.SetZValues(Value:TChartValueList);
Begin
  SetChartValueList(FZValues,Value); { standard method }
End;

Procedure TCustom3DSeries.CalcZOrder;
Begin
  inherited CalcZOrder;
  ParentChart.MaxZOrder:=FTimesZOrder;
end;

Function TCustom3DSeries.AddXYZ( AX:Longint; Const AY:Double; AZ:Longint;
                                Const AXLabel:String; AColor:TColor):Longint;
Begin
  result:=AddXY(AX,AY,AXLabel,AColor); { standard add X,Y }
  FZValues.TempValue:=AZ;
  AddValue(result);
end;

Function TCustom3DSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TCustom3DSeries;
end;

Procedure TCustom3DSeries.SetTimesZOrder(Value:Longint);
Begin
  SetLongintProperty(FTimesZOrder,Value);
End;

Function TCustom3DSeries.MaxZValue:Double;
begin
  result:=FZValues.MaxValue;
end;

Function TCustom3DSeries.MinZValue:Double;
begin
  result:=FZValues.MinValue;
end;

Procedure TCustom3DSeries.Assign(Source:TPersistent);
begin
  if Source is TCustom3DSeries then
  With TCustom3DSeries(Source) do
  begin
    Self.FZValues.Assign(FZValues);
    Self.FTimesZOrder  :=FTimesZOrder;
  end;
  inherited Assign(Source);
end;

Procedure TCustom3DSeries.SetZValue(Index:Longint; Const Value:Double);
Begin
  ZValues.Value[Index]:=Value;
End;

Function TCustom3DSeries.GetZValue(Index:Longint):Double;
Begin
  result:=ZValues.Value[Index];
End;

Function TCustom3DSeries.IsZValuesStored:Boolean;
begin
  With FZValues do
  result:=DateTime or (Name<>'Z') or (Multiplier<>1) or (Order<>loNone) or
                   (ValueSource<>'');
end;

{ TCustom3DGridSeries }
Constructor TCustom3DGridSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FNumXValues:=10;
  FNumZValues:=10;
  Clear;
  FUseColorRange:=True;
  FPalette:=TList.Create;
  FPaletteSteps:=32;
  FStartColor:=clNavy;
  FEndColor:=clWhite;
  { Palette Modifiers }
  RedFactor:=2.0;
  GreenFactor:=1;
  BlueFactor:=1;
  CalcColorRange;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
End;

Destructor TCustom3DGridSeries.Destroy;
begin
  ClearPalette;
  FPalette.Free;
  ClearGridIndex;
  inherited Destroy;
end;

Procedure TCustom3DGridSeries.ClearGridIndex;
var t:Integer;
begin
  for t:=1 to MaxAllowedCells do
  if Assigned(FGridIndex[t]) then
  begin
    Dispose(FGridIndex[t]);
    FGridIndex[t]:=nil;
  end;
end;

Procedure TCustom3DGridSeries.Clear;
begin
  inherited Clear;
  XValues.Order:=loNone;
  ClearGridIndex;
end;

Function TCustom3DGridSeries.GetGridIndex(X,Z:Integer):Integer;
begin
  if Assigned(FGridIndex[X]) then
     result:=FGridIndex[x]^[z]
  else
     result:=-1;
end;

Procedure TCustom3DGridSeries.SetGridIndex(X,Z,Value:Integer);
begin
  if (X>=1) and (X<=MaxAllowedCells) and
     (Z>=1) and (Z<=MaxAllowedCells) then InternalSetGridIndex(x,z,value);
end;

Procedure TCustom3DGridSeries.InternalSetGridIndex(X,Z,Value:Integer);
var t:Integer;
begin
  if not Assigned(FGridIndex[x]) then
  begin
    New(FGridIndex[x]);
    for t:=1 to MaxAllowedCells do FGridIndex[x]^[t]:=-1;
  end;
  FGridIndex[x]^[z]:=Value;
end;

Procedure TCustom3DGridSeries.FillGridIndex;
var t    : Longint;
    x    : Integer;
    z    : Integer;
    MinX : Double;
    MinZ : Double;
begin
  MinX:=XValues.MinValue;
  MinZ:=ZValues.MinValue;
  for t:=0 to Count-1 do
  begin
    x:=1+Round(XValues.Value[t]-MinX);
    z:=1+Round(ZValues.Value[t]-MinZ);
    InternalSetGridIndex(X,Z,t);
    if X>FNumXValues then FNumXValues:=X;
    if Z>FNumZValues then FNumZValues:=Z;
  end;
end;

Function TCustom3DGridSeries.GetXZValue(X,Z:Longint):Double;
Begin
  if Assigned(FOnGetYValue) then result:=FOnGetYValue(Self,X,Z)
  else  { default sample random surface value formula }
  if (csDesigning in ComponentState) or
     (IInGallery) then
     result:=0.5*sqr(Cos(x/(FNumXValues*0.2)))+
                 sqr(Cos(z/(FNumZValues*0.2)))-
                 cos(z/(FNumZValues*0.5))
  else
     result:=0;
end;

Function TCustom3DGridSeries.NumSampleValues:Longint;
begin
  result:=15; { 15x15 default }
end;

Procedure TCustom3DGridSeries.ReCreateValues;
Begin
  CreateValues(FNumXValues,FNumZValues);
end;

Procedure TCustom3DGridSeries.SetNumXValues(Value:Longint);
Begin
  if Value<>FNumXValues then
  begin
    FNumXValues:=Value;
    ReCreateValues;
  end;
End;

Procedure TCustom3DGridSeries.SetNumZValues(Value:Longint);
Begin
  if Value<>FNumZValues then
  begin
    FNumZValues:=Value;
    ReCreateValues;
  end;
End;

Procedure TCustom3DGridSeries.AddValues(Source:TChartSeries);
Begin
  if Source is TCustom3DGridSeries then
  With TCustom3DGridSeries(Source) do
  begin
    Self.FNumXValues:=FNumXValues;
    Self.FNumZValues:=FNumZValues;
  end;
  inherited AddValues(Source);
  FillGridIndex;
  Repaint;
end;

Procedure TCustom3DGridSeries.Assign(Source:TPersistent);
begin
  if Source is TCustom3DGridSeries then
  With TCustom3DGridSeries(Source) do
  begin
    Self.FNumXValues   :=FNumXValues;
    Self.FNumZValues   :=FNumZValues;
    Self.FUsePalette   :=FUsePalette;
    Self.FUseColorRange:=FUseColorRange;
    Self.FStartColor   :=FStartColor;
    Self.FEndColor     :=FEndColor;
    Self.FPaletteSteps :=FPaletteSteps;
  end;
  inherited Assign(Source);
end;

Function TCustom3DGridSeries.CanCreateValues:Boolean;
begin
  result:= Assigned(FOnGetYValue) or (csDesigning in ComponentState)
           or IInGallery;
end;

Procedure TCustom3DGridSeries.CreateValues(NumX,NumZ:Longint);
var x           : Longint;
    z           : Longint;
    OldCapacity : Longint;
Begin
  if CanCreateValues then
  begin
    FNumXValues:=NumX;
    FNumZValues:=NumZ;
    OldCapacity:=TeeDefaultCapacity;
    TeeDefaultCapacity:=NumX*NumZ;
    try
      Clear;
      for x:=1 to NumX do
          for z:=1 to NumZ do AddXYZ(X,GetXZValue(X,Z),Z {$IFNDEF D5},'',clTeeColor{$ENDIF});
      RefreshSeries;
    finally
      TeeDefaultCapacity:=OldCapacity;
    end;
    CreateDefaultPalette(FPaletteSteps);
  end;
End;

Procedure TCustom3DGridSeries.FillSampleValues(NumValues:Longint);
var OldGallery : Boolean;
Begin
  OldGallery:=IInGallery;
  IInGallery:=True;
  try
    if NumValues>0 then
    begin
      FNumXValues:=MinLong(MaxAllowedCells,NumValues);
      FNumZValues:=FNumXValues;
    end;
    CreateValues(FNumXValues,FNumZValues);
  finally
    IInGallery:=OldGallery;
  end;
End;

Procedure TCustom3DGridSeries.CalcColorRange;
Begin
  IEndRed    :=GetRValue(FEndColor);
  IEndGreen  :=GetGValue(FEndColor);
  IEndBlue   :=GetBValue(FEndColor);
  IRangeRed  :=Longint(GetRValue(FStartColor))-IEndRed;
  IRangeGreen:=Longint(GetGValue(FStartColor))-IEndGreen;
  IRangeBlue :=Longint(GetBValue(FStartColor))-IEndBlue;
end;

Procedure TCustom3DGridSeries.SetStartColor(Value:TColor);
Begin
  SetColorProperty(FStartColor,Value);
  CalcColorRange;
End;

Procedure TCustom3DGridSeries.SetEndColor(Value:TColor);
Begin
  SetColorProperty(FEndColor,Value);
  CalcColorRange;
End;

type PGridPalette=^TGridPalette;
     TGridPalette=Record
       UpToValue : Double;
       Color     : TColor;
     end;

Function TCustom3DGridSeries.AddPalette(Const Value:Double; Color:TColor):Longint;
var t:Longint;
    tmp:PGridPalette;
Begin
  New(tmp);
  tmp^.UpToValue:=Value;
  tmp^.Color:=Color;
  for t:=0 to FPalette.Count-1 do
  begin
    if Value<TGridPalette(FPalette[t]^).UpToValue then
    begin
      FPalette.Insert(t,tmp);
      result:=t;
      exit;
    end;
  end;
  result:=FPalette.Add(tmp);
End;

Procedure TCustom3DGridSeries.ClearPalette;
var t:Integer;
Begin
  for t:=0 to FPalette.Count-1 do Dispose(PGridPalette(FPalette[t]));
  FPalette.Clear;
end;

Procedure TCustom3DGridSeries.DoBeforeDrawChart;
begin
  if Count>0 then
  begin
    FillGridIndex;
    if FPalette.Count=0 then CreateDefaultPalette(FPaletteSteps);
  end;
end;

Function TCustom3DGridSeries.CountLegendItems:Integer;
begin
  result:=FPalette.Count;
end;

Function TCustom3DGridSeries.LegendString( LegendIndex:Integer;
                                           LegendTextStyle:TLegendTextStyle ):String;
begin
  With FPalette do
  if Count>LegendIndex then
     result:=FormatFloat(ValueFormat,TGridPalette(FPalette[Count-LegendIndex-1]^).UpToValue)
  else
     result:='';
end;

Function TCustom3DGridSeries.LegendItemColor(LegendIndex:Longint):TColor;
begin
  result:=GetValueColorValue(TGridPalette(FPalette[FPalette.Count-LegendIndex-1]^).UpToValue)
end;

Procedure TCustom3DGridSeries.CreateDefaultPalette(NumSteps:Longint);
Const Delta=127.0;
var t:Longint;
    tmp,Scale,ScaleValue:Double;
Begin
  ClearPalette;
  FPalette.Capacity:=NumSteps+1;
  Scale:=pi/NumSteps;
  ScaleValue:=(YValues.MaxValue-YValues.MinValue)/NumSteps;
  for t:=1 to NumSteps do
  begin
    tmp:=Scale*t;
    AddPalette(YValues.MinValue+ScaleValue*t,
                        RGB( Trunc(Delta * (Sin(tmp/RedFactor)+1)) ,
                             Trunc(Delta * (Sin(tmp/GreenFactor)+1)),
                             Trunc(Delta * (Cos(tmp/BlueFactor)+1))));
  end;
end;

Procedure TCustom3DGridSeries.SetUseColorRange(Value:Boolean);
Begin
  SetBooleanProperty(FUseColorRange,Value);
End;

Procedure TCustom3DGridSeries.SetUsePalette(Value:Boolean);
Begin
  SetBooleanProperty(FUsePalette,Value);
  if Value and (FPalette.Count=0) then CreateDefaultPalette(FPaletteSteps);
End;

Function TCustom3DGridSeries.GetSurfacePaletteColor(Const Y:Double):TColor;
Var t,tmpCount:Longint;
Begin
  With FPalette do
  begin
    tmpCount:=Count-1;
    for t:=0 to tmpCount do
    if TGridPalette(Items[t]^).UpToValue>Y then
    Begin
      if t>0 then result:=TGridPalette(Items[t]^).Color
             else result:=TGridPalette(First^).Color;
      exit;
    end;
    result:=TGridPalette(Last^).Color;
  end;
End;

Function TCustom3DGridSeries.GetValueColorValue(Const AValue:Double):TColor;
var Percent   : Double;
    tmpDeltaY : Double;
begin
  if FUseColorRange then
  With YValues do
  begin
    tmpDeltaY:=(MaxValue-MinValue);
    if tmpDeltaY=0 then result:=FEndColor
    else
    begin
      Percent:=(AValue-MinValue)/tmpDeltaY;
      result:=RGB( IEndRed  +Round(Percent*IRangeRed),
                   IEndGreen+Round(Percent*IRangeGreen),
                   IEndBlue +Round(Percent*IRangeBlue));
    end;
  end
  else
  if FUsePalette and (FPalette.Count>0) then
     result:=GetSurfacePaletteColor(AValue)
  else
     result:=SeriesColor;
end;

Function TCustom3DGridSeries.GetValueColor(ValueIndex:Longint):TColor;
Begin
  result:=inherited GetValueColor(ValueIndex);
  if result<>clNone then result:=GetValueColorValue(YValue[ValueIndex]);
  if Assigned(FOnGetColor) then FOnGetColor(Self,ValueIndex,result);
End;

Procedure TCustom3DGridSeries.GalleryChanged3D(Is3D:Boolean);
begin
  With ParentChart.View3DOptions do
  begin
    Zoom:=60;
    VertOffset:=-2;
    if Is3D then
    begin
      Rotation:=335;
      Elevation:=340;
      Perspective:=60;
    end
    else
    begin
      Rotation:=0;
      Elevation:=270;
      Perspective:=0;
    end;
  end;
end;

Procedure TCustom3DGridSeries.SetPaletteSteps(Value:Longint);
Begin
  FPaletteSteps:=Value;
  CreateDefaultPalette(FPaletteSteps);
End;

{ TSurfaceSeries }
Constructor TSurfaceSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  INextXCell:=-1;
  INextZCell:=-1;
  FPen:=CreateChartPen;
  FBrush:=TChartBrush.Create(CanvasChanged);
End;

Destructor TSurfaceSeries.Destroy;
Begin
  FBrush.Free;
  FPen.Free;
  inherited Destroy;
End;

Procedure TSurfaceSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  IInGallery:=True;
  UseColorRange:=False;
  if IsEnabled then Pen.Color:=clBlack
               else Pen.Color:=clGray;
  UsePalette:=IsEnabled;
  CreateValues(10,10);
end;

Function TSurfaceSeries.CalcPointPos(Index:Longint):TPoint;
begin
  result.x:=GetHorizAxis.CalcXPosValue(GetxValue(Index));
  result.y:=GetVertAxis.CalcYPosValue(GetyValue(Index));
end;

Function TSurfaceSeries.Clicked(x,y:Integer):Longint;
var XIndex : Integer;
    ZIndex : Integer;
begin
  result:=TeeNoPointClicked;
  if Count>0 then
  begin
    INextXCell:=-1;
    INextZCell:=-1;
    for XIndex:=2 to FNumXValues do
      for ZIndex:=2 to FNumZValues do { front to back... }
      begin
        if ExistFourGridIndex(XIndex,ZIndex) then
        if PointInPolygon(Point(x,y),Points) then
        begin
          result:=ValueIndex0;
          exit;
        end;
      end;
  end;
end;

Procedure TSurfaceSeries.SetPen(Value:TChartPen);
Begin
  FPen.Assign(Value);
End;

Procedure TSurfaceSeries.SetBrush(Value:TChartBrush);
Begin
  FBrush.Assign(Value);
End;

Procedure TSurfaceSeries.DrawAllValues;
var x,z:Longint;
Begin
  if Count>0 then
  begin
    With ParentChart.Canvas do
    begin
      Pen.Assign(FPen);
      if (not FPen.Visible) and
         (not FWireFrame)   and
         (not FDotFrame) then Pen.Style:=psClear;
      Brush.Assign(FBrush);
      Brush.Color:=SeriesColor;
      if FUseColorRange or FUsePalette then FSameBrush:=False;
      if FWireFrame or FDotFrame then
      Begin
        Brush.Style:=bsClear;
        FSameBrush:=True;
      end;
    end;
    if ParentChart.DepthAxis.Inverted then
    begin
      INextZCell:=1;
      if GetHorizAxis.Inverted then
      begin
        INextXCell:=1;
        for x:=FNumXValues-1 downto 1 do
            for z:=1 to FNumZValues-1 do DrawCell(x,z);
      end
      else
      begin
        INextXCell:=-1;
        for x:=2 to FNumXValues do
            for z:=1 to FNumZValues-1 do DrawCell(x,z);
      end;
    end
    else
    begin
      INextZCell:=-1;
      if GetHorizAxis.Inverted then
      begin
        INextXCell:=1;
        for x:=FNumXValues-1 downto 1 do
            for z:=FNumZValues downto 2 do DrawCell(x,z);
      end
      else
      begin
        INextXCell:=-1;
        for x:=2 to FNumXValues do
            for z:=FNumZValues downto 2 do DrawCell(x,z);
      end;
    end;
  end;
End;

Function TSurfaceSeries.ExistFourGridIndex(X,Z:Integer):Boolean;
begin
  result:=False;
  ValueIndex0:=GetGridIndex(x,Z);
  if ValueIndex0>-1 then
  begin
    ValueIndex1:=GetGridIndex(x+INextXCell,Z);
    if ValueIndex1>-1 then
    begin
      ValueIndex2:=GetGridIndex(x+INextXCell,Z+INextZCell);
      if ValueIndex2>-1 then
      begin
        ValueIndex3:=GetGridIndex(x,Z+INextZCell);
        result:=ValueIndex3>-1;
        if result then
        begin
          Points[0]:=CalcPointPos(ValueIndex0);
          Points[1]:=CalcPointPos(ValueIndex1);
          Points[2]:=CalcPointPos(ValueIndex2);
          Points[3]:=CalcPointPos(ValueIndex3);
        end;
      end;
    end;
  end;
end;

Procedure TSurfaceSeries.DrawCell(X,Z:Longint);
var tmpColor : TColor;
    Z0       : Integer;
    Z1       : Integer;

  Procedure DrawTheCell;
  var P:Array[0..2] of TPoint;
  begin
    if FDotFrame then
    begin
      With ParentChart.Canvas do
      begin
        With Points[0] do Pixels3D[X,Y,Z0]:=GetValueColor(ValueIndex0);
        With Points[1] do Pixels3D[X,Y,Z0]:=GetValueColor(ValueIndex1);
        With Points[2] do Pixels3D[X,Y,Z1]:=GetValueColor(ValueIndex2);
        With Points[3] do Pixels3D[X,Y,Z1]:=GetValueColor(ValueIndex3);
      end;
    end
    else
    With ParentChart.Canvas do
    if TeeSurfaceTriangles then
    begin
      P[0]:=Calculate3DPosition(Points[0].X,Points[0].y,Z0);
      P[1]:=Calculate3DPosition(Points[1].X,Points[1].y,Z0);
      P[2]:=Calculate3DPosition(Points[3].X,Points[3].y,Z1);
      Polygon(P);
      P[0]:=Calculate3DPosition(Points[2].X,Points[2].y,Z1);
      Polygon(P);
    end
    else PlaneFour3D(Points,Z0,Z1);
  end;

Begin
  if ExistFourGridIndex(X,Z) then
  begin
    With ParentChart.DepthAxis do
    begin
      Z:=Z+Round(ZValues.MinValue)-1;
      Z0:=CalcYPosValue(Z);
      Z1:=CalcYPosValue(Z+INextZCell);
    end;

    if FSameBrush then DrawTheCell
    else
    begin
      tmpColor:=GetValueColor(ValueIndex0);
      if tmpColor<>clNone then
      begin
        ParentChart.Canvas.Brush.Color:=tmpColor;
        DrawTheCell;
      end;
    end;
  end;
end;

Procedure TSurfaceSeries.SetWireFrame(Value:Boolean);
Begin
  if Value then FPen.Visible:=True;
  SetBooleanProperty(FWireFrame,Value);
  if FWireFrame then FDotFrame:=False;
End;

Procedure TSurfaceSeries.SetDotFrame(Value:Boolean);
Begin
  if Value then FPen.Visible:=True;
  SetBooleanProperty(FDotFrame,Value);
  if FDotFrame then FWireFrame:=False;
End;

Function TSurfaceSeries.GetEditorClass:String;
Begin
  result:='TSurfaceSeriesEditor'; { <-- dont translate ! }
end;

Procedure TSurfaceSeries.Assign(Source:TPersistent);
begin
  if Source is TSurfaceSeries then
  With TSurfaceSeries(Source) do
  begin
    Self.FPen.Assign(FPen);
    Self.FBrush.Assign(FBrush);
    Self.FWireFrame:= FWireFrame;
    Self.FDotFrame := FDotFrame;
  end;
  inherited Assign(Source);
end;

{ TContourSeries }
Constructor TContourSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FPen:=CreateChartPen;
  ColorEachPoint:=True;
  FNumLevels:=10;
  FYPositionLevel:=False;
  FYPosition:=0;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
End;

Destructor TContourSeries.Destroy;
begin
  FPen.Free;
  inherited Destroy;
end;

Procedure TContourSeries.Assign(Source:TPersistent);
begin
  if Source is TContourSeries then
  With TContourSeries(Source) do
  begin
    Self.FPen.Assign(FPen);
    Self.FNumLevels:=FNumLevels;
    Self.FYPosition:=FYPosition;
    Self.FYPositionLevel:=FYPositionLevel;
  end;
  inherited Assign(Source);
end;

Procedure TContourSeries.SetNumLevels(Value:Integer);
begin
  SetIntegerProperty(FNumLevels,Value);
end;

Procedure TContourSeries.SetYPosition(Const Value:Double);
begin
  SetDoubleProperty(FYPosition,Value);
end;

Procedure TContourSeries.SetYPositionLevel(Value:Boolean);
begin
  SetBooleanProperty(FYPositionLevel,Value);
end;

procedure TContourSeries.DrawAllValues;
Var x1    : Integer;
    x2    : Integer;
    z1    : Integer;
    z2    : Integer;
    DifY  : Array[0..4] of Double;
    CellX : Array[0..4] of Integer;
    CellZ : Array[0..4] of Integer;
    m1    : Integer;
    m3    : Integer;

  Procedure CalcLinePoints(Side:Integer);

    Procedure PointSect1(p1,p2:Integer);
    Var tmp:Double;
    begin
      tmp:=DifY[p2]-DifY[p1];
      if tmp<>0 then
      begin
        x1:=Round( 1.0*(DifY[p2]*CellX[p1]-DifY[p1]*CellX[p2])/tmp );
        z1:=Round( 1.0*(DifY[p2]*CellZ[p1]-DifY[p1]*CellZ[p2])/tmp );
      end;
    end;

    Procedure PointSect2(p1,p2:Integer);
    Var tmp:Double;
    begin
      tmp:=DifY[p2]-DifY[p1];
      if tmp<>0 then
      begin
        x2:=Round( 1.0*(DifY[p2]*CellX[p1]-DifY[p1]*CellX[p2])/tmp );
        z2:=Round( 1.0*(DifY[p2]*CellZ[p1]-DifY[p1]*CellZ[p2])/tmp );
      end;
    end;

  begin
    case Side of
     1,4,6: begin x1:=CellX[m1]; z1:=CellZ[m1]; end;
       2,5: begin x1:=CellX[0];  z1:=CellZ[0]; end;
    end;
    case Side of
     1: begin x2:=CellX[0];  z2:=CellZ[0]; end;
     2: begin x2:=CellX[m3]; z2:=CellZ[m3]; end;
     3: begin
          x1:=CellX[m3]; z1:=CellZ[m3];
          x2:=CellX[m1]; z2:=CellZ[m1];
        end;
     4: PointSect2(0,m3);
     5: PointSect2(m3,m1);
     6: PointSect2(m1,0);
     7: begin PointSect1(m1,0); PointSect2(0,m3); end;
     8: begin PointSect1(0,m3); PointSect2(m3,m1); end;
     9: begin PointSect1(m3,m1); PointSect2(m1,0); end;
    End;
  end;

var x,y       : Integer;
    tmpYPosition:Integer;
    tmp1      : Double;
    tmp2      : Double;
    tmp3      : Double;
    tmp4      : Double;
    tmpIndex1 : Longint;
    tmpIndex2 : Longint;
    tmpIndex3 : Longint;
    tmpIndex4 : Longint;

  Procedure DrawLevel(TheLevel:Integer);
  Const Sides:Array[-1..1,-1..1,-1..1] of Integer=
                   ( ( (0,0,8),(0,2,5),(7,6,9) ),
                     ( (0,3,4),(1,3,1),(4,3,0) ),
                     ( (9,6,7),(5,2,0),(8,0,0) )  );
  var Corner      : Integer;
      SignHeights : Array[0..4] of Integer;
      Side        : Integer;
  Begin
    { corners dif Y }
    DifY[1]:=tmp1-FLevels[TheLevel];
    DifY[2]:=tmp3-FLevels[TheLevel];
    DifY[3]:=tmp4-FLevels[TheLevel];
    DifY[4]:=tmp2-FLevels[TheLevel];
    { center point dif Y }
    DifY[0] :=0.25*( DifY[1]+DifY[2]+DifY[3]+DifY[4] );

    With ParentChart.DepthAxis do
    begin
      CellZ[1]:=CalcXPosValue(ZValues[tmpIndex1]);
      CellZ[3]:=CalcXPosValue(ZValues[tmpIndex2]);
    end;
    CellZ[2]:=CellZ[1];
    CellZ[4]:=CellZ[3];

    CellX[1] := CalcXPos(tmpIndex1);
    CellX[2] := CalcXPos(tmpIndex3);
    CellX[3] := CellX[2];
    CellX[4] := CellX[1];

    for Corner:=0 to 4 do
    Begin
      if DifY[Corner]>0 then SignHeights[Corner]:= 1 else
      if DifY[Corner]<0 then SignHeights[Corner]:=-1 else
                             SignHeights[Corner]:=0;
    End;

    { mid X pos }
    CellX[0]:=( CellX[1] + CellX[2] ) div 2;
    { mid Z pos }
    CellZ[0]:=( CellZ[1] + CellZ[3] ) div 2;

    for Corner:=1 to 4 do
    Begin
      m1:=Corner;
      if Corner=4 then m3:=1 else m3:=Succ(Corner);
      Side:=Sides[SignHeights[m1],SignHeights[0],SignHeights[m3]];
      if Side<>0 then
      Begin
        CalcLinePoints(Side);
        With ParentChart,Canvas do
        begin
          if FYPositionLevel then
             tmpYPosition:=GetVertAxis.CalcYPosValue(FLevels[TheLevel]);
          MoveTo3D(x1,tmpYPosition,z1);
          LineTo3D(x2,tmpYPosition,z2);
        end;
      End;
    End;
  end;

  Function ExistFourGridIndex:Boolean;
  begin
    result:=False;
    tmpIndex1:=GetGridIndex(x,y);
    if tmpIndex1>-1 then
    begin
      tmpIndex2:=GetGridIndex(x,y+1);
      if tmpIndex2>-1 then
      begin
        tmpIndex3:=GetGridIndex(x+1,y);
        if tmpIndex3>-1 then
        begin
          tmpIndex4:=GetGridIndex(x+1,y+1);
          result:=tmpIndex4>-1;
        end;
      end;
    end;
  end;

var DiffMin  : Double;
    DiffMax  : Double;
    TheLevel : Integer;
begin
  if Count>0 then
  begin
    ParentChart.Canvas.Pen.Assign(FPen);
    tmpYPosition:=GetVertAxis.CalcYPosValue(FYPosition);
    for y:=1 to FNumZValues-1 do
      for x:=1 to FNumXValues-1 do
        if ExistFourGridIndex then
        begin
          With YValues do
          begin
            tmp1:=Value[tmpIndex1];
            tmp2:=Value[tmpIndex2];
            tmp3:=Value[tmpIndex3];
            tmp4:=Value[tmpIndex4];
          end;

          DiffMin  := MinDouble(MinDouble(tmp1,tmp2),MinDouble(tmp3,tmp4));
          DiffMax  := MaxDouble(MaxDouble(tmp1,tmp2),MaxDouble(tmp3,tmp4));
          if (DiffMax>=FLevels[0]) and (DiffMin<=FLevels[NumLevels-1]) then
          for TheLevel:=0 to NumLevels-1 do
          if (FLevels[TheLevel]>=DiffMin) and (FLevels[TheLevel]<=DiffMax) then
          begin
            ParentChart.Canvas.Pen.Color:=FLevelColors[TheLevel];
            DrawLevel(TheLevel);
          end;
        end;
  end;
end;

procedure TContourSeries.FillSampleValues(NumValues:Longint);
begin
  inherited FillSampleValues(NumValues);
  FYPosition:=0.5*(YValues.MaxValue+YValues.MinValue); { mid vertical pos }
end;

Procedure TContourSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  if not IsEnabled then SeriesColor:=clGray;
  UseColorRange:=False;
  UsePalette:=IsEnabled;
end;

Function TContourSeries.GetEditorClass:String;
Begin
  result:='TContourSeriesEditor'; { <-- dont translate ! }
end;

Procedure TContourSeries.SetPen(Value:TChartPen);
Begin
  FPen.Assign(Value);
End;

Function TContourSeries.CountLegendItems:Integer;
begin
  result:=FNumLevels;
end;

Function TContourSeries.LegendItemColor(LegendIndex:Longint):TColor;
begin
       { inverted legend }
  result:=FLevelColors[FNumLevels-LegendIndex-1];
end;

Function TContourSeries.LegendString( LegendIndex:Integer;
                                      LegendTextStyle:TLegendTextStyle ):String;
begin
      { inverted legend }
  result:=FormatFloat(ValueFormat,FLevels[FNumLevels-LegendIndex-1]);
end;

Procedure TContourSeries.DoBeforeDrawChart; { calc Level values and colors }
Var t   : Integer;
    tmp : Double;
begin
  inherited DoBeforeDrawChart;
  tmp:=(YValues.MaxValue-YValues.MinValue)/FNumLevels;
  for t:=0 to FNumLevels-1 do
  begin
    FLevels[t]:=YValues.MinValue+1.0*tmp*t;
    if ColorEachPoint then
       FLevelColors[t]:=GetDefaultColor(t) else
    if UsePalette or UseColorRange then
       FLevelColors[t]:=GetValueColorValue(FLevels[t])
    else
       FLevelColors[t]:=SeriesColor;
    if Assigned(FOnGetLevel) then FOnGetLevel(Self,t,FLevels[t],FLevelColors[t]);
  end;
end;

Procedure Tee3DSeriesExitProc; far;
begin
  UnRegisterTeeSeries([TSurfaceSeries,TContourSeries]);
end;

initialization
  RegisterTeeSeries( TSurfaceSeries, TeeMsg_GallerySurface,
                     TeeMsg_GalleryExtended,1);
  RegisterTeeSeries( TContourSeries, TeeMsg_GalleryContour,
                     TeeMsg_GalleryExtended,1);
{$IFDEF D1}
  AddExitProc(Tee3DSeriesExitProc);
{$ENDIF}
{$IFNDEF D1}
finalization
  Tee3DSeriesExitProc;
{$ENDIF}
end.

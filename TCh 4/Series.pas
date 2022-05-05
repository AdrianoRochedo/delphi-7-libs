{*********************************************}
{  TeeChart Standard Series Types             }
{  Copyright (c) 1995-1998 by David Berneda   }
{  All Rights Reserved                        }
{                                             }
{   TCustomSeries                             }
{     TLineSeries                             }
{     TAreaSeries                             }
{     TPointSeries                            }
{   TCustomBarSeries                          }
{     TBarSeries                              }
{     THorizBarSeries                         }
{   TCircledSeries                            }
{     TPieSeries                              }
{   TFastLineSeries                           }
{*********************************************}
{$I teedefs.inc}
unit Series;

interface

Uses Graphics,WinTypes,WinProcs,TeEngine,Chart,SysUtils,Classes,TeCanvas,
     TeeProcs;

Const TwoPi    : Double = 2.0*pi;
      HalfPi   : Double = 0.5*pi;
      PiDegree : Double = pi/180.0;

Type
  TSeriesPointerStyle=( psRectangle,psCircle,psTriangle,psDownTriangle,
                        psCross,psDiagCross,psStar,psDiamond,psSmallDot );

  TOnGetPointerStyle=Function( Sender:TChartSeries;
                               ValueIndex:Longint):TSeriesPointerStyle of object;

  TSeriesPointer=class(TPersistent)
  private
    FDark3D    : Boolean;
    FDraw3D    : Boolean;
    FBrush     : TChartBrush;
    FHorizSize : Integer;
    FInflateMargins:Boolean;
    FOwner     : TChartSeries;
    FPen       : TChartPen;
    FStyle     : TSeriesPointerStyle;
    FVertSize  : Integer;
    FVisible   : Boolean;
    Procedure CheckPointerSize(Value:Integer);
    Procedure SetDark3D(Value:Boolean);
    Procedure SetDraw3D(Value:Boolean);
    Procedure SetBrush(Value:TChartBrush);
    Procedure SetHorizSize(Value:Integer);
    Procedure SetInflateMargins(Value:Boolean);
    Procedure SetPen(Value:TChartPen);
    Procedure SetStyle(Value:TSeriesPointerStyle);
    Procedure SetVertSize(Value:Integer);
    Procedure SetVisible(Value:Boolean);
  protected
    Procedure SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
    Procedure SetIntegerProperty(Var Variable:Integer; Value:Integer);
  public
    AllowChangeSize:Boolean;

    Constructor Create(AOwner:TChartSeries);
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
    Procedure Change3D(Value:Boolean);
    Procedure ChangeHorizSize(NewSize:Integer);
    Procedure ChangeStyle(NewStyle:TSeriesPointerStyle);
    Procedure ChangeVertSize(NewSize:Integer);
    Procedure Draw(px,py:Integer; ColorValue:TColor; AStyle:TSeriesPointerStyle);
    Procedure DrawLegendShape(AColor:TColor; Const Rect:TRect; DrawPen:Boolean);
    Procedure DrawPointer(Is3D:Boolean; px,py,tmpHoriz,tmpVert:Integer; ColorValue:TColor; AStyle:TSeriesPointerStyle);
    Procedure PrepareCanvas(ColorValue:TColor);

    property ParentSeries:TChartSeries read FOwner;
  published
    property Brush:TChartBrush read FBrush write SetBrush;
    property Dark3D:Boolean read FDark3D write SetDark3D default True;
    Property Draw3D:Boolean read FDraw3D write SetDraw3D default True;
    Property HorizSize:Integer read FHorizSize write SetHorizSize default 4;
    property InflateMargins:Boolean read FInflateMargins write SetInflateMargins;
    property Pen:TChartPen read FPen write SetPen;
    Property Style:TSeriesPointerStyle read FStyle write SetStyle;
    Property VertSize:Integer read FVertSize write SetVertSize default 4;
    Property Visible:Boolean read FVisible write SetVisible;
  end;

  TCustomSeries=class;

  TSeriesClickPointerEvent=Procedure( Sender:TCustomSeries;
                                      ValueIndex:Longint;
                                      X, Y: Integer) of object;

  TCustomSeries=Class(TChartSeries)
  private
    FAreaBrush          : TBrushStyle;
    FAreaColor          : TColor;
    FAreaLinesPen       : TChartPen;
    FClickableLine      : Boolean;
    FDark3D             : Boolean;
    FDrawArea           : Boolean;
    FDrawLine           : Boolean;
    FInvertedStairs     : Boolean;
    FLineBrush          : TBrushStyle;
    FLineHeight         : Integer;
    FLinePen            : TChartPen;
    FPointer            : TSeriesPointer;
    FStairs             : Boolean;

    { events }
    FOnClickPointer     : TSeriesClickPointerEvent;
    FOnGetPointerStyle  : TOnGetPointerStyle;

    { internal }
    BottomPos      : Longint;
    OldBottomPos   : Longint;
    OldX           : Longint;
    OldY           : Longint;
    OldColor       : TColor;
    tmpDark3DRatio : Double;
    Procedure SetAreaBrush(Value:TBrushStyle);
    Procedure SetAreaColor(Value:TColor);
    Procedure SetAreaLinesPen(Value:TChartPen);
    Procedure SetBrushProperty(Var ABrush:TBrushStyle; Value:TBrushStyle);
    Procedure SetDark3D(Value:Boolean);
    Procedure SetDrawArea(Value:Boolean);
    Procedure SetInvertedStairs(Value:Boolean);
    Procedure SetLineBrush(Value:TBrushStyle);
    Procedure SetLineHeight(Value:Integer);
    Procedure SetLinePen(Value:TChartPen);
    Procedure SetPointer(Value:TSeriesPointer);
    Procedure SetStairs(Value:Boolean);
  protected
    Function ClickedPointer( ValueIndex,tmpX,tmpY:Longint;
                             x,y:Longint):Boolean; virtual;
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
    Procedure DrawPointer(AX,AY:Longint; AColor:TColor; ValueIndex:Longint); virtual;
    procedure DrawValue(ValueIndex:Longint); override;
    Function GetAreaBrushColor(AColor:TColor):TColor;
    procedure LinePrepareCanvas(tmpCanvas:TCanvas3D; tmpColor:TColor);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); override;
    Function Clicked(x,y:Integer):Longint; override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Function GetEditorClass:String; override;
    Function GetOriginPos(ValueIndex:Longint):Longint; virtual;

    property AreaBrush:TBrushStyle read FAreaBrush write SetAreaBrush default bsSolid;
    property AreaColor:TColor read FAreaColor write SetAreaColor default clTeeColor;
    property AreaLinesPen:TChartPen read FAreaLinesPen write SetAreaLinesPen;
    property ClickableLine:Boolean read FClickableLine write FClickableLine;
    property Dark3D:Boolean read FDark3D write SetDark3D default True;
    property DrawArea:Boolean read FDrawArea write SetDrawArea default False;
    property InvertedStairs:Boolean read FInvertedStairs write SetInvertedStairs default False;
    property LineBrush:TBrushStyle read FLineBrush write SetLineBrush default bsSolid;
    property LineHeight:Integer read FLineHeight write SetLineHeight default 0;
    property LinePen:TChartPen read FLinePen write SetLinePen;
    property OnClickPointer:TSeriesClickPointerEvent read FOnClickPointer
                                                     write FOnClickPointer;
    property Pointer:TSeriesPointer read FPointer write SetPointer;
    property Stairs:Boolean read FStairs write SetStairs default False;
  published
    { events }
    property OnGetPointerStyle:TOnGetPointerStyle read FOnGetPointerStyle
                                                  write FOnGetPointerStyle;

  end;

  TLineSeries=Class(TCustomSeries)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure Assign(Source:TPersistent); override;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); override;
  published
    property Dark3D;
    property InvertedStairs;
    property LineBrush;
    property LineHeight;
    property LinePen;
    property Pointer;
    property Stairs;
    property XValues;
    property YValues;
  end;

  TPointSeries=Class(TCustomSeries)
  protected
    Procedure SetColorEachPoint(Value:Boolean); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure Assign(Source:TPersistent); override;
    Function GetEditorClass:String; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    property Pointer;
    property XValues;
    property YValues;
   { events }
    property OnClickPointer;
  end;

  TMultiArea=(maNone,maStacked,maStacked100);

  TAreaSeries=Class(TCustomSeries)
  private
    FMultiArea : TMultiArea;
    Procedure SetMultiArea(Value:TMultiArea);
    Procedure SetOtherMultiArea;
    Function InternalCalcStackedYPos(ValueIndex:Integer; Value:Double):Integer;
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure Assign(Source:TPersistent); override;
    Procedure CalcZOrder; override;
    Function CalcYPos(ValueIndex:Longint):Integer; override;
    Function GetEditorClass:String; override;
    Function GetOriginPos(ValueIndex:Longint):Longint; override;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); override;
    Function MaxYValue:Double; override;
    Function MinYValue:Double; override;
  published
    property AreaBrush;
    property AreaColor;
    property AreaLinesPen;
    property Dark3D;
    property DrawArea;
    property InvertedStairs;
    property LinePen;
    property MultiArea:TMultiArea read FMultiArea write SetMultiArea default maNone;
    property Stairs;
    property XValues;
    property YValues;
  end;

  BarException=class(Exception);

  TMultiBar=(mbNone,mbSide,mbStacked,mbStacked100{$IFDEF TEE5},mbSeries{$ENDIF});

  TCustomBarSeries=class;

  TBarStyle=( bsRectangle,bsPyramid,bsInvPyramid,
              bsCilinder,bsEllipse,bsArrow,bsRectGradient );

  TGetBarStyleEvent=Procedure( Sender:TCustomBarSeries; ValueIndex:Longint;
                               Var TheBarStyle:TBarStyle) of object;

  TCustomBarSeries=Class(TChartSeries)
  private
    FAutoBarSize     : Boolean;
    FAutoMarkPosition: Boolean;
    FBarBrush        : TChartBrush;
    FBarPen          : TChartPen;
    FBarStyle        : TBarStyle;
    FBarWidthPercent : Integer;
    FCustomBarSize   : Integer;
    FDark3D          : Boolean;
    FMultiBar        : TMultiBar;
    FOffsetPercent   : Integer;
    FSideMargins     : Boolean;
    FUseOrigin       : Boolean;
    FOrigin          : Double;
    { events }
    FOnGetBarStyle   : TGetBarStyleEvent;

    { internal }
    IBarSize   : Longint;
    FBarBounds : TRect;
    Procedure CalcBarWidth;
    Function InternalCalcMarkLength(ValueIndex:Longint):Longint; virtual; abstract;
    Procedure SetAutoBarSize(Value:Boolean);
    Procedure SetAutoMarkPosition(Value:Boolean);
    Procedure SetBarWidthPercent(Value:Integer);
    Procedure SetOffsetPercent(Value:Integer);
    Procedure SetBarStyle(Value:TBarStyle);
    Procedure SetDark3d(Value:Boolean);
    Procedure SetUseOrigin(Value:Boolean);
    Procedure SetSideMargins(Value:Boolean);
    Procedure SetBarPen(Value:TChartPen);
    Procedure SetBarBrush(Value:TChartBrush);
    Procedure SetOrigin(Const Value:Double);
    Procedure SetMultiBar(Value:TMultiBar);
    Procedure SetOtherMultiBar;

    Procedure AdjustGradientRectPen(Var R:TRect);
    Function BarOrderPos:Longint;
    Function BarSeriesCount:Longint;
    Function MaxMandatoryValue( Const Value:Double;
                                AList:TChartValueList):Double;
    Function MinMandatoryValue(Const Value:Double):Double;
    Procedure InternalApplyBarMargin(Var MarginA,MarginB:Integer);
  protected
    Function InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean; virtual;
    Function InternalGetOriginPos(ValueIndex:Longint; DefaultOrigin:Longint):Longint;
    Procedure SetCustomBarSize(Value:Integer);
  public
    NormalBarColor:TColor;
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    Function AddBar(Const AValue:Double; Const ALabel:String; AColor:TColor):Longint;
    Function ApplyBarOffset(Position:Longint):Longint;
    Procedure Assign(Source:TPersistent); override;
    Function BarMargin:Longint;
    Procedure BarRectangle(BarColor:TColor; ALeft,ATop,ARight,ABottom:Longint);
    Function CalcMarkLength(ValueIndex:Longint):Longint;
    Procedure CalcZOrder; override;
    Function Clicked(x,y:Integer):Longint; override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Function GetBarStyle(ValueIndex:Longint):TBarStyle;
    Function GetEditorClass:String; override;
    Function NumSampleValues:Longint; override;
    Function PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); override;
    Procedure SetPenBrushBar(BarColor:TColor);

    property BarBounds:TRect read FBarBounds;
  published
    property AutoBarSize:Boolean read FAutoBarSize write SetAutoBarSize default False;
    property AutoMarkPosition:Boolean read FAutoMarkPosition write SetAutoMarkPosition default True;
    property BarBrush:TChartBrush read FBarBrush write SetBarBrush;
    property BarPen:TChartPen read FBarPen write SetBarPen;
    property BarStyle:TBarStyle read FBarStyle write SetBarStyle
                                default bsRectangle;
    property BarWidthPercent:Integer read FBarWidthPercent
                                     write SetBarWidthPercent default 70;
    property Dark3D:Boolean read FDark3D write SetDark3D default True;
    property MultiBar:TMultiBar read FMultiBar write SetMultiBar default mbSide;
    property OffsetPercent:Integer read FOffsetPercent
                                   write SetOffsetPercent default 0;
    property SideMargins:Boolean read FSideMargins write SetSideMargins default True;
    property UseYOrigin:Boolean read FUseOrigin write SetUseOrigin default True;
    property YOrigin:Double read FOrigin write SetOrigin;

    { inherited published }
    property XValues;
    property YValues;
    { events }
    property OnGetBarStyle:TGetBarStyleEvent read FOnGetBarStyle write
                                             FOnGetBarStyle;
  end;

  TBarSeries=class(TCustomBarSeries)
  private
    Function InternalCalcMarkLength(ValueIndex:Longint):Longint; override;
  protected
    procedure DrawValue(ValueIndex:Longint); override;
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
  public
    Function InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean; override;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); override;
    Function CalcXPos(ValueIndex:Longint):Integer; override;
    Function CalcYPos(ValueIndex:Longint):Integer; override;
    Procedure DrawBar(BarIndex,StartPos,EndPos:Longint); virtual;
    Function DrawSeriesForward(ValueIndex:Longint):Boolean; override;
    Function DrawValuesForward:Boolean; override;
    Function GetOriginPos(ValueIndex:Longint):Longint;
    Function MaxYValue:Double; override;
    Function MinYValue:Double; override;
    property BarWidth:Longint read IBarSize;
  published
    property CustomBarWidth:Integer read FCustomBarSize
                                    write SetCustomBarSize default 0;
  end;

  THorizBarSeries=class(TCustomBarSeries)
  private
    Function InternalCalcMarkLength(ValueIndex:Longint):Longint; override;
  protected
    procedure DrawValue(ValueIndex:Longint); override;
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
  public
    Function InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean; override;
    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); override;
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); override;
    Function CalcXPos(ValueIndex:Longint):Integer; override;
    Function CalcYPos(ValueIndex:Longint):Integer; override;
    Procedure DrawBar(BarIndex,StartPos,EndPos:Longint); virtual;
    Function DrawSeriesForward(ValueIndex:Longint):Boolean; override;
    Function DrawValuesForward:Boolean; override;
    Procedure FillSampleValues(NumValues:Longint); override;
    Function GetOriginPos(ValueIndex:Longint):Longint;
    Function MandatoryValueList:TChartValueList; override;
    Function MaxXValue:Double; override;
    Function MinXValue:Double; override;
    property BarHeight:Longint read IBarSize;
  published
    property CustomBarHeight:Integer read FCustomBarSize
                                     write SetCustomBarSize default 0;
  end;

  TCircledSeries=class(TChartSeries)
  private
    FCircled         : Boolean;
    FRotationAngle   : Integer;
    FCustomXRadius   : Longint;
    FCustomYRadius   : Longint;
    FXRadius         : Longint;
    FYRadius         : Longint;
    FCircleBackColor : TColor;
    { internal }
    IRotDegree      : Double;
    FCircleWidth    : Longint;
    FCircleHeight   : Longint;
    FCircleXCenter  : Longint;
    FCircleYCenter  : Longint;
    FCircleRect     : TRect;
    procedure SetCircleBackColor(Value:TColor);
    procedure SetCustomXRadius(Value:Longint);
    procedure SetCustomYRadius(Value:Longint);
    Procedure SetCircled(Value:Boolean);
    procedure SetOtherCustomRadius(IsXRadius:Boolean; Value:Longint);
    Procedure SetRotationAngle(Value:Integer);
  protected
    Procedure AdjustCircleRect;
    Function CalcCircleBackColor:TColor;
    Procedure CalcRadius;
    procedure DoBeforeDrawValues; override;
    Procedure SetActive(Value:Boolean); override;
    Procedure SetParentChart(Value:TCustomAxisPanel); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure AngleToPos( Const Angle,AXRadius,AYRadius:Double;
                          Var X,Y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
    Procedure Assign(Source:TPersistent); override;
    Function AssociatedToAxis(Axis:TCustomChartAxis):Boolean; override;
    Function PointToAngle(x,y:Longint):Double;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); override;
    Procedure Rotate(Angle:Integer);
    Procedure SetParentProperties(EnableParentProps:Boolean); virtual;
    Function UseAxis:Boolean; override;
    { read only properties }
    property XRadius:Longint read FXRadius;
    property YRadius:Longint read FYRadius;
    property CircleXCenter:Longint read FCircleXCenter;
    property CircleYCenter:Longint read FCircleYCenter;
    property CircleWidth:Longint read FCircleWidth;
    property CircleHeight:Longint read FCircleHeight;
    property CircleRect:TRect read FCircleRect;
    property CircleBackColor:TColor read FCircleBackColor
                                    write SetCircleBackColor default clTeeColor;
    property RotationAngle:Integer read FRotationAngle write SetRotationAngle
                                   default 0;
  published
    property Circled:Boolean read FCircled write SetCircled default False;
    property CustomXRadius:Longint read FCustomXRadius write SetCustomXRadius default 0;
    property CustomYRadius:Longint read FCustomYRadius write SetCustomYRadius default 0;
  end;

  PieException=class(ChartException);

  TPieAngle=class
  public
    StartAngle : Double;
    MidAngle   : Double;
    EndAngle   : Double;
  end;

  TPieAngles=class(TList)
  private
    Function GetAngle(Index:Integer):TPieAngle;
  public
    property Angle[Index:Integer]:TPieAngle read GetAngle; {$IFNDEF D1}default;{$ENDIF}
  end;

  TExplodedSlices=class(TList)
  private
    Procedure SetValue(Index,Value:Integer);
    Function GetValue(Index:Integer):Integer;
  public
    OwnerSeries:TChartSeries;
    property Value[Index:Integer]:Integer read GetValue write SetValue; {$IFNDEF D1}default;{$ENDIF}
  end;

  TPieOtherStyle=(poNone,poBelowPercent,poBelowValue);

  TPieOtherSlice=class(TPersistent)
  private
    FStyle    : TPieOtherStyle;
    FText     : String;
    FValue    : Double;
    FOwner    : TChartSeries;
    procedure SetStyle(Value:TPieOtherStyle);
    procedure SetText(Const Value:String);
    procedure SetValue(Const Value:Double);
  public
    Constructor Create(AOwner:TChartSeries);
    Procedure Assign(Source:TPersistent); override;
  published
    property Style:TPieOtherStyle read FStyle write SetStyle default poNone;
    property Text:String read FText write SetText;
    property Value:Double read FValue write SetValue;
  end;

  TPieSeries=Class(TCircledSeries)
  private
    FDark3d        : Boolean;
    FExplodedSlice : TExplodedSlices; { <-- Exploded slice % storage }
    FExplodeBiggest: Integer;
    FPiePen        : TChartPen;
    FOtherSlice    : TPieOtherSlice;
    FUsePatterns   : Boolean;

    Procedure DisableRotation;
    Procedure FreePieAngles;
    { props }
    procedure SetUsePatterns(Value:Boolean);
    Procedure SetDark3d(Value:Boolean);
    Function GetPieValues:TChartValueList;
    Procedure SetPieValues(Value:TChartValueList);
    procedure SetExplodeBiggest(Value:Integer);
    procedure SetPiePen(Value:TChartPen);
    procedure SetOtherSlice(Value:TPieOtherSlice);
    Procedure CalcExplodeBiggest;
    Procedure CalcExplodedOffset( ValueIndex:Integer;
                                  Var OffsetX,OffsetY:Integer);
  protected
    FAngles    : TPieAngles;
    IniX,
    IniY,
    EndX,
    EndY:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
    IsExploded : Boolean;
    Procedure CalcAngles;
    Procedure CalcExplodedRadius(ValueIndex:Integer; Var AXRadius,AYRadius:Integer);
    procedure DoBeforeDrawChart; override;
    procedure DrawAllValues; override;
    Procedure DrawPie(ValueIndex:Integer); virtual;
    procedure DrawValue(ValueIndex:Longint); override;
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
    Procedure ClearLists; override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function AddPie(Const AValue:Double; Const ALabel:String; AColor:TColor):Longint;
    Procedure Assign(Source:TPersistent); override;
    Function BelongsToOtherSlice(ValueIndex:Integer):Boolean;
    Function CalcClickedPie(x,y:Integer):Longint;
    Function CalcXPos(ValueIndex:Longint):Integer; override;
    Function Clicked(x,y:Integer):Longint; override;
    Function CountLegendItems:Integer; override;
    Procedure FillSampleValues(NumValues:Longint); override;
    Procedure GalleryChanged3D(Is3D:Boolean); override;
    Function GetEditorClass:String; override;
    Function LegendToValueIndex(LegendIndex:Integer):Integer; override;
    Function MaxXValue:Double; override;
    Function MinXValue:Double; override;
    Function MaxYValue:Double; override;
    Function MinYValue:Double; override;
    Function NumSampleValues:Longint; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); override;
    procedure SwapValueIndex(a,b:Longint); override;

    property Angles:TPieAngles read FAngles;
    property ExplodedSlice:TExplodedSlices read FExplodedSlice;
  published
    property CircleBackColor;
    property ColorEachPoint default True;
    property Dark3D:Boolean read FDark3D write SetDark3D default True;
    property ExplodeBiggest:Integer read FExplodeBiggest write SetExplodeBiggest default 0;
    property OtherSlice:TPieOtherSlice read FOtherSlice write SetOtherSlice;
    property PiePen:TChartPen read FPiePen write SetPiePen;
    property PieValues:TChartValueList read GetPieValues write SetPieValues;
    property RotationAngle;
    property UsePatterns:Boolean read FUsePatterns write SetUsePatterns default False;
  end;

  TFastLineSeries=class(TChartSeries)
  private
    FAutoRepaint : Boolean;
    FLinePen     : TChartPen;
    { internal }
    OldX         : Longint;
    OldY         : Longint;
    Procedure SetLinePen(Value:TChartPen);
  protected
    procedure DrawValue(ValueIndex:Longint); override;
    procedure DrawAllValues; override;
    Procedure SetSeriesColor(AColor:TColor); override;
    procedure PrepareCanvas;
    Procedure DrawMark( ValueIndex:Longint; Const St:String;
                        APosition:TSeriesMarkPosition); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function Clicked(x,y:Integer):Longint; override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Function GetEditorClass:String; override;
    Procedure NotifyNewValue(Sender:TChartSeries; ValueIndex:Longint); override;
  published
    property AutoRepaint:Boolean read FAutoRepaint write FAutoRepaint default True;
    property LinePen:TChartPen read FLinePen write SetLinePen;
    property XValues;
    property YValues;
  end;

implementation

Uses TeeConst;

{ TSeriesPointer }
Constructor TSeriesPointer.Create(AOwner:TChartSeries);
Begin
  inherited Create;
  AllowChangeSize:=True;

  FOwner:=AOwner;
  FInflateMargins:=True;
  FVisible:=True;
  FHorizSize:=4;
  FVertSize:=4;
  FDark3D:=True;
  FDraw3D:=True;
  FStyle:=psRectangle;
  FPen:=FOwner.CreateChartPen;
  FBrush:=TChartBrush.Create(FOwner.CanvasChanged);
end;

Destructor TSeriesPointer.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

Procedure TSeriesPointer.ChangeStyle(NewStyle:TSeriesPointerStyle);
Begin
  FStyle:=NewStyle;
end;

Procedure TSeriesPointer.ChangeHorizSize(NewSize:Integer);
Begin
  FHorizSize:=NewSize;
End;

Procedure TSeriesPointer.ChangeVertSize(NewSize:Integer);
Begin
  FVertSize:=NewSize;
End;

Procedure TSeriesPointer.SetIntegerProperty(Var Variable:Integer; Value:Integer);
begin
  FOwner.SetIntegerProperty(Variable,Value);
end;

Procedure TSeriesPointer.SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
begin
  FOwner.SetBooleanProperty(Variable,Value);
end;

Procedure TSeriesPointer.CheckPointerSize(Value:Integer);
begin
  if Value<1 then Raise ChartException.Create(TeeMsg_CheckPointerSize)
end;

Procedure TSeriesPointer.SetHorizSize(Value:Integer);
Begin
  CheckPointerSize(Value);
  SetIntegerProperty(FHorizSize,Value);
end;

Procedure TSeriesPointer.SetInflateMargins(Value:Boolean);
begin
  SetBooleanProperty(FInflateMargins,Value);
end;

Procedure TSeriesPointer.SetVertSize(Value:Integer);
Begin
  CheckPointerSize(Value);
  SetIntegerProperty(FVertSize,Value);
end;

Procedure TSeriesPointer.SetPen(Value:TChartPen);
Begin
  FPen.Assign(Value);
end;

Procedure TSeriesPointer.SetBrush(Value:TChartBrush);
Begin
  FBrush.Assign(Value);
end;

Procedure TSeriesPointer.SetDark3D(Value:Boolean);
Begin
  SetBooleanProperty(FDark3D,Value);
end;

Procedure TSeriesPointer.SetDraw3D(Value:Boolean);
Begin
  SetBooleanProperty(FDraw3D,Value);
end;

Procedure TSeriesPointer.Change3D(Value:Boolean);
Begin
  FDraw3D:=Value;
end;

Procedure TSeriesPointer.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  if FVisible and FInflateMargins then
  begin
    LeftMargin :=MaxLong(LeftMargin, HorizSize+1);
    RightMargin:=MaxLong(RightMargin,HorizSize+1);
  end;
end;

Procedure TSeriesPointer.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  if FVisible and FInflateMargins then
  begin
    TopMargin   :=MaxLong(TopMargin,   VertSize+1);
    BottomMargin:=MaxLong(BottomMargin,VertSize+1);
  end;
end;

Procedure TSeriesPointer.SetVisible(Value:Boolean);
Begin
  SetBooleanProperty(FVisible,Value);
end;

Procedure TSeriesPointer.SetStyle(Value:TSeriesPointerStyle);
Begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    FOwner.Repaint;
  end;
end;

Procedure TSeriesPointer.PrepareCanvas(ColorValue:TColor);
Begin
  With FOwner.ParentChart.Canvas do
  Begin
    AssignVisiblePen(FPen);
    if (FPen.Visible) and (FPen.Color=clTeeColor) then Pen.Color:=ColorValue;
    Brush.Assign(FBrush);
    if Brush.Color=clTeeColor then Brush.Color:=ColorValue;
  end;
end;

Procedure TSeriesPointer.DrawPointer( Is3D:Boolean;
                                      px,py,tmpHoriz,tmpVert:Integer;
                                      ColorValue:TColor;
                                      AStyle:TSeriesPointerStyle);

Var PXMinus : Integer;
    PXPlus  : Integer;
    PYMinus : Integer;
    PYPlus  : Integer;

  Procedure DrawDiagonalCross;
  Begin
    With FOwner,ParentChart.Canvas do
    Begin
      Pen.Color:=ColorValue; { added in 1.03 }
      if Is3D then
      begin
        LineWithZ(PXMinus, PYMinus, PXPlus+1,PYPlus+1,StartZ);
        LineWithZ(PXPlus,  PYMinus, PXMinus-1,PYPlus+1,StartZ);
      end
      else
      begin
        Line(PXMinus, PYMinus, PXPlus+1,PYPlus+1);
        Line(PXPlus , PYMinus, PXMinus-1,PYPlus+1);
      end;
    end;
  end;

  Procedure DrawCross;
  Begin
    With FOwner,ParentChart.Canvas do
    Begin
      Pen.Color:=ColorValue;  { added in 1.03 }
      if Is3D then
      begin
        VertLine3D(PX,PYMinus,PYPlus+1,StartZ);
        HorizLine3D(PXMinus,PXPlus+1,PY,StartZ);
      end
      else
      begin
        DoVertLine(PX,PYMinus,PYPlus+1);
        DoHorizLine(PXMinus,PXPlus+1,PY);
      end;
    end;
  end;

  Procedure DoTriangle3D(DeltaY:Integer);
  begin
    With FOwner,ParentChart.Canvas do
    if Self.FDraw3D then
       Pyramid( True, PXMinus,PY-DeltaY,PXPlus,PY+DeltaY,StartZ,EndZ,FDark3D)
    else
       TriangleWithZ( Point(PXMinus,PY+DeltaY),
                      Point(PXPlus,PY+DeltaY),
                      Point(PX,PY-DeltaY),StartZ )
  end;

Begin
  PXMinus:=PX-tmpHoriz;
  PXPlus :=PX+tmpHoriz;
  PYMinus:=PY-tmpVert;
  PYPlus :=PY+tmpVert;
  With FOwner,ParentChart.Canvas do
  Begin
    if Is3D then
    Case AStyle of
    psRectangle: if Self.FDraw3D then
                    Cube(PXMinus,PXPlus,PYMinus,PYPlus,StartZ,EndZ,FDark3D)
                 else
                    RectangleWithZ(Rect(PXMinus,PYMinus,PXPlus+1,PYPlus+1),StartZ);
       psCircle: EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,StartZ);
     psTriangle: DoTriangle3D( tmpVert);
 psDownTriangle: DoTriangle3D(-tmpVert);
        psCross: DrawCross;
    psDiagCross: DrawDiagonalCross;
         psStar: Begin DrawCross; DrawDiagonalCross; end;
      psDiamond: PlaneWithZ( Point(PXMinus,PY),
                             Point(PX,PYMinus),
                             Point(PXPlus,PY),
                             Point(PX,PYPlus),StartZ);
     psSmallDot: Pixels3D[PX,PY,StartZ]:=ColorValue
    end
    else
    Case AStyle of
    psRectangle: Rectangle(PXMinus,PYMinus,PXPlus+1,PYPlus+1);
       psCircle: Ellipse(PXMinus,PYMinus,PXPlus,PYPlus);
     psTriangle: Polygon([ Point(PXMinus,PYPlus),
                           Point(PXPlus,PYPlus),
                           Point(PX,PYMinus)]);
 psDownTriangle: Polygon([ Point(PXMinus,PYMinus),
                           Point(PXPlus,PYMinus),
                           Point(PX,PYPlus)]);
        psCross: DrawCross;
    psDiagCross: DrawDiagonalCross;
         psStar: Begin DrawCross; DrawDiagonalCross; end;
      psDiamond: Polygon([Point(PXMinus,PY),
                          Point(PX,PYMinus),
                          Point(PXPlus,PY),
                          Point(PX,PYPlus)] );
     psSmallDot: Pixels[PX,PY]:=ColorValue;
    end;
  end;
end;

Procedure TSeriesPointer.Draw(px,py:Integer; ColorValue:TColor; AStyle:TSeriesPointerStyle);
Begin
  DrawPointer(FOwner.ParentChart.View3D,px,py,FHorizSize,FVertSize,ColorValue,AStyle);
end;

Procedure TSeriesPointer.Assign(Source:TPersistent);
begin
  if Source is TSeriesPointer then
  With TSeriesPointer(Source) do
  begin
    Self.FBrush.Assign(FBrush);
    Self.FDark3D     :=FDark3D;
    Self.FDraw3D     :=FDraw3D;
    Self.FHorizSize  :=FHorizSize;
    Self.FInflateMargins:=FInflateMargins;
    Self.FPen.Assign(FPen);
    Self.FStyle      :=FStyle;
    Self.FVertSize   :=FVertSize;
    Self.FVisible    :=FVisible;
  end
  else inherited Assign(Source);
end;

Procedure TSeriesPointer.DrawLegendShape(AColor:TColor; Const Rect:TRect; DrawPen:Boolean);
var tmpHoriz : Integer;
    tmpVert  : Integer;
begin
  PrepareCanvas(AColor);
  With Rect do
  begin
    if DrawPen then
    begin
      tmpHoriz:=(Right-Left) div 3;
      tmpVert :=(Bottom-Top) div 3;
    end
    else
    begin
      tmpHoriz:=1+((Right-Left) div 2);
      tmpVert :=1+((Bottom-Top) div 2);
    end;
    DrawPointer(False, (Left+Right) div 2,(Top+Bottom) div 2,
                       MinLong(HorizSize,tmpHoriz),
                       MinLong(VertSize,tmpVert),AColor,FStyle);
  end;
end;

{ TCustomSeries }
Constructor TCustomSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  ClickableLine:=True;
  DrawBetweenPoints:=True;
  FPointer:=TSeriesPointer.Create(Self);
  FAreaLinesPen:=CreateChartPen;
  FLinePen:=CreateChartPen;
  FLineHeight:=0;
  FAreaColor:=clTeeColor;
  FDark3D:=True;
end;

Destructor TCustomSeries.Destroy;
Begin
  FLinePen.Free;
  FAreaLinesPen.Free;
  FPointer.Free;
  inherited Destroy;
end;

Procedure TCustomSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                  APosition:TSeriesMarkPosition);
var tmp : Integer;
begin
  Marks.ZPosition:=StartZ;
  tmp:=Marks.ArrowLength;
  With APosition do
  begin
    Dec(LeftTop.Y,tmp);
    Dec(ArrowTo.Y,tmp);
  end;
  inherited DrawMark(ValueIndex,St,APosition);
end;

Function TCustomSeries.ClickedPointer( ValueIndex,tmpX,tmpY:Longint;
                                       x,y:Longint):Boolean;
begin
  result:=(Abs(tmpX-X)<FPointer.FHorizSize) and
          (Abs(tmpY-Y)<FPointer.FVertSize);
end;

Procedure TCustomSeries.Assign(Source:TPersistent);
begin
  if Source is TCustomSeries then
  With TCustomSeries(Source) do
  begin
    Self.ClickableLine   :=ClickableLine;
    Self.FAreaBrush      :=FAreaBrush;
    Self.FAreaColor      :=FAreaColor;
    Self.FAreaLinesPen.Assign(FAreaLinesPen);
    Self.FDark3D         :=FDark3D;
    Self.FDrawArea       :=FDrawArea;
    Self.FDrawLine       :=FDrawLine;
    Self.FInvertedStairs :=FInvertedStairs;
    Self.FLinePen.Assign(FLinePen);
    Self.FLineBrush      :=FLineBrush;
    Self.FLineHeight     :=FLineHeight;
    Self.FPointer.Assign(FPointer);
    Self.FStairs         :=FStairs;
  end;
  inherited Assign(Source);
end;

Function TCustomSeries.Clicked(x,y:Integer):Longint;
var t        : Integer;
    OldXPos  : Integer;
    OldYPos  : Integer;
    tmpX     : Integer;
    tmpY     : Integer;
    P        : TPoint;

    Function CheckPointInLine:Boolean;

      Function PointInVertLine(x0,y0,y1:Integer):Boolean;
      begin
        result:=PointInLine(P,x0,y0,x0,y1);
      end;

      Function PointInHorizLine(x0,y0,x1:Integer):Boolean;
      begin
        result:=PointInLine(P,x0,y0,x1,y0);
      end;

    begin
      With ParentChart do
      if View3D then

         result:=PointInPolygon( P,[ Point(tmpX,tmpY),
                                     Point(tmpX+SeriesWidth3D,tmpY-SeriesHeight3D),
                                     Point(OldXPos+SeriesWidth3D,OldYPos-SeriesHeight3D),
                                     Point(OldXPos,OldYPos) ])
      else
         if FStairs then
         begin
            if FInvertedStairs then result:= PointInVertLine(OldXPos,OldYPos,tmpY) or
                                             PointInHorizLine(OldXPos,tmpY,tmpX)
                               else result:= PointInHorizLine(OldXPos,OldYPos,tmpX) or
                                             PointInVertLine(tmpX,OldYPos,tmpY);
         end
         else
            result:=PointInLine(P,tmpX,tmpY,OldXPos,OldYPos)
    end;

begin
  if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(X,Y,StartZ);
  result:=inherited Clicked(x,y);
  if (result=TeeNoPointClicked) and
     (FirstValueIndex>-1) and (LastValueIndex>-1) then
  begin
    OldXPos:=0;
    OldYPos:=0;
    OldBottomPos:=0;
    P.X:=X;
    P.Y:=Y;
    for t:=FirstValueIndex to LastValueIndex do
    begin
      tmpX:=CalcXPos(t);
      tmpY:=CalcYPos(t);
      if FPointer.FVisible then
      begin
        if ClickedPointer(t,tmpX,tmpY,x,y) then
        begin
          if Assigned(FOnClickPointer) then FOnClickPointer(Self,t,x,y);
          result:=t;
          break;
        end;
      end;
      if (tmpX=X) and (tmpY=Y) then
      begin
        result:=t;
        break;
      end;
      if (t>FirstValueIndex) and ClickableLine then
         if CheckPointInLine or
            ( FDrawArea and
               PointInPolygon( P,[ Point(OldXPos,OldYPos), Point(tmpX,tmpY),
                                   Point(tmpX,GetOriginPos(t)),
                                   Point(OldXPos,GetOriginPos(t-1)) ] )
            ) then
         begin
           result:=t-1;
           break;
         end;
      OldXPos:=tmpX;
      OldYPos:=tmpY;
      OldBottomPos:=BottomPos;
    end;
  end;
end;

Procedure TCustomSeries.SetDrawArea(Value:Boolean);
Begin
  SetBooleanProperty(FDrawArea,Value);
end;

Procedure TCustomSeries.SetPointer(Value:TSeriesPointer);
Begin
  FPointer.Assign(Value);
end;

Procedure TCustomSeries.SetAreaLinesPen(Value:TChartPen);
Begin
  FAreaLinesPen.Assign(Value);
end;

Procedure TCustomSeries.SetLinePen(Value:TChartPen);
Begin
  FLinePen.Assign(Value);
end;

Procedure TCustomSeries.SetLineHeight(Value:Integer);
Begin
  SetIntegerProperty(FLineHeight,Value);
end;

Procedure TCustomSeries.SetStairs(Value:Boolean);
Begin
  SetBooleanProperty(FStairs,Value);
end;

Procedure TCustomSeries.SetInvertedStairs(Value:Boolean);
Begin
  SetBooleanProperty(FInvertedStairs,Value);
end;

Procedure TCustomSeries.SetAreaColor(Value:TColor);
Begin
  SetColorProperty(FAreaColor,Value);
end;

Procedure TCustomSeries.SetBrushProperty(Var ABrush:TBrushStyle; Value:TBrushStyle);
Begin
  if ABrush<>Value then
  begin
    ABrush:=Value;
    Repaint;
  end;
end;

Procedure TCustomSeries.SetAreaBrush(Value:TBrushStyle);
Begin
  SetBrushProperty(FAreaBrush,Value);
end;

Procedure TCustomSeries.SetLineBrush(Value:TBrushStyle);
Begin
  SetBrushProperty(FLineBrush,Value);
end;

Procedure TCustomSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
Var tmpColor : TColor;

    Procedure DrawLine(DrawRectangle:Boolean);
    begin
      LinePrepareCanvas(ParentChart.Canvas,tmpColor);
      With ParentChart.Canvas do
      if DrawRectangle then DoRectangle(Rect)
      else
      With Rect do DoHorizLine(Left,Right,(Top+Bottom) div 2);
    end;

begin
  if ValueIndex=TeeAllValues then tmpColor:=SeriesColor
                             else tmpColor:=ValueColor[ValueIndex];
  if FPointer.Visible then
  begin
    if FDrawLine then DrawLine(False);
    FPointer.DrawLegendShape(tmpColor,Rect,FLinePen.Visible);
  end
  else
  if FDrawLine and (not FDrawArea) then
     DrawLine(ParentChart.View3D)
  else
     inherited DrawLegendShape(ValueIndex,Rect)
end;

procedure TCustomSeries.LinePrepareCanvas(tmpCanvas:TCanvas3D; tmpColor:TColor);
begin
  with tmpCanvas do
  begin
    AssignVisiblePen(FLinePen);
    ParentChart.CheckPenWidth(Pen);
    if ParentChart.View3D then
    begin
      Brush.Style:=FLineBrush;
      Brush.Color:=tmpColor;
    end
    else
    begin
      Brush.Style:=bsClear;
      Pen.Color:=tmpColor;
    end;
  end;
end;

Function TCustomSeries.GetAreaBrushColor(AColor:TColor):TColor;
begin
  if ColorEachPoint or (FAreaColor=clTeeColor) then
     result:=AColor
  else
     result:=FAreaColor;
end;

procedure TCustomSeries.DrawValue(ValueIndex:Longint);
Var x : Longint;
    y : Longint;

  Function CalcYPosLeftRight(Const YLimit:Double; AnotherIndex:Longint):Longint;
  var tmpPredValueX : Double;
      tmpPredValueY : Double;
      tmpDif        : Double;
  begin
    tmpPredValueX:=XValue[AnotherIndex];
    tmpDif:=XValue[ValueIndex]-tmpPredValueX;
    With ParentChart do
    if tmpDif=0 then result:=CalcYPos(AnotherIndex)
    else
    begin
      tmpPredValueY:=YValue[AnotherIndex];
      result:=CalcYPosValue( 1.0*tmpPredValueY+(YLimit-tmpPredValueX)*
                             (YValue[ValueIndex]-tmpPredValueY)/tmpDif );
    end;
  end;

var tmpColor    : TColor;
    IsLastValue : Boolean;

   Procedure InternalDrawArea(BrushStyle:TBrushStyle; BrushColor:TColor);
   var tmpY:Longint;
   Begin
     With ParentChart do
     Begin
       SetBrushCanvas(BrushColor,BrushStyle,SeriesColor);
       if View3D and IsLastValue then
          Canvas.RectangleZ(X,Y,BottomPos,StartZ,EndZ);
       if FStairs then
       begin
         if FInvertedStairs then tmpY:=Y
                            else tmpY:=OldY;
         Canvas.RectangleWithZ(Rect(OldX,BottomPos,X+1,tmpY),StartZ);
       end
       else
         Canvas.PlaneWithZ( Point(OldX,OldBottomPos),Point(OldX,OldY),
                            Point(X,Y),Point(X,BottomPos),StartZ);
     end;
   end;

   Procedure DrawPoint(DrawOldPointer:Boolean);
   var tmpPoint     : TPoint;
       tmpOldP      : TPoint;
       tmpDifX      : Integer;
       P4           : TFourPoints;
       OldDarkColor : TColor;
   Begin
     if ((x<>OldX) or (y<>OldY)) and (tmpColor<>clNone) then { <-- if not null }
     With ParentChart,Canvas do
     Begin
       if View3D then
       Begin
         if FDrawArea or (FDrawLine and (FLineBrush<>bsClear)) then
         Begin
           Brush.Color:=GetAreaBrushColor(tmpColor);
           OldDarkColor:=Brush.Color;
           AssignVisiblePen(FLinePen);
           if FLinePen.Visible then CheckPenWidth(Pen);
           Brush.Style:=FLineBrush;
           tmpPoint.X  :=X;
           tmpPoint.Y  :=Y;
           tmpOldP.X   :=OldX;
           tmpOldP.Y   :=OldY;
           if FStairs then
           Begin
             if FInvertedStairs then
             begin
               if FDark3D then Brush.Color:=ApplyDark(Brush.Color,DarkColorQuantity);
               RectangleZ( tmpOldP.X,tmpOldP.Y, Y,StartZ,EndZ);
               if FDark3D then Brush.Color:=OldDarkColor;
               RectangleY( tmpPoint.X, tmpPoint.Y,OldX,StartZ,EndZ);
             end
             else
             begin
               RectangleY( tmpOldP.X, tmpOldP.Y, X, StartZ, EndZ);
               if FDark3D then Brush.Color:=ApplyDark(Brush.Color,DarkColorQuantity);
               RectangleZ( tmpPoint.X,tmpPoint.Y, OldY,StartZ,EndZ);
               if FDark3D then Brush.Color:=OldDarkColor;
             end;
           end
           else
           begin
             if (FLineHeight>0) and (not FDrawArea) then
             begin
               P4[0]:=tmpPoint;
               P4[1]:=tmpOldP;
               P4[2].X:=tmpOldP.X;
               P4[2].Y:=tmpOldP.Y+FLineHeight;
               P4[3].X:=tmpPoint.X;
               P4[3].Y:=tmpPoint.Y+FLineHeight;
               PlaneFour3D(P4,StartZ,StartZ);
               if IsLastValue then
                  RectangleZ(tmpPoint.X,tmpPoint.Y,tmpPoint.Y+FLineHeight,StartZ,EndZ);
             end;
             if FDark3D then
             begin
               tmpDifX:=tmpPoint.X-tmpOldP.X;
               if (tmpDifX<>0) and
                  (tmpDark3DRatio<>0) and
                  ((tmpOldP.Y-tmpPoint.Y)/tmpDifX > tmpDark3DRatio) then
               begin
                 Brush.Color:=ApplyDark(Brush.Color,DarkColorQuantity);
                 if (FLineHeight>0) and (not FDrawArea) then {special case}
                 begin
                   Inc(tmpPoint.Y,FLineHeight);
                   Inc(tmpOldP.Y,FLineHeight);
                 end;
               end;
             end;
             Plane3D(tmpPoint,tmpOldP,StartZ,EndZ);
             if FDark3D then Brush.Color:=OldDarkColor;
           end;
         end;
       end;
       if FDrawArea then
       Begin
         Brush.Color:=GetAreaBrushColor(tmpColor);
         Pen.Assign(FAreaLinesPen);
         if (Pen.Color=clTeeColor) or (not FAreaLinesPen.Visible) then
            Pen.Color:=tmpColor;
         InternalDrawArea(FAreaBrush,Brush.Color);
       end
       else
       if (not View3D) and FDrawLine then
       Begin
         LinePrepareCanvas(Canvas,tmpColor);
         if FStairs then
         begin
           if FInvertedStairs then DoVertLine(OldX,OldY,Y)
                              else DoHorizLine(OldX,X,OldY);
           LineTo(X,Y);
         end
         else Line(OldX,OldY,X,Y);
       end;
     end;

     { pointers }
     if FPointer.FVisible and DrawOldPointer then
     Begin
       if OldColor<>clNone then { <-- if not null }
          DrawPointer(OldX,OldY,OldColor,Pred(ValueIndex));
       if IsLastValue and (tmpColor<>clNone) then {<-- if not null }
          DrawPointer(X,Y,tmpColor,ValueIndex);
     end;
   end;

Const MaxShortInt=32767;
Begin
  With ParentChart.Canvas do
  Begin
    X:=CalcXPos(ValueIndex);
    Y:=CalcYPos(ValueIndex);
    { Win95 limitation. only 16bit coordinates }
    if (X>-MaxShortInt) and (X<MaxShortInt) and
       (Y>-MaxShortInt) and (Y<MaxShortInt) then
    Begin
      tmpColor:=ValueColor[ValueIndex];
      Pen.Color:=clBlack;
      Brush.Color:=tmpColor;
      if OldColor=clNone then { if null }
      begin
        OldX:=X;
        OldY:=Y;
      end;
      IsLastValue:=ValueIndex=LastValueIndex;
      BottomPos:=GetOriginPos(ValueIndex);
      if ValueIndex=FirstValueIndex then { first point }
      Begin
        if FDark3D then
        With ParentChart do
           if SeriesWidth3D<>0 then
              tmpDark3DRatio:=SeriesHeight3D/SeriesWidth3D
           else
              tmpDark3DRatio:=1;
        if ValueIndex>0 then { previous point outside left }
        Begin
          if FDrawArea then
          begin
            OldX:=CalcXPos(Pred(ValueIndex));
            OldY:=CalcYPos(Pred(ValueIndex));
            OldBottomPos:=GetOriginPos(Pred(ValueIndex));
          end
          else
          begin
            if GetHorizAxis.Inverted then OldX:=ParentChart.ChartRect.Right
                                     else OldX:=ParentChart.ChartRect.Left;
            if FStairs Then
               OldY:=CalcYPos(Pred(ValueIndex))
            else
               OldY:=CalcYPosLeftRight(XScreenToValue(OldX),Pred(ValueIndex));
          end;
          if ValueColor[Pred(ValueIndex)]<>clNone then DrawPoint(False); { if not null }
        end;
        if IsLastValue and FPointer.FVisible then
           DrawPointer(X,Y,tmpColor,ValueIndex);
      end
      else DrawPoint(True);
      OldX:=X;
      OldY:=Y;
      OldBottomPos:=BottomPos;
      OldColor:=tmpColor;
    end;
  end;
end;

Procedure TCustomSeries.DrawPointer(AX,AY:Longint; AColor:TColor; ValueIndex:Longint);
var tmpStyle : TSeriesPointerStyle;
begin
  FPointer.PrepareCanvas(AColor);
  if Assigned(FOnGetPointerStyle) then
     tmpStyle:=FOnGetPointerStyle(Self,ValueIndex)
  else
     tmpStyle:=FPointer.FStyle;
  FPointer.Draw(AX,AY,AColor,tmpStyle);
end;

Function TCustomSeries.GetEditorClass:String;
Begin
  result:='TCustomSeriesEditor'; { <-- dont translate ! }
end;

Procedure TCustomSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin);
  FPointer.CalcHorizMargins(LeftMargin,RightMargin);
  if FStairs and FDrawLine then
  begin
    LeftMargin :=MaxLong(LeftMargin,LinePen.Width);
    RightMargin:=MaxLong(RightMargin,LinePen.Width);
  end;
end;

Procedure TCustomSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  inherited CalcVerticalMargins(TopMargin,BottomMargin);
  FPointer.CalcVerticalMargins(TopMargin,BottomMargin);
  if FStairs and FDrawLine then
  begin
    TopMargin   :=MaxLong(TopMargin,LinePen.Width);
    BottomMargin:=MaxLong(BottomMargin,LinePen.Width+1);
  end;
  if (FLineHeight>0) and (not FDrawArea) then
     if FLineHeight>BottomMargin then BottomMargin:=FLineHeight;
end;

Function TCustomSeries.GetOriginPos(ValueIndex:Longint):Longint;
Begin
  result:=GetVertAxis.IEndPos;
end;

Procedure TCustomSeries.SetDark3D(Value:Boolean);
begin
  SetBooleanProperty(FDark3D,Value);
end;

{ TLineSeries }
Constructor TLineSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FDrawLine:=True;
  AllowSinglePoint:=False;
  FPointer.Visible:=False; { inherited }
end;

Procedure TLineSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                           Var BrushStyle:TBrushStyle);
Begin
  ParentChart.Canvas.AssignVisiblePen(FLinePen);
  BrushStyle:=FLineBrush;
end;

Procedure TLineSeries.Assign(Source:TPersistent);
begin
  inherited Assign(Source);
  FPointer.Visible := False;
  FDrawArea        := False;
  FDrawLine        := True;
end;

{ TPointSeries }
Constructor TPointSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FDrawLine:=False;
  Marks.ArrowLength:=0;
  ClickableLine:=False;
  FPointer.Visible:=True;
end;

Procedure TPointSeries.Assign(Source:TPersistent);
begin
  inherited Assign(Source);
  FPointer.Visible :=True;
  FDrawArea        :=False;
  FDrawLine        :=False;
  ClickableLine    :=False;
end;

Function TPointSeries.GetEditorClass:String;
begin
  result:='TSeriesPointerEditor'; { <-- do not translate }
end;

Procedure TPointSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  Pointer.HorizSize:=6;
  Pointer.VertSize:=6;
  With ParentChart do
  if (SeriesCount>1) and (Self=Series[1]) then Pointer.Style:=psTriangle;
end;

Procedure TPointSeries.SetColorEachPoint(Value:Boolean);
begin
  inherited SetColorEachPoint(Value);
  if Value then Pointer.Brush.Color:=clTeeColor;
end;

{ TAreaSeries }
Constructor TAreaSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FDrawArea:=True;
  AllowSinglePoint:=False;
  FPointer.Visible:=False;
  FMultiArea:=maNone;
end;

Procedure TAreaSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                           Var BrushStyle:TBrushStyle);
Begin
  BackColor:=GetAreaBrushColor(ParentChart.Canvas.Brush.Color);
  ParentChart.Canvas.AssignVisiblePen(FAreaLinesPen);
  BrushStyle:=FAreaBrush;
end;

Procedure TAreaSeries.Assign(Source:TPersistent);
begin
  if Source is TAreaSeries then
  With TAreaSeries(Source) do Self.FMultiArea:=MultiArea;
  inherited Assign(Source);
  FDrawArea:=True;
  FDrawLine:=True;
  FPointer.Visible:=False;
end;

Procedure TAreaSeries.CalcZOrder;
Begin
  if FMultiArea=maNone then inherited CalcZOrder
                       else IZOrder:=ParentChart.MaxZOrder;
End;

Function TAreaSeries.GetEditorClass:String;
Begin
  result:='TAreaSeriesEditor'; { <-- dont translate ! }
end;

Procedure TAreaSeries.SetOtherMultiArea;
var t : Longint;
Begin
  if ParentChart<>nil then
  with ParentChart do
  for t:=0 to SeriesCount-1 do
    if Self is Series[t].ClassType then
       TAreaSeries(Series[t]).FMultiArea:=FMultiArea;
end;

Procedure TAreaSeries.SetMultiArea(Value:TMultiArea);
Begin
  if Value<>FMultiArea then
  Begin
    FMultiArea:=Value;
    SetOtherMultiArea;
    Repaint;
  end;
End;

Function TAreaSeries.InternalCalcStackedYPos(ValueIndex:Integer; Value:Double):Integer;
var tmp : Double;
begin
  Value:=Value+PointOrigin(ValueIndex,False);
  if FMultiArea=maStacked then result:=MinLong(GetVertAxis.IEndPos,CalcYPosValue(Value))
  else
  begin
    tmp:=PointOrigin(ValueIndex,True);
    if tmp<>0 then result:=CalcYPosValue(Value*100.0/tmp)
              else result:=0;
  end;
end;

Function TAreaSeries.GetOriginPos(ValueIndex:Longint):Longint;
Begin
  if FMultiArea=maNone then result:=inherited GetOriginPos(ValueIndex)
                       else result:=InternalCalcStackedYPos(ValueIndex,0);
end;

Function TAreaSeries.MaxYValue:Double;
var t : Longint;
Begin
  if FMultiArea=maStacked100 then result:=100
  else
  begin
    result:=inherited MaxYValue;
    if FMultiArea=maStacked then
    for t:=0 to Count-1 do
        result:=MaxDouble(result,PointOrigin(t,False)+YValue[t]);
  end;
end;

Function TAreaSeries.MinYValue:Double;
Begin
  if FMultiArea=maStacked100 then result:=0
                             else result:=inherited MinYValue;
end;

Function TAreaSeries.CalcYPos(ValueIndex:Longint):Integer;
Begin
  if FMultiArea=maNone then
     result:=inherited CalcYPos(ValueIndex)
  else
     result:=InternalCalcStackedYPos(ValueIndex,YValue[ValueIndex]);
End;

{ TCustomBarSeries }
Constructor TCustomBarSeries.Create(AOwner:TComponent);
Begin
  inherited Create(AOwner);
  FBarPen:=CreateChartPen;
  FBarBrush:=TChartBrush.Create(CanvasChanged);
  FBarWidthPercent:=70; { % }
  FBarStyle:=bsRectangle;
  FUseOrigin:=True;
  FMultiBar:=mbSide;
  FAutoBarSize:=False;
  FAutoMarkPosition:=True;
  Marks.Visible:=True;
  Marks.ArrowLength:=20;
  FDark3D:=True;
  FSideMargins:=True;
  With MandatoryValueList do
  begin
    Name:=TeeMsg_ValuesBar;
    Order:=loNone;
  end;
end;

Destructor TCustomBarSeries.Destroy;
begin
  FBarPen.Free;
  FBarBrush.Bitmap.Free;
  FBarBrush.Free;
  inherited Destroy;
end;

Procedure TCustomBarSeries.CalcZOrder;
Var t         : Longint;
    tmpZOrder : Integer;
Begin
  if FMultiBar=mbNone then inherited CalcZOrder
  else
  Begin
    tmpZOrder:=-1;
    for t:=0 to ParentChart.SeriesCount-1 do
    if ParentChart[t].Active then
    begin
      if ParentChart[t]=Self then break
      else
      if SameClass(ParentChart[t]) then
      Begin
        tmpZOrder:=ParentChart[t].ZOrder;
        break;
      end;
    end;
    if tmpZOrder=-1 then inherited CalcZOrder
                    else IZOrder:=tmpZOrder;
  end;
End;

Function TCustomBarSeries.NumSampleValues:Longint;
Begin
  result:=6; { default number of BarSeries random sample values }
End;

Procedure TCustomBarSeries.SetBarWidthPercent(Value:Integer);
Begin
  if (Value<1) or (Value>100) then
     Raise BarException.Create(TeeMsg_BarWidthPercent)
  else
     SetIntegerProperty(FBarWidthPercent,Value);
end;

Procedure TCustomBarSeries.SetOffsetPercent(Value:Integer);
Begin
  if (Value<-100) or (Value>100) then
     Raise BarException.Create(TeeMsg_BarOffsetPercent)
  else
     SetIntegerProperty(FOffsetPercent,Value);
End;

Procedure TCustomBarSeries.SetCustomBarSize(Value:Integer);
Begin
  SetIntegerProperty(FCustomBarSize,Value);
end;

Procedure TCustomBarSeries.SetBarStyle(Value:TBarStyle);
Begin
  if FBarStyle<>Value then
  begin
    FBarStyle:=Value;
    Repaint;
  end;
end;

{ If more than one bar series exists in chart,
  which position are we? the first, the second, the third? }
Function TCustomBarSeries.BarOrderPos:Longint;
var t : Integer;
Begin
  result:=1;
  With ParentChart do
  for t:=0 to SeriesCount-1 do
  if Series[t].Active then
  begin
    if Series[t]=Self then break
                      else if SameClass(Series[t]) then Inc(result);
  end;
End;

Function TCustomBarSeries.InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean;
Begin
  result:=False; { <-- abstract }
End;

Procedure TCustomBarSeries.CalcBarWidth;
Var t            : Longint;
    NumBarSeries : Longint;
    MaxBarPoints : Longint;
    tmpAxis      : TCustomChartAxis;
    tmp          : Integer;
Begin
  if FCustomBarSize<>0 then IBarSize:=FCustomBarSize
  else
  begin
    NumBarSeries:=1;
    MaxBarPoints:=Count;
    if ParentChart<>nil then
    With ParentChart do
    Begin
      if FMultiBar=mbSide then
      Begin
        NumBarSeries:=0;
        MaxBarPoints:=TeeAllValues;
        for t:=0 to SeriesCount-1 do
        if Series[t].Active and SameClass(Series[t]) then
        Begin
          Inc(NumBarSeries);
          if (MaxBarPoints=TeeAllValues) or (Series[t].Count>MaxBarPoints) then
             MaxBarPoints:=Series[t].Count;
        end;
      end;
      { this should be after calculating NumBarSeries }
      if MaxPointsPerPage>0 then MaxBarPoints:=MaxPointsPerPage;
    end;
    if MaxBarPoints>0 then
    Begin
      if FSideMargins then Inc(MaxBarPoints);
      if MandatoryValueList=XValues then tmpAxis:=GetVertAxis
                                    else tmpAxis:=GetHorizAxis;
      With ParentChart,tmpAxis do
      if FAutoBarSize then tmp:=Round(IAxisSize/(2.0+Maximum-Minimum))
                      else tmp:=IAxisSize div MaxBarPoints;
      IBarSize:=Round((FBarWidthPercent*0.01)*tmp) div NumBarSeries;
      if (IBarSize mod 2)=1 then inc(IBarSize);
    end
    else IBarSize:=0;
  end;
end;

Function TCustomBarSeries.CalcMarkLength(ValueIndex:Longint):Longint;
Begin
  if (Count>0) and Marks.Visible then
  Begin
    ParentChart.FontCanvas(Marks.Font);
    result:=Marks.ArrowLength+InternalCalcMarkLength(ValueIndex);
    if Marks.Frame.Visible then Inc(result,2*Marks.Frame.Width);
  end
  else result:=0;
end;

Procedure TCustomBarSeries.SetUseOrigin(Value:Boolean);
Begin
  SetBooleanProperty(FUseOrigin,Value);
End;

Procedure TCustomBarSeries.SetSideMargins(Value:Boolean);
var t : Longint;
Begin
  SetBooleanProperty(FSideMargins,Value);
  if (ParentChart<>nil) then
  with ParentChart do
  for t:=0 to SeriesCount-1 do
    if (Series[t]<>Self) and SameClass(Series[t]) then
       TCustomBarSeries(Series[t]).FSideMargins:=FSideMargins;
end;

Procedure TCustomBarSeries.SetDark3d(Value:Boolean);
Begin
  SetBooleanProperty(FDark3d,Value);
End;

Procedure TCustomBarSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  BarWidthPercent:=85;
  MultiBar:=mbNone;
end;

Procedure TCustomBarSeries.SetOrigin(Const Value:Double);
Begin
  SetDoubleProperty(FOrigin,Value);
End;

Function TCustomBarSeries.BarSeriesCount:Longint;
var t : Longint;
Begin
  if FMultiBar=mbSide then
  Begin
    result:=0;
    with ParentChart do
    for t:=0 to SeriesCount-1 do
        if Series[t].Active and SameClass(Series[t]) then
           Inc(Result);
  end
  else result:=1;
{      if multibar mbSeries}
end;

Function TCustomBarSeries.Clicked(x,y:Integer):Longint;
var t : Longint;
    P : TPoint;
Begin
  if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(X,Y,StartZ);
  result:=inherited Clicked(x,y);
  if (result=-1) and (FirstValueIndex>-1) and (LastValueIndex>-1) then
  begin
    P.X:=X;
    P.Y:=Y;
    for t:=FirstValueIndex to LastValueIndex do
    if t<Count then
       if InternalClicked(t,P) then
       begin
         result:=t;
         break;
       end;
  end;
end;

Procedure TCustomBarSeries.SetOtherMultiBar;
var t : Longint;
Begin
  if (ParentChart<>nil) then
  with ParentChart do
  for t:=0 to SeriesCount-1 do
    if SameClass(Series[t]) then
       TCustomBarSeries(Series[t]).FMultiBar:=FMultiBar;
end;

Procedure TCustomBarSeries.SetMultiBar(Value:TMultiBar);
Begin
  if Value<>FMultiBar then
  Begin
    FMultiBar:=Value;
    SetOtherMultiBar;
    Repaint;
  end;
End;

Function TCustomBarSeries.InternalGetOriginPos(ValueIndex:Longint; DefaultOrigin:Longint):Longint;
Var tmp      : Double;
    tmpValue : Double;
Begin
  tmpValue:=PointOrigin(ValueIndex,False);
  Case FMultiBar of
    mbStacked: result:=CalcPosValue(tmpValue);
    mbStacked100:
      begin
        tmp:=PointOrigin(ValueIndex,True);
        if tmp<>0 then result:=CalcPosValue(tmpValue*100.0/tmp)
                  else result:=0;
      end;
  else
    if FUseOrigin then result:=CalcPosValue(tmpValue)
                  else result:=DefaultOrigin;
  end;
end;

Function TCustomBarSeries.PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double;
Begin
  if (FMultiBar=mbStacked) or
     (FMultiBar=mbStacked100) then
    result:=inherited PointOrigin(ValueIndex,SumAll)
  else
    result:=FOrigin;
End;

Procedure TCustomBarSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
begin
  if FBarBrush.Bitmap<>nil then ParentChart.Canvas.Brush.Bitmap:=FBarBrush.Bitmap;
  inherited DrawLegendShape(ValueIndex,Rect);
end;

Procedure TCustomBarSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                                Var BrushStyle:TBrushStyle);
Begin
  ParentChart.Canvas.AssignVisiblePen(FBarPen);
  BrushStyle:=FBarBrush.Style;
  if FBarBrush.Color=clTeeColor then BackColor:=ParentChart.Color
                                else BackColor:=FBarBrush.Color;
end;

Procedure TCustomBarSeries.SetPenBrushBar(BarColor:TColor);
var tmpBack : TColor;
Begin
  With ParentChart do
  begin
    Canvas.AssignVisiblePen(FBarPen);
    if FBarBrush.Bitmap=nil then
    begin
      if FBarBrush.Color=clTeeColor then tmpBack:=Color
                                    else tmpBack:=FBarBrush.Color;
      SetBrushCanvas(BarColor,FBarBrush.Style,tmpBack);
    end
    else Canvas.Brush.Bitmap:=FBarBrush.Bitmap;
  end;
end;

Procedure TCustomBarSeries.BarRectangle( BarColor:TColor;
                                         ALeft,ATop,ARight,ABottom:Longint);
Begin
  With ParentChart,Canvas do
  begin
    if FBarBrush.Style=bsSolid then
    Begin
      if (ARight=ALeft) or (ATop=ABottom) then
      Begin
        Pen.Color:=Brush.Color;
        if Pen.Style=psClear then Pen.Style:=psSolid;
        Line(ALeft,ATop,ARight,ABottom);
      end
      else
      if (Abs(ARight-ALeft)<Pen.Width) or
         (Abs(ABottom-ATop)<Pen.Width) then
      Begin
        Pen.Color:=Brush.Color;
        if Pen.Style=psClear then Pen.Style:=psSolid;
        Brush.Style:=bsClear;
      end;
    end;
    Rectangle(ALeft,ATop,ARight,ABottom);
  end;
End;

Procedure TCustomBarSeries.SetBarPen(Value:TChartPen);
Begin
  FBarPen.Assign(Value);
end;

Function TCustomBarSeries.GetBarStyle(ValueIndex:Longint):TBarStyle;
Begin
  result:=FBarStyle;
  if Assigned(FOnGetBarStyle) then FOnGetBarStyle(Self,ValueIndex,result);
end;

Procedure TCustomBarSeries.SetBarBrush(Value:TChartBrush);
Begin
  FBarBrush.Assign(Value);
end;

Function TCustomBarSeries.GetEditorClass:String;
Begin
  result:='TBarSeriesEditor'; { <-- dont translate ! }
end;

Function TCustomBarSeries.BarMargin:Longint;
Begin
  result:=IBarSize*BarSeriesCount;
  if not FSideMargins then result:=result shr 1;
end;

Function TCustomBarSeries.ApplyBarOffset(Position:Longint):Longint;
Begin
  result:=Position;
  if FOffsetPercent<>0 then Inc(result,Round(FOffsetPercent*IBarSize*0.01));
end;

Procedure TCustomBarSeries.SetAutoMarkPosition(Value:Boolean);
Begin
  SetBooleanProperty(FAutoMarkPosition,Value);
end;

Procedure TCustomBarSeries.SetAutoBarSize(Value:Boolean);
Begin
  SetBooleanProperty(FAutoBarSize,Value);
end;

Procedure TCustomBarSeries.Assign(Source:TPersistent);
begin
  if Source is TCustomBarSeries then
  With TCustomBarSeries(Source) do
  begin
    Self.FAutoMarkPosition:=FAutoMarkPosition;
    Self.FBarWidthPercent:=FBarWidthPercent;
    Self.FBarStyle       :=FBarStyle;
    Self.FBarPen.Assign(FBarPen);
    Self.FBarBrush.Assign(FBarBrush);
    Self.FCustomBarSize  :=FCustomBarSize;
    Self.FDark3d         :=FDark3D;
    Self.FMultiBar       :=FMultiBar;
    Self.FOffsetPercent  :=FOffsetPercent;
    Self.FOrigin         :=FOrigin;
    Self.FSideMargins    :=FSideMargins;
    Self.FUseOrigin      :=FUseOrigin;
  end;
  inherited Assign(Source);
end;

Function TCustomBarSeries.AddBar( Const AValue:Double;
                                  Const ALabel:String; AColor:TColor):Longint;
begin
  result:=Add(AValue,ALabel,AColor);
end;

Procedure TCustomBarSeries.AdjustGradientRectPen(Var R:TRect);
begin
  With R do
  begin
    Inc(Left);
    Inc(Top);
    Dec(Right,FBarPen.Width-1);
    Dec(Bottom,FBarPen.Width-1);
  end;
end;

Function TCustomBarSeries.MaxMandatoryValue( Const Value:Double;
                                             AList:TChartValueList):Double;
var t   : Longint;
    tmp : Double;
begin
  if FMultiBar=mbStacked100 then result:=100
  else
  begin
    result:=Value;
    if FMultiBar=mbStacked then
    for t:=0 to Count-1 do
    Begin
      tmp:=PointOrigin(t,False)+AList[t];
      if tmp>result then result:=tmp;
    end;
    if FUseOrigin and (result<FOrigin) then result:=FOrigin;
  end;
end;

Function TCustomBarSeries.MinMandatoryValue(Const Value:Double):Double;
begin
  if FMultiBar=mbStacked100 then result:=0
  else
  begin
    result:=Value;
    if FUseOrigin and (result>FOrigin) then result:=FOrigin;
  end;
end;

Procedure TCustomBarSeries.InternalApplyBarMargin(Var MarginA,MarginB:Integer);
var tmp : Integer;
begin
  CalcBarWidth;
  tmp:=BarMargin;
  Inc(MarginA,tmp);
  Inc(MarginB,tmp);
end;

{ TBarSeries }

{ The horizontal Bar position is the "real" X pos +
  the BarWidth by our BarSeries order }
Function TBarSeries.CalcXPos(ValueIndex:Longint):Integer;
Begin
  result:=inherited CalcXPos(ValueIndex);
  if FMultiBar=mbSide then
     result:=result+Round(IBarSize*((BarOrderPos-(BarSeriesCount*0.5))-1.0))
  else
     result:=result-(IBarSize shr 1);
  result:=ApplyBarOffset(result);
End;

Function TBarSeries.MaxYValue:Double;
Begin
  result:=MaxMandatoryValue(inherited MaxYValue,YValues);
end;

Function TBarSeries.MinYValue:Double;
Begin
  result:=MinMandatoryValue(inherited MinYValue);
end;

Procedure TBarSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin);
  InternalApplyBarMargin(LeftMargin,RightMargin);
end;

Procedure TBarSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
var tmp : Longint;
begin
  inherited CalcVerticalMargins(TopMargin,BottomMargin);
  tmp:=CalcMarkLength(0);
  if tmp>0 then
  begin
    Inc(tmp);
    if FUseOrigin and (inherited MinYValue<FOrigin) then
       if GetVertAxis.Inverted then Inc(TopMargin,tmp) { 4.01 }
                               else Inc(BottomMargin,tmp);
    if (not FUseOrigin) or (inherited MaxYValue>FOrigin) then
       if GetVertAxis.Inverted then Inc(BottomMargin,tmp)
                               else Inc(TopMargin,tmp);
  end;
end;

Function TBarSeries.InternalCalcMarkLength(ValueIndex:Longint):Longint;
Begin
  result:=ParentChart.Canvas.FontHeight;
end;

Procedure TBarSeries.DrawMark( ValueIndex:Longint; Const St:String;
                               APosition:TSeriesMarkPosition);
Var DifW : Integer;
    DifH : Integer;
Begin
  With APosition do
  begin
    DifW:=IBarSize div 2;
    DifH:=Marks.ArrowLength;
    if ArrowFrom.Y>GetOriginPos(ValueIndex) then DifH:=-DifH-Height;
    LeftTop.X:=LeftTop.X+DifW;
    LeftTop.Y:=LeftTop.Y-DifH;
    ArrowTo.X:=ArrowTo.X+DifW;
    ArrowTo.Y:=ArrowTo.Y-DifH;
    ArrowFrom.X:=ArrowFrom.X+DifW;
  end;
  inherited DrawMark(ValueIndex,St,APosition);
end;

Procedure TBarSeries.DrawBar(BarIndex,StartPos,EndPos:Longint);
Var tmpMidX     : Integer;
    tmpGradRect : TRect;
Begin
  SetPenBrushBar(NormalBarColor);
  With ParentChart,Canvas,FBarBounds do
  begin
    tmpMidX:=(Left+Right) shr 1;
    if View3D then
    Case GetBarStyle(BarIndex) of
      bsRectangle: Cube(Left,Right,StartPos,EndPos,StartZ,EndZ,FDark3D);
      bsPyramid  : Pyramid(True,Left,StartPos,Right,EndPos,StartZ,EndZ,FDark3D);
     bsInvPyramid: Pyramid(True,Left,EndPos,Right,StartPos,StartZ,EndZ,FDark3D);
      bsCilinder : Cylinder(True,Left,Top,Right,Bottom,StartZ,EndZ,FDark3D);
      bsEllipse  : EllipseWithZ(Left,Top,Right,Bottom,MiddleZ);
      bsArrow    : Arrow(True,Point(tmpMidX,EndPos),Point(tmpMidX,StartPos),(Right-Left),(Right-Left) div 2,MiddleZ);
   bsRectGradient: begin
                     Cube(Left,Right,StartPos,EndPos,StartZ,EndZ,FDark3D);
                     if View3DOptions.Orthogonal then
                     With FBarBounds do
                     begin
                       tmpGradRect.TopLeft:=Calculate3DPosition(Left,StartPos,StartZ);
                       tmpGradRect.BottomRight:=Calculate3DPosition(Right,EndPos,StartZ);
                       if BarPen.Visible then AdjustGradientRectPen(tmpGradRect);
                       GradientFill(tmpGradRect,clWhite,NormalBarColor,gdBottomTop);
                     end;
                   end;
    end
    else
    begin
      Case GetBarStyle(BarIndex) of
        bsRectangle,
        bsCilinder : BarRectangle(NormalBarColor,Left,Top,Right,Bottom);
        bsPyramid  : Polygon([ Point(Left,EndPos),Point(tmpMidX,StartPos),Point(Right,EndPos) ]);
       bsInvPyramid: Polygon([ Point(Left,StartPos),Point(tmpMidX,EndPos),Point(Right,StartPos) ]);
        bsEllipse  : Ellipse(Left,Top,Right,Bottom);
        bsArrow    : Arrow(True,Point(tmpMidX,EndPos),Point(tmpMidX,StartPos),(Right-Left),(Right-Left) div 2,MiddleZ);
     bsRectGradient: begin
                       GradientFill(Rect(Left,StartPos,Right,EndPos),clWhite,NormalBarColor,gdBottomTop);
                       if FBarPen.Visible then
                       begin
                         Brush.Style:=bsClear;
                         BarRectangle(NormalBarColor,Left,Top,Right,Bottom);
                       end;
                     end;
      end;
    end;
  end;
end;

procedure TBarSeries.DrawValue(ValueIndex:Longint);
Begin
  inherited DrawValue(ValueIndex); { <-- call inherited ! }
  NormalBarColor:=ValueColor[ValueIndex];
  if NormalBarColor<>clNone then { if not null }
  With FBarBounds do
  Begin
    Left  :=CalcXPos(ValueIndex);
    Right :=Left+IBarSize;
    Top   :=CalcYPos(ValueIndex);
    Bottom:=GetOriginPos(ValueIndex);
    if Bottom>Top then DrawBar(ValueIndex,Top,Bottom)
                  else DrawBar(ValueIndex,Bottom,Top);
  end;
end;

Function TBarSeries.CalcYPos(ValueIndex:Longint):Integer;
var tmp      : Double;
    tmpValue : Double;
Begin
  if (FMultiBar=mbNone) or (FMultiBar=mbSide) then
      result:=inherited CalcYPos(ValueIndex)
  else
  begin
    tmpValue:=YValue[ValueIndex]+PointOrigin(ValueIndex,False);
    if FMultiBar=mbStacked then result:=CalcYPosValue(tmpValue)
    else
    begin
      tmp:=PointOrigin(ValueIndex,True);
      if tmp<>0 then result:=CalcYPosValue(tmpValue*100.0/tmp)
                else result:=0;
    end;
  end;
End;

Function TBarSeries.GetOriginPos(ValueIndex:Longint):Longint;
Begin
  result:=InternalGetOriginPos(ValueIndex,GetVertAxis.IEndPos);
end;

Function TBarSeries.InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean;
Var tmpSwap : Longint;
    tmpX    : Longint;
    tmpY    : Longint;
    endY    : Longint;
Begin
  result:=False;
  tmpX:=CalcXPos(ValueIndex);
  if (APoint.x>=tmpX) and (APoint.x<=(tmpX+IBarSize)) then
  Begin
    tmpY:=CalcYPos(ValueIndex);
    endY:=GetOriginPos(ValueIndex);
    if endY<tmpY then
    begin
      tmpSwap:=endY;
      endY:=tmpY;
      tmpY:=tmpSwap;
    end;
    Case FBarStyle of
   bsInvPyramid: result:=PointInTriangle(APoint,tmpX,tmpX+IBarSize,tmpY,endY);
      bsPyramid: result:=PointInTriangle(APoint,tmpX,tmpX+IBarSize,endY,tmpY);
      bsEllipse: result:=PointInEllipse(APoint,Rect(tmpX,tmpY,tmpX+IBarSize,endY));
    else
      result:=(APoint.y>=tmpY) and (APoint.y<=endY);
    end;
  end;
end;

Function TBarSeries.DrawValuesForward:Boolean;
begin
  result:=not GetHorizAxis.Inverted;
end;

Function TBarSeries.DrawSeriesForward(ValueIndex:Longint):Boolean;
begin
  if (FMultiBar=mbNone) or (FMultiBar=mbSide) then
     result:=True
  else
  begin
    result:=GetVertAxis.Inverted;
    if MandatoryValueList[ValueIndex]>=0 then result:=not result;
  end;
end;

{ THorizBarSeries }
Function THorizBarSeries.MandatoryValueList:TChartValueList;
Begin
  result:=XValues;
End;

Procedure THorizBarSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
var tmp : Longint;
begin
  inherited CalcHorizMargins(LeftMargin,RightMargin);
  tmp:=CalcMarkLength(TeeAllValues);
  if tmp>0 then Inc(tmp);
  if (FUseOrigin and (inherited MinXValue<FOrigin)) then
     Inc(LeftMargin,tmp);
  if (not FUseOrigin) or (inherited MaxXValue>FOrigin) then
     if GetHorizAxis.Inverted then Inc(LeftMargin,tmp)
                              else Inc(RightMargin,tmp);
end;

Procedure THorizBarSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  inherited CalcVerticalMargins(TopMargin,BottomMargin);
  InternalApplyBarMargin(TopMargin,BottomMargin);
end;

Function THorizBarSeries.CalcXPos(ValueIndex:Longint):Integer;
var tmp      : Double;
    tmpValue : Double;
Begin
  if (FMultiBar=mbNone) or (FMultiBar=mbSide) then
     result:=inherited CalcXPos(ValueIndex)
  else
  begin
    tmpValue:=XValue[ValueIndex]+PointOrigin(ValueIndex,False);
    if FMultiBar=mbStacked then result:=CalcXPosValue(tmpValue)
    else
    begin
      tmp:=PointOrigin(ValueIndex,True);
      if tmp<>0 then result:=CalcXPosValue(tmpValue*100.0/tmp)
                else result:=0;
    end;
  end;
End;

Function THorizBarSeries.GetOriginPos(ValueIndex:Longint):Longint;
Begin
  result:=InternalGetOriginPos(ValueIndex,GetHorizAxis.IStartPos);
end;

{ The vertical Bar position is the "real" Y pos +
  the Barwidth by our BarSeries order }
Function THorizBarSeries.CalcYPos(ValueIndex:Longint):Integer;
Begin
  result:=inherited CalcYPos(ValueIndex);
  if FMultiBar=mbSide then
     result:=result+Round(IBarSize*(((BarSeriesCount*0.5)-BarOrderPos)))
  else
     result:=result-(IBarSize shr 1);
  result:=ApplyBarOffset(result);
End;

Function THorizBarSeries.InternalClicked(ValueIndex:Longint; Const APoint:TPoint):Boolean;
Var tmpX  : Longint;
    tmpY  : Longint;
    endX  : Longint;
Begin
  result:=False;
  tmpY:=CalcYPos(ValueIndex);
  if (APoint.y>=tmpY) and (APoint.y<=(tmpY+IBarSize)) then
  Begin
    tmpX:=CalcXPos(ValueIndex);
    endX:=GetOriginPos(ValueIndex);
    if endX<tmpX then SwapLongint(tmpX,endX);
    Case FBarStyle of
   bsInvPyramid: result:=PointInHorizTriangle(APoint,tmpY,tmpY+IBarSize,endX,tmpX);
      bsPyramid: result:=PointInHorizTriangle(APoint,tmpY,tmpY+IBarSize,tmpX,endX);
      bsEllipse: result:=PointInEllipse(APoint,Rect(tmpX,tmpY,endX,tmpY+IBarSize));
    else
      result:=(APoint.x>=tmpX) and (APoint.x<=endX);
    end;
  end;
end;

Function THorizBarSeries.MaxXValue:Double;
Begin
  result:=MaxMandatoryValue(inherited MaxXValue,XValues);
end;

Function THorizBarSeries.MinXValue:Double;
Begin
  result:=MinMandatoryValue(inherited MinXValue);
end;

Function THorizBarSeries.InternalCalcMarkLength(ValueIndex:Longint):Longint;
Begin
  if ValueIndex<>TeeAllValues then
     result:=ParentChart.Canvas.TextWidth(GetMarkText(ValueIndex))
  else
     result:=MaxMarkWidth;
end;

Procedure THorizBarSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                    APosition:TSeriesMarkPosition);
Var DifH : Integer;
    DifW : Integer;
Begin
  With APosition do
  begin
    DifH:=IBarSize div 2;
    DifW:=Marks.ArrowLength;
    if ArrowFrom.X<GetOriginPos(ValueIndex) then DifW:=-DifW-Width;
    LeftTop.X:=LeftTop.X+DifW+(Width div 2);
    LeftTop.Y:=LeftTop.Y+DifH+(Height div 2);
    ArrowTo.X:=ArrowTo.X+DifW;
    ArrowTo.Y:=ArrowTo.Y+DifH;
    ArrowFrom.Y:=ArrowFrom.Y+DifH;
  end;
  inherited DrawMark(ValueIndex,St,APosition);
end;

Procedure THorizBarSeries.DrawBar(BarIndex,StartPos,EndPos:Longint);
Var tmpMidY     : Integer;
    tmpGradRect : TRect;
Begin
  SetPenBrushBar(NormalBarColor);
  With ParentChart,Canvas,FBarBounds do
  begin
    tmpMidY:=(Top+Bottom) shr 1;
    if View3D then
    Case GetBarStyle(BarIndex) of
      bsRectangle: Cube(StartPos,EndPos,Top,Bottom,StartZ,EndZ,FDark3D);
      bsPyramid  : Pyramid(False,StartPos,Top,EndPos,Bottom,StartZ,EndZ,FDark3D);
     bsInvPyramid: Pyramid(False,EndPos,Top,StartPos,Bottom,StartZ,EndZ,FDark3D);
      bsCilinder : Cylinder(False,Left,Top,Right,Bottom,StartZ,EndZ,FDark3D);
      bsEllipse  : EllipseWithZ(Left,Top,Right,Bottom,MiddleZ);
      bsArrow    : Arrow(True,Point(StartPos,tmpMidY),Point(EndPos,tmpMidY),(Bottom-Top),(Bottom-Top) div 2,MiddleZ);
   bsRectGradient: begin
                     Cube(StartPos,EndPos,Top,Bottom,StartZ,EndZ,FDark3D);
                     if View3DOptions.Orthogonal then
                     With FBarBounds do
                     begin
                       tmpGradRect.TopLeft:=Calculate3DPosition(StartPos,Top,StartZ);
                       tmpGradRect.BottomRight:=Calculate3DPosition(EndPos,Bottom,StartZ);
                       if BarPen.Visible then AdjustGradientRectPen(tmpGradRect);
                       GradientFill(tmpGradRect,clWhite,NormalBarColor,gdLeftRight);
                     end;
                   end;
    end
    else
    begin
      Case GetBarStyle(BarIndex) of
        bsRectangle,
        bsCilinder : BarRectangle(NormalBarColor,Left,Top,Right,Bottom);
        bsPyramid  : Polygon([ Point(StartPos,Top),Point(EndPos,tmpMidY),Point(StartPos,Bottom) ]);
       bsInvPyramid: Polygon([ Point(EndPos,Top),Point(StartPos,tmpMidY),Point(EndPos,Bottom) ]);
        bsEllipse  : Ellipse(Left,Top,Right,Bottom);
        bsArrow    : Arrow(True,Point(StartPos,tmpMidY),Point(EndPos,tmpMidY),(Bottom-Top),(Bottom-Top) div 2,MiddleZ);
     bsRectGradient: begin
                       GradientFill(Rect(StartPos,Top,EndPos,Bottom),clWhite,NormalBarColor,gdLeftRight);
                       if FBarPen.Visible then
                       begin
                         Brush.Style:=bsClear;
                         BarRectangle(NormalBarColor,Left,Top,Right,Bottom);
                       end;
                     end;
      end;
    end;
  end;
end;

procedure THorizBarSeries.DrawValue(ValueIndex:Longint);
Begin
  inherited DrawValue(ValueIndex); { <-- call inherited ! }
  NormalBarColor:=ValueColor[ValueIndex];
  if NormalBarColor<>clNone then  { <-- if not null }
  With FBarBounds do
  Begin
    Top   :=CalcYPos(ValueIndex);
    Bottom:=Top+IBarSize;
    Right :=CalcXPos(ValueIndex);
    Left  :=GetOriginPos(ValueIndex);
    if Right>Left then DrawBar(ValueIndex,Left,Right)
                  else DrawBar(ValueIndex,Right,Left);
  end;
end;

Procedure THorizBarSeries.FillSampleValues(NumValues:Longint);
var t     : Longint;
    tmpX  : Double;
    tmpY  : Double;
    DifX  : Double;
    StepY : Double;
Begin
  if NumValues<=0 then Raise BarException.Create(TeeMsg_FillSample);
  Clear;
  tmpY:=GetVertAxis.Minimum;
  DifX:=1000;
  tmpX:=DifX+Random(Round(DifX));
  StepY:=1;
  for t:=1 to NumValues do
  Begin
    tmpX:=tmpX+Random(Round(DifX) div 2)-(Round(DifX) div 4);
    AddXY(tmpX,tmpY,'' {$IFNDEF D5},clTeeColor{$ENDIF});
    tmpY:=tmpY+StepY;
  end;
  RefreshSeries;
End;

Function THorizBarSeries.DrawValuesForward:Boolean;
begin
  result:=not GetVertAxis.Inverted;
end;

Function THorizBarSeries.DrawSeriesForward(ValueIndex:Longint):Boolean;
begin
  if (FMultiBar=mbNone) or (FMultiBar=mbSide) then
     result:=True
  else
  begin
    result:=GetHorizAxis.Inverted;
    if MandatoryValueList[ValueIndex]>=0 then result:=not result;
  end;
end;

{ TCircledSeries }
Constructor TCircledSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CalcVisiblePoints:=False; { always draw all points }
  XValues.Name:=TeeMsg_ValuesAngle;
  FCircleBackColor:=clTeeColor;
end;

Destructor TCircledSeries.Destroy;
begin
  SetParentProperties(True);
  inherited Destroy;
end;

procedure TCircledSeries.SetCircled(Value:Boolean);
var t : Longint;
Begin
  SetBooleanProperty(FCircled,Value);
  if (ParentChart<>nil) then
  with ParentChart do
  for t:=0 to SeriesCount-1 do
    if Self is Series[t].ClassType then
       TCircledSeries(Series[t]).FCircled:=Value;
end;

Procedure TCircledSeries.SetRotationAngle(Value:Integer);
Begin
  if Value<0 then
     Raise PieException.CreateFmt(TeeMsg_Angle,[TeeMsg_Rotation]);
  SetIntegerProperty(FRotationAngle,Value mod 360);
  IRotDegree:=FRotationAngle*PiDegree;
end;

procedure TCircledSeries.SetOtherCustomRadius(IsXRadius:Boolean; Value:Longint);
var t : Integer;
Begin
  if ParentChart<>nil then
  with ParentChart do
  for t:=0 to SeriesCount-1 do
  With TCircledSeries(Series[t]) do
    if Self is ClassType then
       if IsXRadius then FCustomXRadius:=Value
                    else FCustomYRadius:=Value;
end;

Function TCircledSeries.UseAxis:Boolean;
begin
  result:=False;
end;

procedure TCircledSeries.SetCustomXRadius(Value:Longint);
Begin
  SetLongintProperty(FCustomXRadius,Value);
  SetOtherCustomRadius(True,Value);
End;

procedure TCircledSeries.SetCustomYRadius(Value:Longint);
Begin
  SetLongintProperty(FCustomYRadius,Value);
  SetOtherCustomRadius(False,Value);
End;

Procedure TCircledSeries.SetParentProperties(EnableParentProps:Boolean);

  Function AllCircled:Boolean;
  var t:Integer;
  begin
    result:=True;
    With ParentChart do
    for t:=0 to SeriesCount-1 do
      if not (Series[t] is TCircledSeries) then
      begin
        result:=False;
        Exit;
      end;
  end;

begin
  if (ParentChart<>nil) then
  With ParentChart do
  if (not (csDestroying in ComponentState)) then
  begin
    if EnableParentProps and AllCircled then Exit;

    AxisVisible  :=EnableParentProps;
    ClipPoints   :=EnableParentProps;
    View3DWalls  :=EnableParentProps;
    if ParentChart is TCustomChart then
    With TCustomChart(ParentChart) do
    begin
      if Frame<>nil then
         Frame.Visible:=EnableParentProps;
      if BackWall<>nil then
         if EnableParentProps then
            BackWall.Brush.Style:=bsSolid
         else
            BackWall.Brush.Style:=bsClear;
      AllowZoom:=EnableParentProps;
      if AllowZoom then AllowPanning:=pmBoth
                   else AllowPanning:=pmNone;
    end;
    if not Canvas.SupportsFullRotation then
    if View3DOptions<>nil then
    With View3DOptions do
    begin
      Orthogonal:=False;
      Rotation:=360;
      Elevation:=315;
      Perspective:=0;
      Tilt:=0;
    end;
  end;
end;

Procedure TCircledSeries.SetParentChart(Value:TCustomAxisPanel);
Begin
  if Value=nil then SetParentProperties(True);
  if Value<>ParentChart then
  begin
    inherited SetParentChart(Value);
    if ParentChart<>nil then SetParentProperties(False);
  end;
end;

Procedure TCircledSeries.Rotate(Angle:Integer);
Begin
  if (Angle<0) or (Angle>360) then
     Raise PieException.CreateFmt(TeeMsg_Angle,[TeeMsg_Rotation]);
  RotationAngle:=(RotationAngle+Angle) mod 360;
End;

Function TCircledSeries.AssociatedToAxis(Axis:TCustomChartAxis):Boolean;
Begin
  result:=True{False};
end;

Procedure TCircledSeries.AdjustCircleRect;
Begin
  with FCircleRect do
  Begin
    if Odd(Right-Left) then Dec(Right);
    if Odd(Bottom-Top) then Dec(Bottom);
    if (Right-Left)<4 then Right:=Left+4;
    if (Bottom-Top)<4 then Bottom:=Top+4;
    FCircleWidth  :=Right-Left;
    FCircleHeight :=Bottom-Top;
    FCircleXCenter:=(Left+Right) shr 1;
    FCircleYCenter:=(Top+Bottom) shr 1;
  end;
End;

Procedure TCircledSeries.DoBeforeDrawValues;

  Procedure CalcCircledRatio;
  Var tmpRatio : Double;
      Ratio    : Double;
      dif      : Integer;
      tmpH     : Integer;
      tmpW     : Integer;
      DC       : HDC;
  Begin
    Ratio:=1.0*GetSystemMetrics(SM_CXSCREEN)/GetSystemMetrics(SM_CYSCREEN);
    DC:=ParentChart.Canvas.Handle;
    tmpW:=GetDeviceCaps(DC,HORZSIZE);
    tmpH:=GetDeviceCaps(DC,VERTSIZE);
    if tmpH<>0 then
    begin
      tmpRatio:=(1.0*tmpW/tmpH);
      if tmpRatio<>0 then Ratio:=1.0*Ratio/tmpRatio;
    end;
    CalcRadius;
    if Round(Ratio*FYRadius)<FXRadius then
    Begin
      dif:=(FXRadius-Round(Ratio*FYRadius));
      Inc(FCircleRect.Left,Dif);
      Dec(FCircleRect.Right,Dif);
    end
    else
    Begin
      dif:=(FYRadius-Round(1.0*FXRadius/Ratio));
      Inc(FCircleRect.Top,Dif);
      Dec(FCircleRect.Bottom,Dif);
    end;
    AdjustCircleRect;
  end;

  Procedure AdjustCircleMarks;
  Var tmpH     : Integer;
      tmpW     : Integer;
      tmpFrame : Integer;
  begin
    tmpFrame:=Marks.ArrowLength;
    With Marks.Frame do if Visible then Inc(tmpFrame,Width*2);
    With ParentChart do
    Begin
      FontCanvas(Marks.Font);
      tmpH:=Canvas.FontHeight+tmpFrame;
      Inc(FCircleRect.Top,tmpH);
      Dec(FCircleRect.Bottom,tmpH);
      tmpW:=MaxMarkWidth+Canvas.TextWidth(TeeCharForHeight)+tmpFrame;
      Inc(FCircleRect.Left,tmpW);
      Dec(FCircleRect.Right,tmpW);
      AdjustCircleRect;
    end;
  end;

begin
  inherited DoBeforeDrawValues;
  FCircleRect:=ParentChart.ChartRect;
  AdjustCircleRect;
  if Marks.Visible then AdjustCircleMarks;
  if FCircled then CalcCircledRatio;
  CalcRadius;
end;

Procedure TCircledSeries.CalcRadius;
Begin
  if FCustomXRadius<>0 then
  Begin
    FXRadius:=FCustomXRadius;
    FCircleWidth:=2*FXRadius;
  end
  else FXRadius:=FCircleWidth shr 1;

  if FCustomYRadius<>0 then
  begin
    FYRadius:=FCustomYRadius;
    FCircleHeight:=2*FYRadius;
  end
  else FYRadius:=FCircleHeight shr 1;

  With FCircleRect do
  begin
    Left  :=FCircleXCenter-FXRadius;
    Right :=FCircleXCenter+FXRadius;
    Top   :=FCircleYCenter-FYRadius;
    Bottom:=FCircleYCenter+FYRadius;
  end;
end;

procedure TCircledSeries.SetCircleBackColor(Value:TColor);
begin
  SetColorProperty(FCircleBackColor,Value);
end;

Procedure TCircledSeries.AngleToPos( Const Angle,AXRadius,AYRadius:Double;
                                     Var X,Y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var tmpSin : Extended;
    tmpCos : Extended;
Begin
  SinCos(IRotDegree+Angle,tmpSin,tmpCos);
  X:=FCircleXCenter+Round(AXRadius*tmpCos);
  Y:=FCircleYCenter-Round(AYRadius*tmpSin);
end;

Function TCircledSeries.CalcCircleBackColor:TColor;
begin
  result:=FCircleBackColor;
  if result=clTeeColor then result:=(ParentChart as TCustomChart).BackColor;
  if result=clTeeColor then result:=ParentChart.Color;
end;

Procedure TCircledSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                              Var BrushStyle:TBrushStyle);
Begin
  BackColor:=CalcCircleBackColor;
end;

Function TCircledSeries.PointToAngle(x,y:Longint):Double;
Begin
{$IFNDEF D1}
  if (x-FCircleXCenter)=0 then
  begin
    if y>FCircleYCenter then result:=-HalfPi{1.5*pi}
                        else result:=HalfPi;
  end
  else
{$ENDIF}
  if (FYRadius=0) or (FYRadius=0) then result:=0
  else
    result:=ArcTan2( ((FCircleYCenter-y)/FYRadius),
                     ((x-FCircleXCenter)/FXRadius));
  if result<0 then result:=TwoPi+result;
  result:=result-IRotDegree;
  if result<0 then result:=TwoPi+result;
End;

Procedure TCircledSeries.Assign(Source:TPersistent);
begin
  if Source is TCircledSeries then
  With TCircledSeries(Source) do
  begin
    Self.FCircled         := FCircled;
    Self.FRotationAngle   := FRotationAngle;
    Self.FCustomXRadius   := FCustomXRadius;
    Self.FCustomYRadius   := FCustomYRadius;
    Self.FCircleBackColor := FCircleBackColor;
  end;
  inherited Assign(Source);
end;

Procedure TCircledSeries.SetActive(Value:Boolean);
begin
  inherited SetActive(Value);
  SetParentProperties(not Active);
end;

{ TPieAngles }
Function TPieAngles.GetAngle(Index:Integer):TPieAngle;
begin
  result:=TPieAngle(Items[Index]);
end;

{ TExplodedSlices }
Function TExplodedSlices.GetValue(Index:Integer):Integer;
begin
  if (Index<Count) and (Items[Index]<>nil) then result:=Integer(Items[Index])
                                           else result:=0
end;

Procedure TExplodedSlices.SetValue(Index,Value:Integer);
begin
  While Index>=Count do Add(nil);
  if GetValue(Index)<>Value then
  begin
    Items[Index]:=Pointer(Value);
    OwnerSeries.Repaint;
  end;
end;

{ TPieOtherSlice }
Constructor TPieOtherSlice.Create(AOwner:TChartSeries);
begin
  inherited Create;
  FOwner:=AOwner;
  FText:=TeeMsg_PieOther;
  FStyle:=poNone;
  FValue:=0;
end;

Procedure TPieOtherSlice.Assign(Source:TPersistent);
begin
  if Source is TPieOtherSlice then
  With TPieOtherSlice(Source) do
  begin
    Self.FStyle:=FStyle;
    Self.FText :=FText;
    Self.FValue:=FValue;
  end;
end;

procedure TPieOtherSlice.SetStyle(Value:TPieOtherStyle);
begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    FOwner.Repaint;
  end;
end;

procedure TPieOtherSlice.SetText(Const Value:String);
begin
  FOwner.SetStringProperty(FText,Value);
end;

procedure TPieOtherSlice.SetValue(Const Value:Double);
begin
  FOwner.SetDoubleProperty(FValue,Value);
end;

{ TPieSeries }
Const TeePieBelongsToOther = -1;
      TeePieOtherFlag      = MaxLongint;

Constructor TPieSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngles:=TPieAngles.Create;
  FPiePen:=CreateChartPen;
  FExplodedSlice:=TExplodedSlices.Create;
  FExplodedSlice.OwnerSeries:=Self;
  FOtherSlice:=TPieOtherSlice.Create(Self);
  XValues.Name:='';
  YValues.Name:=TeeMsg_ValuesPie;
  Marks.Visible:=True;
  Marks.ArrowLength:=8;
  FDark3D:=True;
  ColorEachPoint:=True;
end;

Procedure TPieSeries.FreePieAngles;
var t : Longint;
begin
  With FAngles do
  begin
    for t:=0 to Count-1 do Angle[t].Free;
    Clear;
  end;
end;

Destructor TPieSeries.Destroy;
begin
  FreePieAngles;
  FAngles.Free;
  FExplodedSlice.Free;
  FExplodedSlice:=nil;
  FOtherSlice.Free;
  FPiePen.Free;
  inherited Destroy;
end;

Function TPieSeries.CalcXPos(ValueIndex:Longint):Integer;
begin
  if XValues.Value[ValueIndex]=TeePieOtherFlag then  { do not try to calc }
     result:=0
  else
     result:=inherited CalcXPos(ValueIndex);
end;

Function TPieSeries.NumSampleValues:Longint;
Begin
  result:=8;
End;

Procedure TPieSeries.ClearLists;
begin
  inherited ClearLists;
  if Assigned(FExplodedSlice) then FExplodedSlice.Clear;
end;

Procedure TPieSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                          Var BrushStyle:TBrushStyle);
Begin
  inherited PrepareLegendCanvas(ValueIndex,BackColor,BrushStyle);
  if FUsePatterns then BrushStyle:=GetDefaultPattern(ValueIndex);
end;

Procedure TPieSeries.GalleryChanged3D(Is3D:Boolean);
begin
  inherited GalleryChanged3D(Is3D);
  DisableRotation;
end;

Procedure TPieSeries.FillSampleValues(NumValues:Longint);
Const PieSampleStr:Array[0..7] of String=
         ( TeeMsg_PieSample1,TeeMsg_PieSample2,TeeMsg_PieSample3,
           TeeMsg_PieSample4,TeeMsg_PieSample5,TeeMsg_PieSample6,
           TeeMsg_PieSample7,TeeMsg_PieSample8);

var t : Longint;
Begin
  Clear;
  for t:=0 to NumValues-1 do
      Add( 1+Random(ChartSamplesMax), { <-- Value }
           PieSampleStr[t mod 8]      { <-- Label }
{$IFNDEF D5},clTeeColor{$ENDIF} );    { <-- Color }
  RefreshSeries;
end;

Function TPieSeries.CalcClickedPie(x,y:Integer):Longint;
Var tmpAngle : Double;
    t        : Longint;
    tmpOffX  : Integer;
    tmpOffY  : Integer;
Begin
  result:=TeeNoPointClicked;
  if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(x,y,0);
  tmpAngle:=PointToAngle(x,y);
  for t:=0 to Count-1 do
  begin
    CalcExplodedOffset(t,tmpOffX,tmpOffY);
    if (Abs(x-FCircleXCenter)<(FXRadius+tmpOffX)) and
       (Abs(y-FCircleYCenter)<(FYRadius+tmpOffY)) then
    With FAngles.Angle[t] do
    if (tmpAngle>=StartAngle) and (tmpAngle<=EndAngle) then
    Begin
      result:=t;
      Exit;
    end;
  end;
end;

Function TPieSeries.Clicked(x,y:Integer):Longint;
begin
  result:=inherited Clicked(x,y);
  if result=TeeNoPointClicked then result:=CalcClickedPie(x,y);
end;

Function TPieSeries.CountLegendItems:Integer;
var t : Integer;
begin
  result:=Count;
  for t:=0 to Count-1 do if BelongsToOtherSlice(t) then Dec(result);
end;

Function TPieSeries.LegendToValueIndex(LegendIndex:Integer):Integer;
var Num : Integer;
    t   : Integer;
begin
  result:=LegendIndex;
  Num:=-1;
  for t:=0 to Count-1 do
  if not BelongsToOtherSlice(t) then
  begin
    Inc(Num);
    if Num=LegendIndex then
    begin
      result:=t;
      break;
    end;
  end;
end;

Function TPieSeries.BelongsToOtherSlice(ValueIndex:Integer):Boolean;
begin
  result:=XValues[ValueIndex]=TeePieBelongsToOther;
end;

Procedure TPieSeries.CalcAngles;
Var tmpSumAbs : Double;
    AcumValue : Double;
    PiPortion : Double;
    t         : Longint;
Begin
  AcumValue:=0;
  tmpSumAbs:=YValues.TotalAbs;
  if tmpSumAbs<>0 then PiPortion:=TwoPi/tmpSumAbs
                  else PiPortion:=0;
  FreePieAngles;
  for t:=0 to Count-1 do
  begin
    FAngles.Add(TPieAngle.Create);
    With FAngles.Angle[t] do
    Begin
      if t=0 then StartAngle:=0 else StartAngle:=FAngles.Angle[t-1].EndAngle;
      if tmpSumAbs<>0 then
      Begin
        if not BelongsToOtherSlice(t) then
           AcumValue:=AcumValue+Abs(YValue[t]);
        if AcumValue=tmpSumAbs then EndAngle:=TwoPi
                               else EndAngle:=AcumValue*PiPortion;
        { prevent small pie sectors }
        if (EndAngle-StartAngle)>TwoPi then EndAngle:=StartAngle+TwoPi;
      end
      else EndAngle:=TwoPi;
      MidAngle:=(StartAngle+EndAngle)*0.5;
    end;
  end;
end;

Procedure TPieSeries.CalcExplodedRadius(ValueIndex:Integer; Var AXRadius,AYRadius:Integer);
var tmpExp : Double;
begin
  tmpExp:=1.0+FExplodedSlice.Value[ValueIndex]*0.01;
  AXRadius:=Round(FXRadius*tmpExp);
  AYRadius:=Round(FYRadius*tmpExp);
end;

Procedure TPieSeries.DrawMark( ValueIndex:Longint; Const St:String;
                               APosition:TSeriesMarkPosition);
var tmpXRadius : Integer;
    tmpYRadius : Integer;
Begin
  if not BelongsToOtherSlice(ValueIndex) then
  begin
    With ParentChart,FAngles.Angle[ValueIndex] do
    begin
      CalcExplodedRadius(ValueIndex,tmpXRadius,tmpYRadius);

      With APosition do
      begin
        AngleToPos( MidAngle,
                    tmpXRadius+Canvas.TextWidth(TeeCharForHeight)+Marks.ArrowLength,
                    tmpYRadius+Canvas.FontHeight+Marks.ArrowLength,
                    ArrowTo.X,ArrowTo.Y );

        AngleToPos( MidAngle,tmpXRadius,tmpYRadius,ArrowFrom.X,ArrowFrom.Y );

        With ArrowTo do
        begin
          if X>FCircleXCenter then LeftTop.X:=X
                              else LeftTop.X:=X-Width;
          if Y>FCircleYCenter then LeftTop.Y:=Y
                              else LeftTop.Y:=Y-Height;
        end;
      end;
    end;
    Marks.ZPosition:=EndZ;
    inherited DrawMark(ValueIndex,St,APosition);
  end;
end;

procedure TPieSeries.DrawAllValues;
Var SortedSlice:Array[0..1024] of Integer;
    j : Integer;
    x : Integer;

  Function CompareSliceAngle(A,B:Integer):Integer;

    Function GetAngleSlice(Index:Integer):Double;
    begin
      result:=FAngles.Angle[Index].MidAngle+IRotDegree;
      if result>TwoPi then result:=result-TwoPi;
      if result>HalfPi then
      begin
        result:=result-HalfPi;
        if result>Pi then result:=TwoPi-result;
      end
      else result:=HalfPi-result;
    end;

  Var tmpA : Double;
      tmpB : Double;
  begin
    tmpA:=GetAngleSlice(SortedSlice[A]);
    tmpB:=GetAngleSlice(SortedSlice[B]);
    if tmpA<tmpB then result:=-1 else
    if tmpA>tmpB then result:= 1 else result:= 0;
  end;

  procedure SortSlices(l,r:longint);
  var i : Longint;
  begin
    i:=l;
    j:=r;
    x:=(i+j) shr 1;
    while i<j do
    begin
      while CompareSliceAngle(i,x)<0 do Inc(i);
      while CompareSliceAngle(x,j)<0 do Dec(j);
      if i<j then
      begin
        SwapInteger(SortedSlice[i],SortedSlice[j]);
        if i=x then x:=j else if j=x then x:=i;
      end;
      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;
    if l<j then SortSlices(l,j);
    if i<r then SortSlices(i,r);
  end;

var t                : Integer;
    tmpCount         : Integer;
    MaxExploded      : Integer;
    MaxExplodedIndex : Integer;
    tmpOffX          : Integer;
    tmpOffY          : Integer;
begin
  if FExplodeBiggest>0 then CalcExplodeBiggest;
  MaxExplodedIndex:=-1;
  MaxExploded:=0;
  tmpCount:=Count-1;
  { calc biggest exploded index }
  for t:=0 to tmpCount do
  if FExplodedSlice.Value[t]>MaxExploded then
  begin
    MaxExploded:=Round(FExplodedSlice.Value[t]);
    MaxExplodedIndex:=t;
  end;
  { calc each slice angles }
  CalcAngles;
  { adjust circle rectangle }
  IsExploded:=MaxExplodedIndex<>-1;
  if IsExploded then
  begin
    CalcExplodedOffset(MaxExplodedIndex,tmpOffX,tmpOffY);
    InflateRect(FCircleRect,-Abs(tmpOffX) div 2,-Abs(tmpOffY) div 2);
    AdjustCircleRect;
    CalcRadius;
  end;
  { start xy pos }
  AngleToPos(0,FXRadius,FYRadius,iniX,iniY);

  { exploded slices drawing order... }
  if ParentChart.View3D and IsExploded then
  begin
    for t:=0 to tmpCount do SortedSlice[t]:=t;
    SortSlices(0,tmpCount);
    for t:=0 to tmpCount do DrawValue(SortedSlice[t]);
  end
  else inherited DrawAllValues;
end;

Procedure TPieSeries.DrawPie(ValueIndex:Integer);
var tmpOffX : Integer;
    tmpOffY : Integer;
Begin
  ParentChart.Canvas.AssignVisiblePen(PiePen);
  CalcExplodedOffset(ValueIndex,tmpOffX,tmpOffY);
  With FAngles.Angle[ValueIndex] do
  with ParentChart,Canvas do { Draw pie slice }
  if View3D then
  begin
    if SupportsFullRotation then tmpOffX:=-tmpOffX;
    Pie3D( FCircleXCenter+tmpOffX,
           FCircleYCenter-tmpOffY,
           FXRadius,FYRadius,
           StartZ,EndZ,
           StartAngle+IRotDegree,EndAngle+IRotDegree,
           FDark3D,
           IsExploded)
  end
  else
  begin
     AngleToPos(EndAngle,FXRadius,FYRadius,endX,endY);
     if ((IniX<>EndX) or (IniY<>EndY)) or (Count=1) or
       ( (Count>1) and (EndAngle-StartAngle>1))  then { bug win32 api }
     begin
       With FCircleRect do
         Pie( Left + tmpOffX,Top   -tmpOffY,
              Right+ tmpOffX,Bottom-tmpOffY,
              IniX + tmpOffX,IniY  -tmpOffY,
              EndX + tmpOffX,EndY  -tmpOffY);
       IniX:=EndX;
       IniY:=EndY;
     end;
  end;
end;

Procedure TPieSeries.CalcExplodedOffset( ValueIndex:Integer;
                                         Var OffsetX,OffsetY:Integer);
var tmpExp : Double;
    tmpSin : Extended;
    tmpCos : Extended;
begin
  OffsetX :=0;
  OffsetY :=0;
  if IsExploded then
  begin
    tmpExp:=FExplodedSlice.Value[ValueIndex];
    if tmpExp>0 then
    begin { Apply exploded % to radius }
      SinCos(FAngles.Angle[ValueIndex].MidAngle+IRotDegree,tmpSin,tmpCos);
      tmpExp:=tmpExp*0.01;
      OffsetX:=Round((FXRadius*tmpExp)*tmpCos);
      OffsetY:=Round((FYRadius*tmpExp)*tmpSin);
    end;
  end;
end;

Procedure TPieSeries.CalcExplodeBiggest;
var tmp : Longint;
begin
  With YValues do tmp:=Locate(MaxValue);
  if tmp<>-1 then FExplodedSlice.Value[tmp]:=FExplodeBiggest;
end;

procedure TPieSeries.SetExplodeBiggest(Value:Integer);
begin
  SetIntegerProperty(FExplodeBiggest,Value);
  CalcExplodeBiggest;
end;

procedure TPieSeries.SetPiePen(Value:TChartPen);
begin
  FPiePen.Assign(Value);
end;

procedure TPieSeries.SetOtherSlice(Value:TPieOtherSlice);
begin
  FOtherSlice.Assign(Value);
end;

Procedure TPieSeries.DrawValue(ValueIndex:Longint);
var tmpStyle : TBrushStyle;
Begin
  if (FCircleWidth>4) and (FCircleHeight>4) then
  if not BelongsToOtherSlice(ValueIndex) then
  begin
    { Slice pattern }
    if FUsePatterns then tmpStyle:=GetDefaultPattern(ValueIndex)
                    else tmpStyle:=bsSolid;
    { Set slice back color }
    ParentChart.SetBrushCanvas(ValueColor[ValueIndex],tmpStyle,CalcCircleBackColor);
    DrawPie(ValueIndex);
  end;
end;

procedure TPieSeries.SetUsePatterns(Value:Boolean);
Begin
  SetBooleanProperty(FUsePatterns,Value);
end;

Function TPieSeries.GetEditorClass:String;
Begin
  result:='TPieSeriesEditor'; { <-- dont translate ! }
end;

Function TPieSeries.GetPieValues:TChartValueList;
Begin
  result:=YValues;
end;

Procedure TPieSeries.SetPieValues(Value:TChartValueList);
Begin
  SetYValues(Value);
end;

Procedure TPieSeries.SetDark3d(Value:Boolean);
Begin
  SetBooleanProperty(FDark3d,Value);
End;

Function TPieSeries.MaxXValue:Double;
Begin
  result:=GetHorizAxis.Maximum;
End;

Function TPieSeries.MinXValue:Double;
Begin
  result:=GetHorizAxis.Minimum;
End;

Function TPieSeries.MaxYValue:Double;
Begin
  result:=GetVertAxis.Maximum;
End;

Function TPieSeries.MinYValue:Double;
Begin
  result:=GetVertAxis.Minimum;
End;

Procedure TPieSeries.DisableRotation;
begin
  With ParentChart.View3DOptions do
  begin
    Orthogonal:=False;
    Rotation:=0;
    Elevation:=305;
  end;
end;

Procedure TPieSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(8);
  ParentChart.Chart3DPercent:=75;
  Circled:=not ParentChart.View3D;
  DisableRotation;
  ColorEachPoint:=IsEnabled;
end;

Procedure TPieSeries.Assign(Source:TPersistent);
begin
  if Source is TPieSeries then
  With TPieSeries(Source) do
  begin
    Self.FDark3d        :=FDark3D;
    Self.FUsePatterns   :=FUsePatterns;
    Self.FPiePen.Assign(FPiePen);
    Self.FExplodeBiggest:=FExplodeBiggest;
    Self.FOtherSlice.Assign(FOtherSlice);
  end;
  inherited Assign(Source);
  ColorEachPoint:=True;
end;

Function TPieSeries.AddPie( Const AValue:Double;
                            Const ALabel:String; AColor:TColor):Longint;
begin
  result:=Add(AValue,ALabel,AColor);
end;

procedure TPieSeries.DoBeforeDrawChart;
var t        : Integer;
    tmp      : Double;
    tmpValue : Double;
begin
  { re-order values }
  With PieValues do if Order<>loNone then Sort;
  { remove "other" slice, if exists... }
  for t:=0 to Count-1 do
  if XValues.Value[t]=TeePieOtherFlag then
  begin
    Delete(t);
    Break;
  end;
  { reset X order... }
  XValues.FillSequence;
  { calc "Other" slice... }
  if FOtherSlice.Style<>poNone then
  Begin
    tmpValue:=0;
    for t:=0 to Count-1 do
    begin
      tmp:=YValues.Value[t];
      if FOtherSlice.Style=poBelowPercent then tmp:=tmp*100.0/YValues.TotalAbs;
      if tmp<FOtherSlice.Value then
      begin
        tmpValue:=tmpValue+YValues.Value[t];
        XValues.Value[t]:=TeePieBelongsToOther; { <-- belongs to "other" }
      end;
    end;
    { Add "Other" slice }
    if tmpValue<>0 then
    begin
      AddXY(TeePieOtherFlag,tmpValue,FOtherSlice.Text {$IFNDEF D5},clTeeColor{$ENDIF});
      With YValues do TotalABS:=TotalABS-tmpValue; { reset Y total }
    end;
  end;
end;

procedure TPieSeries.SwapValueIndex(a,b:Longint);
begin
  inherited SwapValueIndex(a,b);
  With FExplodedSlice do
  begin
    While Self.Count>Count do SetValue(Count,0);
    Exchange(a,b);
  end;
end;

{ TFastLineSeries }
Constructor TFastLineSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FLinePen:=CreateChartPen;
  AllowSinglePoint:=False;
  DrawBetweenPoints:=True;
  FAutoRepaint:=True;
End;

Destructor TFastLineSeries.Destroy;
Begin
  FLinePen.Free;
  inherited Destroy;
End;

Procedure TFastLineSeries.SetLinePen(Value:TChartPen);
Begin
  FLinePen.Assign(Value);
  SeriesColor:=FLinePen.Color;
End;

Procedure TFastLineSeries.NotifyNewValue(Sender:TChartSeries; ValueIndex:Longint);
{$IFNDEF TEEOCX}
Var tmpIndex : Longint;
{$ENDIF}
begin
  if AutoRepaint then inherited NotifyNewValue(Sender,ValueIndex)
  {$IFNDEF TEEOCX}
  else
  begin
    With ParentChart,Canvas do
    begin
      Pen.Assign(FLinePen);
      if ValueIndex=0 then tmpIndex:=0
                      else tmpIndex:=Pred(ValueIndex);
      if View3D then
         MoveTo3D(CalcXPos(tmpIndex),CalcYPos(tmpIndex),MiddleZ)
      else
         MoveTo(CalcXPos(tmpIndex),CalcYPos(tmpIndex));
    end;
    DrawValue(ValueIndex);
  end;
  {$ENDIF}
end;

Function TFastLineSeries.GetEditorClass:String;
Begin
  result:='TFastLineSeriesEditor'; { <-- dont translate ! }
End;

procedure TFastLineSeries.PrepareCanvas;
Begin
  With ParentChart,Canvas do
  begin
    FLinePen.Color:=SeriesColor;
    AssignVisiblePen(FLinePen);
    CheckPenWidth(Pen);
    Brush.Style:=bsClear;
  end;
end;

procedure TFastLineSeries.DrawAllValues;
Begin
  PrepareCanvas;
  inherited DrawAllValues;
End;

procedure TFastLineSeries.DrawValue(ValueIndex:Longint);
Var X     : Longint;
    Y     : Longint;
    tmpX  : Longint;
    tmpY  : Longint;
    tmp3D : Boolean;
Begin
  X:=CalcXPos(ValueIndex);
  Y:=CalcYPos(ValueIndex);
  tmp3D:=ParentChart.View3D;
  With ParentChart.Canvas do
  begin
    if ValueIndex=FirstValueIndex then
       if ValueIndex>0 then
       begin
          tmpX:=CalcXPos(ValueIndex-1);
          tmpY:=CalcYPos(ValueIndex-1);
          if tmp3D then LineWithZ(tmpX,tmpY,X,Y,MiddleZ)
                   else Line(tmpX,tmpY,X,Y)
       end
       else
       if tmp3D then MoveTo3D(X,Y,MiddleZ) else MoveTo(X,Y)
    else
       if (X<>OldX) or (Y<>OldY) then
          if tmp3D then LineTo3D(X,Y,MiddleZ) else LineTo(X,Y)
       else
          Exit;
  end;
  OldX:=X;
  OldY:=Y;
End;

Procedure TFastLineSeries.SetSeriesColor(AColor:TColor);
begin
  inherited SetSeriesColor(AColor);
  FLinePen.Color:=AColor;
end;

Procedure TFastLineSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
begin
  PrepareCanvas;
  With Rect do ParentChart.Canvas.DoHorizLine(Left,Right,(Top+Bottom) shr 1);
end;

Procedure TFastLineSeries.Assign(Source:TPersistent);
begin
  if Source is TFastLineSeries then
  With TFastLineSeries(Source) do
  begin
    Self.FAutoRepaint:=FAutoRepaint;
    Self.FLinePen.Assign(FLinePen);
  end;
  inherited Assign(Source);
end;

Function TFastLineSeries.Clicked(x,y:Integer):Longint;
var t    : Longint;
    OldX : Longint;
    OldY : Longint;
    tmpX : Longint;
    tmpY : Longint;
    P    : TPoint;
begin
  result:=TeeNoPointClicked;
  if (FirstValueIndex>-1) and (LastValueIndex>-1) then
  begin
    if (ParentChart<>nil) then ParentChart.Canvas.Calculate2DPosition(X,Y,MiddleZ);
    OldX:=0;
    OldY:=0;
    P.X:=X;
    P.Y:=Y;
    for t:=FirstValueIndex to LastValueIndex do
    begin
      tmpX:=CalcXPos(t);
      tmpY:=CalcYPos(t);
      if (tmpX=X) and (tmpY=Y) then { clicked right on point }
      begin
        result:=t;
        break;
      end
      else
      if (t>FirstValueIndex) and PointInLine(P,tmpX,tmpY,OldX,OldY) then
      begin
        result:=t-1;
        break;
      end;
      OldX:=tmpX;
      OldY:=tmpY;
    end;
  end;
end;

Procedure TFastLineSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                    APosition:TSeriesMarkPosition);
var tmp : Integer;
begin
  tmp:=Marks.ArrowLength;
  With APosition do
  begin
    Dec(ArrowTo.Y,tmp);
    Dec(LeftTop.Y,tmp);
  end;
  inherited DrawMark(ValueIndex,St,APosition);
end;

Procedure RegisterStandardSeries;
begin
  RegisterTeeSeries(TLineSeries,    TeeMsg_GalleryLine,    TeeMsg_GalleryStandard,2);
  RegisterTeeSeries(TBarSeries,     TeeMsg_GalleryBar,     TeeMsg_GalleryStandard,2);
  RegisterTeeSeries(THorizBarSeries,TeeMsg_GalleryHorizBar,TeeMsg_GalleryStandard,2);
  RegisterTeeSeries(TAreaSeries,    TeeMsg_GalleryArea,    TeeMsg_GalleryStandard,2);
  RegisterTeeSeries(TPointSeries,   TeeMsg_GalleryPoint,   TeeMsg_GalleryStandard,2);
  RegisterTeeSeries(TPieSeries,     TeeMsg_GalleryPie,     TeeMsg_GalleryStandard,1);
  RegisterTeeSeries(TFastLineSeries,TeeMsg_GalleryFastLine,TeeMsg_GalleryStandard,2);
end;

initialization
  RegisterStandardSeries;
end.


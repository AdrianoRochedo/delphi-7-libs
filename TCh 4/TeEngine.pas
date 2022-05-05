{****************************************}
{     TeeChart Pro Charting Library      }
{  For Delphi 1,2,3,4 & C++ Builder 1&3  }
{ Copyright (c) 1995-98 by David Berneda }
{         All Rights Reserved            }
{****************************************}
{$R-,Q-,S-,I-,A+,U-,X+,B-,W-,P-,V-,D+}
{$I teedefs.inc}
{$IFDEF D1}
{$L-}
{$ENDIF}

Unit TeEngine;

interface

uses SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
     Forms, ExtCtrls, TeeProcs, TeCanvas;

Const ChartMarkColor   = $80FFFF;  { default Series Mark back color }
      {$IFDEF D1}
      MinAxisIncrement = 0.00001;
      MinAxisRange     = 0.001;
      {$ELSE}
      MinAxisIncrement = 0.000000000001;
      MinAxisRange     = 0.0000000001;
      {$ENDIF}

      TeeAllValues     = -1;

      ChartSamplesMax  = 1000;

      TeeDrawAxisBeforeSeries: Boolean = True;

      TeeAutoZOrder     = -1;
      TeeNoPointClicked = -1;

      clTeeColor       = TColor(clScrollBar or $80000000);
      {$IFDEF D1}
      clNone           = -2;
      {$ENDIF}

      TeeCheckMarkArrowColor:Boolean=True;  { when False, Mark arrow pen
                                              color is not altered }

Type
  { Same like TBrush but with clTeeColor default instead of clWhite }
  TChartBrush=class(TBrush)
  public
    Constructor Create(OnChangeEvent:TNotifyEvent);
  published
    property Color default clTeeColor;
  end;

                                        { Example: }
  TSeriesMarksStyle=( smsValue,             { 1234 }
                      smsPercent,           { 12 % }
                      smsLabel,             { Cars }
                      smsLabelPercent,      { Cars 12 % }
                      smsLabelValue,        { Cars 1234 }
                      smsLegend,            { (Legend.Style) }
                      smsPercentTotal,      { 12 % of 1234 }
                      smsLabelPercentTotal, { Cars 12 % of 1234 }
                      smsXValue);           { 21/6/1996 }

  PChartValue=^TChartValue;
  TChartValue=Double;

  TChartListOrder=(loNone,loAscending,loDescending);

  TChartSeries=class;

  TChartValueList=class(TPersistent)
  private
    FDateTime    : Boolean;
    FList        : TList;
    FMaxValue    : Double;
    FMinValue    : Double;
    FMultiplier  : Double;
    FName        : String;
    FOrder       : TChartListOrder;
    FOwner       : TChartSeries;
    FTempValue   : Double;
    FTotal       : Double;
    FTotalABS    : Double;
    FValueSource : String;
    { internal }
    Function GetMaxValue:Double;
    Function GetMinValue:Double;
    Function GetTotal:Double;
    Function GetTotalABS:Double;
    procedure SetDateTime(Value:Boolean);
    Procedure SetMultiplier(Const Value:Double);
    Procedure SetValueSource(Const Value:String);
  protected
    IDirtyStats  : Boolean;
    Function GetValue(ValueIndex:Longint):Double; virtual;
    Procedure SetValue(ValueIndex:Longint; Const AValue:Double); virtual;
    Procedure ClearValues; virtual;
    Procedure RecalcStats;
    Function AddChartValue(Const Value:TChartValue):Longint; virtual;
    Procedure InsertChartValue(ValueIndex:Longint; Const Value:TChartValue); virtual;
  public
    Constructor Create(AOwner:TChartSeries; Const AName:String); virtual;
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Function Count:Longint; virtual;
    Procedure Delete(ValueIndex:Longint); virtual;
    Procedure FillSequence;
    Function First:Double;
    Function Last:Double;
    Function Locate(Const Value:Double):Longint;
    Procedure Scroll; virtual;
    Procedure Sort;

    property MaxValue:Double read GetMaxValue;
    property MinValue:Double read GetMinValue;
    property Owner:TChartSeries read FOwner;
    property TempValue:Double read FTempValue write FTempValue;
    property Total:Double read GetTotal;
    property TotalABS:Double read GetTotalABS write FTotalABS; { <--write bcos Pie }
    property Value[Index:Longint]:Double read GetValue write SetValue; default;
  published
    property DateTime:Boolean read FDateTime write SetDateTime;
    property Name:String read FName write FName;
    property Multiplier:Double read FMultiplier write SetMultiplier;
    property Order:TChartListOrder read FOrder write FOrder;
    property ValueSource:String read FValueSource write SetValueSource;
  end;

  TCustomAxisPanel=class;

  TChartSeriesList=class(TList)
  private
    FOwner : TCustomAxisPanel;
    procedure SetSeries(Index:Integer; Series:TChartSeries);
    function GetSeries(Index:Integer):TChartSeries;
  public
    Function CountActive:Longint;
    property Owner:TCustomAxisPanel read FOwner;
    property Series[Index:Integer]:TChartSeries read GetSeries write SetSeries; {$IFNDEF D1} default; {$ENDIF}
  end;

  TLegendTextStyle=( ltsPlain,ltsLeftValue,ltsRightValue,
                     ltsLeftPercent,ltsRightPercent,ltsXValue);

  TAxisLabelStyle=(talAuto,talNone,talValue,talMark,talText);

  AxisException=class(Exception);

  TCustomChartAxis=class;

  TChartAxisTitle=class(TChartFontObject)
  private
    FAngle   : Integer;
    FAxis    : TCustomChartAxis;
    FCaption : String;
    Function IsAngleStored:Boolean;
    Procedure SetAngle(Value:Integer);
    Procedure SetCaption(Const Value:String);
  public
    Procedure Assign(Source:TPersistent); override;
  published
    property Angle:Integer read FAngle write SetAngle stored IsAngleStored;
    property Caption:String read FCaption write SetCaption;
    property Font;
  end;

  TCustomChartAxis=class(TPersistent)
  private
    { scales }
    FAutomatic        : Boolean;
    FAutomaticMaximum : Boolean;
    FAutomaticMinimum : Boolean;
    FDesiredIncrement : Double;
    FMaximumValue     : Double;
    FMinimumValue     : Double;
    FLogarithmic      : Boolean;
    FLogarithmicBase  : Integer;

    { axis }
    FAxis             : TChartAxisPen;
    FPosAxis          : Integer;

    { title }
    FAxisTitle        : TChartAxisTitle;
    FTitleSize        : Integer;
    FPosTitle         : Integer;

    { grid }
    FGrid             : TDottedGrayPen;
    FGridCentered     : Boolean;

    { labels }
    FLabels           : Boolean;
    FLabelsAngle      : Integer;
    FLabelsFont       : TFont;
    FLabelsOnAxis     : Boolean;
    FLabelsSeparation : Integer;
    FLabelsSize       : Integer;
    FLabelStyle       : TAxisLabelStyle;
    FPosLabels        : Integer;
    FAxisValuesFormat : String;
    FDateTimeFormat   : String;
    FExactDateTime    : Boolean;
    FRoundFirstLabel  : Boolean;
    FLabelsMultiLine  : Boolean;

    { ticks }
    FMinorTickCount   : Integer;
    FMinorTickLength  : Integer;
    FMinorTicks       : TDarkGrayPen;
    FTicks            : TDarkGrayPen;
    FTicksInner       : TDarkGrayPen;
    FTickInnerLength  : Integer;
    FTickLength       : Integer;
    FTickOnLabelsOnly : Boolean;

    { other }
    FInverted         : Boolean;
    FHorizontal       : Boolean;
    FOtherSide        : Boolean;
    FParentChart      : TCustomAxisPanel;
    FVisible          : Boolean;
    FStartPosition    : Double;
    FEndPosition      : Double;
    FPositionPercent  : Double;

    { internal }
    IMaximum          : Double;
    IMinimum          : Double;
    IRange            : Double;
    IAxisDateTime     : Boolean;
    ICenterPos        : Integer;
    IDefaultTitleAngle: Integer;
    Function CalcDateTimeIncrement(MaxNumLabels:Longint):Double;
    Function CalcLabelsIncrement(MaxNumLabels:Longint):Double;
    Function GetRectangleEdge(Const R:TRect):Integer;
    Function ApplyPosition(APos:Integer; Const R:TRect):Integer;
    Function InternalCalcLabelsIncrement(MaxNumLabels:Longint):Double;
    function IsAxisValuesFormatStored:Boolean;
    Function IsMaxStored:Boolean;
    Function IsMinStored:Boolean;
    Procedure SetAutomatic(Value:Boolean);
    Procedure SetAutomaticMinimum(Value:Boolean);
    Procedure SetAutomaticMaximum(Value:Boolean);
    Procedure SetAxis(Value:TChartAxisPen);
    procedure SetAxisTitle(Value:TChartAxisTitle);
    Procedure SetDateTimeFormat(Const Value:String);
    Procedure SetDesiredIncrement(Const Value:Double);
    Procedure SetExactDateTime(Value:Boolean);
    Procedure SetGrid(Value:TDottedGrayPen);
    procedure SetGridCentered(Value:Boolean);
    Procedure SetInverted(Value:Boolean);
    Procedure SetLabels(Value:Boolean);
    Procedure SetLabelsFont(Value:TFont);
    Function IsFontStored:Boolean;
    Procedure SetLabelStyle(Value:TAxisLabelStyle);
    Procedure SetLabelsOnAxis(Value:Boolean);
    Procedure SetLabelsAngle(Value:Integer);
    Procedure SetLabelsMultiLine(Value:Boolean);
    Procedure SetLabelsSeparation(Value:Integer);
    Procedure SetLabelsSize(Value:Integer);
    Procedure SetLogarithmic(Value:Boolean);
    Procedure SetLogarithmicBase(Value:Integer);
    Procedure SetMaximum(Const Value:Double);
    Procedure SetMinimum(Const Value:Double);
    Procedure SetMinorTickCount(Value:Integer);
    Procedure SetMinorTickLength(Value:Integer);
    Procedure SetMinorTicks(Value:TDarkGrayPen);
    procedure SetStartPosition(Const Value:Double);
    procedure SetEndPosition(Const Value:Double);
    procedure SetPositionPercent(Const Value:Double);
    procedure SetRoundFirstLabel(Value:Boolean);
    Procedure SetTickLength(Value:Integer);
    Procedure SetTickInnerLength(Value:Integer);
    Procedure SetTicks(Value:TDarkGrayPen);
    Procedure SetTicksInner(Value:TDarkGrayPen);
    procedure SetTickOnLabelsOnly(Value:Boolean);
    Procedure SetTitleSize(Value:Integer);
    Procedure SetValuesFormat(Const Value:String);
    Procedure SetVisible(Value:Boolean);

    Function InternalCalcLogPosValue(IsX:Boolean; Const Value:Double):Longint;
    Function InternalCalcDepthPosValue(Const Value:Double):Longint;
    Function InternalCalcLog(Var LogMax,LogMin:Double):Double;
    Function InternalCalcPosValue(Const Value:Double; FromEnd:Boolean):Longint;
    {$IFNDEF D1}
    Function IsPosStored:Boolean;
    Function IsStartStored:Boolean;
    Function IsEndStored:Boolean;
    {$ENDIF}
  protected
    Procedure DrawAxisLine(AWallSize:Integer); virtual; abstract;
  public
    IStartPos         : Integer;
    IEndPos           : Integer;
    IAxisSize         : Integer;
    IsDepthAxis       : Boolean;
    Procedure InternalCalcPositions;
    Function InternalCalcSize( tmpFont:TFont;
                               tmpAngle:Longint;
                               Const tmpText:String;
                               tmpSize:Longint):Longint;
    Procedure InternalSetInverted(Value:Boolean);
    Procedure CalcRect(Var R:TRect; InflateChartRectangle:Boolean);
    Procedure InternalSetMaximum(Const Value:Double);
    Procedure InternalSetMinimum(Const Value:Double);
    Function SizeTickAxis:Integer;
    Function SizeTitle:Integer;
    Function SizeLabels:Integer;
    Procedure RecalcSizeCenter;

    Constructor Create(AOwner: TCustomAxisPanel);
    Destructor Destroy; override;

    Function MaxLabelsWidth:Longint;
    Function LabelWidth(Const Value:Double):Longint;
    Function LabelHeight(Const Value:Double):Longint;
    Function LabelValue(Const Value:Double):String;
    Function CalcLabelStyle:TAxisLabelStyle;
    Procedure DrawTitle(x,y:Longint);
    procedure DrawAxisLabel(x,y,Angle:Integer; Const St:String);

    Function CalcPosValue(Const Value:Double):Longint;
    Function CalcXPosValue(Const Value:Double):Longint;
    Function CalcYPosValue(Const Value:Double):Longint;

    Function CalcSizeValue(Const Value:Double):Longint;

    Function CalcPosPoint(Value:Longint):Double;

    Procedure CustomDrawMinMax( APosLabels,
                                APosTitle,
                                APosAxis:Integer;
                                GridVisible:Boolean;
                                Const AMinimum,AMaximum,AIncrement:Double);
    Procedure CustomDraw( APosLabels,APosTitle,APosAxis:Integer;
                          GridVisible:Boolean);
    Procedure CustomDrawStartEnd( APosLabels,APosTitle,APosAxis:Integer;
                                  GridVisible:Boolean; AStartPos,AEndPos:Integer);
    Procedure CustomDrawMinMaxStartEnd( APosLabels,
                                        APosTitle,
                                        APosAxis:Integer;
                                        GridVisible:Boolean;
                                        Const AMinimum,AMaximum,AIncrement:Double;
                                        AStartPos,AEndPos:Integer);

    Procedure Draw(CalcPosAxis:Boolean);
    procedure Assign(Source: TPersistent); override;
    Procedure AdjustMaxMin;
    Procedure CalcMinMax(Var AMin,AMax:Double);
    Procedure Scroll(Const Offset:Double; CheckLimits:Boolean);
    Function Clicked(x,y:Integer):Boolean;
    Function IsDateTime:Boolean;
    Procedure SetMinMax(Const AMin,AMax:Double);
    Function CalcXYIncrement(MaxLabelSize:Integer):Double;
    Function CalcIncrement:Double;
    Procedure AdjustMaxMinRect(Const Rect:TRect);
    Procedure IncDecDateTime( Increment:Boolean;
                              Var Value:Double;
                              Const AnIncrement:Double;
                              tmpWhichDateTime:TDateTimeStep );

   { public }
    property Horizontal : Boolean read FHorizontal write FHorizontal;
    property OtherSide  : Boolean read FOtherSide write FOtherSide;
    property PosAxis    : Integer read FPosAxis;
    property PosLabels  : Integer read FPosLabels;
    property PosTitle   : Integer read FPosTitle;
    property ParentChart: TCustomAxisPanel read FParentChart;

    { to be published }
    property Automatic:Boolean read FAutomatic write SetAutomatic default True;
    property AutomaticMaximum:Boolean read FAutomaticMaximum write SetAutomaticMaximum default True;
    property AutomaticMinimum:Boolean read FAutomaticMinimum write SetAutomaticMinimum default True;
    property Axis:TChartAxisPen read FAxis write SetAxis;
    property AxisValuesFormat:String read FAxisValuesFormat
                                     write SetValuesFormat stored IsAxisValuesFormatStored;
    property DateTimeFormat:String read FDateTimeFormat write SetDateTimeFormat;
    property ExactDateTime:Boolean read FExactDateTime write SetExactDateTime default True;
    property Grid:TDottedGrayPen read FGrid write SetGrid;
    property GridCentered:Boolean read FGridCentered write SetGridCentered default False;
    property Increment:Double read FDesiredIncrement write SetDesiredIncrement;
    property Inverted:Boolean read FInverted write SetInverted default False;

    property Labels:Boolean read FLabels write SetLabels default True;
    property LabelsAngle:Integer read FLabelsAngle write SetLabelsAngle default 0;
    property LabelsFont:TFont read FLabelsFont write SetLabelsFont stored IsFontStored;
    property LabelsMultiLine:Boolean read FLabelsMultiLine write SetLabelsMultiLine default False;
    property LabelsOnAxis:Boolean read FLabelsOnAxis write SetLabelsOnAxis default True;
    property LabelsSeparation:Integer read FLabelsSeparation
                                      write SetLabelsSeparation default 10;
    property LabelsSize:Integer read FLabelsSize write SetLabelsSize default 0;
    property LabelStyle:TAxisLabelStyle read FLabelStyle write SetLabelStyle default talAuto;

    property Logarithmic:Boolean read FLogarithmic write SetLogarithmic default False;
    property LogarithmicBase:Integer read FLogarithmicBase write SetLogarithmicBase default 10;
    property Maximum:Double read FMaximumValue write SetMaximum stored IsMaxStored;
    property Minimum:Double read FMinimumValue write SetMinimum stored IsMinStored;
    property MinorTickCount:Integer read FMinorTickCount write SetMinorTickCount default 3;
    property MinorTickLength:Integer read FMinorTickLength write SetMinorTickLength default 2;
    property MinorTicks:TDarkGrayPen read FMinorTicks write SetMinorTicks;
    property StartPosition:Double read FStartPosition write SetStartPosition
                                    {$IFNDEF D1}stored IsStartStored{$ENDIF};
    property EndPosition:Double read FEndPosition write SetEndPosition
                                    {$IFNDEF D1}stored IsEndStored{$ENDIF};
    property PositionPercent:Double read FPositionPercent write SetPositionPercent
                                    {$IFNDEF D1}stored IsPosStored{$ENDIF};
    property RoundFirstLabel:Boolean read FRoundFirstLabel write SetRoundFirstLabel default True;
    property TickInnerLength:Integer read FTickInnerLength write SetTickInnerLength default 0;
    property TickLength:Integer read FTickLength write SetTickLength default 4;
    property Ticks:TDarkGrayPen read FTicks write SetTicks;
    property TicksInner:TDarkGrayPen read FTicksInner write SetTicksInner;
    property TickOnLabelsOnly:Boolean read FTickOnLabelsOnly write SetTickOnLabelsOnly default True;
    property Title:TChartAxisTitle read FAxisTitle write SetAxisTitle;
    property TitleSize:Integer read FTitleSize write SetTitleSize default 0;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TChartAxis=class(TCustomChartAxis)
  protected
    Procedure DrawAxisLine(AWallSize:Integer); override;
  published
    property Automatic;
    property AutomaticMaximum;
    property AutomaticMinimum;
    property Axis;
    property AxisValuesFormat;
    property DateTimeFormat;
    property ExactDateTime;
    property Grid;
    property GridCentered;
    property Increment;
    property Inverted;
    property Labels;
    property LabelsAngle;
    property LabelsFont;
    property LabelsMultiLine;
    property LabelsOnAxis;
    property LabelsSeparation;
    property LabelsSize;
    property LabelStyle;
    property Logarithmic;
    property LogarithmicBase;
    property Maximum;
    property Minimum;
    property MinorTickCount;
    property MinorTickLength;
    property MinorTicks;
    property StartPosition;
    property EndPosition;
    property PositionPercent;
    property RoundFirstLabel;
    property TickInnerLength;
    property TickLength;
    property TickOnLabelsOnly;
    property Ticks;
    property TicksInner;
    property Title;
    property TitleSize;
    property Visible;
  end;

  TChartDepthAxis=class(TCustomChartAxis)
  protected
    Procedure DrawAxisLine(AWallSize:Integer); override;
  published
    property Automatic;
    property AutomaticMaximum;
    property AutomaticMinimum;
    property Axis;
    property AxisValuesFormat;
    property DateTimeFormat;
    property ExactDateTime;
    property Grid;
    property Increment;
    property Inverted;
    property Labels;
    property LabelsAngle;
    property LabelsFont;
    property LabelsMultiLine;
    property LabelsOnAxis;
    property LabelsSeparation;
    property LabelsSize;
    property LabelStyle;
    property Logarithmic;
    property LogarithmicBase;
    property Maximum;
    property Minimum;
    property MinorTickCount;
    property MinorTickLength;
    property MinorTicks;
    property RoundFirstLabel;
    property TickInnerLength;
    property TickLength;
    property TickOnLabelsOnly;
    property Ticks;
    property TicksInner;
    property Title;
    property TitleSize;
    property Visible default False;
  end;

  TSeriesMouseMove=procedure( Sender:TChartSeries;
                              Shift: TShiftState;
                              X, Y: Integer) of object;

  TSeriesClick=procedure( Sender:TChartSeries;
                          ValueIndex:Integer;
                          Button:TMouseButton;
                          Shift: TShiftState;
                          X, Y: Integer) of object;

  TSeriesDblClick=procedure( Sender:TChartSeries;
                             ValueIndex:Longint) of object;

  TAxisOnGetLabel=Procedure( Sender:TChartAxis; Series:TChartSeries;
                             ValueIndex:Longint; Var LabelText:String) of object;

  TAxisOnGetNextLabel=Procedure( Sender:TChartAxis; LabelIndex:Longint;
                                 Var LabelValue:Double; Var Stop:Boolean) of object;

  TValueEvent=(veClear,veAdd,veDelete,veRefresh,veModify);

  THorizAxis = (aTopAxis,aBottomAxis,aBothHorizAxis,aCustomHorizAxis);
  TVertAxis  = (aLeftAxis,aRightAxis,aBothVertAxis,aCustomVertAxis);

  TChartClickedPartStyle=( cpNone,
                           cpLegend,
                           cpAxis,
                           cpSeries,
                           cpTitle,
                           cpFoot,
                           cpChartRect );

  TChartClickedPart=Record
    Part       : TChartClickedPartStyle;
    PointIndex : Longint;
    ASeries    : TChartSeries;
    AAxis      : TCustomChartAxis;
  end;

  TCustomAxisPanel=class(TCustomTeePanelExtended)
  private
    { Private declarations }
    FSeriesList       : TChartSeriesList;
    FDepthAxis        : TChartDepthAxis;
    FTopAxis          : TChartAxis;
    FBottomAxis       : TChartAxis;
    FLeftAxis         : TChartAxis;
    FRightAxis        : TChartAxis;

    FView3DWalls      : Boolean;
    FClipPoints       : Boolean;
    FAxisVisible      : Boolean;
    FOnGetAxisLabel   : TAxisOnGetLabel;
    FOnGetNextAxisLabel:TAxisOnGetNextLabel;

    FOnPageChange     : TNotifyEvent;
    FOnBeforeDrawAxes : TNotifyEvent;
    FOnBeforeDrawSeries:TNotifyEvent;

    FPage             : Longint;
    FMaxPointsPerPage : Longint;
    FScaleLastPage    : Boolean;

    FMaxZOrder        : Integer;
    FSeriesWidth3D    : Integer;
    FSeriesHeight3D   : Integer;

    Procedure CheckOtherSeries(Dest,Source:TChartSeries);
    Function GetSeries(Index:Longint):TChartSeries;
    Procedure InternalAddSeries(ASeries:TChartSeries);
    Function InternalMinMax(AAxis:TCustomChartAxis; IsMin,IsX:Boolean):Double;
    Procedure SetAxisVisible(Value:Boolean);
    Procedure SetBottomAxis(Value:TChartAxis);
    procedure SetClipPoints(Value:Boolean);
    Procedure SetDepthAxis(Value:TChartDepthAxis);
    Procedure SetLeftAxis(Value:TChartAxis);
    Procedure SetMaxPointsPerPage(Value:Longint);
    Procedure SetRightAxis(Value:TChartAxis);
    Procedure SetScaleLastPage(Value:Boolean);
    Procedure SetTopAxis(Value:TChartAxis);
    procedure SetView3DWalls(Value:Boolean);
  protected
    FAxes : TList;
    { Protected declarations }
    Procedure CalcAxisRect; virtual; abstract;
    Procedure CalcSeriesRect; virtual; abstract;
    Function CalcWallSize(Axis:TCustomChartAxis):Longint; virtual; abstract;
    procedure DrawTitlesAndLegend; virtual; abstract;
    Procedure DrawWalls; virtual; abstract;
    Function IsAxisVisible(Axis:TCustomChartAxis):Boolean;
    procedure RemovedDataSource( ASeries: TChartSeries;
                                 AComponent: TComponent ); dynamic;
    Procedure SetPage(Value:Longint); virtual;
    {$IFDEF D1}
    Procedure WriteComponents(Writer:TWriter); override;
    {$ELSE}
    // Procedure GetChildren(Proc:TGetChildProc{$IFDEF D3}; Root:TComponent{$ENDIF}); override;
    {$ENDIF}
  public
    { Public declarations }
    LegendColor   : TColor;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    { public methods }
    procedure Assign(Source:TPersistent); override;
    Function ActiveSeriesLegend(ItemIndex:Longint):TChartSeries;
    Procedure AddSeries(ASeries:TChartSeries);
    Procedure CalcSize3DWalls;
    Procedure CheckDatasource(ASeries:TChartSeries); virtual;
    Function FormattedValueLegend(ASeries:TChartSeries; ValueIndex:Longint):String; virtual;
    Procedure FreeAllSeries;
    Function GetAxisSeries(Axis:TCustomChartAxis):TChartSeries;
    Function GetFreeSeriesColor(CheckBackground:Boolean):TColor;
    Function GetMaxValuesCount:Longint;
    Procedure InternalDraw(Const UserRectangle:TRect); override;
    Function IsFreeSeriesColor(AColor:TColor; CheckBackground:Boolean):Boolean; virtual; abstract;
    Function IsValidDataSource(ASeries:TChartSeries; AComponent:TComponent):Boolean; virtual;
    Function MaxXValue(AAxis:TChartAxis):Double;
    Function MaxYValue(AAxis:TChartAxis):Double;
    Function MinXValue(AAxis:TChartAxis):Double;
    Function MinYValue(AAxis:TChartAxis):Double;
    Function MaxMarkWidth:Longint;
    Function MaxTextWidth:Longint;
    Function NumPages:Longint; dynamic;
    Procedure RemoveSeries(ASeries:TChartSeries);
    property Series[Index:Longint]:TChartSeries read GetSeries; default;
    Function SeriesCount:Longint;
    Function SeriesTitleLegend(SeriesIndex:Longint):String;

    { public properties }
    property AxesList:TList read FAxes;
    property MaxZOrder:Integer read FMaxZOrder write FMaxZOrder;
    property SeriesWidth3D:Integer read FSeriesWidth3D;
    property SeriesHeight3D:Integer read FSeriesHeight3D;

    { to be published properties }
    property AxisVisible:Boolean read FAxisVisible write SetAxisVisible default True;
    property BottomAxis:TChartAxis read FBottomAxis write SetBottomAxis;
    property ClipPoints:Boolean read FClipPoints write SetClipPoints default True;
    property Color;
    property DepthAxis:TChartDepthAxis read FDepthAxis write SetDepthAxis;
    property LeftAxis:TChartAxis read FLeftAxis write SetLeftAxis;
    property MaxPointsPerPage:Longint read FMaxPointsPerPage write SetMaxPointsPerPage default 0;
    property Page:Longint read FPage write SetPage default 1;
    property RightAxis:TChartAxis read FRightAxis write SetRightAxis;
    property ScaleLastPage:Boolean read FScaleLastPage write SetScaleLastPage default True;
    property SeriesList:TChartSeriesList read FSeriesList;
    property TopAxis:TChartAxis read FTopAxis write SetTopAxis;
    property View3DWalls:Boolean read FView3DWalls write SetView3DWalls default True;

    { to be published events }
    property OnBeforeDrawAxes:TNotifyEvent read FOnBeforeDrawAxes write FOnBeforeDrawAxes;
    property OnBeforeDrawSeries:TNotifyEvent read FOnBeforeDrawSeries write FOnBeforeDrawSeries;
    property OnGetAxisLabel:TAxisOnGetLabel read FOnGetAxisLabel write FOnGetAxisLabel;
    property OnGetNextAxisLabel:TAxisOnGetNextLabel read FOnGetNextAxisLabel
                                                    write FOnGetNextAxisLabel;
    property OnPageChange:TNotifyEvent read FOnPageChange write FOnPageChange;
  end;

  TSeriesMarkPosition=class
  public
    ArrowFrom   : TPoint;
    ArrowTo     : TPoint;
    Custom      : Boolean;
    Height      : Integer;
    LeftTop     : TPoint;
    Width       : Integer;
    Procedure Assign(Source:TSeriesMarkPosition);
    Function Bounds:TRect;
  end;

  TSeriesMarksPositions=class(TList)
  private
    Procedure SetPosition(Index:Integer; APosition:TSeriesMarkPosition);
    Function GetPosition(Index:Integer):TSeriesMarkPosition;
  public
    Procedure Automatic(Index:Integer);
    property Position[Index:Integer]:TSeriesMarkPosition read GetPosition
                                    write SetPosition;{$IFNDEF D1}default;{$ENDIF}
  end;

  TSeriesMarks=class(TPersistent)
  private
    FArrow           : TChartArrowPen;
    FArrowLength     : Integer;
    FBackColor       : TColor;
    FBackTransparent : Boolean;
    FClip            : Boolean;
    FFrame           : TChartPen;
    FFont            : TFont;
    FMarkerStyle     : TSeriesMarksStyle;
    FParent          : TChartSeries;
    FPositions       : TSeriesMarksPositions;
    FVisible         : Boolean;
    FZPosition       : Integer;
    Procedure SetStyle(Value:TSeriesMarksStyle);
    Procedure SetFont(Value:TFont);
    Procedure SetClip(Value:Boolean);
    Procedure SetFrame(Value:TChartPen);
    Procedure SetArrow(Value:TChartArrowPen);
    Procedure SetArrowLength(Value:Integer);
    Function IsFontStored:Boolean;
    Procedure SetVisible(Value:Boolean);
    Procedure SetBackColor(Value:TColor);
    Procedure SetBackTransparent(Value:Boolean);
  protected
    Procedure ClearPositions;
    Procedure UsePosition(Index:Integer; Var MarkPosition:TSeriesMarkPosition);
  public
    Constructor Create(AOwner:TChartSeries);
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Function Clicked(X,Y:Integer):Integer;
    Procedure Draw(Index:Integer; AColor:TColor; Const St:String; APosition:TSeriesMarkPosition);
    property ParentSeries:TChartSeries read FParent;
    property Positions:TSeriesMarksPositions read FPositions;
    property ZPosition : Integer read FZPosition write FZPosition;
  published
    property Arrow:TChartArrowPen read FArrow write SetArrow;
    property ArrowLength:Integer read FArrowLength write SetArrowLength;
    property BackColor:TColor read FBackColor write SetBackColor default ChartMarkColor;
    property Clip:Boolean read FClip write SetClip default False;
    property Font:TFont read FFont write SetFont stored IsFontStored;
    property Frame:TChartPen read FFrame write SetFrame;
    property Style:TSeriesMarksStyle read FMarkerStyle
                                     write SetStyle default smsLabel;
    property Transparent:Boolean read FBackTransparent write SetBackTransparent default False;
    property Visible:Boolean read FVisible write SetVisible;
  end;

  TSeriesOnBeforeAdd=Function(Sender:TChartSeries):Boolean of object;
  TSeriesOnAfterAdd=Procedure(Sender:TChartSeries; ValueIndex:Longint) of object;
  TSeriesOnClear=Procedure(Sender:TChartSeries) of object;
  TSeriesOnGetMarkText=Procedure(Sender:TChartSeries; ValueIndex:Integer; Var MarkText:String) of object;

  TSeriesRecalcOptions=set of (rOnDelete, rOnModify, rOnInsert, rOnClear);

  TChartSeriesClass=class of TChartSeries;

  TFunctionPeriodStyle=( psNumPoints, psRange );
  TFunctionPeriodAlign=( paFirst,paCenter,paLast );

  TTeeFunction=class(TComponent)
  private
    FPeriod      : Double;
    FPeriodStyle : TFunctionPeriodStyle;
    FPeriodAlign : TFunctionPeriodAlign;
    FParent      : TChartSeries;
    FUpdating    : Boolean;
    Procedure SetPeriod(Const Value:Double);
    Procedure SetParentSeries(AParent:TChartSeries);
    Procedure SetPeriodAlign(Value:TFunctionPeriodalign);
    Procedure SetPeriodStyle(Value:TFunctionPeriodStyle);
  protected
    CanUsePeriod:Boolean;
    {$IFDEF D1}
    procedure ReadState(Reader: TReader); override;
    {$ELSE}
    procedure SetParentComponent(Value: TComponent); override;
    {$ENDIF}
    Procedure InternalSetPeriod(Const APeriod:Double);
    Procedure AddFunctionXY(YMandatorySource:Boolean; tmpX,tmpY:Double);
    Procedure CalculatePeriod( Source:TChartSeries;
                               Const tmpX:Double;
                               FirstIndex,LastIndex:Longint); virtual;
    Procedure CalculateAllPoints(Source:TChartSeries; NotMandatorySource:TChartValueList); virtual;
    Procedure CalculateByPeriod(Source:TChartSeries; NotMandatorySource:TChartValueList); virtual;
    Function ValueList(ASeries:TChartSeries):TChartValueList;
  public
    MovingFunction:Boolean;
    Constructor Create(AOwner: TComponent); override;

    Procedure Assign(Source:TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    Procedure ReCalculate;
    Function Calculate(Series:TChartSeries; First,Last:Longint):Double; virtual;
    Function CalculateMany(SeriesList:TList; ValueIndex:Longint):Double; virtual;
    procedure AddPoints(Source:TChartSeries); virtual;
    {$IFNDEF D1}
    function GetParentComponent: TComponent; override;
    {$ENDIF}
    function HasParent: Boolean; override;
    property ParentSeries:TChartSeries read FParent write SetParentSeries;
  published
    property Period:Double read FPeriod write SetPeriod;
    property PeriodAlign:TFunctionPeriodAlign read FPeriodAlign
                                              write SetPeriodAlign default paCenter;
    property PeriodStyle:TFunctionPeriodStyle read FPeriodStyle
                                              write SetPeriodStyle default psNumPoints;
  end;

  TChartValueLists=class(TList)
  private
    Function GetValueList(Index:Integer):TChartValueList;
  public
    property ValueList[Index:Integer]:TChartValueList read GetValueList;{$IFNDEF D1}default;{$ENDIF}
  end;

  {$IFNDEF D1}
  TChartSeriesStyle=set of ( tssIsTemplate,
                             tssDenyChangeType,
                             tssDenyDelete,
                             tssDenyClone,
                             tssIsPersistent,
                             tssHideDataSource );
  {$ENDIF}

  TChartSeries=class(TComponent)
  private
    FActive             : Boolean;
    FColor              : TColor;
    FColorEachPoint     : Boolean;
    FColors             : TList;
    FColorSource        : String;
    FCursor             : TCursor;
    FDataSources        : TList;
    FFirstVisibleIndex  : Longint;
    FGetHorizAxis       : TChartAxis;
    FGetVertAxis        : TChartAxis;
    FLabelsSource       : String;
    FLastVisibleIndex   : Longint;
    FLinkedSeries       : TList;
    FMarks              : TSeriesMarks;
    FParent             : TCustomAxisPanel;
    FPercentFormat      : String;
    FShowInLegend       : Boolean;
    FTempDataSources    : TStringList;
    FTitle              : String;
    FValueFormat        : String;
    FValuesList         : TChartValueLists;
    FX                  : TChartValueList;
    FXLabels            : TList;
    FY                  : TChartValueList;

    FHorizAxis          : THorizAxis;
    FCustomHorizAxis    : TChartAxis;
    FCustomVertAxis     : TChartAxis;
    FZOrder             : Integer;

    FVertAxis           : TVertAxis;
    FRecalcOptions      : TSeriesRecalcOptions;

    FTeeFunction        : TTeeFunction;
    {$IFNDEF D1}
    FStyle              : TChartSeriesStyle; { decision cube }
    {$ENDIF}

    { Events }
    FOnBeforeAdd        : TSeriesOnBeforeAdd;
    FOnAfterAdd         : TSeriesOnAfterAdd;
    FOnClearValues      : TSeriesOnClear;
    FOnGetMarkText      : TSeriesOnGetMarkText;
    FBeforeDrawValues   : TNotifyEvent;
    FAfterDrawValues    : TNotifyEvent;
    FOnClick            : TSeriesClick;
    FOnDblClick         : TSeriesClick;

    {$IFDEF D3}
    FIdentifier         : String;  { decision cube }
    {$ENDIF}

    Procedure InsertLabel(ValueIndex:Longint; Const ALabel:String);
    Function GetMemLabel(Const ALabel:String):PChar;
    Procedure FreeXLabel(ValueIndex:Longint);
    Function GetValue(IsX:Boolean; ValueIndex:Longint):Double;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    {$IFDEF D3}
    procedure ReadIdentifier(Reader: TReader);
    procedure WriteIdentifier(Writer: TWriter);
    procedure ReadStyle(Reader: TReader);
    procedure WriteStyle(Writer: TWriter);
    {$ENDIF}
    Procedure InternalAddDataSource(Value:TComponent);
    Procedure RemoveAllLinkedSeries;
    Procedure SetTitle(Value:String);
    Procedure SetValueFormat(Const Value:String);
    Procedure SetPercentFormat(Const Value:String);
    Function IsValueFormatStored:Boolean;
    Function IsPercentFormatStored:Boolean;
    Procedure SetHorizAxis(Value:THorizAxis);
    Procedure SetVertAxis(Value:TVertAxis);
    Procedure SetColorSource(Const Value:String);
    Procedure SetLabelsSource(Const Value:String);
    Procedure SetShowInLegend(Value:Boolean);
    Procedure SetCustomHorizAxis(Value:TChartAxis);
    Procedure SetCustomVertAxis(Value:TChartAxis);
    Function GetZOrder:Integer;
    Procedure SetZOrder(Value:Integer);
    Procedure RecalcGetAxis;
  Protected
    IZOrder : Integer;
    Function AddChartValue(Source:TChartSeries; ValueIndex:Longint):Longint; virtual;
    Procedure AddedValue(Source:TChartSeries; ValueIndex:Longint); virtual;
    Procedure AddValues(Source:TChartSeries); virtual;
    Procedure ClearLists; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    Procedure DeletedValue(Source:TChartSeries; ValueIndex:Longint); virtual;
    procedure DoAfterDrawValues; virtual;
    procedure DoBeforeDrawChart; virtual;
    procedure DoBeforeDrawValues; virtual;
    procedure DrawAllValues; virtual;
    Procedure DrawMark(ValueIndex:Longint; Const St:String; APosition:TSeriesMarkPosition); virtual;
    procedure DrawMarks;
    procedure DrawValue(ValueIndex:Longint); virtual;
    Function FirstInZOrder:Boolean;
    Function GetDataSource:TComponent;
    Function GetMarkText(ValueIndex:Longint):String;
    Function GetValueColor(ValueIndex:Longint):TColor; virtual;
    Function GetXLabel(Index:Longint):String; virtual;
    Function GetxValue(Index:Longint):Double; virtual;  { conflicts with c++ wingdi.h }
    Function GetyValue(Index:Longint):Double; virtual;  { conflicts with c++ wingdi.h }
    Procedure Loaded; override;
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
    Procedure NotifyNewValue(Sender:TChartSeries; ValueIndex:Longint); virtual;
    Procedure NotifyValue(ValueEvent:TValueEvent; ValueIndex:Longint);
    Function MoreSameZOrder:Boolean; virtual;
    Procedure SetActive(Value:Boolean); virtual;
    Procedure SetChartValueList( Var AValueList:TChartValueList;
                                 Value:TChartValueList);
    Procedure SetColorEachPoint(Value:Boolean); virtual;
    Procedure SetDataSource(Value:TComponent);
    procedure SetMarks(Value:TSeriesMarks);
    Procedure SetParentChart(Value:TCustomAxisPanel); virtual;
    Procedure SetRecalcOptions(Value:TSeriesRecalcOptions);
    Procedure SetSeriesColor(AColor:TColor); virtual;
    Procedure SetValueColor(ValueIndex:Longint; AColor:TColor);
    Procedure SetXLabel(Index:Longint; Const AXLabel:String);
    Procedure SetXValue(Index:Longint; Const Value:Double);
    Procedure SetYValue(Index:Longint; Const Value:Double);
    Procedure SetXValues(Value:TChartValueList);
    Procedure SetYValues(Value:TChartValueList);
    {$IFDEF D1}
    Procedure WriteComponents(Writer:TWriter); override;
    procedure ReadState(Reader: TReader); override;
    {$ELSE}
    //Procedure GetChildren(Proc:TGetChildProc{$IFDEF D3}; Root:TComponent{$ENDIF}); override;
    procedure SetParentComponent(AParent: TComponent); override;
    {$ENDIF}
  public
    CalcVisiblePoints : Boolean;
    DrawBetweenPoints : Boolean;
    AllowSinglePoint  : Boolean;
    HasZValues        : Boolean;
    StartZ            : Integer;
    MiddleZ           : Integer;
    EndZ              : Integer;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Function Add(Const AValue:Double; Const ALabel:String = '';
                       AColor:TColor = clTeeColor):Longint; overload; virtual;
    Function AddArray(Const Values:array of Double):Longint;
    Procedure AddLinkedSeries(ASeries:TChartSeries);
    Function AddNull(Const ALabel:String =''):Longint; virtual;
    Function AddNullXY(Const X,Y:Double; Const ALabel:String):Longint; virtual;
    Function AddX(Const AXValue:Double; Const ALabel:String ='';
                         AColor:TColor=clTeeColor):Longint;
    Function AddXY(Const AXValue,AYValue:Double; Const ALabel:String ='';
                         AColor:TColor =clTeeColor):Longint; virtual;
    Function AddY(Const AYValue:Double; Const ALabel:String ='';
                         AColor:TColor = clTeeColor):Longint;

    procedure Assign(Source:TPersistent); override;
    Function AssociatedToAxis(Axis:TCustomChartAxis):Boolean; virtual;
    Procedure CalcRandomBounds(NumValues:Longint; Var tmpX,StepX,tmpY,MinY,DifY:Double);
    Procedure Clear; virtual;
    Function Count:Longint;
    Function CountLegendItems:Integer; virtual;
    Procedure Delete(ValueIndex:Longint); virtual;
    Procedure FillSampleValues(NumValues:Longint); virtual;
    Procedure GalleryChanged3D(Is3D:Boolean); virtual;
    {$IFNDEF D1}
    function GetParentComponent: TComponent; override;
    {$ENDIF}
    function HasParent: Boolean; override;
    Function IsNull(ValueIndex:Longint):Boolean;
    Function IsValidSourceOf(Value:TChartSeries):Boolean; virtual;
    Function IsValidSeriesSource(Value:TChartSeries):Boolean; virtual;
    Function LegendToValueIndex(LegendIndex:Integer):Integer; virtual;
    Function LegendItemColor(LegendIndex:Longint):TColor; virtual;
    Function LegendString(LegendIndex:Integer; LegendTextStyle:TLegendTextStyle):String; virtual;
    property LinkedSeries:TList read FLinkedSeries;
    Function MaxXValue:Double; virtual;
    Function MaxYValue:Double; virtual;
    Function MaxZValue:Double; virtual;
    Function MinXValue:Double; virtual;
    Function MinYValue:Double; virtual;
    Function MinZValue:Double; virtual;
    Function NumSampleValues:Longint; virtual;
    Procedure PrepareForGallery(IsEnabled:Boolean); virtual;
    Procedure RemoveDataSource(Value:TComponent);
    Procedure RemoveLinkedSeries(ASeries:TChartSeries);
    Procedure Repaint;
    Function VisibleCount:Longint; { <-- Number of VISIBLE points (Last-First+1) }

    property ValuesLists:TChartValueLists read FValuesList;
    property XValue[Index:Longint]:Double read GetxValue write SetXValue;
    property YValue[Index:Longint]:Double read GetyValue write SetYValue;

    property ZOrder:Integer read GetZOrder write SetZOrder default TeeAutoZOrder;
    Function MaxMarkWidth:Longint;
    Function GetEditorClass:String; virtual;

    Function CalcXPos(ValueIndex:Longint):Integer; virtual;
    Function CalcXPosValue(Const Value:Double):Longint;
    Function CalcXSizeValue(Const Value:Double):Longint;

    Function CalcYPos(ValueIndex:Longint):Integer; virtual;
    Function CalcYPosValue(Const Value:Double):Longint;
    Function CalcYSizeValue(Const Value:Double):Longint;

    Function CalcPosValue(Const Value:Double):Longint;

    Function XScreenToValue(ScreenPos:Longint):Double;
    Function YScreenToValue(ScreenPos:Longint):Double;

    Function XValueToText(Const AValue:Double):String;
    Function YValueToText(Const AValue:Double):String;

    Procedure ColorRange( AValueList:TChartValueList;
                          Const FromValue,ToValue:Double;
                          AColor:TColor);
    Procedure CheckDataSource;
    Procedure AddValue(ValueIndex:Longint); virtual;

    { Public Properties }
    property Labels:TList read FXLabels;
    property XLabel[Index:Longint]:String read GetXLabel write SetXLabel;
    property ValueMarkText[Index:Longint]:String read GetMarkText;

    property ValueColor[Index:Longint]:TColor read GetValueColor write SetValueColor;
    property XValues:TChartValueList read FX write SetXValues;
    property YValues:TChartValueList read FY write SetYValues;

    Function GetYValueList(AListName:String):TChartValueList; virtual;
    property GetVertAxis:TChartAxis read FGetVertAxis;
    property GetHorizAxis:TChartAxis read FGetHorizAxis;
    Function MarkPercent(ValueIndex:Longint; AddTotal:Boolean):String;
    Function Clicked(x,y:Integer):Longint; virtual;
    Procedure RefreshSeries;
    property FirstValueIndex:Longint read FFirstVisibleIndex;
    property LastValueIndex:Longint read FLastVisibleIndex;
    Function GetOriginValue(ValueIndex:Longint):Double; virtual;
    Function GetMarkValue(ValueIndex:Longint):Double; virtual;
    Procedure AssignValues(Source:TChartSeries);
    Function SameClass(tmpSeries:TChartSeries):Boolean;
    Function PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double; virtual;
    Function YMandatory:Boolean;
    Function MandatoryValueList:TChartValueList; virtual;
    Function DrawValuesForward:Boolean; virtual;
    Function DrawSeriesForward(ValueIndex:Longint):Boolean; virtual;
    procedure SwapValueIndex(a,b:Longint); virtual;
    property RecalcOptions: TSeriesRecalcOptions read FRecalcOptions
                                                 write SetRecalcOptions
                                       default [ rOnDelete,
                                                 rOnModify,
                                                 rOnInsert,
                                                 rOnClear];
    Procedure CalcZOrder; virtual;
    Procedure DoSeriesMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    Function DoSeriesClick( Sender:TChartSeries;
                            ValueIndex:Longint;
                            Button:TMouseButton;
                            Shift: TShiftState; X, Y: Integer):Boolean; virtual;
    Procedure DoSeriesMouseUp( Button:TMouseButton;
                               Shift: TShiftState; X, Y: Integer); virtual;

    Function GetCursorValueIndex:Longint;
    Procedure GetCursorValues(Var x,y:Double);
    Procedure ClearTempValue(ValueList:TChartValueList); virtual;
    Procedure DrawLegend(ValueIndex:Longint; Const Rect:TRect); virtual;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); virtual;
    Procedure PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                   Var BrushStyle:TBrushStyle); virtual;

    Procedure CalcHorizMargins(Var LeftMargin,RightMargin:Integer); virtual;
    Procedure CalcVerticalMargins(Var TopMargin,BottomMargin:Integer); virtual;

    Function UseAxis:Boolean; virtual;
    procedure SetFunction(AFunction:TTeeFunction); virtual;

    Procedure CalcFirstLastVisibleIndex;

    { other }
    Function CreateChartPen:TChartPen;
    Procedure CanvasChanged(Sender:TObject); virtual;
    property DataSources:TList read FDataSources;
    property FunctionType:TTeeFunction read FTeeFunction;
    Procedure SetLongintProperty(Var Variable:Longint; Value:Longint);
    Procedure SetIntegerProperty(Var Variable:Integer; Value:Integer);
    Procedure SetStringProperty(Var Variable:String; Const Value:String);
    Procedure SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
    Procedure SetColorProperty(Var Variable:TColor; Value:TColor);
    Procedure SetDoubleProperty(Var Variable:Double; Const Value:Double);
    Procedure CheckOtherSeries(Source:TChartSeries);
    Procedure GetBitmapEditor(ABitmap:TBitmap); virtual;
    Procedure ReplaceList(OldList,NewList:TChartValueList);
    { DSS, hidden }
    {$IFDEF D3}
    property Identifier:String read FIdentifier write FIdentifier;
    property Style:TChartSeriesStyle read FStyle write FStyle default [];
    {$ENDIF}
    property CustomHorizAxis:TChartAxis read FCustomHorizAxis write SetCustomHorizAxis;
    property CustomVertAxis:TChartAxis read FCustomVertAxis write SetCustomVertAxis;
  published
    property Active:Boolean read FActive write SetActive default True;
    property ColorEachPoint:Boolean read FColorEachPoint write SetColorEachPoint default False;
    property ColorSource:String read FColorSource write SetColorSource;
    property Cursor:TCursor read FCursor write FCursor default crDefault;
    property HorizAxis:THorizAxis read FHorizAxis write SetHorizAxis default aBottomAxis;
    property Marks:TSeriesMarks read FMarks write SetMarks;
    property ParentChart:TCustomAxisPanel read FParent write SetParentChart stored False;
    { datasource below parentchart }
    property DataSource:TComponent read GetDataSource write SetDataSource;
    property PercentFormat:String read FPercentFormat write SetPercentFormat stored IsPercentFormatStored;
    property SeriesColor:TColor read FColor write SetSeriesColor default clTeeColor;
    property ShowInLegend:Boolean read FShowInLegend write SetShowInLegend default True;
    property Title:String read FTitle write SetTitle;
    property ValueFormat:String read FValueFormat write SetValueFormat stored IsValueFormatStored;
    property VertAxis:TVertAxis read FVertAxis write SetVertAxis default aLeftAxis;
    property XLabelsSource:String read FLabelsSource write SetLabelsSource;

    { events }
    property AfterDrawValues:TNotifyEvent read FAfterDrawValues
                                           write FAfterDrawValues;
    property BeforeDrawValues:TNotifyEvent read FBeforeDrawValues
                                           write FBeforeDrawValues;
    property OnAfterAdd:TSeriesOnAfterAdd read FOnAfterAdd write FOnAfterAdd;
    property OnBeforeAdd:TSeriesOnBeforeAdd read FOnBeforeAdd write FOnBeforeAdd;
    property OnClearValues:TSeriesOnClear read FOnClearValues
                                          write FOnClearValues;
    property OnClick:TSeriesClick read FOnClick write FOnClick;
    property OnDblClick:TSeriesClick read FOnDblClick write FOnDblClick;
    property OnGetMarkText:TSeriesOnGetMarkText read FOnGetMarkText
                                                write FOnGetMarkText;
  end;

  TTeeFunctionClass=class of TTeeFunction;

Var  { see initialization section at bottom of this unit }
  TeeAxisClickGap       : Longint=3;  { minimum pixels distance to trigger axis click }
  TeeDefaultCapacity    : Longint=0;  { default TList.Capacity to speed-up Lists }

implementation

Uses TeeConst;

{ TChartBrush }
Constructor TChartBrush.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create;
  Color:=clTeeColor;
  OnChange:=OnChangeEvent;
end;

{ TChartValueList }
Constructor TChartValueList.Create(AOwner:TChartSeries; Const AName:String);
Begin
  inherited Create;
  IDirtyStats:=True;
  FMultiplier:=1;
  FOwner:=AOwner;
  FOwner.FValuesList.Add(Self);
  FName:=AName;
  FList:=TList.Create;
  ClearValues;
end;

Destructor TChartValueList.Destroy;
Begin
  FList.Free;
  inherited Destroy;
End;

Function TChartValueList.First:Double;
Begin
  result:=GetValue(0);
end;

Function TChartValueList.Count:Longint;
Begin
  result:=FList.Count; { <-- virtual }
end;

Function TChartValueList.Last:Double;
Begin
  result:=GetValue(Count-1);
end;

Procedure TChartValueList.Delete(ValueIndex:Longint);
Begin
  Dispose(PChartValue(FList[ValueIndex]));  { <- virtual }
  FList.Delete(ValueIndex);
  IDirtyStats:=True;
end;

Procedure TChartValueList.InsertChartValue(ValueIndex:Longint; Const Value:TChartValue);
Var p : PChartValue;
Begin
  New(p);
  p^:=Value;
  FList.Insert(ValueIndex,p);  { <- virtual }
  IDirtyStats:=True;
end;

Function TChartValueList.AddChartValue(Const Value:TChartValue):Longint;
Var t : Longint;
    p : PChartValue;
Begin
  New(p);    { virtual }
  p^:=Value;
  if FOrder=loNone then result:=FList.Add(p)
  else
  begin
    t:=FList.Count-1;
    if (t=-1) or
       ( (FOrder=loAscending) and (Value>=PChartValue(FList[t])^) ) or
       ( (FOrder=loDescending) and (Value<=PChartValue(FList[t])^) ) then
          result:=FList.Add(p)
    else
    Begin
      if FOrder=loAscending then
         While (t>=0) and (PChartValue(FList[t])^>Value) do Dec(t)
      else
         While (t>=0) and (PChartValue(FList[t])^<Value) do Dec(t);
      result:=t+1;
      FList.Insert(result,p);
    end;
  end;
  IDirtyStats:=True;
end;

Procedure TChartValueList.SetMultiplier(Const Value:Double);
Begin
  if Value<>FMultiplier then
  begin
    FMultiplier:=Value;
    IDirtyStats:=True;
    FOwner.NotifyValue(veRefresh,0);
    FOwner.Repaint;
  end;
end;

Function TChartValueList.GetValue(ValueIndex:Longint):Double;
begin
  result:=TChartValue(FList[ValueIndex]^)*FMultiplier; { virtual }
end;

Function TChartValueList.Locate( Const Value :Double ):Longint;
Var t : Longint;
Begin
  for t:=0 to Count-1 do
  if GetValue(t)=Value then
  begin
    result:=t;
    exit;
  end;
  result:=-1;
end;

Procedure TChartValueList.SetValue(ValueIndex:Longint; Const AValue:Double);
begin
  PChartValue(FList[ValueIndex])^:=AValue; { virtual }
  IDirtyStats:=True;
  FOwner.NotifyValue(veModify,ValueIndex);
end;

Function TChartValueList.GetMaxValue:Double;
begin
  if IDirtyStats then RecalcStats;
  result:=FMaxValue;
end;

Function TChartValueList.GetMinValue:Double;
begin
  if IDirtyStats then RecalcStats;
  result:=FMinValue;
end;

Function TChartValueList.GetTotal:Double;
begin
  if IDirtyStats then RecalcStats;
  result:=FTotal;
end;

Function TChartValueList.GetTotalABS:Double;
begin
  if IDirtyStats then RecalcStats;
  result:=FTotalABS;
end;

Procedure TChartValueList.RecalcStats;
var t        : Longint;
    tmpValue : Double;
Begin
  if Count>0 then
  begin
    tmpValue:=GetValue(0);
    FMinValue:=tmpValue;
    FMaxValue:=tmpValue;
    FTotal:=tmpValue;
    FTotalABS:=Abs(tmpValue);
    for t:=1 to Count-1 do
    Begin
      tmpValue:=GetValue(t);
      if tmpValue<FMinValue then FMinValue:=tmpValue else
      if tmpValue>FMaxValue then FMaxValue:=tmpValue;
      FTotal:=FTotal+tmpValue;
      FTotalABS:=FTotalABS+Abs(tmpValue);
    end;
  end
  else
  begin
    FMinValue:=0;
    FMaxValue:=0;
    FTotal:=0;
    FTotalABS:=0;
  end;
  IDirtyStats:=False;
end;

procedure TChartValueList.SetDateTime(Value:Boolean);
Begin
  FOwner.SetBooleanProperty(FDateTime,Value);
end;

Procedure TChartValueList.ClearValues;
Var t : Longint;
Begin
  for t:=0 to FList.Count-1 do Dispose(PChartValue(FList[t])); { virtual }
  FList.Clear;
  {$IFDEF D1}
  if TeeDefaultCapacity<16384 then
  {$ENDIF}
  FList.Capacity:=TeeDefaultCapacity;
  IDirtyStats:=True;
end;

Procedure TChartValueList.Scroll;
var t      : Longint;
    tmpVal : PChartValue;
Begin
  With FList do  { virtual }
  if Count>0 then
  Begin
    tmpVal:=First;
    for t:=1 to Count-1 do Items[t-1]:=Items[t];
    Items[Count-1]:=tmpVal;
  end;
end;

Procedure TChartValueList.SetValueSource(Const Value:String);
Begin
  if FValueSource<>Value then
  begin
    FValueSource:=Value;
    FOwner.CheckDataSource;
  end;
end;

Procedure TChartValueList.FillSequence;
var t : Longint;
begin
  for t:=0 to Count-1 do SetValue(t,t);
end;

procedure TChartValueList.Sort;

  Function CompareValueIndex(a,b:Longint):Longint;
  var tmpA : Double;
      tmpB : Double;
  begin
    tmpA:=GetValue(a);
    tmpB:=GetValue(b);
    if tmpA<tmpB then result:=-1 else
    if tmpA>tmpB then result:= 1 else result:= 0;
    if FOrder=loDescending then result:=-result;
  end;

  procedure PrivateSort(l,r:longint);
  var i : Longint;
      j : Longint;
      x : Longint;
  begin
    i:=l;
    j:=r;
    x:=(i+j) shr 1;
    while i<j do
    begin
      while CompareValueIndex(i,x)<0 do inc(i);
      while CompareValueIndex(x,j)<0 do dec(j);
      if i<j then
      begin
        Owner.SwapValueIndex(i,j);
        if i=x then x:=j else if j=x then x:=i;
      end;
      if i<=j then
      begin
        inc(i);
        dec(j)
      end;
    end;
    if l<j then PrivateSort(l,j);
    if i<r then PrivateSort(i,r);
  end;

begin
  if FOrder<>loNone then PrivateSort(0,Count-1);
end;

Procedure TChartValueList.Assign(Source:TPersistent);
begin
  if Source is TChartValueList then
  With TChartValueList(Source) do
  begin
    Self.FOrder      :=FOrder;
    Self.FDateTime   :=FDateTime;
    Self.FMultiplier :=FMultiplier;
    Self.FValueSource:=FValueSource;
  end;
end;

{ TSeriesMarksPosition }
Procedure TSeriesMarkPosition.Assign(Source:TSeriesMarkPosition);
begin
  ArrowFrom :=Source.ArrowFrom;
  ArrowTo   :=Source.ArrowTo;
  LeftTop   :=Source.LeftTop;
  Height    :=Source.Height;
  Width     :=Source.Width;
end;

Function TSeriesMarkPosition.Bounds:TRect;
begin
  result:=Classes.Bounds(LeftTop.X,LeftTop.Y,Width,Height);
end;

{ TSeriesMarksPositions }
Procedure TSeriesMarksPositions.SetPosition(Index:Integer; APosition:TSeriesMarkPosition);
begin
  While Index>=Count do Add(nil);
  if Items[Index]=nil then Items[Index]:=TSeriesMarkPosition.Create;
  With Position[Index] do
  begin
    Custom:=True;
    Assign(APosition);
  end;
end;

Function TSeriesMarksPositions.GetPosition(Index:Integer):TSeriesMarkPosition;
begin
  if (Index<Count) and (Items[Index]<>nil) then result:=TSeriesMarkPosition(Items[Index])
                                           else result:=nil
end;

Procedure TSeriesMarksPositions.Automatic(Index:Integer);
begin
  if (Index<Count) and (Items[Index]<>nil) then Position[Index].Custom:=False;
end;

{ TSeriesMarks }
Constructor TSeriesMarks.Create(AOwner:TChartSeries);
Begin
  inherited Create;
  FParent         := AOwner;
  FMarkerStyle    := smsLabel;
  FFrame          := FParent.CreateChartPen;
  FBackColor      := ChartMarkColor;
  FArrow          := TChartArrowPen.Create(FParent.CanvasChanged);
  FArrowLength    := 8;
  FFont           := CreateDefaultFont(FParent.CanvasChanged);
  FPositions      := TSeriesMarksPositions.Create;
end;

Procedure TSeriesMarks.Assign(Source:TPersistent);
Begin
  if Source is TSeriesMarks then
  With TSeriesMarks(Source) do
  Begin
    Self.FArrow.Assign(FArrow);
    Self.FArrowLength:=FArrowLength;
    Self.FBackColor  :=FBackColor;
    Self.FBackTransparent:=FBackTransparent;
    Self.FClip       :=FClip;
    Self.FFrame.Assign(FFrame);
    Self.FFont.Assign(FFont);
    Self.FMarkerStyle:=FMarkerStyle;
    Self.FVisible    :=FVisible;
  end
  else inherited Assign(Source);
end;

Function TSeriesMarks.IsFontStored:Boolean;
begin
  result:=not IsDefaultFont(FFont);
end;

Procedure TSeriesMarks.SetVisible(Value:Boolean);
Begin
  FParent.SetBooleanProperty(FVisible,Value);
end;

Procedure TSeriesMarks.SetFrame(Value:TChartPen);
begin
  FFrame.Assign(Value);
end;

Procedure TSeriesMarks.SetArrow(Value:TChartArrowPen);
begin
  FArrow.Assign(Value);
end;

Procedure TSeriesMarks.SetArrowLength(Value:Integer);
Begin
  FParent.SetIntegerProperty(FArrowLength,Value);
end;

Procedure TSeriesMarks.SetBackColor(Value:TColor);
Begin
  FParent.SetColorProperty(FBackColor,Value);
end;

Procedure TSeriesMarks.SetBackTransparent(Value:Boolean);
Begin
  FParent.SetBooleanProperty(FBackTransparent,Value);
end;

Function TSeriesMarks.Clicked(X,Y:Integer):Integer;
var t : Integer;
begin
  if (FParent.ParentChart<>nil) then
      FParent.ParentChart.Canvas.Calculate2DPosition(x,y,ZPosition);
  with FPositions do
  for t:=0 to Count-1 do
  if (Position[t]<>nil) and PtInRect(Position[t].Bounds,Point(X,Y)) then
  begin
    result:=t;
    Exit;
  end;
  result:=-1;
end;

Procedure TSeriesMarks.UsePosition( Index:Integer; Var MarkPosition:TSeriesMarkPosition);
var tmp : TSeriesMarkPosition;
begin
  with FPositions do
  begin
    while Index>=Count do Add(nil);
    if Items[Index]=nil then
    begin
      tmp:=TSeriesMarkPosition.Create;
      tmp.Custom:=False;
      Items[Index]:=tmp;
    end;
    tmp:=Position[Index];
    if tmp.Custom then MarkPosition.Assign(tmp)
                  else tmp.Assign(MarkPosition);
  end;
end;

Procedure TSeriesMarks.ClearPositions;
var t : Integer;
begin
  With FPositions do
  begin
    for t:=0 to Count-1 do Position[t].Free;
    Clear;
  end;
end;

Destructor TSeriesMarks.Destroy;
begin
  FFont.Free;
  FFrame.Free;
  FArrow.Free;
  ClearPositions;
  FPositions.Free;
  inherited Destroy;
end;

Procedure TSeriesMarks.SetFont(Value:TFont);
begin
  FFont.Assign(Value);
end;

Procedure TSeriesMarks.SetStyle(Value:TSeriesMarksStyle);
Begin
  if FMarkerStyle<>Value then
  begin
    FMarkerStyle:=Value;
    FParent.Repaint;
  end;
end;

Procedure TSeriesMarks.SetClip(Value:Boolean);
Begin
  FParent.SetBooleanProperty(FClip,Value);
end;

Procedure TSeriesMarks.Draw(Index:Integer; AColor:TColor; Const St:String; APosition:TSeriesMarkPosition);

  Procedure DrawMarkText;
  Var tmpPosRow    : Integer;
      tmpCenter    : Integer;

    Procedure DrawLine(Const LineSt:String);
    begin
      With FParent.ParentChart.Canvas do
      if Supports3DText then TextOut3D(tmpCenter,tmpPosRow,ZPosition,LineSt)
                        else TextOut(tmpCenter,tmpPosRow,LineSt);
    end;

  var tmpSt : String;
      i     : Integer;
      tmpRowHeight : Integer;
  begin
    With APosition do
    begin
      tmpCenter:=LeftTop.X+(Width div 2);
      tmpPosRow:=LeftTop.Y;
    end;
    i:=AnsiPos(TeeLineSeparator,St);
    if i>0 then
    begin
      tmpSt:=St;
      tmpRowHeight:=FParent.ParentChart.Canvas.FontHeight;
      Repeat
        DrawLine(Copy(tmpSt,1,i-1));
        tmpSt:=Copy(tmpSt,i+1,255);
        Inc(tmpPosRow,tmpRowHeight);
        i:=AnsiPos(TeeLineSeparator,tmpSt);
      Until i=0;
      if tmpSt<>'' then DrawLine(tmpSt);
    end
    else DrawLine(St);
  end;

Const ArrowColors:Array[Boolean] of TColor=(clBlack,clWhite);
      BrushStyles:Array[Boolean] of TBrushStyle=(bsSolid,bsClear);
var tmpPos2D : TPoint;
    tmpDifX  : Integer;
    tmpDifY  : Integer;
    tmp3D    : Boolean;
begin
  UsePosition(Index,APosition);
  with FParent,ParentChart.Canvas do
  Begin
    tmp3D:=ParentChart.View3D;
    if FArrow.Visible then
    Begin
      Pen.Assign(FArrow);
      Brush.Style:=bsClear;
      if TeeCheckMarkArrowColor then
         if (Pen.Color=AColor) or (Pen.Color=ParentChart.Color) then
            Pen.Color:=ArrowColors[ParentChart.Color=clBlack];
      With APosition do
      if tmp3D then LineWithZ(ArrowFrom.X,ArrowFrom.Y,ArrowTo.X,ArrowTo.Y,ZPosition)
               else Line(ArrowFrom.X,ArrowFrom.Y,ArrowTo.X,ArrowTo.Y);
    end;

    if tmp3D and (not Supports3DText) then
    With APosition do
    begin
      tmpDifX:=ArrowTo.X-LeftTop.X;
      tmpDifY:=ArrowTo.Y-LeftTop.Y;
      tmpPos2D:=Calculate3DPosition(ArrowTo.X,ArrowTo.Y,ZPosition);
      LeftTop:=Point(tmpPos2D.X-tmpDifX,tmpPos2D.Y-tmpDifY);
    end;

    if FFrame.Visible or (not FBackTransparent) then
    Begin
      Brush.Color:=FBackColor;
      Brush.Style:=BrushStyles[FBackTransparent];
      AssignVisiblePen(FFrame);
      if tmp3D and Supports3DText then
         RectangleWithZ(APosition.Bounds,Succ(ZPosition))
      else
         DoRectangle(APosition.Bounds);
      if FFrame.Visible then Inc(APosition.LeftTop.Y,FFrame.Width);
    end;
    Brush.Style:=bsClear;
    TextAlign:=TA_CENTER;
    DrawMarkText;
    TextAlign:=TA_LEFT;
  end;
end;

{ TTeeFunction }
Constructor TTeeFunction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CanUsePeriod:=True;
  FPeriodAlign:=paCenter;
  FPeriodStyle:=psNumPoints;
  FPeriod:=0; { = calculate with all points (no period) }
end;

Procedure TTeeFunction.BeginUpdate;
begin
  FUpdating:=True;
end;

Procedure TTeeFunction.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating:=False;
    Recalculate;
  end;
end;

Procedure TTeeFunction.Recalculate;
begin
  if not FUpdating then
     if Assigned(FParent) then FParent.CheckDataSource;
end;

Function TTeeFunction.Calculate(Series:TChartSeries; First,Last:Longint):Double;
begin
  result:=0; { abstract }
end;

Function TTeeFunction.ValueList(ASeries:TChartSeries):TChartValueList;
var tmp:String;
begin
  tmp:=ParentSeries.MandatoryValueList.ValueSource;
  With ASeries do
  if tmp='' then result:=MandatoryValueList
            else result:=GetYValueList(tmp);
end;

Procedure TTeeFunction.CalculateAllPoints( Source:TChartSeries;
                                           NotMandatorySource:TChartValueList);
var tmpX : Double;
    tmpY : Double;
begin
  with ParentSeries do
  begin
    tmpY:=Calculate(Source,TeeAllValues,TeeAllValues);
    if not AllowSinglePoint then
    begin
      tmpX:=NotMandatorySource.MinValue;
      AddFunctionXY(Source.YMandatory,tmpX,tmpY);
      tmpX:=NotMandatorySource.MaxValue;
      AddFunctionXY(Source.YMandatory,tmpX,tmpY);
    end
    else { centered point }
    if (not Source.YMandatory) and (ParentSeries.YMandatory) then
    begin
      With NotMandatorySource do tmpX:=MinValue+0.5*(MaxValue-MinValue);
      AddXY(tmpX,tmpY{$IFNDEF D5},'', clTeeColor{$ENDIF});
    end
    else
    begin
      With NotMandatorySource do tmpX:=MinValue+0.5*(MaxValue-MinValue);
      if ParentSeries.YMandatory then
         AddFunctionXY(Source.YMandatory,tmpX,tmpY)
      else
         AddXY(tmpY,tmpX{$IFNDEF D5},'', clTeeColor{$ENDIF})
    end;
  end;
end;

Procedure TTeeFunction.CalculatePeriod( Source:TChartSeries;
                                        Const tmpX:Double;
                                        FirstIndex,LastIndex:Longint);
begin
  AddFunctionXY( Source.YMandatory, tmpX, Calculate(Source,FirstIndex,LastIndex) );
end;

Procedure TTeeFunction.CalculateByPeriod( Source:TChartSeries;
                                          NotMandatorySource:TChartValueList);
var tmpX     : Double;
    tmpCount : Longint;
    tmpFirst : Longint;
    tmpLast  : Longint;
    tmpBreakPeriod:Double;
    PosFirst : Double;
    PosLast  : Double;
    tmpStep  : TDateTimeStep;
    tmpCalc  : Boolean;
begin
  With ParentSeries do
  begin
    tmpFirst:=0;
    tmpCount:=Source.Count;
    tmpBreakPeriod:=NotMandatorySource.Value[tmpFirst];
    tmpStep:=FindDateTimeStep(FPeriod);
    Repeat
      PosLast:=0;
      if FPeriodStyle=psNumPoints then
      begin
        tmpLast:=tmpFirst+Round(FPeriod)-1;
        With NotMandatorySource do
        begin
          PosFirst:=Value[tmpFirst];
          if tmpLast<tmpCount then PosLast:=Value[tmpLast];
        end;
      end
      else
      begin
        tmpLast:=tmpFirst;
        PosFirst:=tmpBreakPeriod;
        GetHorizAxis.IncDecDateTime(True,tmpBreakPeriod,FPeriod,tmpStep);
        PosLast:=tmpBreakPeriod-(FPeriod*0.001);
        While tmpLast<(tmpCount-1) do
          if NotMandatorySource.Value[tmpLast+1]<tmpBreakPeriod then Inc(tmpLast)
                                                                else break;
      end;
      tmpCalc:=False;
      if tmpLast<tmpCount then
      begin
        { align periods }
        if FPeriodAlign=paFirst then tmpX:=PosFirst else
        if FPeriodAlign=paLast then tmpX:=PosLast else
                                    tmpX:=(PosFirst+PosLast)*0.5;
        if (FPeriodStyle=psRange) and (NotMandatorySource.Value[tmpFirst]<tmpBreakPeriod) then
           tmpCalc:=True;
        if (FPeriodStyle=psNumPoints) or tmpCalc then
           CalculatePeriod(Source,tmpX,tmpFirst,tmpLast)
        else
           AddFunctionXY( Source.YMandatory, tmpX, 0 );
      end;
      if (FPeriodStyle=psNumPoints) or tmpCalc then tmpFirst:=tmpLast+1;
    until tmpFirst>tmpCount-1;
  end;
end;

Procedure TTeeFunction.AddFunctionXY(YMandatorySource:Boolean; tmpX,tmpY:Double);
begin
  if not YMandatorySource then SwapDouble(tmpX,tmpY);
  ParentSeries.AddXY( tmpX, tmpY{$IFNDEF D5},'', clTeeColor{$ENDIF});
end;

procedure TTeeFunction.AddPoints(Source:TChartSeries);
Var NotMandatorySource : TChartValueList;
    YMandatorySource   : Boolean;

    Procedure CalculateMovingFunction;
    Var t : Longint;
        P : Longint;
    begin
      P:=Round(FPeriod);
      for t:=P-1 to Source.Count-1 do
          AddFunctionXY( Source.YMandatory, NotMandatorySource[t], Calculate(Source,t-P+1,t));
    end;

    Procedure CalculateFunctionMany;
    var t    : Longint;
        tmpX : Double;
        tmpY : Double;
    begin
      for t:=0 to Source.Count-1 do
      begin
        tmpX:=NotMandatorySource[t];
        tmpY:=CalculateMany(ParentSeries.FDataSources,t);
        if not YMandatorySource then SwapDouble(tmpX,tmpY);
        ParentSeries.AddXY( tmpX,tmpY,Source.XLabel[t]{$IFNDEF D5},clTeeColor{$ENDIF});
      end;
    end;

begin
  if Source<>nil then
  With ParentSeries do
  begin
    YMandatorySource:=Source.YMandatory;
    if YMandatorySource then NotMandatorySource:=Source.XValues
                        else NotMandatorySource:=Source.YValues;
    if FDataSources.Count>1 then
       CalculateFunctionMany
    else
    if Source.Count>0 then
       if FunctionType.MovingFunction then CalculateMovingFunction
       else
       if FPeriod=0 then CalculateAllPoints(Source,NotMandatorySource)
                    else CalculateByPeriod(Source,NotMandatorySource);
  end;
end;

Procedure TTeeFunction.SetParentSeries(AParent:TChartSeries);
begin
  if AParent<>FParent then
  begin
    if Assigned(FParent) then FParent.FTeeFunction:=nil;
    AParent.SetFunction(Self);
  end;
end;

Function TTeeFunction.CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;
begin
  result:=0;
end;

Procedure TTeeFunction.Assign(Source:TPersistent);
begin
  if Source is TTeeFunction then
  With TTeeFunction(Source) do
  begin
    Self.FPeriod     :=FPeriod;
    Self.FPeriodStyle:=FPeriodStyle;
    Self.FPeriodAlign:=FPeriodAlign;
  end;
end;

Procedure TTeeFunction.InternalSetPeriod(Const APeriod:Double);
begin
  FPeriod:=APeriod;
end;

Procedure TTeeFunction.SetPeriod(Const Value:Double);
begin
  if Value<0 then Raise Exception.Create(TeeMsg_FunctionPeriod);
  if FPeriod<>Value then
  begin
    FPeriod:=Value;
    Recalculate;
  end;
end;

Procedure TTeeFunction.SetPeriodAlign(Value:TFunctionPeriodalign);
begin
  if Value<>FPeriodAlign then
  begin
    FPeriodAlign:=Value;
    Recalculate;
  end;
end;

Procedure TTeeFunction.SetPeriodStyle(Value:TFunctionPeriodStyle);
begin
  if Value<>FPeriodStyle then
  begin
    FPeriodStyle:=Value;
    Recalculate;
  end;
end;

{$IFDEF D1}
procedure TTeeFunction.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TChartSeries then
     ParentSeries:=TChartSeries(Reader.Parent);
end;
{$ELSE}
function TTeeFunction.GetParentComponent: TComponent;
begin
  result:=FParent;
end;

procedure TTeeFunction.SetParentComponent(Value: TComponent);
begin
  if Assigned(Value) then ParentSeries:=TChartSeries(Value);
end;
{$ENDIF}

function TTeeFunction.HasParent: Boolean;
begin
  result:=True;
end;

{ TChartValueLists }
Function TChartValueLists.GetValueList(Index:Integer):TChartValueList;
begin
  result:=TChartValueList(inherited Items[Index]);
end;

{ TChartSeries }
Constructor TChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZOrder:=TeeAutoZOrder;
  IZOrder:=0;
  FActive:=True;
  FHorizAxis:=aBottomAxis;
  FGetHorizAxis:=nil;
  FVertAxis:=aLeftAxis;
  FGetVertAxis:=nil;
  FValuesList:=TChartValueLists.Create;
  FX:=TChartValueList.Create(Self,TeeMsg_ValuesX);
  FX.FDateTime:=False; { It was "True" in 1.03 version }
  FX.FOrder:=loAscending;
  FY:=TChartValueList.Create(Self,TeeMsg_ValuesY);
  FColors :=TList.Create;
  FXLabels:=TList.Create;
  FColor  :=clTeeColor;
  FShowInLegend:=True;
  FCursor:=crDefault;
  FMarks :=TSeriesMarks.Create(Self);
  FValueFormat    :=TeeMsg_DefValueFormat;
  FPercentFormat  :=TeeMsg_DefPercentFormat;
  FDataSources    :=TList.Create;
  FTempDataSources:=nil;
  FLinkedSeries:=TList.Create;
  FRecalcOptions := [rOnDelete,rOnModify,rOnInsert,rOnClear];
  FFirstVisibleIndex:=-1;
  FLastVisibleIndex :=-1;
  AllowSinglePoint:=True;
  CalcVisiblePoints:=True;
  HasZValues:=False;
  {$IFDEF D3}
  FIdentifier:='';
  FStyle:=[];
  {$ENDIF}
end;

{$IFDEF D1}
procedure TChartSeries.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TCustomAxisPanel then
     ParentChart := TCustomAxisPanel(Reader.Parent);
end;

Procedure TChartSeries.WriteComponents(Writer:TWriter);
begin
  if FunctionType<>nil then Writer.WriteComponent(FunctionType);
  inherited WriteComponents(Writer);
end;
{$ELSE}
(*
Procedure TChartSeries.GetChildren( Proc:TGetChildProc
                                    {$IFDEF D3}; Root:TComponent {$ENDIF} );
begin
  inherited GetChildren(Proc{$IFDEF D3},Root{$ENDIF});
  if FunctionType<>nil then Proc(FunctionType);
end;
*)
function TChartSeries.GetParentComponent: TComponent;
begin
  result:=FParent;
end;

procedure TChartSeries.SetParentComponent(AParent: TComponent);
begin
  if AParent is TCustomAxisPanel then ParentChart:=TCustomAxisPanel(AParent);
end;
{$ENDIF}

function TChartSeries.HasParent: Boolean;
begin
  result:=True;
end;

procedure TChartSeries.SetFunction(AFunction:TTeeFunction);
begin
  FTeeFunction.Free;
  FTeeFunction:=AFunction;
  if Assigned(FTeeFunction) then FTeeFunction.FParent:=Self;
  CheckDataSource;
end;

procedure TChartSeries.Notification( AComponent: TComponent;
                                     Operation: TOperation);
var tmp : TComponent;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    tmp:=DataSource;
    if Assigned(tmp) and (AComponent=tmp) then SetDataSource(nil);
  end;
end;

procedure TChartSeries.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('DataSources',ReadData,WriteData,FDataSources.Count > 1);  { <-- don't translate }
  {$IFDEF D3}
  Filer.DefineProperty('Identifier',ReadIdentifier,WriteIdentifier,Identifier<>'');  { <-- don't translate }
  Filer.DefineProperty('Style',ReadStyle,WriteStyle,Style<>[]);  { <-- don't translate }
  {$ENDIF}
end;

{$IFDEF D3}
procedure TChartSeries.ReadIdentifier(Reader: TReader);
begin
  Identifier:=Reader.ReadString;
end;

procedure TChartSeries.WriteIdentifier(Writer: TWriter);
begin
  Writer.WriteString(Identifier);
end;

procedure TChartSeries.ReadStyle(Reader: TReader);
var tmp : Integer;
begin
  tmp:=Reader.ReadInteger;
  FStyle:=[];
  if (tmp and 1)=1 then FStyle:=FStyle+[tssIsTemplate];
  if (tmp and 2)=2 then FStyle:=FStyle+[tssDenyChangeType];
  if (tmp and 4)=4 then FStyle:=FStyle+[tssDenyDelete];
  if (tmp and 8)=8 then FStyle:=FStyle+[tssDenyClone];
  if (tmp and 16)=16 then FStyle:=FStyle+[tssIsPersistent];
  if (tmp and 32)=32 then FStyle:=FStyle+[tssHideDataSource];
end;

procedure TChartSeries.WriteStyle(Writer: TWriter);
var tmp : Integer;
begin
  tmp:=0;
  if tssIsTemplate     in FStyle then Inc(tmp);
  if tssDenyChangeType in FStyle then Inc(tmp,2);
  if tssDenyDelete     in FStyle then Inc(tmp,4);
  if tssDenyClone      in FStyle then Inc(tmp,8);
  if tssIsPersistent   in FStyle then Inc(tmp,16);
  if tssHideDataSource in FStyle then Inc(tmp,32);
  Writer.WriteInteger(tmp);
end;
{$ENDIF}

procedure TChartSeries.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  FTempDataSources:=TStringList.Create;
  With FTempDataSources do
  begin
    Clear;
    while not Reader.EndOfList do Add(Reader.ReadString);
  end;
  Reader.ReadListEnd;
end;

procedure TChartSeries.WriteData(Writer: TWriter);
var t : Longint;
begin
  With Writer do
  begin
    WriteListBegin;
    With FDataSources do
         for t:=0 to Count-1 do WriteString(TComponent(Items[t]).Name);
    WriteListEnd;
  end;
end;

Function TChartSeries.YMandatory:Boolean;
begin
  result:=MandatoryValueList=YValues;
end;

Function TChartSeries.MandatoryValueList:TChartValueList; { virtual }
Begin
  result:=YValues;
end;

Function TChartSeries.DrawValuesForward:Boolean;
begin
  result:=True;
end;

Function TChartSeries.DrawSeriesForward(ValueIndex:Longint):Boolean;
begin
  result:=True;
end;

procedure TChartSeries.DrawAllValues;
Var t : Longint;
Begin
  if DrawValuesForward then
     for t:=FFirstVisibleIndex to FLastVisibleIndex do DrawValue(t)
  else
     for t:=FLastVisibleIndex downto FFirstVisibleIndex do DrawValue(t);
End;

Function TChartSeries.SameClass(tmpSeries:TChartSeries):Boolean;
begin
  result:=( (Self is tmpSeries.ClassType) or (tmpSeries is Self.ClassType));
end;

Function TChartSeries.PointOrigin(ValueIndex:Longint; SumAll:Boolean):Double;
var t         : Longint;
    tmpSeries : TChartSeries;
Begin
  result:=0;
  if Assigned(FParent) then
  with FParent do
  for t:=0 to FSeriesList.Count-1 do
  Begin
    tmpSeries:=Series[t];
    if (not SumAll) and (tmpSeries=Self) then Break
    else
    With tmpSeries do
    if Active and SameClass(Self) and (Count>ValueIndex) then
       result:=result+GetOriginValue(ValueIndex);
  end;
end;

Function TChartSeries.CreateChartPen:TChartPen;
begin
  result:=TChartPen.Create(CanvasChanged);
end;

Procedure TChartSeries.CheckDataSource;
begin
  if Assigned(FParent) then FParent.CheckDataSource(Self);
end;

procedure TChartSeries.SetRecalcOptions(Value: TSeriesRecalcOptions);
begin
  FRecalcOptions:=Value;
end;

Procedure TChartSeries.SetColorSource(Const Value:String);
Begin
  if FColorSource<>Value then
  Begin
    FColorSource:=Value;
    if Assigned(FParent) then FParent.CheckDataSource(Self);
  end;
end;

Procedure TChartSeries.SetLabelsSource(Const Value:String);
Begin
  if FLabelsSource<>Value then
  Begin
    FLabelsSource:=Value;
    if Assigned(FParent) then FParent.CheckDataSource(Self);
  end;
end;

Function TChartSeries.GetCursorValueIndex:Longint;
Var Mouse : TPoint;
Begin
  Mouse:=ParentChart.GetCursorPos;
  result:=Clicked(Mouse.X,Mouse.Y);
end;

Procedure TChartSeries.GetCursorValues(Var x,y:Double);
Var Mouse : TPoint;
Begin
  Mouse:=ParentChart.GetCursorPos;
  x:=XScreenToValue(Mouse.X);
  y:=YScreenToValue(Mouse.Y);
end;

Function TChartSeries.GetDataSource:TComponent;
begin
  if Assigned(FDataSources) and (FDataSources.Count>0) then
     result:=FDataSources[0]
  else
     result:=nil;
end;

Procedure TChartSeries.InternalAddDataSource(Value:TComponent);
begin
  if Assigned(Value) then
  begin
    FDataSources.Add(Value);
    if Value is TChartSeries then
       TChartSeries(Value).AddLinkedSeries(Self)
    {$IFNDEF D1}
    else
       Value.FreeNotification(Self)
    {$ENDIF}
    ;
  end;
end;

Procedure TChartSeries.RemoveAllLinkedSeries;
var t : Integer;
begin
  if Assigned(FDataSources) then
     for t:=0 to FDataSources.Count-1 do
     if (FDataSources[t]<>nil) and (TComponent(FDataSources[t]) is TChartSeries) then
        TChartSeries(FDataSources[t]).RemoveLinkedSeries(Self);
end;

Procedure TChartSeries.SetDataSource(Value:TComponent);

  Procedure ClearDataSources;
  begin
    RemoveAllLinkedSeries;
    FDataSources.Clear;
  end;

  Procedure InternalRemoveDataSource;
  Begin
    if Assigned(FParent) and
       (DataSource<>nil) then
         FParent.RemovedDataSource(Self,DataSource);
    ClearDataSources;
    if Assigned(FParent) and
       (csDesigning in FParent.ComponentState) then
       FillSampleValues(NumSampleValues)
    else
       Clear;
    Repaint;
  end;

  Procedure InternalSetDataSource;
  Begin
    if not Assigned(FParent) then
       raise ChartException.Create(TeeMsg_SeriesSetDataSource)
    else
    if FParent.IsValidDataSource(Self,Value) then
    Begin
      if FDataSources.IndexOf(Value)=-1 then
      begin
        if Value is TChartSeries then CheckOtherSeries(TChartSeries(Value));
        if not (csLoading in ComponentState) then ClearDataSources;

        InternalAddDataSource(Value);
        if not (csLoading in ComponentState) then FParent.CheckDataSource(Self);
      end;
    end
    else
      Raise ChartException.CreateFmt(TeeMsg_SeriesInvDataSource,[Value.Name]);
  end;

Begin
  if not Assigned(Value) then InternalRemoveDataSource
                         else InternalSetDataSource;
end;

Function TChartSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
Begin
  result:=Value<>Self;
end;

Function TChartSeries.IsValidSeriesSource(Value:TChartSeries):Boolean;
Begin
  result:=True; { abstract }
end;

Procedure TChartSeries.CanvasChanged(Sender:TObject);
Begin
  Repaint;
end;

Function TChartSeries.GetYValueList(AListName:String):TChartValueList;
Var t : Integer;
Begin
  result:=FY;
  AListName:=AnsiUppercase(AListName);
  With FValuesList do
  for t:=2 to Count-1 do
  if AListName=AnsiUpperCase(ValueList[t].Name) then
  Begin
    result:=ValueList[t];
    exit;
  end;
end;

Procedure TChartSeries.CheckOtherSeries(Source:TChartSeries);
Begin
  if Assigned(FParent) then FParent.CheckOtherSeries(Self,Source);
end;

Procedure TChartSeries.ColorRange( AValueList:TChartValueList;
                                   Const FromValue,ToValue:Double; AColor:TColor);
var t        : Longint;
    tmpValue : Double;
begin
  for t:=0 to AValueList.FList.Count-1 do
  Begin
    tmpValue:=AValueList.GetValue(t);
    if (tmpValue>=FromValue) and (tmpValue<=ToValue) then
       FColors[t]:=Pointer(AColor);
  end;
  Repaint;
end;

Function TChartSeries.Clicked(x,y:Integer):Longint;
begin
  Result:=-1; { abstract, no point clicked }
end;

Procedure TChartSeries.RecalcGetAxis;
Begin
  if Assigned(FParent) then
  begin
    FGetHorizAxis:=FParent.FBottomAxis;
    Case FHorizAxis of
      aTopAxis        : FGetHorizAxis:=FParent.FTopAxis;
      aCustomHorizAxis: if Assigned(FCustomHorizAxis) then FGetHorizAxis:=FCustomHorizAxis;
    end;
    FGetVertAxis:=FParent.FLeftAxis;
    Case FVertAxis of
      aRightAxis     : FGetVertAxis:=FParent.FRightAxis;
      aCustomVertAxis: if Assigned(FCustomVertAxis) then FGetVertAxis:=FCustomVertAxis;
    end;
  end
  else
  begin
    FGetHorizAxis:=nil;
    FGetVertAxis:=nil;
  end;
end;

Procedure TChartSeries.SetHorizAxis(Value:THorizAxis);
Begin
  if FHorizAxis<>Value then
  begin
    FHorizAxis:=Value;
    RecalcGetAxis;
    Repaint;
  end;
end;

Procedure TChartSeries.SetVertAxis(Value:TVertAxis);
Begin
  if FVertAxis<>Value then
  begin
    FVertAxis:=Value;
    RecalcGetAxis;
    Repaint;
  end;
end;

Procedure TChartSeries.SetChartValueList( Var AValueList:TChartValueList;
                                          Value:TChartValueList);
Begin
  AValueList.Assign(Value);
  Repaint;
end;

Procedure TChartSeries.SetXValues(Value:TChartValueList);
Begin
  SetChartValueList(FX,Value);
end;

Procedure TChartSeries.SetYValues(Value:TChartValueList);
Begin
  SetChartValueList(FY,Value);
end;

Procedure TChartSeries.SetTitle(Value:String);
Begin
  SetStringProperty(FTitle,Value);
end;

Function TChartSeries.GetValue(IsX:Boolean; ValueIndex:Longint):Double;
begin
  if IsX then result:=GetxValue(ValueIndex)
         else result:=GetyValue(ValueIndex);  { <-- lowercase, conflict with BCB }
end;

Procedure TChartSeries.CalcFirstLastVisibleIndex;

  Function CalcMinMaxValue(A,B,C,D:Integer):Double;
  begin
    if YMandatory then
    With GetHorizAxis do
         if Inverted then result:=CalcPosPoint(C)
                     else result:=CalcPosPoint(A)
    else
    With GetVertAxis do
         if Inverted then result:=CalcPosPoint(B)
                     else result:=CalcPosPoint(D);
  end;

Var tmpMin           : Double;
    tmpMax           : Double;
    NotMandatory     : TChartValueList;
    tmpLastIndex     : Longint;
    OrderedPoints    : Boolean;
Begin
  FFirstVisibleIndex:=-1;
  FLastVisibleIndex:=-1;
  if Count>0 then
  Begin
    tmpLastIndex:=Count-1;
    if YMandatory then OrderedPoints:=XValues.Order<>loNone
                  else OrderedPoints:=YValues.Order<>loNone;

    if CalcVisiblePoints and OrderedPoints then
    begin
      FFirstVisibleIndex:=0;

      With ParentChart.ChartRect do
           tmpMin:=CalcMinMaxValue(Left,Top,Right,Bottom);

      if YMandatory then NotMandatory:=XValues
                    else NotMandatory:=YValues;

      While NotMandatory.GetValue(FFirstVisibleIndex)<tmpMin do
      Begin
        inc(FFirstVisibleIndex);
        if FFirstVisibleIndex>tmpLastIndex then
        begin
          FFirstVisibleIndex:=-1;
          break;
        end;
      end;
      if FFirstVisibleIndex>=0 then
      Begin
        With ParentChart.ChartRect do
             tmpMax:=CalcMinMaxValue(Right,Bottom,Left,Top);

        if NotMandatory.Last<=tmpMax then FLastVisibleIndex:=tmpLastIndex
        else
        Begin
          FLastVisibleIndex:=FFirstVisibleIndex;
          While NotMandatory.GetValue(FLastVisibleIndex)<tmpMax do
          begin
            inc(FLastVisibleIndex);
            if FLastVisibleIndex>tmpLastIndex then
            begin
              FLastVisibleIndex:=tmpLastIndex;
              break;
            end;
          end;
          if (not DrawBetweenPoints) and
             (NotMandatory.GetValue(FLastVisibleIndex)>tmpMax) then
                Dec(FLastVisibleIndex);
        end;
      end;
    end
    else
    begin
      FFirstVisibleIndex:=0;
      FLastVisibleIndex:=tmpLastIndex;
    end;
  end;
end;

Procedure TChartSeries.Repaint;
Begin
  if Assigned(FParent) then FParent.Invalidate;
end;

procedure TChartSeries.DrawValue(ValueIndex:Longint);
begin
      { nothing }
end;

Procedure TChartSeries.SetActive(Value:Boolean);
Begin
  SetBooleanProperty(FActive,Value);
end;

Function TChartSeries.IsValueFormatStored:Boolean;
Begin
  result:=FValueFormat<>TeeMsg_DefValueFormat;
end;

Function TChartSeries.IsPercentFormatStored:Boolean;
Begin
  result:=FPercentFormat<>TeeMsg_DefPercentFormat;
end;

Procedure TChartSeries.DeletedValue(Source:TChartSeries; ValueIndex:Longint);
Begin
  Delete(ValueIndex);
end;

Procedure TChartSeries.AddValue(ValueIndex: Longint);
var t : Integer;
Begin
  With FValuesList do
  for t:=2 to Count-1 do
      With ValueList[t] do InsertChartValue(ValueIndex, TempValue);
end;

Function TChartSeries.AddChartValue(Source:TChartSeries; ValueIndex:Longint):Longint;
var t    : Longint;
    tmpX : Double;
    tmpY : Double;
begin
  With Source do
  begin
    tmpX:=FX.GetValue(ValueIndex);
    tmpY:=FY.GetValue(ValueIndex);
  end;
  { if we are adding values from an Horizontal Bar series... }
  if Self.YMandatory<>Source.YMandatory then SwapDouble(tmpX,tmpY);
  { pending: if ...FY.Order<>loNone then (inverted) }
  result:=FX.AddChartValue(tmpX);
  FY.InsertChartValue(result,tmpY);
  { rest of lists... }
  With FValuesList do
  for t:=2 to Count-1 do
      ValueList[t].InsertChartValue(result,Source.FValuesList.ValueList[t].Value[ValueIndex]);
end;

Procedure TChartSeries.AddedValue(Source:TChartSeries; ValueIndex:Longint);
Var tmpIndex : Longint;
Begin
  tmpIndex:=AddChartValue(Source,ValueIndex);
  if Source.FColors.Count>ValueIndex then FColors.Insert(tmpIndex,Source.FColors[ValueIndex]);
  if Source.FXLabels.Count>ValueIndex then InsertLabel(tmpIndex,Source.GetXLabel(ValueIndex));
  NotifyNewValue(Self,tmpIndex);
end;

Function TChartSeries.LegendToValueIndex(LegendIndex:Integer):Integer;
begin
  result:=LegendIndex;
end;

Function TChartSeries.LegendString( LegendIndex:Integer;
                                    LegendTextStyle:TLegendTextStyle ):String;
Var tmpValue        : Double;
    tmpPercentValue : Double;
    tmpAxis         : TChartAxis;
    tmpSt           : String;
    tmpPercentSt    : String;
    ValueIndex      : Integer;
begin
  ValueIndex:=LegendToValueIndex(LegendIndex);
  result:=XLabel[ValueIndex];
  if LegendTextStyle<>ltsPlain then
  begin
    tmpValue:=GetMarkValue(ValueIndex);
    tmpSt:=FormatFloat(ValueFormat,tmpValue);
    if (LegendTextStyle=ltsLeftPercent) or (LegendTextStyle=ltsRightPercent) then
    With MandatoryValueList do
    begin
      if TotalAbs=0 then tmpPercentValue:=100
                    else tmpPercentValue:=100.0*tmpValue/TotalAbs;
      tmpPercentSt:=FormatFloat(PercentFormat,tmpPercentValue);
    end;
    Case LegendTextStyle of
      ltsLeftValue   : result:=tmpSt+' '+result;
      ltsRightValue  : result:=result+' '+tmpSt;
      ltsLeftPercent : result:=tmpPercentSt+' '+result;
      ltsRightPercent: result:=result+' '+tmpPercentSt;
      ltsXValue      : begin
                         tmpAxis:=GetHorizAxis;
                         if tmpAxis=nil then
                            result:=FormatFloat(ValueFormat,XValue[ValueIndex])
                         else
                            result:=tmpAxis.LabelValue(XValue[ValueIndex]);
                       end;
    end;
  end;
end;

Procedure TChartSeries.AddValues(Source:TChartSeries);
var t : Longint;
Begin
  if IsValidSourceOf(Source) then
  begin
    Clear;
    if FunctionType=nil then { copy values... }
       for t:=0 to Source.Count-1 do AddedValue(Source,t)
    else
    begin
      XValues.DateTime:=Source.XValues.DateTime;
      YValues.DateTime:=Source.YValues.DateTime;
      FunctionType.AddPoints(Source); { calculate function }
    end;
    RefreshSeries; { propagate changes... }
  end;
end;

Procedure TChartSeries.AssignValues(Source:TChartSeries);
Begin
  AddValues(Source);
End;

Procedure TChartSeries.RefreshSeries;
Begin
  NotifyValue(veRefresh,0);
End;

Function TChartSeries.GetEditorClass:String;
Begin
  result:='';
end;

Function TChartSeries.MoreSameZOrder:Boolean;
Var tmpSeries : TChartSeries;
    t         : Longint;
Begin
  for t:=0 to FParent.FSeriesList.Count-1 do
  Begin
    tmpSeries:=FParent.Series[t];
    if tmpSeries<>Self then
    With tmpSeries do
    if FActive and (not HasZValues) and (ZOrder=Self.ZOrder) then
    Begin
      result:=True;
      exit;
    end;
  end;
  result:=False;
End;

Function TChartSeries.FirstInZOrder:Boolean;
Var tmpSeries : TChartSeries;
    t         : Longint;
Begin
  if FActive then
  begin
    result:=True;
    for t:=0 to FParent.FSeriesList.Count-1 do
    Begin
     tmpSeries:=FParent.Series[t];
     if tmpSeries=Self then break
     else
       With tmpSeries do
       if FActive and (ZOrder=Self.ZOrder) then
       Begin
         result:=False;
         break;
       end;
    end;
  end
  else result:=False;
end;

procedure TChartSeries.DoBeforeDrawChart;
begin
end;

procedure TChartSeries.DoBeforeDrawValues;
Begin
  if Assigned(FBeforeDrawValues) then FBeforeDrawValues(Self);
end;

procedure TChartSeries.DoAfterDrawValues;
Begin
  if Assigned(FAfterDrawValues) then FAfterDrawValues(Self);
end;

procedure TChartSeries.DrawMarks;
Var t                   : Longint;
    St                  : String;
    tmpDoubleFrameWidth : Integer;
    tmpW,tmpH           : Integer;
    APosition           : TSeriesMarkPosition;
Begin
  With FParent,FMarks do
  begin
    APosition:=TSeriesMarkPosition.Create;
    for t:=FFirstVisibleIndex to FLastVisibleIndex do
    if ValueColor[t]<>clNone then
    Begin
      St:=GetMarkText(t);
      if St<>'' then
      Begin
        FontCanvas(FFont);
        if View3D and View3Doptions.ZoomText then
        With Canvas.Font do
          Size:=MaxLong(1,Round(0.01*View3DOptions.Zoom*Size));

        tmpW:=MultiLineTextWidth(St,tmpH)+Canvas.TextWidth(TeeCharForHeight);
        tmpH:=tmpH*Canvas.FontHeight;

        Canvas.AssignVisiblePen(FFrame);
        if FFrame.Visible then
        Begin
          tmpDoubleFrameWidth:=2*FFrame.Width;
          Inc(tmpW,tmpDoubleFrameWidth);
          Inc(tmpH,tmpDoubleFrameWidth);
        end;

        With APosition do
        begin
          Width:=tmpW;
          Height:=tmpH;
          ArrowTo.X:=CalcXPos(t);
          ArrowTo.Y:=CalcYPos(t);
          ArrowFrom:=ArrowTo;
          LeftTop.X:=ArrowTo.X-(tmpW div 2);
          LeftTop.Y:=ArrowTo.Y-tmpH+1;
        end;
        DrawMark(t,St,APosition);
      end;
    end;
    APosition.Free;
  end;
end;

Procedure TChartSeries.CalcHorizMargins(Var LeftMargin,RightMargin:Integer);
begin
  LeftMargin:=0; RightMargin:=0;
end;

Procedure TChartSeries.CalcVerticalMargins(Var TopMargin,BottomMargin:Integer);
begin
  TopMargin:=0; BottomMargin:=0;
end;

Procedure TChartSeries.DrawMark( ValueIndex:Longint; Const St:String;
                                 APosition:TSeriesMarkPosition);
Begin
  FMarks.Draw(ValueIndex,ValueColor[ValueIndex],St,APosition);
end;

Procedure TChartSeries.SetIntegerProperty(Var Variable:Integer; Value:Integer);
Begin
  if Variable<>Value then
  begin
    Variable:=Value;   Repaint;
  end;
end;

Procedure TChartSeries.SetLongintProperty(Var Variable:Longint; Value:Longint);
Begin
  if Variable<>Value then
  begin
    Variable:=Value;   Repaint;
  end;
end;

Procedure TChartSeries.SetStringProperty(Var Variable:String; Const Value:String);
Begin
  if Variable<>Value then
  begin
    Variable:=Value;   Repaint;
  end;
end;

Procedure TChartSeries.SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
Begin
  if Variable<>Value then
  begin
    Variable:=Value;   Repaint;
  end;
end;

Procedure TChartSeries.InsertLabel(ValueIndex:Longint; Const ALabel:String);
Begin
  FXLabels.Insert(ValueIndex,GetMemLabel(ALabel));
end;

Function TChartSeries.AddXY( Const AXValue,AYValue:Double;
                             Const ALabel:String; AColor:TColor):Longint;
Begin
  result:=-1;
  FX.TempValue:=AXValue;
  FY.TempValue:=AYValue;
  if (not Assigned(FOnBeforeAdd)) or FOnBeforeAdd(Self) then
  Begin
    result:=FX.AddChartValue(FX.TempValue);
    FY.InsertChartValue(result,FY.TempValue);
    {$IFDEF TEEOCX}
    if AColor=clDefault then AColor:=clTeeColor;  { patch for OCX }
    {$ENDIF}
    FColors.Insert(result,Pointer(AColor));
    InsertLabel(result,ALabel);
    NotifyNewValue(Self,result);
  end;
end;

Procedure TChartSeries.NotifyNewValue(Sender:TChartSeries; ValueIndex:Longint);
Begin
  if Assigned(FOnAfterAdd) then FOnAfterAdd(Sender,ValueIndex);
  NotifyValue(veAdd,ValueIndex);
  if FActive then Repaint;
end;

Function TChartSeries.IsNull(ValueIndex:Longint):Boolean;
begin
  result:=ValueColor[ValueIndex]=clNone;
end;

Function TChartSeries.AddNull(Const ALabel:String):Longint;
begin
  result:=Add( 0, ALabel, clNone );
end;

Function TChartSeries.AddNullXY(Const X,Y:Double; Const ALabel:String):Longint;
begin
  result:=AddXY(X,Y,ALabel,clNone );
end;

Function TChartSeries.AddArray(Const Values:array of Double):Longint;
var t : Longint;
begin
  result:=High(Values)-Low(Values)+1;
  for t:=Low(Values) to High(Values) do Add( Values[t], ''{$IFNDEF D5},clTeeColor{$ENDIF} );
  RefreshSeries;
end;

Function TChartSeries.Add(Const AValue:Double; Const ALabel:String; AColor:TColor):Longint;
Begin
  if YMandatory then result:=AddY( AValue, ALabel, AColor )
                else result:=AddX( AValue, ALabel, AColor );
end;

Function TChartSeries.AddX(Const AXValue:Double; Const ALabel:String; AColor:TColor):Longint;
Begin
  result:=AddXY(AXValue,FX.Count,ALabel,AColor);
end;

Function TChartSeries.AddY(Const AYValue:Double; Const ALabel:String; AColor:TColor):Longint;
Begin
  result:=AddXY(FX.Count,AYValue,ALabel,AColor);
end;

Function TChartSeries.Count:Longint; { <-- Total number of points in the series }
Begin
  result:=FX.Count;
end;

Function TChartSeries.CountLegendItems:Integer;
begin
  result:=Count
end;

Function TChartSeries.VisibleCount:Longint;
Begin
  result:=FLastVisibleIndex-FFirstVisibleIndex+1;
end;

Function TChartSeries.MarkPercent(ValueIndex:Longint; AddTotal:Boolean):String;
var tmp : Double;
Begin
  With MandatoryValueList do
  Begin
    if TotalAbs<>0 then tmp:=100.0*Abs(GetMarkValue(ValueIndex))/TotalAbs
                   else tmp:=100;
    result:=FormatFloat(FPercentFormat,tmp);
    if AddTotal then
       FmtStr(result,TeeMsg_DefaultPercentOf,[result,FormatFloat(FValueFormat,TotalAbs)]);
  end;
end;

Function TChartSeries.GetOriginValue(ValueIndex:Longint):Double;
begin
  result:=GetMarkValue(ValueIndex);
end;

Function TChartSeries.GetMarkValue(ValueIndex:Longint):Double;
begin
  result:=MandatoryValueList[ValueIndex];
end;

Function TChartSeries.GetMarkText(ValueIndex:Longint):String;

  Function XLabelOrValue(ValueIndex:Longint):String;
  Begin
    result:=XLabel[ValueIndex];
    if result='' then result:=FormatFloat(FValueFormat,GetMarkValue(ValueIndex));
  End;

Begin
  With FMarks do
  Case FMarkerStyle of
    smsValue:        result:=FormatFloat(FValueFormat,GetMarkValue(ValueIndex));
    smsPercent:      result:=MarkPercent(ValueIndex,False);
    smsLabel:        result:=XLabelOrValue(ValueIndex);
    smsLabelPercent: result:=XLabelOrValue(ValueIndex)+' '+MarkPercent(ValueIndex,False);
    smsLabelValue:   result:=XLabelOrValue(ValueIndex)+' '+FormatFloat(FValueFormat,GetMarkValue(ValueIndex));
    smsLegend:       result:=ParentChart.FormattedValueLegend(Self,ValueIndex);
    smsPercentTotal: result:=MarkPercent(ValueIndex,True);
    smsLabelPercentTotal: result:=XLabelOrValue(ValueIndex)+' '+MarkPercent(ValueIndex,True);
    smsXValue:       if GetHorizAxis=nil then
                        result:=FormatFloat(FValueFormat,XValue[ValueIndex])
                     else
                        result:=GetHorizAxis.LabelValue(XValue[ValueIndex]);
  else
    result:='';
  end;
  if Assigned(FOnGetMarkText) then FOnGetMarkText(Self,ValueIndex,result);
end;

Procedure TChartSeries.SetValueFormat(Const Value:String);
Begin
  SetStringProperty(FValueFormat,Value);
end;

Procedure TChartSeries.SetPercentFormat(Const Value:String);
Begin
  SetStringProperty(FPercentFormat,Value);
end;

Procedure TChartSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  FillSampleValues(4);
  Marks.Visible:=False;
  FColorEachPoint:=False;
  if not IsEnabled then SeriesColor:=clSilver;
end;

Function TChartSeries.NumSampleValues:Longint;
Begin
  result:=26;  { default number or random values at design time }
end;

Procedure TChartSeries.CalcRandomBounds( NumValues:Longint;
                                         Var tmpX,StepX,tmpY,MinY,DifY:Double);
Var MinX : Double;
    MaxX : Double;
    MaxY : Double;
Begin
  Randomize;
  MinY:=0;
  MaxY:=ChartSamplesMax;
  if Assigned(FParent) and (FParent.GetMaxValuesCount>0) then
  begin
    MinY:=FParent.MinYValue(GetVertAxis);
    MaxY:=FParent.MaxYValue(GetVertAxis);
    if MaxY=MinY then
       if MaxY=0 then MaxY:=ChartSamplesMax
                 else MaxY:=2.0*MinY;
    MinX:=FParent.MinXValue(GetHorizAxis);
    MaxX:=FParent.MaxXValue(GetHorizAxis);
    if MaxX=MinX then
       if MaxX=0 then MaxX:=NumValues
                 else MaxX:=2.0*MinX;
  end
  else
  begin
    if XValues.DateTime then
    begin
      MinX:=Date;
      MaxX:=MinX+NumValues-1;
    end
    else
    begin
      MinX:=0;
      MaxX:=NumValues-1;
    end;
  end;
  StepX:=MaxX-MinX;
  if NumValues>1 then StepX:=StepX/(NumValues-1);
  DifY:=MaxY-MinY;
  if DifY>MaxLongint then DifY:=MaxLongint else
  if DifY<-MaxLongint then DifY:=-MaxLongint;
  tmpY:=MinY+Random(Round(DifY));
  tmpX:=MinX;
end;

Procedure TChartSeries.FillSampleValues(NumValues:Longint);
var t     : Longint;
    tmp   : Longint;
    StepX : Double;
    tmpX  : Double;
    tmpY  : Double;
    MinY  : Double;
    DifY  : Double;
Begin
  if NumValues<=0 then Raise Exception.Create(TeeMsg_FillSample);
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  tmp:=Round(DifY) div 4;
  if Assigned(FParent) then FParent.AutoRepaint:=False;
  for t:=1 to NumValues do
  Begin
    tmpY:=Abs(tmpY+Random(tmp)-(tmp div 2));
    AddXY(tmpX,tmpY{$IFNDEF D5},'', clTeeColor{$ENDIF});
    tmpX:=tmpX+StepX;
  end;
  if Assigned(FParent) then FParent.AutoRepaint:=True;
  RefreshSeries;
End;

Procedure TChartSeries.GalleryChanged3D(Is3D:Boolean);
begin
  With ParentChart do
  begin
    View3D:=Is3D;
    ClipPoints:=not Is3D;
  end;
end;

Procedure TChartSeries.SetDoubleProperty(Var Variable:Double; Const Value:Double);
begin
  if Variable<>Value then
  begin
    Variable:=Value; Repaint;
  end;
end;

Procedure TChartSeries.SetColorProperty(Var Variable:TColor; Value:TColor);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Repaint;
  end;
end;

procedure TChartSeries.Assign(Source:TPersistent);

  Procedure SelfSetDataSources(Value:TList);
  var t : Integer;
  begin
    FDataSources.Clear;
    for t:=0 to Value.Count-1 do InternalAddDataSource(TComponent(Value[t]));
  end;

var t : Integer;
Begin
  if Source is TChartSeries then
  With TChartSeries(Source) do
  begin
    Self.Tag         := Tag;
    Self.DataSource  := nil;
    if Self.YMandatory and YMandatory then
    begin
      Self.FX.Assign(FX);
      Self.FY.Assign(FY);
    end
    else
    begin
      Self.FY.Assign(FX);
      Self.FX.Assign(FY);
    end;
    Self.FLabelsSource:=FLabelsSource;
    Self.FColorSource :=FColorSource;

    { other lists }
    for t:=2 to MinLong(Self.ValuesLists.Count-1,ValuesLists.Count-1) do
        Self.ValuesLists.ValueList[t].Assign(ValuesLists[t]);

    if (FunctionType<>nil) and (Self.FunctionType=nil) then
       FunctionType.ParentSeries:=Self;

    if DataSource=nil then
    begin
      if not (csDesigning in ComponentState) then
      begin
        Self.FColors.Clear;
        for t:=0 to FColors.Count-1 do Self.FColors.Add(FColors[t]);
        Self.FXLabels.Clear;
        for t:=0 to FXLabels.Count-1 do Self.InsertLabel(t,XLabel[t]);
        Self.AssignValues(TChartSeries(Source));
      end;
    end
    else SelfSetDataSources(DataSources); { <-- important !!! }

    Self.FMarks.Assign(FMarks);
    Self.FColor          := FColor;
    Self.FColorEachPoint := FColorEachPoint;
    Self.FTitle          := FTitle;
    Self.FValueFormat    := FValueFormat;
    Self.FPercentFormat  := FPercentFormat;
    Self.FActive         := FActive;
    Self.FHorizAxis      := FHorizAxis;
    Self.FVertAxis       := FVertAxis;
    Self.FRecalcOptions  := FRecalcOptions;
    Self.FCursor         := FCursor;
    Self.FShowInLegend   := FShowInLegend;
    {$IFDEF D3}
    Self.FStyle          := FStyle;
    Self.FIdentifier     := FIdentifier;
    {$ENDIF}
    Self.CheckDataSource;
  end
  else Inherited Assign(Source);
end;

Procedure TChartSeries.SetColorEachPoint(Value:Boolean);
var t : Longint; { <- D1 }
Begin
  SetBooleanProperty(FColorEachPoint,Value);
  if not FColorEachPoint then
     for t:=0 to Count-1 do ValueColor[t]:=clTeeColor;
end;

Procedure TChartSeries.SetShowInLegend(Value:Boolean);
Begin
  SetBooleanProperty(FShowInLegend,Value);
end;

Procedure TChartSeries.GetBitmapEditor(ABitmap:TBitmap);
begin
  TeeGetClassNameBitmap(Self,ABitmap);
end;

Procedure TChartSeries.ReplaceList(OldList,NewList:TChartValueList);
var t:Integer;
begin
  With FValuesList do
  begin
    t:=IndexOf(NewList);
    if t<>-1 then Delete(t);
    for t:=0 to Count-1 do
    if ValueList[t]=OldList then
    begin
      NewList.Name:=OldList.Name;
      if OldList=FX then FX:=NewList else
      if OldList=FY then FY:=NewList;
      TChartValueList(Items[t]).Free;
      Items[t]:=NewList;
      Break;
    end;
  end;
end;

Function TChartSeries.GetxValue(Index:Longint):Double;
Begin
  result:=FX.GetValue(Index);
end;

Function TChartSeries.GetyValue(Index:Longint):Double;
Begin
  result:=FY.GetValue(Index);
end;

Procedure TChartSeries.SetYValue(Index:Longint; Const Value:Double);
Begin
  FY.SetValue(Index,Value);
  Repaint;
end;

Procedure TChartSeries.SetXValue(Index:Longint; Const Value:Double);
Begin
  FX.SetValue(Index,Value);
  Repaint;
end;

Function TChartSeries.GetXLabel(Index:Longint):String;
Begin
  if (FXLabels.Count<=Index) or (FXLabels[Index]=nil) then
     result:=''
  else
     result:=StrPas(FXLabels[Index]);
end;

Procedure TChartSeries.SetXLabel(Index:Longint; Const AXLabel:String);
Begin
  if FXLabels[Index]<>nil then FreeXLabel(Index);
  FXLabels[Index]:=GetMemLabel(AXLabel);
  Repaint;
end;

Function TChartSeries.GetValueColor(ValueIndex:Longint):TColor;
Begin
  if FColors.Count>ValueIndex then
  Begin
    result:=TColor(FColors[ValueIndex]);
    if result=clTeeColor then
       if FColorEachPoint then result:=GetDefaultColor(ValueIndex)
                          else result:=FColor;
  end
  else result:=FColor;
end;

Procedure TChartSeries.SetParentChart(Value:TCustomAxisPanel);
Begin
  if FParent<>Value then
  Begin
    if Assigned(FParent) then FParent.RemoveSeries(Self);
    FParent:=Value;
    RecalcGetAxis;
    if Assigned(FParent) then FParent.InternalAddSeries(Self);
  end;
end;

Procedure TChartSeries.SetValueColor(ValueIndex:Longint; AColor:TColor);
Begin
  FColors[ValueIndex]:=Pointer(AColor);
  Repaint;
end;

Function TChartSeries.GetMemLabel(Const ALabel:String):PChar;
Begin
  if ALabel='' then result:=nil
  else
  Begin
    GetMem(result,Length(ALabel)+1);
    StrPCopy(result,ALabel);
  end;
end;

Procedure TChartSeries.FreeXLabel(ValueIndex:Longint);
Var tmpText : PChar;
Begin
  tmpText:=FXLabels[ValueIndex];
  if Assigned(tmpText) then FreeMem(tmpText,StrLen(tmpText)+1);
end;

Procedure TChartSeries.ClearLists;
Var t : Longint;
Begin
  With FValuesList do for t:=0 to Count-1 do ValueList[t].ClearValues;
  FColors.Clear;
  FColors.Capacity:=TeeDefaultCapacity;   
  for t:=0 to FXLabels.Count-1 do FreeXLabel(t);
  FXLabels.Clear;
  FXLabels.Capacity:=TeeDefaultCapacity;   
  FMarks.ClearPositions;
end;

Procedure TChartSeries.Clear;
begin
  ClearLists;
  if Assigned(FOnClearValues) then FOnClearValues(Self);
  NotifyValue(veClear,0);
  if Assigned(FParent) and (not (csDestroying in FParent.ComponentState)) then Repaint;
end;

Function TChartSeries.AssociatedToAxis(Axis:TCustomChartAxis):Boolean;
Begin
  result:=UseAxis and (
         (Axis.Horizontal and ((FHorizAxis=aBothHorizAxis) or (GetHorizAxis=Axis))) or
         ((not Axis.Horizontal) and ((FVertAxis=aBothVertAxis) or (GetVertAxis=Axis)))
        );
end;

Procedure TChartSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
begin
  With FParent,Canvas do
  begin
    DoRectangle(Rect);  { <-- rectangle }
    if Brush.Color=LegendColor then  { <-- color conflict ! }
    Begin
      if Brush.Color=clBlack then Pen.Color:=clWhite;
      Brush.Style:=bsClear;
      DoRectangle(Rect);  { <-- frame rectangle }
      Pen.Color:=clBlack;
    end;
  end;
end;

Function TChartSeries.LegendItemColor(LegendIndex:Longint):TColor;
begin
  result:=ValueColor[LegendIndex];
end;

Procedure TChartSeries.DrawLegend(ValueIndex:Longint; Const Rect:TRect);
Var tmpBack  : TColor;
    tmpStyle : TBrushStyle;
Begin
  if (ValueIndex<>-1) or (not ColorEachPoint) then
  begin
    with FParent.Canvas.Brush do
    if ValueIndex=-1 then Color:=SeriesColor
                     else Color:=LegendItemColor(ValueIndex);
    if FParent.Canvas.Brush.Color<>clNone then
    begin
      tmpBack:=FParent.LegendColor;
      tmpStyle:=bsSolid;
      PrepareLegendCanvas(ValueIndex,tmpBack,tmpStyle);
      if tmpBack=clTeeColor then
      begin
        tmpBack:=FParent.LegendColor;
        if tmpBack=clTeeColor then tmpBack:=FParent.Color;
      end;
      With FParent do SetBrushCanvas(Canvas.Brush.Color,tmpStyle,tmpBack);
      DrawLegendShape(ValueIndex,Rect);
    end;
  end;
End;

Procedure TChartSeries.PrepareLegendCanvas( ValueIndex:Longint; Var BackColor:TColor;
                                            Var BrushStyle:TBrushStyle);
begin
      { abstract }
End;

Procedure TChartSeries.CalcZOrder;
Begin
  if FZOrder=TeeAutoZOrder then
  begin
    if FParent.View3D then
    begin
      Inc(FParent.FMaxZOrder);
      IZOrder:=FParent.FMaxZOrder;
    end
    else IZOrder:=0;
  end
  else FParent.FMaxZOrder:=MaxLong(FParent.FMaxZOrder,ZOrder);
End;

Function TChartSeries.CalcXPosValue(Const Value:Double):Longint;
Begin
  result:=GetHorizAxis.CalcXPosValue(Value);
end;

Function TChartSeries.XScreenToValue(ScreenPos:Longint):Double;
Begin
  result:=GetHorizAxis.CalcPosPoint(ScreenPos);
end;

Function TChartSeries.CalcXSizeValue(Const Value:Double):Longint;
Begin
  result:=GetHorizAxis.CalcSizeValue(Value);
end;

Function TChartSeries.CalcYPosValue(Const Value:Double):Longint;
Begin
  result:=GetVertAxis.CalcYPosValue(Value);
end;

Function TChartSeries.CalcPosValue(Const Value:Double):Longint;
Begin
  if YMandatory then result:=CalcYPosValue(Value)
                else result:=CalcXPosValue(Value);
end;

Function TChartSeries.XValueToText(Const AValue:Double):String;
Begin
  result:=GetHorizAxis.LabelValue(AValue);
end;

Function TChartSeries.YValueToText(Const AValue:Double):String;
begin
  result:=GetVertAxis.LabelValue(AValue);
end;

Function TChartSeries.YScreenToValue(ScreenPos:Longint):Double;
Begin
  result:=GetVertAxis.CalcPosPoint(ScreenPos);
end;

Function TChartSeries.CalcYSizeValue(Const Value:Double):Longint;
Begin
  result:=GetVertAxis.CalcSizeValue(Value);
end;

Function TChartSeries.CalcXPos(ValueIndex:Longint):Integer;
Begin
  result:=GetHorizAxis.CalcXPosValue(GetXValue(ValueIndex));
end;

Procedure TChartSeries.DoSeriesMouseMove(Shift: TShiftState; X, Y: Integer);
begin
      { abstract }
end;

Procedure TChartSeries.DoSeriesMouseUp(Button:TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
begin
      { abstract }
end;

Function TChartSeries.DoSeriesClick( Sender:TChartSeries;
                                     ValueIndex:Longint;
                                     Button:TMouseButton;
                                     Shift: TShiftState; X, Y: Integer):Boolean;
Begin
  ParentChart.CancelMouse:=False;
  if (ssDouble in Shift) and Assigned(FOnDblClick) then
  begin
    ParentChart.CancelMouse:=True;
    FOnDblClick(Sender,ValueIndex,Button,Shift,x,y);
  end
  else
  if Assigned(FOnClick) then
  begin
    ParentChart.CancelMouse:=True;
    FOnClick(Sender,ValueIndex,Button,Shift,x,y);
  end;
  result:=ParentChart.CancelMouse;
end;

Function TChartSeries.CalcYPos(ValueIndex:Longint):Integer;
begin
  result:=GetVertAxis.CalcYPosValue(GetyValue(ValueIndex)); { "y" in lowercase , BCB3 bug! }
end;

Procedure TChartSeries.Delete(ValueIndex:Longint);
Var t : Integer;
Begin
  With FValuesList do for t:=0 to Count-1 do ValueList[t].Delete(ValueIndex);
  if FColors.Count>ValueIndex then FColors.Delete(ValueIndex);
  if FXLabels.Count>ValueIndex then
  Begin
    FreeXLabel(ValueIndex);
    FXLabels.Delete(ValueIndex);
  end;
  With FMarks.FPositions do if Count>ValueIndex then Delete(ValueIndex);
  NotifyValue(veDelete,ValueIndex);
  Repaint;
end;

procedure TChartSeries.SwapValueIndex(a,b:Longint);
var t : Integer;
begin
  With FValuesList do for t:=0 to Count-1 do ValueList[t].FList.Exchange(a,b);
  FColors.Exchange(a,b);
  FXLabels.Exchange(a,b);
end;

procedure TChartSeries.SetMarks(Value:TSeriesMarks);
begin
  FMarks.Assign(Value);
end;

Procedure TChartSeries.ClearTempValue(ValueList:TChartValueList);
Begin
  ValueList.TempValue:=0;
End;

Procedure TChartSeries.SetSeriesColor(AColor:TColor);
Begin
  SetColorProperty(FColor,AColor);
end;

Function TChartSeries.MaxXValue:Double;
Begin
  result:=FX.MaxValue;
end;

Function TChartSeries.MaxYValue:Double;
Begin
  result:=FY.MaxValue;
end;

Function TChartSeries.MinXValue:Double;
Begin
  result:=FX.MinValue;
end;

Function TChartSeries.MinYValue:Double;
Begin
  result:=FY.MinValue;
end;

Function TChartSeries.MaxZValue:Double;
begin
  result:=ZOrder;
end;

Function TChartSeries.MinZValue:Double;
begin
  result:=ZOrder;
end;

Function TChartSeries.MaxMarkWidth:Longint;
var t : Longint;
Begin
  result:=0;
  for t:=0 to Count-1 do
      result:=MaxLong(result,ParentChart.Canvas.TextWidth(GetMarkText(t)));
end;

Procedure TChartSeries.AddLinkedSeries(ASeries:TChartSeries);
Begin
  if FLinkedSeries.IndexOf(ASeries)=-1 then FLinkedSeries.Add(ASeries);
end;

Procedure TChartSeries.RemoveDataSource(Value:TComponent);
begin
  FDataSources.Remove(Value);
  CheckDataSource;
end;

Procedure TChartSeries.RemoveLinkedSeries(ASeries:TChartSeries);
Begin
  if Assigned(FLinkedSeries) then FLinkedSeries.Remove(ASeries);
end;

Procedure TChartSeries.NotifyValue(ValueEvent:TValueEvent; ValueIndex:Longint);
var t : Integer;
Begin
  for t:=0 to FLinkedSeries.Count-1 do
  if FLinkedSeries[t]<>nil then
  With TChartSeries(FLinkedSeries[t]) do
    if DataSources.IndexOf(Self)<>-1 then { is this necessary? }
    Case ValueEvent of
      veClear  : if rOnClear in FRecalcOptions then Clear;
      veDelete : if rOnDelete in FRecalcOptions then
                    if FunctionType=nil then DeletedValue(Self,ValueIndex);
      veAdd    : if rOnInsert in FRecalcOptions then
                    if FunctionType=nil then AddedValue(Self,ValueIndex);
      veModify : if rOnModify in FRecalcOptions then AddValues(Self);
      veRefresh: AddValues(Self);
    end;
end;

Function TChartSeries.UseAxis:Boolean;
begin
  result:=True;
end;

Procedure TChartSeries.Loaded;
var t : Integer;
begin
  inherited Loaded;
  if Assigned(FTempDataSources) then
  begin
    if FTempDataSources.Count>0 then
    begin
      FDataSources.Clear;
      for t:=0 to FTempDataSources.Count-1 do
          InternalAddDataSource( Owner.FindComponent(FTempDataSources[t]) );
    end;
    FTempDataSources.Free;
  end;
  CheckDatasource;
end;

Destructor TChartSeries.Destroy;
Var t : Integer;
Begin
  FTeeFunction.Free;
  FTeeFunction:=nil;

  RemoveAllLinkedSeries;

  FDataSources.Free;
  FDataSources:=nil;

  for t:=0 to FLinkedSeries.Count-1 do
      TChartSeries(FLinkedSeries[t]).RemoveDataSource(Self);

  Clear;

  FLinkedSeries.Free;
  FLinkedSeries:=nil;

  With FValuesList do for t:=0 to Count-1 do ValueList[t].Free;
  FValuesList.Free;

  FColors.Free;
  FXLabels.Free;
  FMarks.Free;
  if Assigned(FParent) then ParentChart:=nil;
  inherited Destroy;
end;

Procedure TChartSeries.SetCustomHorizAxis(Value:TChartAxis);
begin
  if Value<>FCustomHorizAxis then
  begin
    FCustomHorizAxis:=Value;
    if Assigned(Value) then FHorizAxis:=aCustomHorizAxis
                       else FHorizAxis:=aBottomAxis;
    RecalcGetAxis;
    Repaint;
  end;
end;

Function TChartSeries.GetZOrder:Integer;
begin
  if FZOrder=TeeAutoZOrder then result:=IZOrder
                           else result:=FZOrder;
end;

Procedure TChartSeries.SetZOrder(Value:Integer);
begin
  SetIntegerProperty(FZOrder,Value);
  if FZOrder=TeeAutoZOrder then IZOrder:=0 else IZOrder:=FZOrder;
end;

Procedure TChartSeries.SetCustomVertAxis(Value:TChartAxis);
begin
  if Value<>FCustomVertAxis then
  begin
    FCustomVertAxis:=Value;
    if Assigned(Value) then FVertAxis:=aCustomVertAxis
                       else FVertAxis:=aLeftAxis;
    RecalcGetAxis;
    Repaint;
  end;
end;

{ TChartSeriesList }
procedure TChartSeriesList.SetSeries(Index:Integer; Series:TChartSeries);
begin
  inherited Items[Index]:=Series;
end;

function TChartSeriesList.GetSeries(Index:Integer):TChartSeries;
begin
  result:=TChartSeries(inherited Items[Index]);
end;

Function TChartSeriesList.CountActive:Longint;
var t : Integer;
Begin
  result:=0;
  for t:=0 to Count-1 do if TChartSeries(Items[t]).FActive then Inc(result);
end;

{ TChartAxisTitle }
Procedure TChartAxisTitle.Assign(Source:TPersistent);
Begin
  if Source is TChartAxisTitle then
  With TChartAxisTitle(Source) do
  Begin
    Self.FAxis    := FAxis;
    Self.FAngle   := FAngle;
    Self.FCaption := FCaption;
  end;
  inherited Assign(Source);
end;

Function TChartAxisTitle.IsAngleStored:Boolean;
begin
  result:=FAngle<>FAxis.IDefaultTitleAngle;
end;

Procedure TChartAxisTitle.SetAngle(Value:Integer);
Begin
  if FAngle<>Value then
  begin
    TeeCheckAngle(Value,TeeMsg_AxisTitle);
    FAngle:=Value;
    Repaint;
  end;
end;

Procedure TChartAxisTitle.SetCaption(Const Value:String);
Begin
  ParentChart.SetStringProperty(FCaption,Value);
end;

{ TCustomChartAxis }
Constructor TCustomChartAxis.Create(AOwner: TCustomAxisPanel);
Begin
  inherited Create;
  FParentChart:=AOwner;
  FLabels:=True;
  FLogarithmicBase:=10;
  FAutomatic:=True;
  FAutomaticMaximum:=True;
  FAutomaticMinimum:=True;
  FLabelsSeparation:=10; { % }
  FAxisValuesFormat:=TeeMsg_DefValueFormat;
  FLabelStyle:=talAuto;
  FLabelsOnAxis:=True;
  FLabelsFont:=CreateDefaultFont(FParentChart.CanvasChanged);

  FTickOnLabelsOnly:=True;

  IDefaultTitleAngle:=0;
  FAxisTitle:=TChartAxisTitle.Create(ParentChart);
  FAxisTitle.FAxis:=Self;

  FTicks:=TDarkGrayPen.Create(ParentChart.CanvasChanged);
  FTickLength:=4;

  FMinorTicks:=TDarkGrayPen.Create(ParentChart.CanvasChanged);
  FMinorTickLength:=2;
  FMinorTickCount:=3;

  FTicksInner:=TDarkGrayPen.Create(ParentChart.CanvasChanged);
  FGrid:=TDottedGrayPen.Create(ParentChart.CanvasChanged);
  FAxis:=TChartAxisPen.Create(ParentChart.CanvasChanged);

  FVisible:=True;
  RoundFirstLabel:=True;
  FExactDateTime:=True;
  FParentChart.FAxes.Add(Self);
  FStartPosition:=0;
  FEndPosition:=100;
  FPositionPercent:=0;
end;


Procedure TCustomChartAxis.IncDecDateTime( Increment:Boolean;
                                           Var Value:Double;
                                           Const AnIncrement:Double;
                                           tmpWhichDateTime:TDateTimeStep);
begin
 TeeDateTimeIncrement( FExactDateTime and IAxisDateTime and
                       (tmpWhichDateTime>=dtHalfMonth),
                       Increment,
                       Value,
                       AnIncrement,
                       tmpWhichDateTime );
end;

{ An axis is "DateTime" if at least one Active Series with
  datetime values is associated to it }
Function TCustomChartAxis.IsDateTime:Boolean;
Var tmpSeries : Longint;
Begin
  With ParentChart do
  for tmpseries:=0 to FSeriesList.Count-1 do
  With Series[tmpSeries] do
  if FActive then
      if AssociatedToAxis(Self) then
      begin
        if Self.Horizontal then result:=FX.DateTime
                           else result:=FY.DateTime;
        exit;
      end;
  result:=False;
end;

Procedure TCustomChartAxis.SetTicks(Value:TDarkGrayPen);
Begin
  FTicks.Assign(Value);
end;

Procedure TCustomChartAxis.SetMinorTicks(Value:TDarkGrayPen);
Begin
  FMinorTicks.Assign(Value);
end;

Procedure TCustomChartAxis.SetTicksInner(Value:TDarkGrayPen);
Begin
  FTicksInner.Assign(Value);
end;

Procedure TCustomChartAxis.SetGrid(Value:TDottedGrayPen);
Begin
  FGrid.Assign(Value);
end;

Procedure TCustomChartAxis.SetGridCentered(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FGridCentered,Value);
end;

Procedure TCustomChartAxis.SetAxis(Value:TChartAxisPen);
Begin
  FAxis.Assign(Value);
end;

Destructor TCustomChartAxis.Destroy;
begin
  FMinorTicks.Free;
  FTicks.Free;
  FTicksInner.Free;
  FGrid.Free;
  FAxis.Free;
  FAxisTitle.Free;
  FLabelsFont.Free;
  inherited Destroy;
end;

{$IFNDEF D1}
Function TCustomChartAxis.IsPosStored:Boolean;
begin
  result:=FEndPosition<>0;
end;

Function TCustomChartAxis.IsStartStored:Boolean;
begin
  result:=FEndPosition<>0;
end;

Function TCustomChartAxis.IsEndStored:Boolean;
begin
  result:=FEndPosition<>100;
end;
{$ENDIF}

Function TCustomChartAxis.CalcPosPoint(Value:Longint):Double;

  Function InternalCalcPos(Const A,B:Double):Double;
  begin
    if (Horizontal and FInverted) or
       ((not Horizontal) and (not FInverted)) then result:=A
                                              else result:=B
  end;

var tmp    : Double;
    LogMin : Double;
    LogMax : Double;
Begin
  if FLogarithmic then
  Begin
    if Value=IStartPos then result:=InternalCalcPos(IMaximum,IMinimum)
    else
    if Value=IEndPos then result:=InternalCalcPos(IMinimum,IMaximum)
    else
    begin
      tmp:=InternalCalcLog(LogMax,LogMin);
      if tmp=0 then result:=IMinimum
      else
      begin
        if FInverted then tmp:=((IEndPos-Value)*tmp/IAxisSize)
                     else tmp:=((Value-IStartPos)*tmp/IAxisSize);
        if Horizontal then result:=Exp(LogMin+tmp)
                      else result:=Exp(LogMax-tmp);
      end;
    end;
  end
  else
  if IAxisSize>0 then
  begin
    if FInverted then tmp:=IEndPos-Value
                 else tmp:=Value-IStartPos;
    tmp:=tmp*IRange/IAxisSize;
    if Horizontal then result:=IMinimum+tmp
                  else result:=IMaximum-tmp;
  end
  else result:=0;
end;

Procedure TCustomChartAxis.SetDateTimeFormat(Const Value:String);
Begin
  ParentChart.SetStringProperty(FDateTimeFormat,Value);
end;

procedure TCustomChartAxis.SetAxisTitle(Value:TChartAxisTitle);
begin
  FAxisTitle.Assign(Value);
end;

procedure TCustomChartAxis.SetStartPosition(Const Value:Double);
begin
  ParentChart.SetDoubleProperty(FStartPosition,Value);
end;

procedure TCustomChartAxis.SetEndPosition(Const Value:Double);
begin
  ParentChart.SetDoubleProperty(FEndPosition,Value);
end;

procedure TCustomChartAxis.SetPositionPercent(Const Value:Double);
begin
  ParentChart.SetDoubleProperty(FPositionPercent,Value);
end;

procedure TCustomChartAxis.SetRoundFirstLabel(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FRoundFirstLabel,Value);
end;

Procedure TCustomChartAxis.SetLabelsMultiLine(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FLabelsMultiLine,Value);
end;

procedure TCustomChartAxis.SetTickOnLabelsOnly(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FTickOnLabelsOnly,Value);
end;

Function TCustomChartAxis.CalcDateTimeIncrement(MaxNumLabels:Longint):Double;
Var TempNumLabels : Longint;
Begin
  result:=MaxDouble(FDesiredIncrement,DateTimeStep[dtOneSecond]);
  if (result>0) and (MaxNumLabels>0) then
  begin
    if (IRange/Result)>1000000 then Result:=IRange/1000000;
    Repeat
      TempNumLabels:=Round(IRange/result);
      if TempNumLabels>MaxNumLabels then
         if result<DateTimeStep[dtOneYear] then
            result:=NextDateTimeStep(result)
         else
            result:=2.0*result;
    Until (TempNumLabels<=MaxNumLabels){ or (result=DateTimeStep[dtOneYear])};
  end;
  result:=MaxDouble(result,DateTimeStep[dtOneSecond]);
end;

Function TCustomChartAxis.InternalCalcLabelsIncrement(MaxNumLabels:Longint):Double;
Var TempNumLabels : Longint;
    tmp           : Double;
Begin
  if FDesiredIncrement<=0 then
  Begin
    Result:=Abs(IRange);
    if Result<1 then Result:=MinAxisIncrement
                else Result:=1;
  end
  else
    Result:=FDesiredIncrement;
  if MaxNumLabels>0 then
  Begin
    TempNumLabels:=MaxNumLabels+1;
    if LabelsSeparation>0 then
    Repeat
      tmp:=IRange/Result;
      if Abs(tmp)<MaxLongint then
      begin
        TempNumLabels:=Round(tmp);
        if TempNumLabels>MaxNumLabels then Result:=TeeNextStep(Result);
      end
      else Result:=TeeNextStep(Result);
    Until (TempNumLabels<=MaxNumLabels){ or (result>1000000000)};
  end;
  result:=MaxDouble(Result,MinAxisIncrement);
end;

Function TCustomChartAxis.CalcLabelsIncrement(MaxNumLabels:Longint):Double;
Begin
  if MaxNumLabels>0 then
  Begin
    if IAxisDateTime then result:=CalcDateTimeIncrement(MaxNumLabels)
                     else result:=InternalCalcLabelsIncrement(MaxNumLabels);
  end
  else
  if IAxisDateTime then result:=DateTimeStep[dtOneSecond]
                   else result:=MinAxisIncrement;
end;

Function TCustomChartAxis.LabelWidth(Const Value:Double):Longint;
var tmp:Integer;
Begin
  result:=ParentChart.MultiLineTextWidth(LabelValue(Value),tmp);
  if (FLabelsAngle=90) or (FLabelsAngle=270) then
     result:=ParentChart.Canvas.FontHeight*tmp;
End;

Function TCustomChartAxis.LabelHeight(Const Value:Double):Longint;
var tmp:Integer;
Begin
  result:=ParentChart.MultiLineTextWidth(LabelValue(Value),tmp);
  if (FLabelsAngle=0) or (FLabelsAngle=180) then
     result:=ParentChart.Canvas.FontHeight*tmp;
End;

Function TCustomChartAxis.IsMaxStored:Boolean;
Begin
      { dont store max property if automatic }
  result:=(not FAutomatic) and (not FAutomaticMaximum);
end;

Function TCustomChartAxis.IsMinStored:Boolean;
Begin
     { dont store min property if automatic }
  result:=(not FAutomatic) and (not FAutomaticMinimum);
end;

Function TCustomChartAxis.CalcXYIncrement(MaxLabelSize:Integer):Double;
begin
  if FLabelsSeparation>0 then
     Inc(MaxLabelSize,Round(0.01*FLabelsSeparation*MaxLabelSize));
  if MaxLabelSize>0 then
     result:=CalcLabelsIncrement(Round((1.0*IAxisSize)/MaxLabelSize))
  else
     result:=CalcLabelsIncrement(1);
end;

Function TCustomChartAxis.CalcIncrement:Double;
Begin
  if Horizontal then
     result:=CalcXYIncrement( MaxLong(LabelWidth(IMinimum),LabelWidth(IMaximum)) )
  else
     result:=CalcXYIncrement( MaxLong(LabelHeight(IMinimum),LabelHeight(IMaximum)) );
End;

Procedure TCustomChartAxis.AdjustMaxMinRect(Const Rect:TRect);
Var tmpMin : Double;
    tmpMax : Double;

    Procedure RecalcAdjustedMinMax(Pos1,Pos2:Integer);
    Var OldStart : Integer;
        OldEnd   : Integer;
    Begin
      OldStart :=IStartPos;
      OldEnd   :=IEndPos;
      Inc(IStartPos,Pos1);
      Dec(IEndPos,Pos2);
      IAxisSize:=IEndPos-IStartPos;
      tmpMin:=CalcPosPoint(OldStart);
      tmpMax:=CalcPosPoint(OldEnd);
    end;

Begin
  With ParentChart do
  begin
    with Rect do
    if Horizontal then ReCalcAdjustedMinMax(Left,Right)
                  else ReCalcAdjustedMinMax(Top,Bottom);
    InternalCalcPositions;
    IMinimum:=tmpMin;
    IMaximum:=tmpMax;
  end;
  if IMinimum>IMaximum then SwapDouble(IMinimum,IMaximum);
  IRange:=IMaximum-IMinimum;
end;

Procedure TCustomChartAxis.CalcMinMax(Var AMin,AMax:Double);
Begin
  if FAutomatic or FAutomaticMaximum then
     AMax:=ParentChart.InternalMinMax(Self,False,Horizontal);
  if FAutomatic or FAutomaticMinimum then
     AMin:=ParentChart.InternalMinMax(Self,True,Horizontal);
end;

Procedure TCustomChartAxis.AdjustMaxMin;
Begin
  CalcMinMax(FMinimumValue,FMaximumValue);
  IMaximum:=FMaximumValue;
  IMinimum:=FMinimumValue;
  IRange  :=IMaximum-IMinimum;
end;

procedure TCustomChartAxis.Assign(Source: TPersistent);
Begin
  if Source is TCustomChartAxis then
  With TCustomChartAxis(Source) do
  Begin
    Self.FVisible             :=FVisible;
    Self.FLabels              :=FLabels;
    Self.FLabelsAngle         :=FLabelsAngle;
    Self.FLabelsFont.Assign(FLabelsFont);
    Self.FLabelsSeparation    :=FLabelsSeparation;
    Self.FLabelsSize          :=FLabelsSize;
    Self.FTitleSize           :=FTitleSize;
    Self.FLabelStyle          :=FLabelStyle;
    Self.FLabelsOnAxis        :=FLabelsOnAxis;
    Self.FTicks.Assign(FTicks);
    Self.FMinorTicks.Assign(FMinorTicks);
    Self.FTicksInner.Assign(FTicksInner);
    Self.FGrid.Assign(FGrid);
    Self.FAxis.Assign(FAxis);
    Self.FTickLength          :=FTickLength;
    Self.FMinorTickLength     :=FMinorTickLength;
    Self.FMinorTickCount      :=FMinorTickCount;
    Self.FTickInnerLength     :=FTickInnerLength;
    Self.FAxisValuesFormat    :=FAxisValuesFormat;
    Self.FDesiredIncrement    :=FDesiredIncrement;
    Self.FMaximumValue        :=FMaximumValue;
    Self.FMinimumValue        :=FMinimumValue;
    Self.FAutomatic           :=FAutomatic;
    Self.FAutomaticMaximum    :=FAutomaticMaximum;
    Self.FAutomaticMinimum    :=FAutomaticMinimum;
    Self.FAxisTitle.Assign(FAxisTitle);
    Self.FDateTimeFormat      :=FDateTimeFormat;
    Self.FLogarithmic         :=FLogarithmic;
    Self.FLogarithmicBase     :=FLogarithmicBase;
    Self.FInverted            :=FInverted;
    Self.FExactDateTime       :=FExactDateTime;
    Self.FRoundFirstLabel     :=FRoundFirstLabel;
    Self.FTickOnLabelsOnly    :=FTickOnLabelsOnly;
    Self.IsDepthAxis          :=IsDepthAxis;
    Self.FStartPosition       :=FStartPosition;
    Self.FEndPosition         :=FEndPosition;
    Self.FPositionPercent     :=FPositionPercent;
  end
  else inherited Assign(Source);
end;

Function TCustomChartAxis.LabelValue(Const Value:Double):String;
Begin
  if IAxisDateTime then
  begin
    if Value>=0 then
    Begin
      if FDateTimeFormat='' then
         DateTimeToString(result,DateTimeDefaultFormat(IRange),Value)
      else
         DateTimeToString(result,FDateTimeFormat,Value)
    end
    else result:='';
  end
  else result:=FormatFloat(FAxisValuesFormat,Value);
  if Assigned(ParentChart.FOnGetAxisLabel) then
     ParentChart.FOnGetAxisLabel(TChartAxis(Self),nil,-1,Result);
  if FLabelsMultiLine then TeeSplitInLines(Result,' ');
end;

Function TCustomChartAxis.CalcLabelStyle:TAxisLabelStyle;
var {tmpSeries : TChartSeries;}
    t         : Integer;
Begin
 if FLabelStyle=talAuto then
 Begin
   if IsDepthAxis then
   begin
     result:=talText;
     With ParentChart do
     if SeriesList.CountActive>0 then
     for t:=0 to FSeriesList.Count-1 do
     With Series[t] do
     if Active then
        if HasZValues or (MinZValue<>MaxZValue) then
        begin
          result:=talValue;
          break;
        end;
   end
   else
   begin
     result:=talNone;
     for t:=0 to ParentChart.SeriesList.Count-1 do
     With ParentChart.Series[t] do
     if Active and AssociatedToAxis(Self) then
     begin
       result:=talValue;
       if (Horizontal and {tmpSeries.}YMandatory) or
          ((not Horizontal) and (not {tmpSeries.}YMandatory)) then
          if (FXLabels.Count>0) and (FXLabels.First<>nil) then
          begin
            result:=talText;
            break;
          end;
     end;

   {  tmpSeries:=ParentChart.GetAxisSeries(Self);
     if not Assigned(tmpSeries) then result:=talNone
     else
     Begin
       if (Horizontal and tmpSeries.YMandatory) or
          ((not Horizontal) and (not tmpSeries.YMandatory)) then
       begin
         if (tmpSeries<>nil) and (tmpSeries.FXLabels.Count>0) and
            (tmpSeries.FXLabels.First<>nil) then
            result:=talText
         else
            result:=talValue;
       end
       else result:=talValue;
     end;}
   end;
 end
 else result:=FLabelStyle;
End;

Function TCustomChartAxis.MaxLabelsWidth:Longint;

  Function MaxLabelsValueWidth:Longint;
  var tmp    : Double;
      tmpA   : Double;
      tmpB   : Double;
      OldGetAxisLabel : TAxisOnGetLabel;
      tmpNum : Integer;
  begin
    if (IsDateTime and FExactDateTime) or RoundFirstLabel then
    begin
      tmp:=CalcIncrement;
      tmpA:=tmp*Int(IMinimum/tmp);
      tmpB:=tmp*Int(IMaximum/tmp);
    end
    else
    begin
      tmpA:=IMinimum;
      tmpB:=IMaximum;
    end;
    With ParentChart do
    begin
      OldGetAxisLabel:=FOnGetAxisLabel;
      FOnGetAxisLabel:=nil;
      With Canvas do
           result:=TextWidth(' ')+ MaxLong( MultiLineTextWidth(LabelValue(tmpA),tmpNum),
                                            MultiLineTextWidth(LabelValue(tmpB),tmpNum));
      FOnGetAxisLabel:=OldGetAxisLabel;
    end;
  end;

Begin
  Case CalcLabelStyle of
    talValue : result:=MaxLabelsValueWidth;
    talMark  : result:=ParentChart.MaxMarkWidth;
    talText  : result:=ParentChart.MaxTextWidth;
  else
  {talNone : } result:=0;
  end;
end;

Procedure TCustomChartAxis.SetLabels(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FLabels,Value);
end;

Procedure TCustomChartAxis.SetLabelsFont(Value:TFont);
begin
  FLabelsFont.Assign(Value);
end;

Function TCustomChartAxis.IsFontStored:Boolean;
begin
  result:=not IsDefaultFont(FLabelsFont);
end;

Procedure TCustomChartAxis.SetAutomatic(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FAutomatic,Value);
  if {Value and} (not (csLoading in ParentChart.ComponentState)) then { 4.01 }
  begin
    FAutomaticMinimum:=Value;
    FAutomaticMaximum:=Value;
  end;
end;

Procedure TCustomChartAxis.SetAutomaticMinimum(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FAutomaticMinimum,Value);
  if Value then
  begin { if both are automatic, then Automatic should be True too }
    if FAutomaticMaximum then FAutomatic:=True;
  end
  else FAutomatic:=False;
end;

Procedure TCustomChartAxis.SetAutomaticMaximum(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FAutomaticMaximum,Value);
  if Value then
  begin { if both are automatic, then Automatic should be True too }
    if FAutomaticMinimum then FAutomatic:=True;
  end
  else FAutomatic:=False;
end;

Function TCustomChartAxis.IsAxisValuesFormatStored:Boolean;
begin
  result:=FAxisValuesFormat<>TeeMsg_DefValueFormat;
end;

Procedure TCustomChartAxis.SetValuesFormat(Const Value:String);
Begin
  ParentChart.SetStringProperty(FAxisValuesFormat,Value);
end;

Procedure TCustomChartAxis.SetInverted(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FInverted,Value);
end;

Procedure TCustomChartAxis.InternalSetInverted(Value:Boolean);
Begin
  FInverted:=Value;
end;

Procedure TCustomChartAxis.SetLogarithmicBase(Value:Integer);
begin
  if FLogarithmicBase<2 then raise AxisException.Create(TeeMsg_AxisLogBase);
  ParentChart.SetIntegerProperty(FLogarithmicBase,Value);
end;

Procedure TCustomChartAxis.SetLogarithmic(Value:Boolean);
Begin
  if Value and IsDateTime then
     Raise AxisException.Create(TeeMsg_AxisLogDateTime);
  if Value then
  begin
    AdjustMaxMin;
    if ((IMinimum<0) or (IMaximum<0)) then
       Raise AxisException.Create(TeeMsg_AxisLogNotPositive);
  end;
  ParentChart.SetBooleanProperty(FLogarithmic,Value);
End;

Procedure TCustomChartAxis.SetLabelsAngle(Value:Integer);
Begin
  TeeCheckAngle(Value,TeeMsg_AxisLabels);
  ParentChart.SetIntegerProperty(FLabelsAngle,Value);
end;

Procedure TCustomChartAxis.SetLabelsSeparation(Value:Integer);
Begin
  if Value<0 then
     Raise AxisException.Create(TeeMsg_AxisLabelSep);
  ParentChart.SetIntegerProperty(FLabelsSeparation,Value);
end;

Procedure TCustomChartAxis.SetLabelsSize(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FLabelsSize,Value);
end;

Procedure TCustomChartAxis.SetTitleSize(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FTitleSize,Value);
end;

Procedure TCustomChartAxis.SetLabelsOnAxis(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FLabelsOnAxis,Value);
end;

Procedure TCustomChartAxis.SetExactDateTime(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FExactDateTime,Value);
end;

Procedure TCustomChartAxis.SetLabelStyle(Value:TAxisLabelStyle);
begin
  if FLabelStyle<>Value then
  begin
    FLabelStyle:=Value;
    ParentChart.Repaint;
  end;
end;

Procedure TCustomChartAxis.SetVisible(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FVisible,Value);
end;

Procedure TCustomChartAxis.SetDesiredIncrement(Const Value:Double);
Begin
  if Value<0 then Raise AxisException.Create(TeeMsg_AxisIncrementNeg);
  if IsDateTime then DateTimeToStr(Value);
  ParentChart.SetDoubleProperty(FDesiredIncrement,Value);
end;

Procedure TCustomChartAxis.SetMinimum(Const Value:Double);
Begin
  if (not (csReading in ParentChart.ComponentState)) and
     (Value>FMaximumValue) then
       Raise AxisException.Create(TeeMsg_AxisMinMax);
  InternalSetMinimum(Value);
end;

Procedure TCustomChartAxis.InternalSetMinimum(Const Value:Double);
Begin
  ParentChart.SetDoubleProperty(FMinimumValue,Value);
end;

Procedure TCustomChartAxis.SetMaximum(Const Value:Double);
Begin
  if (not (csReading in ParentChart.ComponentState)) and
     (Value<FMinimumValue) then
       Raise AxisException.Create(TeeMsg_AxisMaxMin);
  InternalSetMaximum(Value);
end;

Procedure TCustomChartAxis.SetMinMax(Const AMin,AMax:Double);
Begin
  FAutomatic:=False;
  FAutomaticMinimum:=False;
  FAutomaticMaximum:=False;
  if AMin<=AMax then
  Begin
    InternalSetMinimum(AMin);
    InternalSetMaximum(AMax);
  End
  else
  Begin
    InternalSetMinimum(AMax);
    InternalSetMaximum(AMin);
  End;
  if (FMaximumValue-FMinimumValue)<MinAxisRange then
     InternalSetMaximum(FMinimumValue+MinAxisRange);
end;

Procedure TCustomChartAxis.InternalSetMaximum(Const Value:Double);
Begin
  ParentChart.SetDoubleProperty(FMaximumValue,Value);
end;

Procedure TCustomChartAxis.SetTickLength(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FTickLength,Value);
end;

Procedure TCustomChartAxis.SetMinorTickLength(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FMinorTickLength,Value);
end;

Procedure TCustomChartAxis.SetMinorTickCount(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FMinorTickCount,Value);
End;

Procedure TCustomChartAxis.SetTickInnerLength(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FTickInnerLength,Value);
end;

Procedure TCustomChartAxis.DrawTitle(x,y:Longint);
begin
  With ParentChart,Canvas do
  if Printing and (Font.Color=clWhite) then Font.Color:=clBlack;
  With FAxisTitle do
  if IsDepthAxis then
  With ParentChart,Canvas do
  begin
    TextAlign:=TA_LEFT;
    TextOut3D(x,y,Width3D div 2,FCaption);
  end
  else DrawAxisLabel(x,y,FAngle,FCaption);
end;

procedure TCustomChartAxis.DrawAxisLabel(x,y,Angle:Integer; Const St:String);
Const Aligns:Array[Boolean,Boolean] of Integer=
        ( { vertical }   (TA_RIGHT +TA_TOP, TA_LEFT  +TA_TOP    ),
          { horizontal } (TA_CENTER+TA_TOP, TA_CENTER+TA_BOTTOM )
        );


var Delta    : Integer;
    t        : Integer;
    n        : Integer;
    i        : Integer;
    tmpZ     : Integer;
    tmpSt    : String;
    tmpSt2   : String;
    tmpH     : Integer;
    tmpD     : Integer;
    tmpAlign : TCanvasTextAlign;
begin
  tmpH:=(ParentChart.Canvas.FontHeight div 2);
  Case Angle of
    0: Begin
         tmpAlign:=Aligns[Horizontal,OtherSide];
         if not Horizontal then Dec(Y,tmpH);
       end;
   90: Begin
         if Horizontal then
         begin
           tmpAlign:=Aligns[False,OtherSide];
           Dec(X,tmpH);
         end
         else tmpAlign:=Aligns[True,not OtherSide];
       end;
  180: Begin
         tmpAlign:=Aligns[Horizontal,not OtherSide];
         if not Horizontal then Inc(Y,tmpH);
       end;
  270: Begin
         if Horizontal then
         begin
           tmpAlign:=Aligns[False,not OtherSide];
           Inc(X,tmpH);
         end
         else tmpAlign:=Aligns[True,OtherSide];
       end;
    else tmpAlign:=TA_LEFT; { non-supported angles }
  end;
  With ParentChart.Canvas do
  begin
    if OtherSide then tmpZ:=ParentChart.Width3D
                 else tmpZ:=0;
    TextAlign:=tmpAlign;
    n:=TeeNumTextLines(St);
    Delta:=FontHeight;
    if (Angle=180) or (Angle=270) then Delta:=-Delta;

    tmpD:=Round(Delta*n);
    if Horizontal then
    begin
      if Angle=0 then
         if OtherSide then y:=y-tmpD else y:=y-Delta
      else
      if Angle=180 then
         if OtherSide then y:=y-Delta else y:=y-tmpD
      else
      if (Angle=90) or (Angle=270) then
         x:=x-Round(0.5*Delta*(n+1));
    end
    else
      if (Angle=0) or (Angle=180) then
         y:=y-Round(0.5*Delta*(n+1))
      else
      if OtherSide then
      begin
         if Angle=90 then x:=x-Delta
                     else if Angle=270 then x:=x-tmpD
      end
      else
      if Angle=90 then x:=x-tmpD
                  else if Angle=270 then x:=x-Delta;

    tmpSt:=St;
    for t:=1 to n do
    begin
      i:=AnsiPos(TeeLineSeparator,tmpSt);
      if i>0 then tmpSt2:=Copy(tmpSt,1,i-1) else tmpSt2:=tmpSt;
      if Angle=0 then
      begin
        y:=y+Delta;
        TextOut3D(X,Y,tmpZ,tmpSt2);
      end
      else
      begin
        if Angle=180 then y:=y+Delta
        else
        if (Angle=90) or (Angle=270) then x:=x+Delta;
        RotateLabel3D(X,Y,tmpZ,tmpSt2,Angle);
      end;
      Delete(tmpSt,1,i);
    end;
    TextAlign:=TA_LEFT;
  end;
end;

Procedure TCustomChartAxis.Scroll(Const Offset:Double; CheckLimits:Boolean);
Begin
  if (not CheckLimits) or
     ( ((Offset>0) and (FMaximumValue<ParentChart.InternalMinMax(Self,False,Horizontal))) or
       ((Offset<0) and (FMinimumValue>ParentChart.InternalMinMax(Self,True,Horizontal)))
     ) then
  begin
    FAutomatic:=False;
    FAutomaticMaximum:=False;
    FMaximumValue:=FMaximumValue+Offset;
    FAutomaticMinimum:=False;
    FMinimumValue:=FMinimumValue+Offset;
    ParentChart.Repaint;
  end;
end;

Function TCustomChartAxis.InternalCalcLog(Var LogMax,LogMin:Double):Double;
Begin
  if IMinimum<=0 then LogMin:=0 else LogMin:=ln(IMinimum);
  if IMaximum<=0 then LogMax:=0 else LogMax:=ln(IMaximum);
  result:=LogMax-LogMin;
end;

Function TCustomChartAxis.InternalCalcDepthPosValue(Const Value:Double):Longint;
var tmp:Double;
begin
  With ParentChart do
  if IRange=0 then result:=Width3D div 2
  else
  begin
    if FInverted then tmp:=IMaximum-Value
                 else tmp:=Value-IMinimum;
    result:=Round(1.0*Width3D*tmp/IRange);
  end;
end;

Function TCustomChartAxis.InternalCalcLogPosValue(IsX:Boolean; Const Value:Double):Longint;
var tmp      : Double;
    tmpValue : Double;
    LogMax   : Double;
    LogMin   : Double;
begin
  tmp:=InternalCalcLog(LogMax,LogMin);
  if tmp=0 then result:=ICenterPos
  else
  begin
    if Value<=0 then
       if (IsX and FInverted) or
          ((not IsX) and (not FInverted)) then result:=IEndPos
                                          else result:=IStartPos
    else
    begin
      if FInverted then tmpValue:=LogMax-ln(Value)
                   else tmpValue:=ln(Value)-LogMin;
      if IsX then result:=IStartPos+Round(tmpValue*IAxisSize/tmp)
             else result:=IEndPos-Round(tmpValue*IAxisSize/tmp);
    end;
  end;
end;

Function TCustomChartAxis.CalcPosValue(Const Value:Double):Longint;
begin
  if Horizontal then result:=CalcXPosValue(Value)
                else result:=CalcYPosValue(Value);
end;

Function TCustomChartAxis.CalcXPosValue(Const Value:Double):Longint;
begin
  if IsDepthAxis then result:=InternalCalcDepthPosValue(Value) else
  if FLogarithmic then result:=InternalCalcLogPosValue(True,Value)
                  else result:=InternalCalcPosValue(Value,FInverted);
end;

Function TCustomChartAxis.InternalCalcPosValue( Const Value:Double;
                                                FromEnd:Boolean):Longint;
var tmp : Double;
begin
  if IRange=0 then result:=ICenterPos
  else
  begin
    tmp:=(Value-IMinimum)*IAxisSize/IRange;
    if Abs(tmp)>=MaxLongint then
       result:=ICenterPos
    else
       if FromEnd then result:=IEndPos-Round(tmp)
                  else result:=IStartPos+Round(tmp)
  end;
end;

Function TCustomChartAxis.CalcYPosValue(Const Value:Double):Longint;
begin
  if IsDepthAxis then result:=InternalCalcDepthPosValue(Value) else
  if FLogarithmic then result:=InternalCalcLogPosValue(False,Value)
                  else result:=InternalCalcPosValue(Value,not FInverted);
end;

Function TCustomChartAxis.CalcSizeValue(Const Value:Double):Longint;
var tmp    : Double;
    LogMax : Double;
    LogMin : Double;
begin
  result:=0;
  if Value>0 then
    if FLogarithmic then
    Begin
      tmp:=InternalCalcLog(LogMax,LogMin);
      if tmp<>0 then result:=Round(ln(Value)*IAxisSize/tmp);
    end
    else
    if IRange<>0 then result:=Round(Value*IAxisSize/IRange);
end;

Function TCustomChartAxis.Clicked(x,y:Integer):Boolean;
Var tmpPos1 : Integer;
    Pos1    : Integer;
    tmpPos2 : Integer;
    Pos2    : Integer;
    tmpR    : TRect;
Begin
  if ParentChart.IsAxisVisible(Self) then
  begin
    if IStartPos>IEndPos then
    begin
      tmpPos1:=IEndPos;
      tmpPos2:=IStartPos;
    end
    else
    begin
      tmpPos1:=IStartPos;
      tmpPos2:=IEndPos;
    end;

    if PosAxis>FPosLabels then
    begin
      Pos1:=FPosLabels;
      Pos2:=PosAxis+TeeAxisClickGap;
    end
    else
    begin
      Pos1:=PosAxis-TeeAxisClickGap;
      Pos2:=FPosLabels;
    end;

    if Horizontal then tmpR:=Rect(tmpPos1,Pos1,tmpPos2,Pos2)
                  else tmpR:=Rect(Pos1,tmpPos1,Pos2,tmpPos2);
    result:=PtInRect(tmpR,Point(x,y));
  end
  else result:=False;
end;

Procedure TCustomChartAxis.CustomDrawMinMaxStartEnd( APosLabels,
                                                     APosTitle,
                                                     APosAxis:Integer;
                                                     GridVisible:Boolean;
                                                     Const AMinimum,AMaximum,
                                                           AIncrement:Double;
                                                     AStartPos,AEndPos:Integer);

  Procedure SetInternals;
  begin
    IMaximum :=FMaximumValue;
    IMinimum :=FMinimumValue;
    IRange   :=IMaximum-IMinimum;
  end;

var OldMin       : Double;
    OldMax       : Double;
    OldIncrement : Double;
    OldAutomatic : Boolean;
begin
  OldMin      :=FMinimumValue;
  OldMax      :=FMaximumValue;
  OldIncrement:=FDesiredIncrement;
  OldAutomatic:=FAutomatic;
  try
    FAutomatic       :=False;
    FMinimumValue    :=AMinimum;
    FMaximumValue    :=AMaximum;
    FDesiredIncrement:=AIncrement;
    SetInternals;
    CustomDrawStartEnd(APosLabels,APosTitle,APosAxis,GridVisible,AStartPos,AEndPos);
  finally
    FMinimumValue    :=OldMin;
    FMaximumValue    :=OldMax;
    FDesiredIncrement:=OldIncrement;
    FAutomatic       :=OldAutomatic;
    SetInternals;
  end;
end;

Procedure TCustomChartAxis.CustomDrawMinMax( APosLabels,
                                             APosTitle,
                                             APosAxis:Integer;
                                             GridVisible:Boolean;
                                             Const AMinimum,AMaximum,
                                                   AIncrement:Double);
begin
  CustomDrawMinMaxStartEnd(APosLabels,APosTitle,APosAxis,GridVisible,
        AMinimum,AMaximum,AIncrement,IStartPos,IEndPos);
end;

Procedure TCustomChartAxis.CustomDraw( APosLabels,APosTitle,APosAxis:Integer;
                                       GridVisible:Boolean);
begin
  InternalCalcPositions;
  CustomDrawStartEnd(APosLabels,APosTitle,APosAxis,GridVisible,IStartPos,IEndPos);
End;

Procedure TCustomChartAxis.CustomDrawStartEnd( APosLabels,APosTitle,APosAxis:Integer;
                                               GridVisible:Boolean; AStartPos,AEndPos:Integer);
var OldGridVisible : Boolean;
    OldChange      : TNotifyEvent;
Begin
  FPosLabels:=APosLabels;
  FPosTitle :=APosTitle;
  FPosAxis  :=APosAxis;
  IStartPos :=AStartPos;
  IEndPos   :=AEndPos;
  RecalcSizeCenter;
  OldGridVisible:=FGrid.Visible;
  OldChange:=FGrid.OnChange;
  FGrid.OnChange:=nil;
  FGrid.Visible:=GridVisible;
  Draw(False);
  FGrid.Visible:=OldGridVisible;
  FGrid.OnChange:=OldChange;
end;

Procedure TCustomChartAxis.RecalcSizeCenter;
begin
  IAxisSize:=IEndPos-IStartPos;
  ICenterPos:=(IStartPos+IEndPos) div 2;
end;

Procedure TCustomChartAxis.InternalCalcPositions;

  Procedure DoCalculation(AStartPos:Integer; ASize:Integer);
  begin
    IStartPos:=AStartPos+Round(0.01*ASize*FStartPosition);
    IEndPos  :=AStartPos+Round(0.01*ASize*FEndPosition);
  end;

begin
  With ParentChart do
  if Horizontal then DoCalculation(ChartRect.Left,ChartWidth)
                else DoCalculation(ChartRect.Top,ChartHeight);
  RecalcSizeCenter;
end;

Function TCustomChartAxis.ApplyPosition(APos:Integer; Const R:TRect):Integer;
Var tmpSize:Integer;
begin
  result:=APos;
  if FPositionPercent<>0 then
  With R do
  begin
    if Horizontal then tmpSize:=Bottom-Top else tmpSize:=Right-Left;
    tmpSize:=Round(0.01*FPositionPercent*tmpSize);
    if OtherSide then tmpSize:=-tmpSize;
    if Horizontal then tmpSize:=-tmpSize;
    result:=APos+tmpSize;
  end;
end;

Function TCustomChartAxis.GetRectangleEdge(Const R:TRect):Integer;
begin
  With R do
  if OtherSide then
     if Horizontal then result:=Top else result:=Right
  else
     if Horizontal then result:=Bottom else result:=Left;
end;

Procedure TCustomChartAxis.Draw(CalcPosAxis:Boolean);
Var OldPosTick  : Longint;
    tmpWallSize : Longint;
    tmpValue    : Double;

  Procedure InternalDrawTick(tmp,Delta,tmpTickLength:Longint);
  var tmpA : Longint;
      tmpB : Longint;
  Begin
    with ParentChart do
    Begin
      if IsDepthAxis then
         Canvas.HorizLine3D(PosAxis+Delta,PosAxis+Delta+tmpTickLength,ChartRect.Bottom,tmp)
      else
      if OtherSide then
      Begin
        if Horizontal then
           Canvas.VertLine3D(tmp,PosAxis-Delta,PosAxis-Delta-tmpTickLength,Width3D)
        else
           Canvas.HorizLine3D(PosAxis+Delta,PosAxis+Delta+tmpTickLength,tmp,Width3D)
      end
      else
      begin
        tmpA:=Delta;
        Inc(tmpA,tmpWallSize);
        tmpB:=tmpA+tmpTickLength;
        if Horizontal then Canvas.VertLine3D(tmp,PosAxis+tmpA,PosAxis+tmpB,0)
                      else Canvas.HorizLine3D(PosAxis-tmpA,PosAxis-tmpB,tmp,0);
      end;
    end;
  end;

  Procedure DrawGrid(tmp:Longint);
  Begin
    With ParentChart,Canvas,ChartRect do
    begin
      Brush.Style:=bsClear;
      BackMode:=cbmTransparent;
      AssignVisiblePen(FGrid);
      CheckPenWidth(Pen);
      if Pen.Color=clTeeColor then Pen.Color:=clGray;
      if IsDepthAxis then
      begin
        VertLine3D(Left,Top,Bottom,tmp);
        HorizLine3D(Left,Right,Bottom,tmp);
      end
      else
      if Horizontal then
      Begin
        if View3D then
        Begin
          if OtherSide then
          begin
            VertLine3D(tmp,Top,Bottom,Width3D)
          end
          else
          begin
            if TeeDrawAxisBeforeSeries then
            begin
              ZLine3D(tmp,PosAxis,0,Width3D);
              VertLine3D(tmp,Top,Bottom,Width3D);
            end
            else VertLine3D(tmp,Top,Bottom,0);
          end;
        end
        else
        DoVertLine(tmp,Top,Bottom);
      end
      else
      Begin
        if View3D then
        Begin
          if OtherSide then
          begin
            HorizLine3D(Left,Right,tmp,Width3D);
          end
          else
          begin
            if TeeDrawAxisBeforeSeries then
            begin
              ZLine3D(PosAxis,tmp,0,Width3D);
              HorizLine3D(Left,Right,tmp,Width3D);
            end
            else HorizLine3D(Left,Right,tmp,0);
          end;
        end
        else
        DoHorizLine(Left,Right,tmp);
      end;
      BackMode:=cbmOPAQUE;
    end;
  end;

  Procedure DrawLabel(Const tmpSt:String; tmp:Longint);
  var tmpPosLabels : Integer;
  Begin
    With ParentChart,Canvas do
    Begin
      FontCanvas(LabelsFont);
      Brush.Color:=ParentChart.Color;
      Brush.Style:=bsClear;
      if Printing and (Font.Color=clWhite) then Font.Color:=clBlack;
      tmpPosLabels:=PosLabels;
      if IsDepthAxis then
      begin
        TextAlign:=ta_Left;
        TextOut3D(tmpPosLabels,ChartRect.Bottom,tmp,tmpSt);
      end
      else
      begin
        if Horizontal then DrawAxisLabel(tmp,tmpPosLabels,FLabelsAngle,tmpSt)
                      else DrawAxisLabel(tmpPosLabels,tmp,FLabelsAngle,tmpSt);
      end;
    end;
  end;

  Procedure DrawTicksGrid(tmp:Longint);
  Var t        : Longint;
      tmpDelta : Double;
  Begin
    if FTicks.Visible then
    begin
      ParentChart.Canvas.Pen.Assign(FTicks);
      InternalDrawTick(tmp,1,FTickLength);
    end;
    if FGrid.Visible {and
       ( (tmpValue<>IMaximum) and (tmpValue<>IMinimum) )} then
       if FGridCentered then
       begin
         if OldPosTick>-1 then DrawGrid(Round(0.5*(tmp+OldPosTick)))
       end
       else DrawGrid(tmp);
    if FTicksInner.Visible then
    begin
      ParentChart.Canvas.Pen.Assign(FTicksInner);
      InternalDrawTick(tmp,-1,-FTickInnerLength);
    end;
    if FMinorTicks.Visible and (OldPosTick>-1) then
    Begin
      ParentChart.Canvas.Pen.Assign(FMinorTicks);
      if not FLogarithmic then
      begin
        tmpDelta:=1.0*(tmp-OldPosTick)/(FMinorTickCount+1);
        for t:=1 to FMinorTickCount do
            InternalDrawTick(tmp-Round(t*tmpDelta),1,FMinorTickLength);
      end;
    end;
    OldPosTick:=tmp;
  end;

  Procedure DrawThisLabel(LabelPos:Longint; Const tmpSt:String);
  begin
    if TickOnLabelsOnly then DrawTicksGrid(LabelPos);
    DrawLabel(tmpSt,LabelPos);
  end;

Var tmpLabelStyle : TAxisLabelStyle;
    tmpSeriesList : TList;

  Function GetAxisSeriesLabel(AIndex:Longint; Var AValue:Double;
                              Var ALabel:String):Boolean;
  var t:Integer;
  begin
    result:=False;
    for t:=0 to tmpSeriesList.Count-1 do
    With TChartSeries(tmpSeriesList[t]) do
    if FXLabels.Count>AIndex then
    begin
      Case tmpLabelStyle of
        talMark : ALabel:=ValueMarkText[AIndex];
        talText : ALabel:=XLabel[AIndex];
      end;
      if Assigned(ParentChart.FOnGetAxisLabel) then
         ParentChart.FOnGetAxisLabel( TChartAxis(Self),
                         TChartSeries(tmpSeriesList[t]),AIndex,ALabel);
      AValue:=GetValue(Horizontal,AIndex);
      result:=True;
      break;
    end;
  end;

  { Select all active Series that have "Labels" }
  Procedure CalcAllSeries;
  var t:Integer;
  begin
    tmpSeriesList.Clear;
    With ParentChart do
    for t:=0 to SeriesList.Count-1 do
    With Series[t] do
    if Active and AssociatedToAxis(Self) then
    begin
{   FIXED BUG 4.0:   ** "If" commented **
      if (Horizontal and YMandatory) or
        ((not Horizontal) and (not YMandatory)) then }
        if FXLabels.Count>0 then
           tmpSeriesList.Add(Series[t]);
    end;
  end;

  Procedure CalcFirstLastAllSeries(Var tmpFirst,tmpLast:Longint);
  var t:Integer;
  begin
    tmpFirst:=-1;
    tmpLast:=-1;
    for t:=0 to tmpSeriesList.Count-1 do
    With TChartSeries(tmpSeriesList[t]) do
    begin
      CalcFirstLastVisibleIndex;
      if (tmpFirst=-1) or (tmpFirst>FFirstVisibleIndex) then
         tmpFirst:=FFirstVisibleIndex;
      if (tmpLast=-1) or (tmpLast<FLastVisibleIndex) then
         tmpLast:=FLastVisibleIndex;
    end;
  end;

  Procedure AxisLabelsSeries;
  Var t            : Longint;
      tmp          : Longint;
      tmpNum       : Integer;
      tmpFirst     : Longint;
      tmpLast      : Longint;
      tmpSt        : String;
      tmpValue     : Double;
      OldPosLabel  : Longint;
      OldSizeLabel : Longint;
      tmpLabelSize : Integer;
      tmpDraw      : Boolean;
      tmpLabelW    : Boolean;
  Begin
    tmpSeriesList:=TList.Create;
    CalcAllSeries;
    CalcFirstLastAllSeries(tmpFirst,tmpLast);

    if tmpFirst<>-1 then
    begin
      OldPosLabel :=-1;
      OldSizeLabel:= 0;
      tmpLabelW:=Horizontal;
      Case FLabelsAngle of
         90,270: tmpLabelW:=not tmpLabelW;
      end;
      for t:=tmpFirst to tmpLast do
      Begin
        if GetAxisSeriesLabel(t,tmpValue,tmpSt) then
        begin
          if (tmpValue>=IMinimum) and (tmpValue<=IMaximum) then
          begin
            if Horizontal then tmp:=Self.CalcXPosValue(tmpValue)
                          else tmp:=Self.CalcYPosValue(tmpValue);
            if not TickOnLabelsOnly then DrawTicksGrid(tmp);
            if FLabels then
            begin
              With ParentChart.Canvas do
              begin
                { fixed 4.01 }
                tmpLabelSize:=ParentChart.MultiLineTextWidth(tmpSt,tmpNum);
                if not tmpLabelW then tmpLabelSize:=FontHeight*tmpNum;
              end;
              if (FLabelsSeparation<>0) and (OldPosLabel<>-1) then
              begin
                Inc(tmpLabelSize,Trunc(0.02*tmpLabelSize*FLabelsSeparation));
                tmpLabelSize:=tmpLabelSize div 2;

                if tmp>=OldPosLabel then
                   tmpDraw:=(tmp-tmpLabelSize)>=(OldPosLabel+OldSizeLabel)
                else
                   tmpDraw:=(tmp+tmpLabelSize)<=(OldPosLabel-OldSizeLabel);
                if tmpDraw then
                begin { fixed 4.01 }
                  DrawThisLabel(tmp,tmpSt);
                  OldPosLabel:=tmp;
                  OldSizeLabel:=tmpLabelSize;
                end;
              end
              else
              begin { fixed 4.01 }
                DrawThisLabel(tmp,tmpSt);
                OldPosLabel:=tmp;
                OldSizeLabel:=tmpLabelSize div 2;
              end;
            end;
          end;
        end;
      end;
    end;
    tmpSeriesList.Free;
  end;

var IIncrement       : Double;
    tmpWhichDateTime : TDateTimeStep;

  Procedure InternalDrawLabel(DecValue:Boolean);
  var tmp : Longint;
  Begin
    if Horizontal then tmp:=CalcXPosValue(tmpValue)
                  else tmp:=CalcYPosValue(tmpValue);
    if not TickOnLabelsOnly then DrawTicksGrid(tmp);
    if FLabels then DrawThisLabel(tmp,LabelValue(tmpValue));
    if DecValue then IncDecDateTime(False,tmpValue,IIncrement,tmpWhichDateTime);
  end;

  Procedure DoDefaultLogLabels;

    {$IFDEF D1}
    function IntPower(Const Base: Extended; Exponent: Integer): Extended;
    var t:Integer;
    begin
      result:=Base;
      for t:=1 to Exponent do result:=result*Base;
    end;

    function LogBaseN(Const Base, X: Extended): Extended;
    begin
      result:=Ln(X)/Ln(Base);
    end;
    {$ELSE}

    { From Borland's Math.pas unit. Some Delphi and BCB
      versions do not include math.pas unit }
    function IntPower(Const Base: Extended; Exponent: Integer): Extended;
    asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
    @@1:    fmul    ST, ST            { X := Base * Base }
    @@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
    @@3:
        fwait
    end;

    function LogBaseN(Const Base, X: Extended): Extended;
    asm
      FLD1
      FLD     X
      FYL2X
      FLD1
      FLD     Base
      FYL2X
      FDIV
      FWAIT
    end;
    {$ENDIF}

  Var tmpDelta     : Double;
      tmpValueTick : Double;
      t            : Integer;
  begin
    if IMinimum<>IMaximum then
    begin
      if IMinimum<=0 then
      begin
        if IMinimum=0 then IMinimum:=0.1
                      else IMinimum:=MinAxisRange;
        tmpValue:=IMinimum;
      end
      else tmpValue:=IntPower(FLogarithmicBase,Round(LogBaseN(FLogarithmicBase,IMinimum)));
      While tmpValue<=IMaximum do
      begin
        if tmpValue>=IMinimum then
        begin
          InternalDrawLabel(False);
          if FMinorTicks.Visible then
          begin
            ParentChart.Canvas.Pen.Assign(FMinorTicks);
            tmpDelta:=((tmpValue*FLogarithmicBase)-tmpValue)/(FMinorTickCount+1);
            for t:=1 to FMinorTickCount do
            begin
              tmpValueTick:=tmpValue+(t*tmpDelta);
              if tmpValueTick>IMaximum then break
              else
                 InternalDrawTick(CalcPosValue(tmpValueTick),1,FMinorTickLength);
            end;
          end;
        end;
        tmpValue:=tmpValue*FLogarithmicBase;
      end;
    end;
  end;

  Procedure DoDefaultLabels;
  Begin
    tmpValue:=IMaximum/IIncrement;
    if (Abs(tmpValue)<MaxLongint) and
       (Abs(IRange/IIncrement)<10000) then
    Begin
      if IAxisDateTime and FExactDateTime and
         (tmpWhichDateTime<>dtNone) and (tmpWhichDateTime>=dtOneDay) then
             tmpValue:=TeeRoundDate(IMaximum,tmpWhichDateTime)
      else
      if (IMinimum=IMaximum) or (not RoundFirstLabel) then
         tmpValue:=IMaximum
      else
         tmpValue:=IIncrement*Trunc(tmpValue);
      if FLabelsOnAxis then
      begin
        While tmpValue>IMaximum do
              IncDecDateTime(False,tmpValue,IIncrement,tmpWhichDateTime);
        While tmpValue>=IMinimum do InternalDrawLabel(True);
      end
      else
      begin
        While tmpValue>=IMaximum do
              IncDecDateTime(False,tmpValue,IIncrement,tmpWhichDateTime);
        While tmpValue>IMinimum do InternalDrawLabel(True);
      end;
    end;
  end;

  Procedure DoCustomLabels;
  Const DifFloat = 0.0000001;
  var LabelIndex  : Longint;
      Stop        : Boolean;
      LabelInside : Boolean;
  Begin
    tmpValue:=IMinimum;
    Stop:=True;
    LabelIndex:=0;
    LabelInside:=False;
    Repeat
      ParentChart.FOnGetNextAxisLabel(TChartAxis(Self),LabelIndex,tmpValue,Stop);
      if Stop then
      Begin
        if LabelIndex=0 then DoDefaultLabels;
        Exit;
      end
      else
      begin { Trick with doubles... }
        LabelInside:=(tmpValue>=(IMinimum-DifFloat)) and
                     (tmpValue<=(IMaximum+DifFloat));
        if LabelInside then InternalDrawLabel(False);
        Inc(LabelIndex);
      end;
    Until Stop or (not LabelInside) or (LabelIndex>1000); { maximum 1000 labels... }
  end;

  Procedure DrawAxisTitle;
  begin
    With ParentChart do
    Begin
      FontCanvas(FAxisTitle.Font);
      Canvas.Brush.Color:=Color;
      Canvas.Brush.Style:=bsClear;
      if IsDepthAxis then DrawTitle(PosTitle,ChartRect.Bottom)
      else
      if Horizontal then DrawTitle(ICenterPos,PosTitle)
                    else DrawTitle(PosTitle,ICenterPos);
    end;
  end;

  Procedure DepthAxisLabels;
  Var t     : Longint;
      tmp   : Longint;
      tmpSt : String;
  Begin
    if ParentChart.SeriesList.CountActive>0 then
    for t:=Trunc(IMinimum) to Trunc(IMaximum) do
    Begin
      tmp:=Self.InternalCalcDepthPosValue(IMaximum-t-0.5);
      if not TickOnLabelsOnly then DrawTicksGrid(tmp);
      if FLabels then
      begin
        With ParentChart do
        begin
          tmpSt:=SeriesTitleLegend(t);
          if Assigned(FOnGetAxisLabel) then FOnGetAxisLabel(TChartAxis(Self),nil,t,tmpSt);
        end;
        DrawThisLabel(tmp,tmpSt);
      end;
    end;
  end;

Begin
  With ParentChart,ChartRect do
  Begin
    IAxisDateTime:=IsDateTime;
    tmpWallSize:=CalcWallSize(Self);
    FontCanvas(Self.FLabelsFont);
    IIncrement:=CalcIncrement;
    if CalcPosAxis then FPosAxis:=ApplyPosition(GetRectangleEdge(ChartRect),ChartRect);
    if FAxisTitle.FCaption<>'' then DrawAxisTitle;

    if IAxisDateTime and FExactDateTime and (FDesiredIncrement<>0) then
    begin
      tmpWhichDateTime:=FindDateTimeStep(FDesiredIncrement);
      if tmpWhichDateTime<>dtNone then
      While (IIncrement>DateTimeStep[tmpWhichDateTime]) and
            (tmpWhichDateTime<>dtOneYear) do
                tmpWhichDateTime:=Succ(tmpWhichDateTime);
    end
    else tmpWhichDateTime:=dtNone;

    if ((IIncrement>0) or
       ( (tmpWhichDateTime>=dtHalfMonth) and (tmpWhichDateTime<=dtOneYear)) )
       and (IMaximum>=IMinimum) then
    Begin
      OldPosTick   :=-1;
      tmpLabelStyle:=CalcLabelStyle;
      Case tmpLabelStyle of
       talValue: if Assigned(FOnGetNextAxisLabel) then
                    DoCustomLabels
                 else
                 if FLogarithmic and (FDesiredIncrement=0) then
                    DoDefaultLogLabels
                 else
                    DoDefaultLabels;
        talMark: AxisLabelsSeries;
        talText: if IsDepthAxis then DepthAxisLabels else AxisLabelsSeries;
      end;
    end;
    if FAxis.Visible then
    begin
      Canvas.Pen.Assign(FAxis);
      DrawAxisLine(tmpWallSize);
    end;
  end;
end;

Function TCustomChartAxis.SizeTickAxis:Integer;
begin
  if FAxis.Visible then result:=FAxis.Width
                   else result:=0;
  if FTicks.Visible then result:=result+FTickLength;
  if FMinorTicks.Visible then result:=MaxLong(result,FMinorTickLength);
end;

Function TCustomChartAxis.SizeTitle:Integer;
begin
  With FAxisTitle do result:=InternalCalcSize(Font,FAngle,FCaption,FTitleSize);
end;

Function TCustomChartAxis.SizeLabels:Integer;
begin
  result:=InternalCalcSize(FLabelsFont,FLabelsAngle,'',FLabelsSize);
end;

Function TCustomChartAxis.InternalCalcSize( tmpFont:TFont;
                                            tmpAngle:Longint;
                                            Const tmpText:String;
                                            tmpSize:Longint):Longint;
Begin
  if tmpSize<>0 then result:=tmpSize
  else
  With ParentChart,Canvas do
  Begin
    FontCanvas(tmpFont);
    if Horizontal then
    Case tmpAngle of
      0, 180: result:=FontHeight;
    else { optimized for speed }
      if tmpText='' then result:=MaxLabelsWidth
                    else result:=Canvas.TextWidth(tmpText);
    end
    else
    Case tmpAngle of
     90, 270: result:=FontHeight;
    else { optimized for speed }
      if tmpText='' then result:=MaxLabelsWidth
                    else result:=Canvas.TextWidth(tmpText);
    end;
  end;
end;

Procedure TCustomChartAxis.CalcRect(Var R:TRect; InflateChartRectangle:Boolean);

  Procedure InflateAxisRect(Value:Longint);
  Begin
    With R do
    if Horizontal then
       if OtherSide then Inc(Top,Value) else Dec(Bottom,Value)
    else
       if OtherSide then Dec(Right,Value) else Inc(Left,Value);
  end;

  Function InflateAxisPos(Value:Integer; Amount:Integer):Integer;
  Begin
    result:=Value;
    if Horizontal then
       if OtherSide then Dec(result,Amount) else Inc(result,Amount)
    else
       if OtherSide then Inc(result,Amount) else Dec(result,Amount);
  end;

  Function CalcLabelsRect(tmpSize:Integer):Integer;
  begin
    InflateAxisRect(tmpSize);
    result:=GetRectangleEdge(R);
  end;

var tmp : Integer;
Begin
  IAxisDateTime:=IsDateTime;

  if InflateChartRectangle then
  begin
    if IsDepthAxis then
       FPosTitle:=R.Right
    else
       With FAxisTitle do
       if FCaption<>'' then FPosTitle:=CalcLabelsRect(SizeTitle);

    if FLabels then FPosLabels:=CalcLabelsRect(SizeLabels);

    tmp:=SizeTickAxis;
    Inc(tmp,ParentChart.CalcWallSize(Self));
    if tmp>0 then InflateAxisRect(tmp);
    FPosTitle:=ApplyPosition(FPosTitle,R);
    FPosLabels:=ApplyPosition(FPosLabels,R);
  end
  else
  begin
    FPosAxis:=ApplyPosition(GetRectangleEdge(R),R);
    FPosLabels:=InflateAxisPos(FPosAxis,SizeTickAxis);
    FPosTitle:=InflateAxisPos(FPosLabels,SizeLabels);
  end;
end;

Procedure TChartAxis.DrawAxisLine(AWallSize:Integer);
begin
  With ParentChart,Canvas do
  if Horizontal then
     if OtherSide then
        HorizLine3D(IStartPos,IEndPos,PosAxis,Width3D)
     else
        HorizLine3D(IStartPos-CalcWallSize(LeftAxis),
                    IEndPos,PosAxis+AWallSize,0)
  else
     if OtherSide then
        VertLine3D(PosAxis,IStartPos,IEndPos,Width3D)
     else
        VertLine3D(PosAxis-AWallSize,
                   IStartPos,IEndPos+CalcWallSize(BottomAxis),0);
end;

{ TChartDepthAxis }
Procedure TChartDepthAxis.DrawAxisLine;
var tmp:Integer;
begin
  With ParentChart,ChartRect,Canvas do
  begin
    tmp:=Bottom+CalcWallSize(BottomAxis);
    MoveTo3D(Right,tmp,0);
    LineTo3D(Right,tmp,Width3D);
  end;
end;

{ TCustomAxisPanel }
Constructor TCustomAxisPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAxisVisible:=True;
  FSeriesList:=TChartSeriesList.Create;
  FSeriesList.FOwner:=Self;
  FView3DWalls:=True;

  FClipPoints:=True;

  FAxes:=TList.Create;

  { Create axes... }
  FBottomAxis:=TChartAxis.Create(Self);
  FBottomAxis.FHorizontal:=True;

  FTopAxis:=TChartAxis.Create(Self);
  With FTopAxis do
  begin
    FHorizontal:=True;
    FOtherSide:=True;
  end;

  FLeftAxis:=TChartAxis.Create(Self);
  FLeftAxis.FAxisTitle.FAngle:=90;
  FLeftAxis.IDefaultTitleAngle:=90;

  FRightAxis:=TChartAxis.Create(Self);
  With FRightAxis do
  begin
    FAxisTitle.FAngle:=270;
    IDefaultTitleAngle:=270;
    FOtherSide:=True;
  end;

  FDepthAxis := TChartDepthAxis.Create(Self);
  With FDepthAxis do
  begin
    FVisible:=False;
    IsDepthAxis:=True;
    FOtherSide:=True;
  end;

  { Paging default values }
  FPage:=1;
  FScaleLastPage:=True;
end;

Procedure TCustomAxisPanel.SetPage(Value:Longint);
var tmp:Longint;
Begin
  { Allow "Page" to be into: >= 1 , <= NumPages }
  tmp:=NumPages;
  if Value>tmp then Value:=tmp;
  if Value<1 then Value:=1;
  if FPage<>Value then
  begin
    SetLongintProperty(FPage,Value);
    { Trigged "changed page" event }
    if Assigned(FOnPageChange) then FOnPageChange(Self);
  end;
end;

Procedure TCustomAxisPanel.SetScaleLastPage(Value:Boolean);
Begin
  SetBooleanProperty(FScaleLastPage,Value);
End;

Function TCustomAxisPanel.NumPages:Longint;

  Function CalcNumPages(AAxis:TChartAxis):Longint;
  var tmp       : Longint;
      t         : Integer;
      FirstTime : Boolean;
  Begin
    { By default, one single page }
    result:=1;
    { Calc max number of points for all active series
      associated to "AAxis" axis }
    tmp:=0;
    FirstTime:=True;
    for t:=0 to FSeriesList.Count-1 do
    With Series[t] do
    if FActive and AssociatedToAxis(AAxis) then
       if FirstTime or (Count>tmp) then
       begin
         tmp:=Count;
         FirstTime:=False;
       end;
    { If there are points... divide into pages... }
    if tmp>0 then
    Begin
      result:=tmp div FMaxPointsPerPage;
      { extra page for remaining points... }
      if (tmp mod FMaxPointsPerPage)>0 then Inc(result);
    end;
  end;

Begin
  if FMaxPointsPerPage=0 then result:=1
  else
  begin
    if FSeriesList.Count>0 then
    begin
      if Series[0].YMandatory then
         result:=MaxLong(CalcNumPages(FTopAxis),CalcNumPages(FBottomAxis))
      else
         result:=MaxLong(CalcNumPages(FLeftAxis),CalcNumPages(FRightAxis));
    end
    else result:=1;
  end;
End;

Procedure TCustomAxisPanel.SetMaxPointsPerPage(Value:Longint);
var tmp : Longint;
Begin
  if Value<0 then Raise ChartException.Create(TeeMsg_MaxPointsPerPage)
             else
             Begin
               SetLongintProperty(FMaxPointsPerPage,Value);
               tmp:=NumPages;
               if FPage>tmp then Page:=tmp;
             end;
end;

Procedure TCustomAxisPanel.FreeAllSeries;
begin
  While SeriesCount>0 do
  With Series[0] do
  begin
    ParentChart:=nil;
    Free;
  end;
end;

Destructor TCustomAxisPanel.Destroy;
Begin
  FreeAllSeries;
  FSeriesList.Free;
  With FAxes do
  While Count>0 do
  begin
    TChartAxis(First).Free;
    Delete(0);
  end;
  FAxes.Free;
  inherited Destroy;
end;

{ Steps to determine if an axis is Visible:

  1) The global Chart.AxisVisible property is True... and...
  2) The Axis Visible property is True... and...
  3) At least there is a Series Active and associated to the axis and
     the Series has the "UseAxis" property True.
}
Function TCustomAxisPanel.IsAxisVisible(Axis:TCustomChartAxis):Boolean;
var t : Integer;
Begin
  result:=FAxisVisible and Axis.FVisible;
  if result then { if still visible... }
     if Axis.IsDepthAxis then result:=View3D
     else
     for t:=0 to FSeriesList.Count-1 do
       With Series[t] do
       if FActive and UseAxis then
       begin
         result:=AssociatedToAxis(Axis);
         if result then exit;
       end;
end;

Function TCustomAxisPanel.InternalMinMax(AAxis:TCustomChartAxis; IsMin,IsX:Boolean):Double;
var t             : Longint;
    tmpCount      : Longint;
    FirstTime     : Boolean;
    tmpPagingAxis : Boolean;
    tmp           : Double;
    tmpSeries     : TChartSeries;
    tmpFirstPoint : Longint;
    tmpLastPoint  : Longint;
    tmpNumPoints  : Longint;
    tt            : Integer;
Begin
  if AAxis.IsDepthAxis then
  begin
    if AAxis.CalcLabelStyle=talValue then
    begin
      result:=0;
      for tt:=0 to FSeriesList.Count-1 do
      With Series[tt] do
      if Active then
      begin
        if IsMin then result:=MinZValue else result:=MaxZValue;
        Break;
      end;
    end
    else
    if IsMin then result:=-0.5 else result:=MaxZOrder+0.5;
  end
  else
  begin
    result:=0;
    tmpSeries:=GetAxisSeries(AAxis);
    if Assigned(tmpSeries) then
    begin
      if tmpSeries.YMandatory then tmpPagingAxis:=IsX
                              else tmpPagingAxis:=not IsX;
    end
    else tmpPagingAxis:=IsX;
    if (FMaxPointsPerPage>0) and tmpPagingAxis then
    Begin
      if Assigned(tmpSeries) and (tmpSeries.Count>0) then
      Begin
        tmpFirstPoint:=(FPage-1)*FMaxPointsPerPage;
        tmpCount:=tmpSeries.Count;
        if tmpCount<=tmpFirstPoint then
           tmpFirstPoint:=MaxLong(0,(tmpCount div FMaxPointsPerPage)-1)*FMaxPointsPerPage;
        tmpLastPoint:=tmpFirstPoint+FMaxPointsPerPage-1;
        if tmpCount<=tmpLastPoint then
           tmpLastPoint:=tmpFirstPoint+(tmpCount mod FMaxPointsPerPage)-1;
        if IsMin then
           result:=tmpSeries.GetValue(IsX,tmpFirstPoint)
        else
        Begin
          result:=tmpSeries.GetValue(IsX,tmpLastPoint);
          if not FScaleLastPage then
          begin
            tmpNumPoints:=tmpLastPoint-tmpFirstPoint+1;
            if tmpNumPoints<FMaxPointsPerPage then
            begin
              tmp:=tmpSeries.GetValue(IsX,tmpFirstPoint);
              result:=tmp+FMaxPointsPerPage*(result-tmp)/tmpNumPoints;
            end;
          end;
        end;
      end;
    end
    else
    begin
      FirstTime:=True;
      for t:=0 to FSeriesList.Count-1 do
      With Series[t] do
      if FActive and (Count>0) then
      Begin
        if (      IsX  and ((HorizAxis=aBothHorizAxis) or (GetHorizAxis=AAxis)) ) or
           ( (Not IsX) and ((VertAxis=aBothVertAxis) or (GetVertAxis =AAxis)) )  then
        Begin
          if IsMin then
             if IsX then tmp:=MinXValue else tmp:=MinYValue
          else
             if IsX then tmp:=MaxXValue else tmp:=MaxYValue;
          if FirstTime or
           ( IsMin and (tmp<result) ) or
           ( (Not IsMin) and (tmp>result) ) then
          Begin
            result:=tmp;
            FirstTime:=False;
          end;
        end;
      end;
    end;
  end;
End;

Function TCustomAxisPanel.MaxXValue(AAxis:TChartAxis):Double;
Begin
  result:=InternalMinMax(AAxis,False,True);
end;

Function TCustomAxisPanel.MaxYValue(AAxis:TChartAxis):Double;
Begin
  result:=InternalMinMax(AAxis,False,False);
end;

Function TCustomAxisPanel.MinXValue(AAxis:TChartAxis):Double;
Begin
  result:=InternalMinMax(AAxis,True,True);
end;

Function TCustomAxisPanel.MinYValue(AAxis:TChartAxis):Double;
Begin
  result:=InternalMinMax(AAxis,True,False);
end;

Function TCustomAxisPanel.ActiveSeriesLegend(ItemIndex:Longint):TChartSeries;
var t   : Integer;
    tmp : Longint;
begin
  tmp:=0;
  for t:=0 to FSeriesList.Count-1 do
  With Series[t] do
  if Active and ShowInLegend then
     if tmp=ItemIndex then
     begin
       result:=Series[t];
       exit;
     end
     else Inc(tmp);
  result:=nil;
end;

Function TCustomAxisPanel.SeriesTitleLegend(SeriesIndex:Longint):String;
var tmpSeries : TChartSeries;
Begin
  tmpSeries:=ActiveSeriesLegend(SeriesIndex);
  if Assigned(tmpSeries) then
  with tmpSeries do
  Begin
    if FTitle='' then
    Begin
      result:=Name;
      if result='' then result:='Series '+IntToStr(SeriesIndex); { <-- do not translate }
    end
    else result:=FTitle;
  end
  else result:='';
end;

Function TCustomAxisPanel.MaxTextWidth:Longint;
var t  : Integer;
    tt : Longint;
    tmp: Integer;
Begin
  result:=0;
  for t:=0 to FSeriesList.Count-1 do
  With Series[t] do
  if FXLabels.Count>0 then
     for tt:=0 to Count-1 do
         result:=MaxLong(result,MultiLineTextWidth(XLabel[tt],tmp));
end;

Function TCustomAxisPanel.MaxMarkWidth:Longint;
var t : Integer;
Begin
  result:=0;
  for t:=0 to FSeriesList.Count-1 do
  With Series[t] do if FActive then result:=MaxLong(result,MaxMarkWidth);
end;

Function TCustomAxisPanel.GetSeries(Index:Longint):TChartSeries;
Begin
  result:=FSeriesList.Items[Index];
end;

Procedure TCustomAxisPanel.CalcSize3DWalls;
var tmpNumSeries : Integer;
    tmp          : Double;
Begin
  if View3D then
  Begin
    tmp:=0.001*Chart3DPercent;
    if not View3DOptions.Orthogonal then tmp:=tmp*2;
    FSeriesWidth3D :=Round(tmp*ChartWidth);
    FSeriesHeight3D:=FSeriesWidth3D;
{ratio:   FSeriesHeight3D:=Round(tmp*ChartHeight);  *10/100.0 % }
    if ApplyZOrder then tmpNumSeries:=MaxLong(1,MaxZOrder+1)
                   else tmpNumSeries:=1;
    Height3D:=FSeriesHeight3D * tmpNumSeries;
    Width3D :=FSeriesWidth3D  * tmpNumSeries;
  end
  else
  begin
    FSeriesWidth3D :=0;
    FSeriesHeight3D:=0;
    Width3D        :=0;
    Height3D       :=0;
  end;
end;

Procedure TCustomAxisPanel.InternalDraw(Const UserRectangle:TRect);

  Procedure DrawSeries(TheSeries:TChartSeries);
  Var ActiveRegion : Boolean;

    Procedure ClipRegionCreate;
    Begin
      if CanClip then
      begin
        Canvas.ClipCube(ChartRect,0,Width3D);
        ActiveRegion:=True;
      end;
    end;

    Procedure ClipRegionDone;
    Begin
      if ActiveRegion then
      begin
        Canvas.UnClipRectangle;
        ActiveRegion:=False;
      end;
    end;

    Procedure DrawAllSeriesValue(ValueIndex:Longint);

      Procedure TryDrawSeries(ASeries:TChartSeries);
      begin
        With ASeries do
         if FActive and (ZOrder=TheSeries.ZOrder) and (ValueIndex<Count) then
            DrawValue(ValueIndex)
      end;

    var t    : Integer;
        tmp1 : Integer;
        tmp2 : Integer;
    Begin
      tmp1:=FSeriesList.IndexOf(TheSeries);
      tmp2:=FSeriesList.Count-1;
      if ValueIndex<TheSeries.Count then
      begin
        if TheSeries.DrawSeriesForward(ValueIndex) then
           for t:=tmp1 to tmp2 do TryDrawSeries(Series[t])
        else
           for t:=tmp2 downto tmp1 do TryDrawSeries(Series[t])
      end
      else
           for t:=tmp1 to tmp2 do TryDrawSeries(Series[t])
    end;

    Procedure DrawMarksSeries(ASeries:TChartSeries);
    begin
      With ASeries,FMarks do
      if FVisible then
      Begin
        if FClip then ClipRegionCreate;
        DrawMarks;
        if FClip then ClipRegionDone;
      end;
    end;

  Var t        : Longint;
      tmpFirst : Longint;
      tmpLast  : Longint;
  Begin
    ActiveRegion:=False;  { <-- VERY IMPORTANT !!!! }
    With TheSeries do
    if View3D and MoreSameZOrder then
    Begin
      if FirstInZOrder then
      Begin
        ActiveRegion:=False;
        tmpFirst:=-1;
        tmpLast :=-1;
        for t:=SeriesList.IndexOf(TheSeries) to FSeriesList.Count-1 do
        With Series[t] do
        if Active and (ZOrder=TheSeries.ZOrder) then
        Begin
          CalcFirstLastVisibleIndex;
          if FFirstVisibleIndex<>-1 then
          Begin
            if tmpFirst=-1 then tmpFirst:=FFirstVisibleIndex
                           else tmpFirst:=MaxLong(tmpFirst,FFirstVisibleIndex);
            if tmpLast=-1 then tmpLast:=FLastVisibleIndex
                          else tmpLast:=MaxLong(tmpLast,FLastVisibleIndex);
            DoBeforeDrawValues;
            if FClipPoints and (not ActiveRegion) then ClipRegionCreate;
          end;
        end;
        { values }
        if tmpFirst<>-1 then
          if DrawValuesForward then
             for t:=tmpFirst to tmpLast do DrawAllSeriesValue(t)
          else
             for t:=tmpLast downto tmpFirst do DrawAllSeriesValue(t);

        { Region }
        ClipRegionDone;

        { Marks and DoAfterDrawValues }
        for t:=SeriesList.IndexOf(TheSeries) to FSeriesList.Count-1 do
        With Series[t] do
        if Active and (ZOrder=TheSeries.ZOrder) and (FFirstVisibleIndex<>-1) then
        begin
          DrawMarksSeries(Series[t]);
          DoAfterDrawValues;
        end;
      end;
    end
    else
    Begin
      CalcFirstLastVisibleIndex;
      if FFirstVisibleIndex<>-1 then
      Begin
        DoBeforeDrawValues;
        if FClipPoints then ClipRegionCreate;
        DrawAllValues;
        if FClipPoints then ClipRegionDone;
        DrawMarksSeries(TheSeries);
        DoAfterDrawValues;
      end;
    End;
  end;

  Procedure SetSeriesZOrder;
  Var tmpSeries : Integer;
  begin
    FMaxZOrder:=0;
    if ApplyZOrder then
       if View3D then
       Begin
         FMaxZOrder:=-1;
         for tmpSeries:=0 to FSeriesList.Count-1 do
             With Series[tmpSeries] do if FActive then CalcZOrder;
       end;
    { invert Z Orders }
    for tmpSeries:=0 to FSeriesList.Count-1 do
    With Series[tmpSeries] do
    if FActive then
       if View3D and ApplyZOrder then IZOrder:=MaxZOrder-ZOrder
                                 else IZOrder:=0;
  end;

  Procedure SetSeriesZPositions;
  Var tmpSeries : Integer;
  begin
    for tmpSeries:=0 to FSeriesList.Count-1 do
    With Series[tmpSeries] do
    if Active then
    begin
      StartZ :=IZOrder*SeriesWidth3D;
      EndZ   :=StartZ+SeriesWidth3D;
      MiddleZ:=(StartZ+EndZ) div 2;
      FMarks.ZPosition:=MiddleZ;
    end;
  end;

  Procedure DrawAllAxis;
  var t       : Integer;
      tmpAxis : TChartAxis;
  Begin
    if Assigned(FOnBeforeDrawAxes) then FOnBeforeDrawAxes(Self);
    With FAxes do
    for t:=0 to Count-1 do
    begin
      tmpAxis:=TChartAxis(Items[t]);
      if IsAxisVisible(tmpAxis) then tmpAxis.Draw(True);
    end;
  end;

Var tmpSeries : Integer;
Begin
  PanelPaint(UserRectangle);
  for tmpSeries:=0 to FSeriesList.Count-1 do
  With Series[tmpSeries] do if Active then DoBeforeDrawChart;

  DrawTitlesAndLegend;

  SetSeriesZOrder;
  CalcAxisRect;
  SetSeriesZPositions;
  CalcSeriesRect;

  InternalCanvas.Projection(Width3D,ChartBounds,ChartRect);

  DrawWalls;
  if FAxisVisible and TeeDrawAxisBeforeSeries then DrawAllAxis;

  if Assigned(FOnBeforeDrawSeries) then FOnBeforeDrawSeries(Self);

  for tmpSeries:=0 to FSeriesList.Count-1 do
  if Series[tmpSeries].Active then DrawSeries(Series[tmpSeries]);

  if FAxisVisible and (not TeeDrawAxisBeforeSeries) then DrawAllAxis;

  if IZoom.Active then DrawZoomRectangle;
  Canvas.ResetState;
  if Assigned(FOnAfterDraw) then FOnAfterDraw(Self);
end;

procedure TCustomAxisPanel.SetView3DWalls(Value:Boolean);
Begin
  SetBooleanProperty(FView3DWalls,Value);
end;

{$IFDEF D1}
Procedure TCustomAxisPanel.WriteComponents(Writer:TWriter);
var t : Integer;
begin
  for t:=0 to FSeriesList.Count-1 do Writer.WriteComponent(FSeriesList[t]);
  inherited WriteComponents(Writer);
end;
{$ELSE}
(*
Procedure TCustomAxisPanel.GetChildren(Proc:TGetChildProc{$IFDEF D3}; Root:TComponent{$ENDIF});
var t : Integer;
begin
  inherited GetChildren(Proc{$IFDEF D3},Root{$ENDIF});
  for t := 0 to FSeriesList.Count-1 do Proc(FSeriesList[t]);
end;
*)
{$ENDIF}

procedure TCustomAxisPanel.RemovedDataSource( ASeries: TChartSeries;
                                              AComponent: TComponent );
begin
end;

procedure TCustomAxisPanel.SetClipPoints(Value:Boolean);
Begin
  SetBooleanProperty(FClipPoints,Value);
end;

procedure TCustomAxisPanel.SetLeftAxis(Value:TChartAxis);
begin
  FLeftAxis.Assign(Value);
end;

procedure TCustomAxisPanel.SetDepthAxis(Value:TChartDepthAxis);
begin
  FDepthAxis.Assign(Value);
end;

procedure TCustomAxisPanel.SetRightAxis(Value:TChartAxis);
begin
  FRightAxis.Assign(Value);
end;

procedure TCustomAxisPanel.SetTopAxis(Value:TChartAxis);
begin
  FTopAxis.Assign(Value);
end;

procedure TCustomAxisPanel.SetBottomAxis(Value:TChartAxis);
begin
  FBottomAxis.Assign(Value);
end;

Procedure TCustomAxisPanel.RemoveSeries(ASeries:TChartSeries);
var t : Integer;
Begin
  t:=FSeriesList.IndexOf(ASeries);
  if t<>-1 then
  begin
    With ASeries do
    Begin
      RemoveAllLinkedSeries;
      FParent:=nil;
      { this is necesary... }
      if Self.Cursor=Cursor then Self.Cursor:=OriginalCursor;
    end;
    FSeriesList.Delete(t);
{    if not (csDesigning in ComponentState) then  <--hotfix for AV bug in D3 }
       Invalidate;
  end;
end;

Function TCustomAxisPanel.GetAxisSeries(Axis:TCustomChartAxis):TChartSeries;
Var t : Integer;
Begin
  for t:=0 to FSeriesList.Count-1 do
  begin
    result:=Series[t];
    With result do
    if FActive and AssociatedToAxis(Axis) then Exit;
  end;
  result:=nil;
end;

Function TCustomAxisPanel.FormattedValueLegend(ASeries:TChartSeries; ValueIndex:Longint):String;
begin
      { virtual, overrided at TChart }
end;

Function TCustomAxisPanel.GetFreeSeriesColor(CheckBackground:Boolean):TColor;
var t : Integer;
Begin
  t:=0;
  Repeat
    result:=GetDefaultColor(t);
    inc(t);
  Until (t>MaxDefaultColors) or IsFreeSeriesColor(result,CheckBackground);
end;

Procedure TCustomAxisPanel.AddSeries(ASeries:TChartSeries);
begin
  ASeries.ParentChart:=Self;
end;

Procedure TCustomAxisPanel.InternalAddSeries(ASeries:TChartSeries);
Begin
  if FSeriesList.IndexOf(ASeries)=-1 then
  begin
    with ASeries do
    begin
      if FColor=clTeeColor then SeriesColor:=GetFreeSeriesColor(True);
      FParent:=Self;
      if (csDesigning in ComponentState) and (ASeries.FunctionType=nil) then
         FillSampleValues(NumSampleValues)
      else
         CheckDatasource;
    end;
    FSeriesList.Add(ASeries);
    Invalidate;
  end;
end;

Function TCustomAxisPanel.GetMaxValuesCount:Longint;
var t         : Integer;
    FirstTime : Boolean;
Begin
  result:=0;
  FirstTime:=True;
  for t:=0 to FSeriesList.Count-1 do
  with Series[t] do
  if FActive and ( FirstTime or (Count>result) ) then
  begin
    result:=Count;
    FirstTime:=False;
  end;
end;

Procedure TCustomAxisPanel.SetAxisVisible(Value:Boolean);
Begin
  SetBooleanProperty(FAxisVisible,Value);
end;

Procedure TCustomAxisPanel.CheckOtherSeries(Dest,Source:TChartSeries);
var t:Longint;
Begin
  if Assigned(Source) then
  if Source.DataSource=Dest then
     Raise ChartException.Create(TeeMsg_CircularSeries)
  else
     if Source.DataSource is TChartSeries then
     for t:=0 to Source.FDataSources.Count-1 do
        CheckOtherSeries(Dest,TChartSeries(Source.DataSources[t]));
end;

Function TCustomAxisPanel.IsValidDataSource(ASeries:TChartSeries; AComponent:TComponent):Boolean;
Begin
  result:=(ASeries<>AComponent) and
          (AComponent is TChartSeries) and
          ASeries.IsValidSeriesSource(TChartSeries(AComponent));
end;

Procedure TCustomAxisPanel.CheckDatasource(ASeries:TChartSeries);
Begin
  With ASeries do
  if not (csLoading in ComponentState) then
     if (DataSource<>nil) and (DataSource is TChartSeries) then
        AddValues(TChartSeries(DataSource));
end;

Function TCustomAxisPanel.SeriesCount:Longint;
Begin
  if FSeriesList=nil then result:=0
                     else result:=FSeriesList.Count;
end;

Procedure TCustomAxisPanel.Assign(Source:TPersistent);
begin
  if Source is TCustomAxisPanel then
  With TCustomAxisPanel(Source) do
  begin
    Self.FAxisVisible      := FAxisVisible;
    Self.FBottomAxis.Assign(FBottomAxis);
    Self.FClipPoints       := FClipPoints;
    Self.FLeftAxis.Assign(FLeftAxis);
    Self.FDepthAxis.Assign(FDepthAxis);
    Self.FMaxPointsPerPage := FMaxPointsPerPage;
    Self.FPage:=FPage;
    Self.FRightAxis.Assign(FRightAxis);
    Self.FScaleLastPage    := FScaleLastPage;
    Self.FTopAxis.Assign(FTopAxis);
    Self.FView3DWalls      := FView3DWalls;
  end;
  inherited Assign(Source);
end;

initialization
  RegisterClasses([ TChartAxis,TChartDepthAxis,TSeriesMarks,TChartAxisTitle ]);
end.

{******************************************}
{   TCustomChart & TChart Components       }
{ Copyright (c) 1995-1998 by David Berneda }
{    All Rights Reserved                   }
{******************************************}
{$R-,S-,Q-,I-,A+,U-,X+,B-,W-,P-,V-}
{$I teedefs.inc}
unit Chart;

interface

uses Classes, Messages, WinTypes, WinProcs, SysUtils, Graphics,
     TeEngine, Printers, Controls, ExtCtrls, TeeProcs, TeeFunci, Forms,
     TeCanvas
     {$IFDEF D1}
     ,Menus
     {$ENDIF}
     ;

Const
     TeeMsg_DefaultFunctionName = 'TeeFunction'; { <-- dont translate }
     TeeMsg_DefaultSeriesName   = 'Series';      { <-- dont translate }
     ChartComponentPalette      = 'TeeChart';    { <-- dont translate }


Var  AnimatedZoomFactor   : Double;
     TeeZoomMouseButton   : TMouseButton; { button used to zoom }
     TeeScrollMouseButton : TMouseButton; { button used to scroll }
     TeeZoomKeyShift      : TShiftState;  { keys that should be pressed to start zoom }
     TeeScrollKeyShift    : TShiftState;  { keys that should be pressed to start scroll }

Type
  TeeString        = Array[0..255] of char;
  TeeGalleryString = String[20];

  TCustomChart=class;

  TChartWall=class(TPersistent)
  private
    FColor       : TColor;
    FBrush       : TChartBrush;
    FDark3D      : Boolean;
    FPen         : TChartPen;
    FSize        : Longint;
    FParentChart : TCustomChart;
    Procedure SetBrush(Value:TChartBrush);
    Procedure SetColor(Value:TColor);
    Procedure SetDark3D(Value:Boolean);
    Procedure SetPen(Value:TChartPen);
    Procedure SetSize(Value:Longint);
    Function IsColorStored:Boolean;
    Function ApplyDark3D:Boolean;
  protected
    DefaultColor:TColor;
  public
    Constructor Create(AOwner:TCustomChart);
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    property ParentChart:TCustomChart read FParentChart;
  published
    property Brush:TChartBrush read FBrush write SetBrush;
    property Color:TColor read FColor write SetColor stored IsColorStored;
    property Dark3D:Boolean read FDark3D write SetDark3D default True;
    property Pen:TChartPen read FPen write SetPen;
    property Size:Longint read FSize write SetSize default 0;
  end;

 { TCustomChartLegend Component }

  TLegendStyle    =(lsAuto,lsSeries,lsValues,lsLastValues);
  TLegendAlignment=(laLeft,laRight,laTop,laBottom);
  LegendException =class(Exception);

  TOnGetLegendText=Procedure( Sender:TCustomAxisPanel;
			      LegendStyle:TLegendStyle;
			      Index:Longint;
			      Var LegendText:String) of Object;

  TCustomChartLegend=class(TChartFontObject)
  private
    FAlignment     : TLegendAlignment;
    FBrush         : TChartBrush;
    FColor         : TColor;
    FColorWidth    : Integer;
    FDividingLines : TChartHiddenPen;
    FFirstValue    : Longint;
    FFrame         : TChartPen;
    FHorizMargin   : Integer;
    FInverted      : Boolean;
    FLegendStyle   : TLegendStyle;
    FMaxNumRows    : Integer;
    FRectLegend    : TRect;
    FResizeChart   : Boolean;
    FShadowColor   : TColor;
    FShadowSize    : Integer;
    FTextStyle     : TLegendTextStyle;
    FTopLeftPos    : Integer;
    FVertMargin    : Integer;
    FVisible       : Boolean;
    { Internal }
    IColorWidth    : Longint;
    ILegendStyle   : TLegendStyle;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    Function GetParentChart:TCustomChart;
    Function GetVertical:Boolean;
    Procedure SetAlignment(Value:TLegendAlignment);
    Procedure SetBrush(Value:TChartBrush);
    Procedure SetColor(Value:TColor);
    Procedure SetColorWidth(Value:Integer);
    Procedure SetDividingLines(Value:TChartHiddenPen);
    Procedure SetFirstValue(Value:Longint);
    Procedure SetFrame(Value:TChartPen);
    Procedure SetHorizMargin(Value:Integer);
    Procedure SetInverted(Value:Boolean);
    Procedure SetLegendStyle(Value:TLegendStyle);
    Procedure SetMaxNumRows(Value:Integer);
    Procedure SetResizeChart(Value:Boolean);
    Procedure SetShadowColor(Value:TColor);
    Procedure SetShadowSize(Value:Integer);
    Procedure SetTextStyle(Value:TLegendTextStyle);
    Procedure SetTopLeftPos(Value:Integer);
    Procedure SetVertMargin(Value:Integer);
    Procedure SetVisible(Value:Boolean);
  protected
    Procedure CalcLegendStyle;
    property InternalLegendStyle:TLegendStyle read ILegendStyle;
    Function MaxLegendValues(YLegend,ItemHeight:Longint):Longint; virtual;
  public
    NumCols      : Longint;
    NumRows      : Longint;

    Constructor Create(AOwner:TCustomChart);
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function FormattedValue(ASeries:TChartSeries; ValueIndex:Longint):String;
    Function FormattedLegend(SeriesOrValueIndex:Longint):String;
    property ParentChart:TCustomChart read GetParentChart;
    Procedure Draw; virtual;
    Function GetColorRect(X1,Y0,Y1:Longint):TRect;
    Function TotalLegendItems:Longint;
    Function MaxLegendWidth(NumLegendValues:Longint):Longint;
    Function Clicked(x,y:Integer):Integer;
    { public property }
    property RectLegend : TRect read FRectLegend write FRectLegend;
    { to be published }
    property Alignment:TLegendAlignment read FAlignment write SetAlignment
					default laRight;
    property Brush:TChartBrush read FBrush write SetBrush;
    property Color:TColor read FColor write SetColor default clWhite;
    property ColorWidth:Integer read FColorWidth write SetColorWidth default 12;
    property DividingLines:TChartHiddenPen read FDividingLines write SetDividingLines;
    property FirstValue:Longint read FFirstValue write SetFirstValue default 0;
    property Font;
    property Frame:TChartPen read FFrame write SetFrame;
    property HorizMargin:Integer read FHorizMargin write SetHorizMargin default 0;
    property Inverted:Boolean read FInverted write SetInverted default False;
    property LegendStyle:TLegendStyle read FLegendStyle
				      write SetLegendStyle default lsAuto;
    property MaxNumRows:Integer read FMaxNumRows write SetMaxNumRows default 10;
    property ResizeChart:Boolean read FResizeChart write SetResizeChart default True;
    property ShadowColor:TColor read FShadowColor write SetShadowColor default clBlack;
    property ShadowSize:Integer read FShadowSize write SetShadowSize default 3;
    property TextStyle:TLegendTextStyle read FTextStyle
					write SetTextStyle default ltsLeftValue;
    property TopPos:Integer read FTopLeftPos write SetTopLeftPos default 10;
    property Vertical:Boolean read GetVertical;
    property VertMargin:Integer read FVertMargin write SetVertMargin default 0;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TChartLegend=class(TCustomChartLegend)
  published
    property Alignment;
    property Brush;
    property Color;
    property ColorWidth;
    property DividingLines;
    property FirstValue;
    property Font;
    property Frame;
    property HorizMargin;
    property Inverted;
    property LegendStyle;
    property MaxNumRows;
    property ResizeChart;
    property ShadowColor;
    property ShadowSize;
    property TextStyle;
    property TopPos;
    property VertMargin;
    property Visible;
  end;

  TChartTitle=class(TChartFontObject)
  private
    FAdjustFrame : Boolean;
    FAlignment   : TAlignment;
    FBrush       : TChartBrush;
    FColor       : TColor;
    FFrame       : TChartHiddenPen;
    FText        : TStrings;
    FTitleRect   : TRect;
    FVisible     : Boolean;
    Function IsFontStored:Boolean;
    Procedure SetAdjustFrame(Value:Boolean);
    Procedure SetAlignment(Value:TAlignment);
    Procedure SetBrush(Value:TChartBrush);
    Procedure SetColor(Value:TColor);
    Procedure SetFrame(Value:TChartHiddenPen);
    Procedure SetText(Value:TStrings);
    Procedure SetVisible(Value:Boolean);
  protected
    DefaultFontColor : TColor;
    DefaultFontStyle : TFontStyles;
  public
    Constructor Create(AOwner: TCustomChart);
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Function Clicked(x,y:Integer):Boolean;
    procedure Draw(OnTop:Boolean);
    property TitleRect:TRect read FTitleRect;
  published
    property AdjustFrame:Boolean read FAdjustFrame write SetAdjustFrame
				 default True;
    property Alignment: TAlignment read FAlignment
				   write SetAlignment default taCenter;
    property Brush:TChartBrush read FBrush write SetBrush;
    property Color:TColor read FColor write SetColor default clTeeColor;
    property Font:TFont read FFont write SetFont stored IsFontStored;
    property Frame:TChartHiddenPen read FFrame write SetFrame;
    property Text:TStrings read FText write SetText;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TChartClick=procedure( Sender:TCustomChart;
			 Button:TMouseButton;
			 Shift: TShiftState;
			 X, Y: Integer) of object;

  TChartClickAxis=procedure( Sender:TCustomChart;
			     Axis:TChartAxis;
			     Button:TMouseButton;
			     Shift: TShiftState;
			     X, Y: Integer) of object;

  TChartClickSeries=procedure( Sender:TCustomChart;
			       Series:TChartSeries;
			       ValueIndex:Longint;
			       Button:TMouseButton;
			       Shift: TShiftState;
			       X, Y: Integer) of object;

  TOnGetLegendPos=Procedure( Sender:TCustomChart; Index:Longint;
			     Var X,Y,XColor:Longint) of object;

  TOnGetLegendRect=Procedure( Sender:TCustomChart; Var Rect:TRect) of object;

  TChartAllowScrollEvent=Procedure( Sender:TChartAxis; Var AMin,AMax:Double;
				    Var AllowScroll:Boolean ) of object;

  TAxisSavedScales=Record
    Auto    : Boolean;
    AutoMin : Boolean;
    AutoMax : Boolean;
    Min     : Double;
    Max     : Double;
  end;

  TAllAxisSavedScales=Record
    Top    : TAxisSavedScales;
    Left   : TAxisSavedScales;
    Right  : TAxisSavedScales;
    Bottom : TAxisSavedScales;
  end;

  TCustomChart=class(TCustomAxisPanel)
  private
    FBackImageInside   : Boolean;
    FBackWall          : TChartWall;
    FBottomWall        : TChartWall;
    FFoot              : TChartTitle;
    FLeftWall          : TChartWall;
    FLegend            : TChartLegend;
    FRestoredAxisScales: Boolean;
    FSavedScales       : TAllAxisSavedScales;
    FTitle             : TChartTitle;

    { events }
    FOnAllowScroll     : TChartAllowScrollEvent;
    FOnClickAxis       : TChartClickAxis;
    FOnClickBackGround : TChartClick;
    FOnClickLegend     : TChartClick;
    FOnClickSeries     : TChartClickSeries;
    FOnGetLegendPos    : TOnGetLegendPos;
    FOnGetLegendRect   : TOnGetLegendRect;
    FOnGetLegendText   : TOnGetLegendText;
    Function GetBackColor:TColor;
    Function GetFrame:TChartPen;
    Procedure RestoreScales(Const Saved:TAllAxisSavedScales);
    Function SaveScales:TAllAxisSavedScales;
    Procedure SetBackColor(Value:TColor);
    procedure SetBackImageInside(Value:Boolean);
    Procedure SetBackWall(Value:TChartWall);
    Procedure SetBottomWall(Value:TChartWall);
    Procedure SetFoot(Value:TChartTitle);
    Procedure SetFrame(Value:TChartPen);
    Procedure SetLeftWall(Value:TChartWall);
    Procedure SetLegend(Value:TChartLegend);
    Procedure SetTitle(Value:TChartTitle);
  protected
    Procedure CalcAxisRect; override;
    Procedure CalcSeriesRect; override;
    Function CalcWallSize(Axis:TCustomChartAxis):Longint; override;
    Procedure CalcZoomPoints; virtual;
    Procedure DoZoom(Const TopI,TopF,BotI,BotF,LefI,LefF,RigI,RigF:Double);
    procedure DrawImage(Const R:TRect); override;
    procedure DrawTitlesAndLegend; override;
    Procedure DrawWalls; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Function AxisTitleOrName(Axis:TCustomChartAxis):String;
    Procedure CalcClickedPart( Pos:TPoint; Var Part:TChartClickedPart );
    Procedure ExchangeSeries(Series1,Series2:Longint);
    Procedure FillSeriesSourceItems(ASeries:TChartSeries; Proc:TGetStrProc); virtual;
    Procedure FillValueSourceItems(ValueList:TChartValueList; Proc:TGetStrProc); virtual;
    Function GetASeries:TChartSeries; { return first active Series }
    Function IsFreeSeriesColor(AColor:TColor; CheckBackground:Boolean):Boolean; override;
    Procedure NextPage;
    Procedure PreviousPage;
    Procedure RemoveAllSeries;
    Procedure SeriesDown(ASeries:TChartSeries);
    Procedure SeriesUp(ASeries:TChartSeries);
    procedure UndoZoom; override;
    Procedure ZoomPercent(Const PercentZoom:Double);
    Procedure ZoomRect(Const Rect:TRect);
    Function FormattedValueLegend(ASeries:TChartSeries; ValueIndex:Longint):String; override;
    Function FormattedLegend(SeriesOrValueIndex:Longint):String;

    { to be published }
    property BackImageInside:Boolean read FBackImageInside
				     write SetBackImageInside default False;
    property BackColor:TColor read GetBackColor write SetBackColor default clTeeColor;
    property BackWall:TChartWall read FBackWall write SetBackWall;
    property Frame:TChartPen read GetFrame write SetFrame;
    property BottomWall:TChartWall read FBottomWall write SetBottomWall;
    property Foot:TChartTitle read FFoot write SetFoot;
    property LeftWall:TChartWall read FLeftWall write SetLeftWall;
    property Legend:TChartLegend read FLegend write SetLegend;
    property Title:TChartTitle read FTitle write SetTitle;

    { events }
    property OnAllowScroll:TChartAllowScrollEvent read FOnAllowScroll write FOnAllowScroll;
    property OnClickAxis:TChartClickAxis read FOnClickAxis write FOnClickAxis;
    property OnClickLegend:TChartClick read FOnClickLegend write FOnClickLegend;
    property OnClickSeries:TChartClickSeries read FOnClickSeries write FOnClickSeries;
    property OnClickBackground:TChartClick read FOnClickbackground write FOnClickBackGround;
    property OnGetLegendPos:TOnGetLegendPos read FOnGetLegendPos write FOnGetLegendPos;
    property OnGetLegendRect:TOnGetLegendRect read FOnGetLegendRect write FOnGetLegendRect;
    property OnGetLegendText:TOnGetLegendText read FOnGetLegendText write FOnGetLegendText;

    { TPanel properties }
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF D5}
    property AutoSize;
    property Constraints;
    property DragKind;
    {$ENDIF}

    { TPanel events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF D5}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}
  end;

  TChart=class(TCustomChart)
  published
    { TCustomChart Properties }
    property AllowPanning;
    property AllowZoom;
    property AnimatedZoom;
    property AnimatedZoomSteps;
    property BackImage;
    property BackImageInside;
    property BackImageMode;
    property BackWall;
    property BottomWall;
    property Foot;
    property Gradient;
    property LeftWall;
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    property PrintProportional;
    property Title;

    { TCustomChart Events }
    property OnAllowScroll;
    property OnClickAxis;
    property OnClickLegend;
    property OnClickSeries;
    property OnClickBackground;
    property OnGetLegendPos;
    property OnGetLegendRect;
    property OnScroll;
    property OnUndoZoom;
    property OnZoom;

    { TCustomAxisPanel properties }
    property AxisVisible;
    property BackColor;
    property BottomAxis;
    property Chart3DPercent;
    property ClipPoints;
    property DepthAxis;
    property Frame;
    property LeftAxis;
    property Legend;
    property MaxPointsPerPage;
    property Monochrome;
    property Page;
    property RightAxis;
    property ScaleLastPage;
    property SeriesList;
    property TopAxis;
    property View3D;
    property View3DOptions;
    property View3DWalls;

    { TCustomAxisPanel events }
    property OnAfterDraw;
    property OnBeforeDrawAxes;
    property OnBeforeDrawSeries;
    property OnGetAxisLabel;
    property OnGetLegendText;
    property OnGetNextAxisLabel;
    property OnPageChange;

    { TPanel properties }
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF D5}
    property AutoSize;
    property Constraints;
    property DragKind;
    {$ENDIF}

    { TPanel events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF D3}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF D5}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}
  end;

{ This function returns a Brush Style }
Function GetDefaultPattern(PatternIndex:Longint):TBrushStyle;

type
  TTeeSeriesType = class
  public
    SeriesClass      : TChartSeriesClass;
    FunctionClass    : TTeeFunctionClass;
    Description      : TeeGalleryString;
    GalleryPage      : TeeGalleryString;
    NumGallerySeries : Integer;
  end;

  TTeeSeriesTypes = class(TList)
  private
    Function GetSeriesType(Index:Integer):TTeeSeriesType;
  public
    property SeriesType[Index:Integer]:TTeeSeriesType read GetSeriesType; {$IFNDEF D1}default;{$ENDIF}
  end;

Var TeeSeriesTypes : TTeeSeriesTypes;  { List of Series types }

{$IFNDEF D1}
type
  TTeeDragObject = class(TDragObject)
  private
    FPart: TChartClickedPart;
  protected
{    function GetDragImages: TCustomImageList; override;}
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(Const APart: TChartClickedPart);
    property Part: TChartClickedPart read FPart;
{    procedure HideDragImage; override;
    procedure ShowDragImage; override;}
  end;
{$ENDIF}

{ Adds a new Series component definition for the Gallery }
{ Setting ANumGallerySeries to zero makes the Series to not appear on the
  Gallery. }
Procedure RegisterTeeSeries( ASeriesClass:TChartSeriesClass;
			     Const ADescription,AGalleryPage:String;
			     ANumGallerySeries : Integer );

{ Adds a new Function component definition for the Gallery }
Procedure RegisterTeeFunction( AFunctionClass:TTeeFunctionClass;
			       Const ADescription,AGalleryPage:String;
			       ANumGallerySeries : Integer );

{ Adds a new Function component to the basic functions gallery }
Procedure RegisterTeeBasicFunction( AFunctionClass:TTeeFunctionClass;
				    Const ADescription:String );

{ Adds a new Series & Function components definition for the Gallery }
Procedure RegisterTeeSeriesFunction( ASeriesClass:TChartSeriesClass;
				     AFunctionClass:TTeeFunctionClass;
				     Const ADescription,AGalleryPage:String;
				     ANumGallerySeries : Integer );

{ Removes Series from Gallery }
Procedure UnRegisterTeeSeries(Const ASeriesList:Array of TChartSeriesClass);

{ Removes Functions from Gallery }
Procedure UnRegisterTeeFunctions(Const AFunctionList:Array of TTeeFunctionClass);

{ Returns the description string for "FunctionType" TeeFunction class }
Function GetTeeFunctionDescription( AFunctionClass:TTeeFunctionClass):String;


{ Assigns all Series properties from Old to New.
  (Data values are not assigned). }
Procedure AssignSeries(Var OldSeries,NewSeries:TChartSeries);

{ Creates a new "class" TeeFunction and sets its ParentSeries to ASeries }
Function CreateNewTeeFunction( ASeries:TChartSeries;
			       AClass:TTeeFunctionClass):TTeeFunction;

{ Creates a new "class" TChartSeries and sets its ParentChart to AChart }
Function CreateNewSeries( AOwner:TComponent;
			  AChart:TCustomAxisPanel;
			  AClass:TChartSeriesClass;
			  AFunctionClass:TTeeFunctionClass):TChartSeries;

Function CloneChartSeries(ASeries:TChartSeries):TChartSeries;

procedure ChangeSeriesType( Var ASeries:TChartSeries;
				NewType:TChartSeriesClass );

procedure ChangeAllSeriesType( AChart:TCustomChart; AClass:TChartSeriesClass );

implementation

Uses TeeConst
     {$IFNDEF TEEOCX}
     ,DsIntf
     {$ENDIF};

Const TeeLegendOff2  = 2;
      TeeLegendOff4  = 4;

Function GetDefaultPattern(PatternIndex:Longint):TBrushStyle;

Const MaxDefaultPatterns = 6;
      PatternPalette     : Array[1..MaxDefaultPatterns] of TBrushStyle=
	( bsHorizontal,
	  bsVertical,
	  bsFDiagonal,
	  bsBDiagonal,
	  bsCross,
	  bsDiagCross
	);
Begin
  result:=PatternPalette[1+(PatternIndex mod MaxDefaultPatterns)];
End;

{ TChartWall }
Constructor TChartWall.Create(AOwner:TCustomChart);
Begin
  inherited Create;
  FParentChart:=AOwner;
  FColor:=clTeeColor;
  FDark3D:=True;
  DefaultColor:=clTeeColor;
  FBrush:=TChartBrush.Create(ParentChart.CanvasChanged);
  FPen:=TChartPen.Create(ParentChart.CanvasChanged);
End;

Destructor TChartWall.Destroy;
Begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
End;

Function TChartWall.IsColorStored:Boolean;
begin
  result:=FColor<>DefaultColor;
end;

Procedure TChartWall.Assign(Source:TPersistent);
Begin
  if Source is TChartWall then
  With TChartWall(Source) do
  Begin
    Self.FBrush.Assign(FBrush);
    Self.FColor   := FColor;
    Self.FDark3D  := FDark3D;
    Self.FPen.Assign(FPen);
    Self.FSize    := FSize;
  end
  else inherited Assign(Source);
End;

Function TChartWall.ApplyDark3D:Boolean;
begin
  result:=(FBrush.Style<>bsClear) and (FBrush.Bitmap=nil) and FDark3D;
end;

Procedure TChartWall.SetPen(Value:TChartPen);
Begin
  FPen.Assign(Value);
end;

Procedure TChartWall.SetBrush(Value:TChartBrush);
Begin
  FBrush.Assign(Value);
End;

Procedure TChartWall.SetSize(Value:Longint);
Begin
  ParentChart.SetLongintProperty(FSize,Value);
End;

Procedure TChartWall.SetColor(Value:TColor);
Begin
  ParentChart.SetColorProperty(FColor,Value);
End;

Procedure TChartWall.SetDark3D(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FDark3D,Value);
end;

{ TChartTitle }  { Used in Chart Title and Chart Foot }
Constructor TChartTitle.Create(AOwner: TCustomChart);
Begin
  inherited Create(AOwner);
  FColor:=clTeeColor;
  FText:=TStringList.Create;
  TStringList(FText).OnChange:=ParentChart.CanvasChanged;
  FVisible:=True;
  FAlignment:=taCenter;
  FFont.Color:=clBlue;
  DefaultFontColor:=clBlue;
  DefaultFontStyle:=[];
  FFrame:=TChartHiddenPen.Create(ParentChart.CanvasChanged);
  FAdjustFrame:=True;
  FBrush:=TChartBrush.Create(ParentChart.CanvasChanged);
end;

Procedure TChartTitle.Assign(Source:TPersistent);
Begin
  if Source is TChartTitle then
  With TChartTitle(Source) do
  Begin
    Self.FAdjustFrame  := FAdjustFrame;
    Self.FAlignment    := FAlignment;
    Self.FBrush.Assign(FBrush);
    Self.FColor        := FColor;
    Self.FFrame.Assign(FFrame);
    Self.FText.Assign(FText);
    Self.FVisible      := FVisible;
  end;
  inherited Assign(Source);
end;

Function TChartTitle.IsFontStored:Boolean;
begin
  With Font do
  result:= (Name<>GetDefaultFontName) or
	   (Size<>GetDefaultFontSize) or
	   (Color<>DefaultFontColor) or
	   (Style<>DefaultFontStyle);
end;

Destructor TChartTitle.Destroy;
Begin
  FText.Free;
  FFrame.Free;
  FBrush.Free;
  inherited Destroy;
end;

Procedure TChartTitle.SetText(Value:TStrings);
begin
  FText.Assign(Value);
  Repaint;
end;

Procedure TChartTitle.SetVisible(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FVisible,Value);
end;

Procedure TChartTitle.SetAdjustFrame(Value:Boolean);
begin
  ParentChart.SetBooleanProperty(FAdjustFrame,Value);
end;

Procedure TChartTitle.SetAlignment(Value:TAlignment);
Begin
  if FAlignment<>Value then
  begin
    FAlignment:=Value;
    Repaint;
  end;
end;

Procedure TChartTitle.SetBrush(Value:TChartBrush);
begin
  FBrush.Assign(Value);
end;

Procedure TChartTitle.SetColor(Value:TColor);
begin
  ParentChart.SetColorProperty(FColor,Value);
end;

Procedure TChartTitle.SetFrame(Value:TChartHiddenPen);
begin
  FFrame.Assign(Value);
end;

Function TChartTitle.Clicked(x,y:Integer):Boolean;
begin
  result:=FVisible and PtInRect(TitleRect,Classes.Point(x,y));
end;

procedure TChartTitle.Draw(OnTop:Boolean);
var R         : TRect;
    tmpMargin : Integer;

  Procedure DrawTitleLine(Const St:String);
  Var tmpXPosTitle : Longint;
      tmpWidth     : Longint;
  Begin
    if Alignment=taLeftJustify then tmpXPosTitle:=R.Left+(tmpMargin div 2)
    else
    begin
      tmpWidth:=ParentChart.Canvas.TextWidth(St);
      if Alignment=taRightJustify then
	 tmpXPosTitle:=R.Right-tmpWidth-(tmpMargin div 2)
      else { center }
	 tmpXPosTitle:=((R.Left+R.Right) div 2)-(tmpWidth div 2);
    end;
    ParentChart.Canvas.TextOut(tmpXPosTitle,R.Top,St);
  end;

Var FontH        : Longint;
    t            : Longint;
    tmpWidth     : Longint;
    tmpMaxWidth  : Longint;
    tmp          : Longint;
    tmpFrameVisible : Boolean;
    tmpFrameWidth   : Integer;
Begin
  ParentChart.FontCanvas(Font);
  With ParentChart.Canvas do
  begin
    FontH:=FontHeight;
    if Self.Color=clTeeColor then
    begin
      if Self.Brush.Style=bsSolid then
      begin
	if Self.Brush.Color<>clTeeColor then
	begin
	  Self.Color:=Self.Brush.Color;
	  Brush.Style:=bsSolid;
	end
	else Brush.Style:=bsClear;
      end
      else
      begin
	Brush.Assign(Self.Brush);
	BackMode:=cbmTransparent;
      end;
    end
    else
    begin
      if Self.Brush.Style=bsSolid then
      begin
	Brush.Style:=bsSolid;
	Brush.Color:=Self.Color;
      end
      else
      begin
	Brush.Assign(Self.Brush);
	BackMode:=cbmOpaque;
	BackColor:=ColorToRGB(Self.Color);
      end;
    end;
  end;

  tmpFrameVisible:=Frame.Visible and (Frame.Color<>clTeeColor);
  if tmpFrameVisible then tmpFrameWidth:=Frame.Width
		     else tmpFrameWidth:=0;
  R.Left  :=ParentChart.ChartRect.Left+tmpFrameWidth;
  if OnTop then
  Begin
    R.Top:=ParentChart.ChartRect.Top+tmpFrameWidth;
    R.Bottom:=R.Top-5;
  end
  else
  Begin
    R.Bottom:=ParentChart.ChartRect.Bottom-tmpFrameWidth;
    R.Top:=R.Bottom+5;
  end;
  R.Right :=ParentChart.ChartRect.Right-tmpFrameWidth;
  FTitleRect:=R;
  if tmpFrameVisible then
  begin
    ParentChart.Canvas.Pen.Assign(Frame);
    tmpWidth:=Frame.Width;
  end
  else
  begin
    ParentChart.Canvas.Pen.Style:=psClear;
    tmpWidth:=0;
  end;
  if OnTop then FTitleRect.Bottom:=R.Top+Text.Count*FontH
	   else FTitleRect.Top   :=R.Bottom-Text.Count*FontH;
  InflateRect(FTitleRect,tmpWidth,tmpWidth);
  tmpMargin:=ParentChart.Canvas.TextWidth('W');
  if tmpFrameVisible or (ParentChart.Canvas.Brush.Style<>bsClear) then
  begin
    if AdjustFrame then
    Begin
      tmpMaxWidth:=0;
      for t:=0 to Text.Count-1 do
      Begin
	tmp:=ParentChart.Canvas.TextWidth(Text[t]);
	if tmp>tmpMaxWidth then tmpMaxWidth:=tmp;
      end;
      tmpMaxWidth:=tmpMaxWidth+tmpMargin;
      With FTitleRect do
      Case Alignment of
	taLeftJustify  : Right:=Left +tmpMaxWidth;
	taRightJustify : Left :=Right-tmpMaxWidth;
	taCenter       : begin
			   tmp:=(Left+Right) div 2;
			   Left :=tmp-(tmpMaxWidth div 2);
			   Right:=tmp+(tmpMaxWidth div 2);
			 end;
      end;
    End;
    ParentChart.Canvas.DoRectangle(FTitleRect);
  end;

  ParentChart.Canvas.BackMode:=cbmTransparent;
  if OnTop then
  for t:=0 to Text.Count-1 do
  Begin
    R.Bottom:=R.Top+FontH;
    DrawTitleLine(Text[t]);
    R.Top:=R.Bottom;
  end
  else
  for t:=Text.Count-1 downto 0 do
  Begin
    R.Top:=R.Bottom-FontH;
    DrawTitleLine(Text[t]);
    R.Bottom:=R.Top;
  end;
  if tmpFrameVisible then
     if OnTop then Inc(R.Bottom,tmpWidth)
	      else Dec(R.Top,tmpWidth);
  With ParentChart.ChartRect do
  if OnTop then Top:=R.Bottom+5
	   else Bottom:=R.Top-5;
  ParentChart.RecalcWidthHeight;
end;

{ TCustomChart }
Constructor TCustomChart.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FTitle:=TChartTitle.Create(Self);
  FFoot:=TChartTitle.Create(Self);
  if csDesigning in ComponentState then
  Begin
    FTitle.FText.Add(ClassName);
    With FFoot.Font do
    Begin
      Color:=clRed;
      Size:=GetDefaultFontSize;
      Style:=[fsItalic];
    end;
    FFoot.DefaultFontColor:=clRed;
    FFoot.DefaultFontStyle:=[fsItalic];
  end;
  FBackWall :=TChartWall.Create(Self);
  FBackWall.Brush.Style:=bsClear;

  FLeftWall :=TChartWall.Create(Self);
  With FLeftWall do
  begin
    Color:=ChartMarkColor;
    DefaultColor:=Color;
  end;

  FLegend:=TChartLegend.Create(Self);

  FBottomWall:=TChartWall.Create(Self);
  With FBottomWall do
  begin
    Color:=clWhite;
    DefaultColor:=Color;
  end;

  CancelMouse:=False;
  FRestoredAxisScales:=True;
end;

Destructor TCustomChart.Destroy;
Begin
  FTitle.Free;
  FFoot.Free;
  FBackWall.Free;
  FBackWall:=nil;
  FBottomWall.Free;
  FLeftWall.Free;
  FLegend.Free;
  inherited Destroy;
end;

procedure TCustomChart.SetTitle(Value:TChartTitle);
begin
  FTitle.Assign(Value);
end;

procedure TCustomChart.SetBackImageInside(Value:Boolean);
begin
  SetBooleanProperty(FBackImageInside,Value);
end;

procedure TCustomChart.SetFoot(Value:TChartTitle);
begin
  FFoot.Assign(Value);
end;

Function TCustomChart.FormattedValueLegend(ASeries:TChartSeries; ValueIndex:Longint):String;
Begin
  if Assigned(ASeries) then result:=FLegend.FormattedValue(ASeries,ValueIndex)
		       else result:='';
end;

Function TCustomChart.FormattedLegend(SeriesOrValueIndex:Longint):String;
Begin
  result:=FLegend.FormattedLegend(SeriesOrValueIndex);
  if Assigned(FOnGetLegendText) then
     FOnGetLegendText(Self,FLegend.ILegendStyle,SeriesOrValueIndex,result);
end;

procedure TCustomChart.SetLegend(Value:TChartLegend);
begin
  FLegend.Assign(Value);
end;

Function TCustomChart.SaveScales:TAllAxisSavedScales;

{  "remember" the axis scales when zooming, to restore if unzooming }
  Procedure SaveAxisScales(Axis:TChartAxis; Var tmp:TAxisSavedScales);
  begin
    tmp.Auto:=Axis.Automatic;
    tmp.AutoMin:=Axis.AutomaticMinimum;
    tmp.AutoMax:=Axis.AutomaticMaximum;
    tmp.Min:=Axis.Minimum;
    tmp.Max:=Axis.Maximum;
  end;

begin
  With Result do
  begin
    SaveAxisScales(TopAxis,Top);
    SaveAxisScales(BottomAxis,Bottom);
    SaveAxisScales(LeftAxis,Left);
    SaveAxisScales(RightAxis,Right);
  end;
end;

Procedure TCustomChart.RestoreScales(Const Saved:TAllAxisSavedScales);

{  restore the "remembered" axis scales when unzooming }
  Procedure RestoreAxisScales(Axis:TChartAxis; Const tmp:TAxisSavedScales);
  begin
    With Axis do
    begin
      Automatic:=tmp.Auto;
      AutomaticMinimum:=tmp.AutoMin;
      AutomaticMaximum:=tmp.AutoMax;
      if not Automatic then SetMinMax(tmp.Min,tmp.Max);
    end;
  end;

begin
  With Saved do
  begin
    RestoreAxisScales(TopAxis,Top);
    RestoreAxisScales(BottomAxis,Bottom);
    RestoreAxisScales(LeftAxis,Left);
    RestoreAxisScales(RightAxis,Right);
  end;
end;

procedure TCustomChart.SetBackWall(Value:TChartWall);
begin
  FBackWall.Assign(Value);
end;

Function TCustomChart.GetFrame:TChartPen;
begin
  if Assigned(FBackWall) then result:=FBackWall.Pen
			 else result:=nil;
end;

Procedure TCustomChart.SetFrame(Value:TChartPen);
begin
  FBackWall.Pen.Assign(Value);
end;

Function TCustomChart.GetBackColor:TColor;
begin
  result:=FBackWall.Color;
end;

Procedure TCustomChart.SetBackColor(Value:TColor);
begin
  FBackWall.Color:=Value;
  { fix 4.01: do not set backwall solid when loading dfms... }
  if (Parent<>nil) and (not (csLoading in ComponentState)) then
     FBackWall.Brush.Style:=bsSolid;
end;

Function TCustomChart.IsFreeSeriesColor(AColor:TColor; CheckBackground:Boolean):Boolean;
var t        : Longint;
    tmpColor : TColor;
Begin
  for t:=0 to SeriesCount-1 do
  Begin
    tmpColor:=Series[t].SeriesColor;
    if (tmpColor=AColor) or
       (CheckBackground and
       ( (AColor=Color) or (AColor=BackColor) )) then
    Begin
      result:=False;
      exit;
    end;
  end;
  result:=(not CheckBackground) or ( (AColor<>Color) and (AColor<>BackColor) );
end;

procedure TCustomChart.SetLeftWall(Value:TChartWall);
begin
  FLeftWall.Assign(Value);
end;

procedure TCustomChart.SetBottomWall(Value:TChartWall);
begin
  FBottomWall.Assign(Value);
end;

procedure TCustomChart.DrawTitlesAndLegend;

  Procedure DrawTitleFoot;
  Begin
    if FTitle.Visible then FTitle.Draw(True);
    if FFoot.Visible then FFoot.Draw(False);
  end;

begin
  { draw title and foot, or draw foot and title, depending
    if legend is at left/right or at top/bottom. }
  { top/bottom legends need to leave space for the title and foot
    before they get displayed.  }
  Canvas.FrontPlaneBegin;
  if Legend.Visible And (SeriesList.CountActive>0) then
  begin
    if Legend.Vertical then
    begin
      Legend.Draw;
      DrawTitleFoot;
    end
    else
    begin
      DrawTitleFoot;
      Legend.Draw;
    end;
  end
  else DrawTitleFoot;
  Canvas.FrontPlaneEnd;
end;

procedure TCustomChart.DrawImage(Const R:TRect);
begin
  if (BackImage.Graphic<>nil)
     and (not FBackImageInside) then DrawBitmap(R,0);
end;

Function TCustomChart.CalcWallSize(Axis:TCustomChartAxis):Longint;
begin
  result:=0;
  if View3D and View3DWalls then
  begin
    if Axis=LeftAxis then   result:=FLeftWall.FSize   else
    if Axis=BottomAxis then result:=FBottomWall.FSize
  end;
end;

Procedure TCustomChart.DrawWalls;  { Left and Bottom wall only }

  Procedure PrepareWallCanvas(AWall:TChartWall);
  begin
    With AWall do
    begin
      Canvas.AssignVisiblePen(FPen);
      if FBrush.Bitmap=nil then
      begin
	Canvas.Brush.Bitmap:=nil;
	if (FColor<>clTeeColor) or (FBrush.Style<>bsSolid) then
	   SetBrushCanvas(FColor,FBrush.Style,FBrush.Color)
	else
	   Canvas.Brush.Style:=bsClear
      end
      else Canvas.Brush.Bitmap:=FBrush.Bitmap;
    end;
  end;

var tmpRect:TRect;
Begin
  PrepareWallCanvas(FBackWall);
  With FBackWall do
  begin
    if View3D then
    begin
      if View3DWalls and (FSize>0) then { <-- trick, was backcolor }
      With ChartRect do
	Canvas.Cube( Left-CalcWallSize(LeftAxis),Right,
		     Top,Bottom+CalcWallSize(BottomAxis),Width3D,Width3D+FSize,
		     ApplyDark3D)
      else
      With ChartRect do
       Canvas.RectangleWithZ(Rect(Succ(Left),Pred(Top),Right,Bottom),Width3D);
{        Canvas.GradientFill( ChartRect,clSilver,clWhite,gdRightLeft);}
    end
    else
    With ChartRect do Canvas.Rectangle(Left,Top,Succ(Right),Bottom);

    if (BackImage.Graphic<>nil) and FBackImageInside then
    begin
      tmpRect:=ChartRect;
      if Pen.Visible then InflateRect(tmpRect,0,-1);
      DrawBitmap(tmpRect,Width3D);
    end;
  end;
  if View3D and View3DWalls then
  begin
    PrepareWallCanvas(FLeftWall);
    With FLeftWall,ChartRect do
    if FSize>0 then
       Canvas.Cube( Left-FSize,Left,
		    Top,Bottom+CalcWallSize(BottomAxis),0,Width3D+1,
		    ApplyDark3D)
    else
       Canvas.RectangleZ(Left,Top,Bottom+CalcWallSize(BottomAxis),0,Width3D+1);
    PrepareWallCanvas(FBottomWall);
    With FBottomWall,ChartRect do
    if FSize>0 then
       Canvas.Cube(Left,Right,Bottom,Bottom+FSize,0,Width3D,ApplyDark3D)
    else
       Canvas.RectangleY(Left,Bottom,Right,0,Width3D);
  end;
end;

Procedure TCustomChart.SeriesUp(ASeries:TChartSeries);
var t : Longint;
Begin
       { scroll up ASeries in SeriesList. this changes the Z order }
  t:=SeriesList.IndexOf(ASeries);
  if t>0 then
  Begin
    SeriesList.Exchange(t,t-1);
    Invalidate;
  end;
end;

Procedure TCustomChart.SeriesDown(ASeries:TChartSeries);
var t : Longint;
Begin
       { scroll down ASeries in SeriesList. this changes the Z order }
  t:=SeriesList.IndexOf(ASeries);
  if t<SeriesCount-1 then
  Begin
    SeriesList.Exchange(t,t+1);
    Invalidate;
  end;
end;

Procedure TCustomChart.NextPage;
Begin
  if (MaxPointsPerPage>0) and (Page<NumPages) then Page:=Page+1;
End;

Procedure TCustomChart.PreviousPage;
Begin
  if (MaxPointsPerPage>0) and (Page>1) then Page:=Page-1;
End;

Procedure TCustomChart.RemoveAllSeries;
Begin
  While SeriesCount>0 do RemoveSeries(Series[0]);
End;

Procedure TCustomChart.DoZoom(Const topi,topf,boti,botf,lefi,leff,rigi,rigf:Double);

  Procedure ZoomAxis(AAxis:TChartAxis; Const tmpA,tmpB:Double);
  Begin
        { apply a portion of the desired zoom to achieve animation }
    with AAxis do
      SetMinMax(Minimum+((tmpA-Minimum)/AnimatedZoomFactor),
		Maximum-((Maximum-tmpB)/AnimatedZoomFactor));
  end;

Var t : Longint;
Begin
  if FRestoredAxisScales then
  begin
    FSavedScales:=SaveScales;
    FRestoredAxisScales:=False;
  end;
  if AnimatedZoom then  {apply animation... }
    for t:=1 to AnimatedZoomSteps-1 do
    begin
      ZoomAxis(LeftAxis,Lefi,Leff);
      ZoomAxis(RightAxis,Rigi,Rigf);
      ZoomAxis(TopAxis,Topi,Topf);
      ZoomAxis(BottomAxis,Boti,Botf);
      Refresh;
    end;
  { final zoom }
  LeftAxis.SetMinMax(Lefi,Leff);
  RightAxis.SetMinMax(Rigi,Rigf);
  TopAxis.SetMinMax(Topi,Topf);
  BottomAxis.SetMinMax(Boti,Botf);
  Zoomed:=True;
  if Assigned(FOnZoom) then FOnZoom(Self);
end;

Procedure TCustomChart.ZoomRect(Const Rect:TRect);
Begin
  With IZoom do
  Begin
    X0:=Rect.Left;
    Y0:=Rect.Top;
    X1:=Rect.Right;
    Y1:=Rect.Bottom;
    CalcZoomPoints;
  end;
End;

Procedure TCustomChart.ZoomPercent(Const PercentZoom:Double);

  Procedure CalcAxisScale(Axis:TChartAxis; Var tmpA,tmpB:Double);
  Var tmpDelta : Double;
      AMin     : Double;
      AMax     : Double;
  Begin
    AMin:=Axis.Minimum;
    AMax:=Axis.Maximum;
    Axis.CalcMinMax(AMin,AMax);
    tmpDelta:=(AMax-AMin)*(PercentZoom-100.0)*0.01;
    tmpA:=AMin+tmpDelta;
    tmpB:=AMax-tmpDelta;
  end;

var Lefi : Double;
    Leff : Double;
    Rigi : Double;
    Rigf : Double;
    Topi : Double;
    Topf : Double;
    Boti : Double;
    Botf : Double;
Begin
       { zoom a given "percent" }
  CalcAxisScale(LeftAxis,Lefi,Leff);
  CalcAxisScale(RightAxis,Rigi,Rigf);
  CalcAxisScale(TopAxis,Topi,Topf);
  CalcAxisScale(BottomAxis,Boti,Botf);
  DoZoom(Topi,Topf,Boti,Botf,Lefi,Leff,Rigi,Rigf);
  Repaint;
end;

Procedure TCustomChart.CalcClickedPart( Pos:TPoint; Var Part:TChartClickedPart );

  Function ClickedAxis(Axis:TCustomChartAxis):Boolean;
  begin
    result:=Axis.Clicked(Pos.x,Pos.y);
    if result then
    begin
      Part.Part:=cpAxis;
      Part.AAxis:=Axis;
    end;
  end;

var t : Longint;
begin
  With Part do
  begin
    Part:=cpNone;
    PointIndex:=-1;
    ASeries:=nil;
    AAxis:=nil;
    { IMPORTANT: Z order inverted }
    for t:=SeriesCount-1 downto 0 do
    With Self.Series[t] do
    if Active then
    Begin
      PointIndex:=Clicked(Pos.X,Pos.Y);
      if PointIndex<>-1 then
      begin
	ASeries:=Series[t];
	Part:=cpSeries;
	Exit;
      end;
    end;

    for t:=0 to FAxes.Count-1 do if ClickedAxis(FAxes[t]) then Exit;

    if Title.Clicked(Pos.X,Pos.Y) then Part:=cpTitle
    else
    if Foot.Clicked(Pos.X,Pos.Y) then  Part:=cpFoot
    else
    if PtInRect(ChartRect,Pos) and  { <-- should be PtInCube(ChartRect,0,MaxZ) }
       (SeriesList.CountActive>0) then
	  Part:=cpChartRect
    else
    if Legend.Visible then
    begin
      PointIndex:=Legend.Clicked(Pos.X,Pos.Y);
      if PointIndex<>-1 then Part:=cpLegend;
    end;
  end;
end;

Function TCustomChart.AxisTitleOrName(Axis:TCustomChartAxis):String;
Begin
  result:=Axis.Title.Caption;
  if result='' then
  begin
    if Axis=DepthAxis then result:=TeeMsg_DepthAxis
    else
    With Axis do
    if Horizontal then
       if OtherSide then result:=TeeMsg_TopAxis
		    else result:=TeeMsg_BottomAxis
    else
       if OtherSide then result:=TeeMsg_RightAxis
		    else result:=TeeMsg_LeftAxis;
  end;
End;

procedure TCustomChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var IClicked : Boolean;

    Procedure CheckZoomPanning;
    Begin
      if AllowZoom and (Button=TeeZoomMouseButton) and
	 ( TeeZoomKeyShift <= Shift ) then
      Begin
	Cursor:=crDefault;
	IZoom.Activate(x,y);
	DrawZoomRectangle;
	IClicked:=True;
      end;
      if (AllowPanning<>pmNone) and (Button=TeeScrollMouseButton) and
	 ( TeeScrollKeyShift <= Shift ) then
      Begin
	Cursor:=crDefault;
	IPanning.Activate(x,y);
	IClicked:=True;
      end;
    end;

    Procedure CheckBackground;
    begin
      if Assigned(FOnClickBackground) then
      begin
	CancelMouse:=True;
	FOnClickBackGround(Self,Button,Shift,x,y);
	IClicked:=CancelMouse;
      end;
    end;

var tmpPart : TChartClickedPart;
Begin
  inherited MouseDown(Button,Shift,x,y);
  if IZoom.Active or IPanning.Active then Exit;
  if CancelMouse then CancelMouse:=False
  else
  begin
    CalcClickedPart(Classes.Point(x,y),tmpPart);
    IClicked:=False;
    Case tmpPart.Part of
      cpLegend: begin
		  if Assigned(FOnClickLegend) then
		  begin
		    CancelMouse:=True;
		    FOnClickLegend(Self,Button,Shift,x,y);
		    IClicked:=CancelMouse;
		  end;
		end;
      cpSeries: Begin
		  With tmpPart do
		  IClicked:=ASeries.DoSeriesClick(ASeries,PointIndex,Button,Shift,x,y);
		  if Assigned(FOnClickSeries) then
		  begin
		    CancelMouse:=True;
		    FOnClickSeries(Self,tmpPart.ASeries,tmpPart.PointIndex,Button,Shift,x,y);
		    IClicked:=CancelMouse;
		  end;
		  if not IClicked then CheckZoomPanning;
		end;
	cpAxis: begin
		  if Assigned(FOnClickAxis) then
		  begin
		    CancelMouse:=True;
		    FOnClickAxis(Self,TChartAxis(tmpPart.AAxis),Button,Shift,x,y);
		    IClicked:=CancelMouse;
		  end;
		  if not IClicked then CheckZoomPanning;
		end;
   cpChartRect: begin
		  CheckBackground;
		  if not IClicked then CheckZoomPanning;
		end;
    end;
    if not IClicked then CheckBackground;
    CancelMouse:=False;
  end;
End;

procedure TCustomChart.MouseMove(Shift: TShiftState; X, Y: Integer);

  Procedure ProcessPanning(Axis:TChartAxis; IniPos,EndPos:Longint);
  Var Delta          : Double;
      tmpMin         : Double;
      tmpMax         : Double;
      tmpAllowScroll : Boolean;
  Begin
    With Axis do
    begin
      Delta:=CalcPosPoint(IniPos)-CalcPosPoint(EndPos);
      tmpMin:=Minimum+Delta;
      tmpMax:=Maximum+Delta;
    end;
    tmpAllowScroll:=True;
    if Assigned(FOnAllowScroll) then
       FOnAllowScroll(Axis,tmpMin,tmpMax,tmpAllowScroll);
    if tmpAllowScroll then Axis.SetMinMax(tmpMin,tmpMax);
  end;

  Function CheckMouseSeries:Boolean;
  Var t : Integer;
  Begin
    result:=False;
    for t:=0 to SeriesCount-1 do
    With Series[t] do
    if Active and (Cursor<>crDefault) then
    begin
      if Clicked(X,Y)<>-1 then
      Begin
	Self.Cursor:=Cursor; { <-- mouse is over a Series point ! }
	result:=True;
	break;
      end;
    end;
    for t:=0 to SeriesCount-1 do Series[t].DoSeriesMouseMove(Shift,X,Y);
   end;

Var Panned : Boolean;

   Procedure PanAxis(AxisHorizontal:Boolean; Const Pos1,Pos2:Integer);
   Var tmpAxis : TChartAxis;
       t       : Integer;
       tmpMode : TPanningMode;
   begin
     if AxisHorizontal then tmpMode:=pmHorizontal
		       else tmpMode:=pmVertical;
     if (Pos1<>Pos2) and
	( (AllowPanning=tmpMode) or (AllowPanning=pmBoth) ) then
     Begin
       for t:=0 to FAxes.Count-1 do
       begin
	 tmpAxis:=TChartAxis(FAxes[t]);
	 if not tmpAxis.IsDepthAxis then
	    if (AxisHorizontal and tmpAxis.Horizontal) or
	       ((not AxisHorizontal) and (not tmpAxis.Horizontal)) then
	       ProcessPanning(tmpAxis,Pos2,Pos1);
       end;
       Panned:=True;
     end;
   end;

Begin
  inherited MouseMove(Shift,x,y);
  if csDesigning in ComponentState then Exit;
  With IZoom do  { zoom }
  if Active then
  Begin
    if (x<>X1) or (y<>Y1) then
    Begin
      DrawZoomRectangle;
      X1:=x;
      Y1:=y;
      DrawZoomRectangle;
    end;
  end
  else
  With IPanning do { scroll }
  if Active then
  Begin
    if PtInRect(ChartRect,Classes.Point(x,y)) then
    Begin
      if (x<>X1) or (y<>Y1) then
      Begin
	Panned:=False;
	Check;
	if FRestoredAxisScales then
	begin
	  FSavedScales:=SaveScales;
	  FRestoredAxisScales:=False;
	end;

	PanAxis(True,X,X1);
	PanAxis(False,Y,Y1);
	X1:=x;
	Y1:=y;
	if Panned then
	Begin
	  if Assigned(FOnScroll) then FOnScroll(Self);
	  Invalidate;
	end;
      End;
    end
    else IPanning.Active:=False;
  End
  else
  if not CheckMouseSeries then Cursor:=OriginalCursor;{ <-- guess cursor shape }
End;

procedure TCustomChart.UndoZoom;
begin
  if not FRestoredAxisScales then RestoreScales(FSavedScales);
  FRestoredAxisScales:=True;
  inherited UndoZoom;
end;

Procedure TCustomChart.CalcZoomPoints;
Begin
  With IZoom do
  Begin
    Check;
    DoZoom( TopAxis.CalcPosPoint(X0),TopAxis.CalcPosPoint(X1),
	    BottomAxis.CalcPosPoint(X0),BottomAxis.CalcPosPoint(X1),
	    LeftAxis.CalcPosPoint(Y1),LeftAxis.CalcPosPoint(Y0),
	    RightAxis.CalcPosPoint(Y1),RightAxis.CalcPosPoint(Y0));
  end;
End;

procedure TCustomChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Const MinZoomDrag=16;
Var t:Longint;
Begin
  inherited MouseUp(Button,Shift,x,y);
  for t:=0 to SeriesCount-1 do Series[t].DoSeriesMouseUp(Button,Shift,X,Y);
  if IZoom.Active and (Button=TeeZoomMouseButton) then
  With IZoom do
  Begin
    Active:=False;
    DrawZoomRectangle;
    Canvas.Pen.Mode:=pmCopy;
    X1:=x;
    Y1:=y;
    if (Abs(X1-X0)>MinZoomDrag) and (Abs(Y1-Y0)>MinZoomDrag) then
    Begin
      if (X1>X0) and (Y1>Y0) then CalcZoomPoints { <-- do zoom in ! }
			     else UndoZoom;      { <-- do zoom out ! }
      Invalidate;
    end;
  end;
  IPanning.Active:=False;
End;

Procedure TCustomChart.CalcAxisRect;
var tmpR    : TRect;
    OldR    : TRect;
    tmp     : Integer;
    tmpAxis : TChartAxis;
Begin
  with FAxes do
  for tmp:=0 to FAxes.Count-1 do TChartAxis(FAxes[tmp]).AdjustMaxMin;

  CalcSize3DWalls;
  { for orthogonal only }
  if View3D and View3DOptions.Orthogonal then
  begin
    if View3DWalls then tmp:=BackWall.Size
		   else tmp:=0;
    With ChartRect do
    begin
      Dec(Right,Width3D+tmp);
      Inc(Top,Height3D+tmp);
    end;
  end;
  RecalcWidthHeight;

  tmpR:=ChartRect;
  with FAxes do
  for tmp:=0 to FAxes.Count-1 do
  begin
    tmpAxis:=TChartAxis(FAxes[tmp]);
    if IsAxisVisible(tmpAxis) then
    Begin
      OldR:=TmpR;
      tmpAxis.CalcRect(OldR,tmp<5);  { <-- inflate only for first 4 axes }
      if IntersectRect(OldR,TmpR,OldR)
	 {$IFDEF D1}
	   <>0
	 {$ENDIF}
	    then tmpR:=OldR;
    end;
  end;
  ChartRect:=tmpR;
  RecalcWidthHeight;
  with FAxes do
  for tmp:=0 to FAxes.Count-1 do TChartAxis(FAxes[tmp]).InternalCalcPositions;
end;

Procedure TCustomChart.CalcSeriesRect;

  Procedure CalcSeriesAxisRect(Axis:TChartAxis);
  Var tmpR      : TRect;
      a         : Integer;
      b         : Integer;
      tmpSeries : Integer;
  Begin
    tmpR:=Rect(0,0,0,0);
    for tmpSeries:=0 to SeriesCount-1 do
    With Series[tmpSeries] do
    if Active then
       if AssociatedToAxis(Axis) then
	  if Axis.Horizontal then
	  begin
	    CalcHorizMargins(a,b);
	    With tmpR do
	    begin
	      if Axis.AutomaticMinimum then Left :=MaxLong(Left,a);
	      if Axis.AutomaticMaximum then Right:=MaxLong(Right,b);
	    end;
	  end
	  else
	  begin
	    CalcVerticalMargins(a,b);
	    With tmpR do
	    begin
	      if Axis.AutomaticMaximum then Top   :=MaxLong(Top,a);
	      if Axis.AutomaticMinimum then Bottom:=MaxLong(Bottom,b);
	    end;
	  end;
    Axis.AdjustMaxMinRect(tmpR);
  end;

Begin
  CalcSeriesAxisRect(TopAxis);
  CalcSeriesAxisRect(LeftAxis);
  CalcSeriesAxisRect(RightAxis);
  CalcSeriesAxisRect(BottomAxis);
end;

Procedure TCustomChart.Assign(Source:TPersistent);
begin
  if Source is TCustomChart then
  With TCustomChart(Source) do
  begin
{    Self.FBackImage.Assign(FBackImage);
    Self.FBackImageMode   := FBackImageMode; }
    Self.FBackImageInside := FBackImageInside;
    Self.FBackWall.Assign(FBackWall);
    Self.FBottomWall.Assign(FBottomWall);
{    Self.FGradient.Assign(FGradient);}
    Self.FLeftWall.Assign(FLeftWall);
    Self.FLegend.Assign(FLegend);
{    Self.FAllowPanning    := FAllowPanning;
    Self.FAllowZoom       := FAllowZoom;
    Self.FAnimatedZoom    := FAnimatedZoom;
    Self.FAnimatedZoomSteps:=FAnimatedZoomSteps;}
    Self.FFoot.Assign(FFoot);
    Self.FTitle.Assign(FTitle);
  end;
  inherited Assign(Source);
end;

Procedure TCustomChart.ExchangeSeries(Series1,Series2:Longint);
var tmp : Integer;
begin
  SeriesList.Exchange(Series1,Series2);
  tmp:=Series[Series1].ComponentIndex;
  Series[Series1].ComponentIndex:=Series[Series2].ComponentIndex;
  Series[Series2].ComponentIndex:=tmp;
  Repaint;
end;
(*
function TCustomChart.GetPalette: HPALETTE;
begin
  Result := 0;  { default result is no palette }
  if Assigned(FBackImage) and (FBackImage.Graphic<>nil) and
     (FBackImage.Graphic is TBitmap) then       { only bitmaps have palettes }
	 Result := TBitmap(FBackImage.Graphic).Palette; { use it if available }
end;
*)
Procedure TCustomChart.FillSeriesSourceItems( ASeries:TChartSeries;
					Proc:TGetStrProc);
begin
      { abstract }
end;

Procedure TCustomChart.FillValueSourceItems( ValueList:TChartValueList;
				       Proc:TGetStrProc);
Var t:Integer;
Begin
  With ValueList.Owner do
  if (DataSource<>nil) and (DataSource is TChartSeries) then
     With TChartSeries(DataSource) do
     for t:=0 to ValuesLists.Count-1 do Proc(ValuesLists.ValueList[t].Name);
end;

Function TCustomChart.GetASeries:TChartSeries;
var t : Integer;
Begin
  for t:=0 to SeriesCount-1 do
  if Series[t].Active then
  begin
    result:=SeriesList[t];
    exit;
  end;
  result:=nil;
End;

{ TChartLegend Sub-Component }
Constructor TCustomChartLegend.Create(AOwner:TCustomChart);
begin
  inherited Create(AOwner);
  FVisible:=True;
  FAlignment:=laRight;
  ILegendStyle:=lsAuto;
  FMaxNumRows:=10;
  FColor:=clWhite;
  FFrame:=TChartPen.Create(ParentChart.CanvasChanged);
  FTextStyle:=ltsLeftValue;
  FLegendStyle:=lsAuto;
  FColorWidth:=12;
  FTopLeftPos:=10; { 10 % }
  FResizeChart:=True;
  FShadowColor:=clBlack;
  FShadowSize:=3;
  FDividingLines:=TChartHiddenPen.Create(ParentChart.CanvasChanged);
  FBrush:=TChartBrush.Create(ParentChart.CanvasChanged);
end;

Destructor TCustomChartLegend.Destroy;
begin
  FDividingLines.Free;
  FFrame.Free;
  FBrush.Free;
  Inherited Destroy;
end;

Procedure TCustomChartLegend.Assign(Source:TPersistent);
Begin
  if Source is TCustomChartLegend then
  With TCustomChartLegend(Source) do
  Begin
    Self.FVisible    :=FVisible;
    Self.FLegendStyle:=FLegendStyle;
    Self.FFirstValue :=FFirstValue;
    Self.FAlignment  :=FAlignment;
    Self.FTextStyle  :=FTextStyle;
    Self.FColorWidth :=FColorWidth;
    Self.FHorizMargin:=FHorizMargin;
    Self.FVertMargin :=FVertMargin;
    Self.FResizeChart:=FResizeChart;
    Self.FInverted   :=FInverted;
    Self.FTopLeftPos :=FTopLeftPos;
    Self.FColor      :=FColor;
    Self.FFrame.Assign(FFrame);
    Self.FMaxNumRows :=FMaxNumRows;
    Self.FBrush.Assign(FBrush);
  end;
  inherited Assign(Source);
end;

Procedure TCustomChartLegend.SetVisible(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FVisible,Value);
end;

Procedure TCustomChartLegend.SetInverted(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FInverted,Value);
end;

Procedure TCustomChartLegend.SetAlignment(Value:TLegendAlignment);
Begin
  if FAlignment<>Value then
  begin
    FAlignment:=Value;
    Repaint;
  end;
end;

Procedure TCustomChartLegend.SetFrame(Value:TChartPen);
Begin
  FFrame.Assign(Value);
end;

procedure TCustomChartLegend.CMVisibleChanged(var Message: TMessage);
Begin
  Repaint;
end;

procedure TCustomChartLegend.CMColorChanged(var Message: TMessage);
Begin
  Repaint;
end;

Procedure TCustomChartLegend.SetTopLeftPos(Value:Integer);
Begin
  if (Value<0) or (Value>100) then
     Raise LegendException.Create(TeeMsg_LegendTopPos)
  else
    ParentChart.SetIntegerProperty(FTopLeftPos,Value);
end;

Procedure TCustomChartLegend.SetFirstValue(Value:Longint);
Begin
  if Value<0 then
     Raise LegendException.Create(TeeMsg_LegendFirstValue)
  else
     ParentChart.SetLongintProperty(FFirstValue,Value);
End;

Procedure TCustomChartLegend.SetTextStyle(Value:TLegendTextStyle);
Begin
  if FTextStyle<>Value then
  begin
    FTextStyle:=Value;
    Repaint;
  end;
end;

Procedure TCustomChartLegend.SetMaxNumRows(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FMaxNumRows,Value);
end;

Procedure TCustomChartLegend.SetLegendStyle(Value:TLegendStyle);
Begin
  if FLegendStyle<>Value then
  begin
    FLegendStyle:=Value;
    CalcLegendStyle; { <-- force recalc of ILegendStyle }
    Repaint;
  end;
end;

Procedure TCustomChartLegend.SetHorizMargin(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FHorizMargin,Value);
end;

Procedure TCustomChartLegend.SetVertMargin(Value:Integer);
Begin
  ParentChart.SetIntegerProperty(FVertMargin,Value);
end;

Function TCustomChartLegend.GetParentChart:TCustomChart;
begin
  result:=TCustomChart(inherited ParentChart);
end;

Procedure TCustomChartLegend.SetResizeChart(Value:Boolean);
Begin
  ParentChart.SetBooleanProperty(FResizeChart,Value);
end;

Procedure TCustomChartLegend.SetBrush(Value:TChartBrush);
begin
  FBrush.Assign(Value);
end;

Procedure TCustomChartLegend.SetColor(Value:TColor);
Begin
  ParentChart.SetColorProperty(FColor,Value);
end;

Procedure TCustomChartLegend.SetColorWidth(Value:Integer);
Begin
  if (Value<0) or (Value>100) then
     Raise LegendException.Create(TeeMsg_LegendColorWidth)
  else
     ParentChart.SetIntegerProperty(FColorWidth,Value);
end;

Procedure TCustomChartLegend.CalcLegendStyle;
begin
  if FLegendStyle=lsAuto then
  begin
    if ParentChart.SeriesList.CountActive>1 then
       ILegendStyle:=lsSeries
    else
       ILegendStyle:=lsValues;
  end
  else ILegendStyle:=FLegendStyle;
end;

Function TCustomChartLegend.FormattedLegend(SeriesOrValueIndex:Longint):String;
Begin
  With ParentChart do
  Case ILegendStyle of
    lsSeries:     result:=SeriesTitleLegend(SeriesOrValueIndex);
    lsValues:     result:=FormattedValueLegend( GetASeries,SeriesOrValueIndex );
    lsLastValues: result:=FormattedValueLegend( Series[SeriesOrValueIndex],
						Series[SeriesOrValueIndex].Count-1);
  end;
end;

Procedure TCustomChartLegend.SetShadowColor(Value:TColor);
begin
  ParentChart.SetColorProperty(FShadowColor,Value);
end;

Procedure TCustomChartLegend.SetShadowSize(Value:Integer);
begin
  ParentChart.SetIntegerProperty(FShadowSize,Value);
end;

Procedure TCustomChartLegend.SetDividingLines(Value:TChartHiddenPen);
begin
  FDividingLines.Assign(Value);
end;

Function TCustomChartLegend.MaxLegendValues(YLegend,ItemHeight:Longint):Longint;

  Function CalcMaxLegendValues(A,B,C:Longint):Longint;
  begin
    With ParentChart do
    Begin
      if (YLegend<A) and (ItemHeight>0) then
      Begin
	result:=((B-2*Self.Frame.Width)-YLegend+C) div ItemHeight;
	result:=MinLong(result,TotalLegendItems);
      end
      else result:=0;
    end;
  end;

Begin
  With ParentChart do
  if Self.Vertical then
     result:=CalcMaxLegendValues(ChartRect.Bottom,ChartHeight,ChartRect.Top)
  else
     result:=CalcMaxLegendValues(ChartRect.Right,ChartWidth,0);
end;

Function TCustomChartLegend.TotalLegendItems:Longint;
var tmpSeries : TChartSeries;
    t         : Longint;
begin
  result:=0;
  if (InternalLegendStyle=lsSeries) or (InternalLegendStyle=lsLastValues) then
  begin
    for t:=0 to ParentChart.SeriesCount-1 do
    begin
      With ParentChart.Series[t] do
      if Active and ShowInLegend then Inc(result);
    end;
    Dec(result,FirstValue);
  end
  else
  begin
    tmpSeries:=ParentChart.GetASeries;
    if Assigned(tmpSeries) and (tmpSeries.ShowInLegend) then
       result:=tmpSeries.CountLegendItems-FirstValue;
  end;
  result:=MaxLong(0,result);
end;

Function TCustomChartLegend.MaxLegendWidth(NumLegendValues:Longint):Longint;
var t               : Longint;
    LastLegendIndex : Longint;
Begin
  result:=0;
  if NumLegendValues=TeeAllValues then
     LastLegendIndex:=TotalLegendItems+FirstValue-1
  else
     LastLegendIndex:=FirstValue+NumLegendValues-1;
  With ParentChart do
  for t:=FirstValue to LastLegendIndex do
      result:=MaxLong(result,Canvas.TextWidth(FormattedLegend(t)));
end;

Function TCustomChartLegend.GetColorRect(X1,Y0,Y1:Longint):TRect;
begin
  With result do
  begin
    Left   :=X1-IColorWidth;
    Right  :=Left+IColorWidth;
    Top    :=Y0+2;
    Bottom :=Y1-2;
  end;
end;

Function TCustomChartLegend.GetVertical:Boolean;
begin
  result:=(Alignment=laLeft) or (Alignment=laRight)
end;

Procedure TCustomChartLegend.Draw;
Var XLegendText  : Longint;
    XLegendColor : Longint;
    tmpMaxWidth  : Longint;
    ItemHeight   : Longint;
    tmpSeries    : TChartSeries;
    tmpTotalItems: Longint;

  Procedure DrawLegendItem(ItemIndex,ItemOrder:Longint);
  Var PosXLegend : Longint;
      PosYLegend : Longint;
      R          : TRect;
      tmpSt      : String;
  Begin
    if ItemOrder>=tmpTotalItems then exit;
    with ParentChart,Canvas do
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Self.Color;

      posXLegend:=XLegendText;
      posYLegend:=RectLegend.Top+1;
      if Self.Vertical then Inc(posYLegend,ItemOrder*ItemHeight)
      else
      begin
	Inc(posYLegend,(ItemOrder div NumCols)*ItemHeight);
	Inc(posXLegend,(tmpMaxWidth+IColorWidth+TeeLegendOff4)*(ItemOrder mod NumCols));
      end;

      if Assigned(FOnGetLegendPos) then
	 FOnGetLegendPos(ParentChart,ItemIndex,posXLegend,posYLegend,XLegendColor);

      Brush.Style:=bsClear;
      tmpSt:=FormattedLegend(ItemIndex);

      if tmpSt<>'' then
      begin
	TextOut(posXLegend,posYLegend,tmpSt);
	R:=GetColorRect(posXLegend-TeeLegendOff4,posYLegend,posYLegend+ItemHeight);

	if (InternalLegendStyle=lsSeries) or (InternalLegendStyle=lsLastValues) then
	   ActiveSeriesLegend(ItemIndex).DrawLegend(-1,R)
	else
	Begin
	  if Assigned(tmpSeries) then
	     tmpSeries.DrawLegend(tmpSeries.LegendToValueIndex(ItemIndex),R)
	  else
	  Begin
	    Brush.Color:=clWhite;
	    Brush.Style:=bsSolid;
	    DoRectangle(R);
	  end;
	end;
      end;
      if (ItemOrder>0) and (FDividingLines.Visible) then
      begin
	Canvas.Pen.Assign(FDividingLines);
	Canvas.BackMode:=cbmTransparent;
	if Self.Vertical then
	   DoHorizLine( RectLegend.Left,RectLegend.Right,PosYLegend )
	else
	   DoVertLine( R.Left-TeeLegendOff4,RectLegend.Top,RectLegend.Bottom );
      end;
    end;
  end;

Var FrameWidth : Integer;

  Procedure DrawLegendBackground;
  var tmpRect      : TRect;
      tmpPosShadow : Integer;
  begin
    tmpRect:=RectLegend;
    With ParentChart.Canvas do
    begin
      AssignVisiblePen(Self.Frame);
      if Self.Frame.Visible then
      Begin
	if Pen.Color=clTeeColor then Pen.Color:=clBlack;
	InflateRect(tmpRect,FrameWidth,FrameWidth);
	tmpPosShadow:=FShadowSize+FrameWidth+( (FrameWidth-1) div 2);
      end
      else tmpPosShadow:=FShadowSize-1;
      Brush.Style:=bsSolid;
      if FShadowSize>0 then
      begin
	Brush.Color:=FShadowColor;
	with RectLegend do
	     FillRect(Rect( Left+FShadowSize,Top+FShadowSize,
			    Right+tmpPosShadow,Bottom+tmpPosShadow ));
      end;
      Brush.Assign(Self.FBrush);
      if Brush.Style=bsSolid then Brush.Color:=Self.Color
			     else
			     begin
			       BackColor:=Self.Color;
			       BackMode:=cbmOpaque;
			     end;
      if Self.FBrush.Bitmap<>nil then Brush.Bitmap:=Self.FBrush.Bitmap;
{      GradientFill(tmpRect,clSilver,clWhite,gdRightLeft);}
      DoRectangle(tmpRect);
    end;
  end;

var t              : Longint;
    NumLegendItems : Longint;
    HalfMaxWidth   : Longint;
    tmpLastValue   : Longint;

  Procedure CalcHorizontalColsRows;
  begin
    NumCols:=MaxLegendValues(2*TeeLegendOff4,tmpMaxWidth+IColorWidth+2*TeeLegendOff4);
    if NumCols>0 then
    begin
      NumRows:=tmpTotalItems div NumCols;
      if (tmpTotalItems mod NumCols)>0 then Inc(NumRows);
      NumRows:=MinLong(NumRows,MaxNumRows);
    end
    else NumRows:=0;
  end;

  Function CalcHorizMargin:Integer;
  begin
    if HorizMargin=0 then result:=MulDiv(TeeDefHorizMargin,ParentChart.ChartWidth,100)
		     else result:=HorizMargin;
  end;

  Function CalcVertMargin:Integer;
  begin
    if VertMargin=0 then result:=MulDiv(TeeDefVerticalMargin,ParentChart.ChartHeight,100)
		    else result:=VertMargin;
  end;

Var tmpRect : TRect;
    tmpW    : Integer;
Begin
  CalcLegendStyle;
  With ParentChart do
  Begin
    LegendColor:=FColor;
    FontCanvas(Self.Font);
    ItemHeight:=Canvas.FontHeight;

    tmpMaxWidth:=MaxLegendWidth(TeeAllValues)+TeeLegendOff4;
    IColorWidth:=Round(ColorWidth*tmpMaxWidth*0.01);

    tmpTotalItems:=TotalLegendItems;
    FrameWidth:=Self.Frame.Width;

    With FRectLegend do
    Case Self.Alignment of
    laBottom: begin
		Bottom:=ChartRect.Bottom-FShadowSize;
		if Self.Frame.Visible then Dec(Bottom,FrameWidth+1);
		CalcHorizontalColsRows;
		Top:=Bottom-TeeLegendOff2-ItemHeight*NumRows;
	      end;
    laTop:    begin
		Top:=ChartRect.Top;
		if Self.Frame.Visible then Inc(Top,FrameWidth+1);
		CalcHorizontalColsRows;
		Bottom:=Top+TeeLegendOff2+ItemHeight*NumRows;
	      end;
    else
      begin
	Top:=ChartBounds.Top+Round(1.0*FTopLeftPos*(ChartBounds.Bottom-ChartBounds.Top)*0.01);
	NumCols:=1;
	NumRows:=MaxLegendValues(Top,ItemHeight);
	Bottom:=Top+TeeLegendOff2+ItemHeight*NumRows;
      end;
    end;

    NumLegendItems:=0;

    With FRectLegend do
    Case Self.Alignment of
    laLeft:
    Begin
      tmpMaxWidth     :=MaxLegendWidth(NumRows);
      Left            :=ChartRect.Left;
      if Self.Frame.Visible then Inc(Left,FrameWidth+1);
      XLegendColor    :=Left+TeeLegendOff4;
      IColorWidth     :=Round(ColorWidth*((XLegendColor+TeeLegendOff4+tmpMaxWidth+TeeLegendOff2)-Left)*0.01);
      XLegendText     :=XLegendColor+IColorWidth+TeeLegendOff4;
      Right           :=XLegendText+tmpMaxWidth+TeeLegendOff2;
      NumLegendItems  :=NumRows;
    end;
    laRight:
    begin
      tmpMaxWidth     :=MaxLegendWidth(NumRows);
      Right           :=ChartRect.Right;
      if Self.Frame.Visible then Dec(Right,FrameWidth+1);
      Dec(Right,FShadowSize);
      XLegendText     :=Right-tmpMaxWidth-TeeLegendOff2;
      IColorWidth     :=Round(ColorWidth*(Right-(XLegendText-TeeLegendOff4-TeeLegendOff4))*0.01);
      XLegendColor    :=XLegendText-IColorWidth-TeeLegendOff4;
      Left            :=XLegendColor-TeeLegendOff4;
      NumLegendItems  :=NumRows;
    end;
    laTop,
    laBottom:
    begin
      HalfMaxWidth    :=2*TeeLegendOff4+ ((tmpMaxWidth+IColorWidth+TeeLegendOff4)*NumCols);
      HalfMaxWidth    :=MinLong(ChartWidth,HalfMaxWidth+2*FShadowSize);
      tmpW:=Round(1.0*FTopLeftPos*(ChartBounds.Right-ChartBounds.Left-HalfMaxWidth)*0.01);
      HalfMaxWidth    :=HalfMaxWidth div 2;
      Left            :=ChartXCenter-HalfMaxWidth+tmpW;
      Right           :=ChartXCenter+HalfMaxWidth+tmpW;
      XLegendColor    :=Left+TeeLegendOff4;
      XLegendText     :=XLegendColor+IColorWidth+TeeLegendOff4;
      NumLegendItems  :=NumCols*NumRows;
    end;
    end;

    if Assigned(FOnGetLegendRect) then
    begin
      tmpRect:=RectLegend;
      FOnGetLegendRect(ParentChart,tmpRect);
      RectLegend:=tmpRect;
    end;

    if NumLegendItems>0 then
    begin
      DrawLegendBackground;

      With Canvas.Pen do
      begin
	if Self.Color=clBlack then Color:=clWhite
			      else Color:=clBlack;
	Style:=psSolid;
	Width:=1;
      end;
      tmpSeries:=GetASeries;
      tmpLastValue:=FirstValue+MinLong(tmpTotalItems,NumLegendItems)-1;
      if Inverted then
	 for t:=tmpLastValue downto FirstValue do DrawLegendItem(t,(tmpLastValue-t))
      else
	 for t:=FirstValue to tmpLastValue do DrawLegendItem(t,(t-FirstValue));

      With ChartRect do
      begin
	if ResizeChart then
	Case Self.Alignment of
	  laLeft   : Left  :=RectLegend.Right;
	  laRight  : Right :=RectLegend.Left;
	  laTop    : Top   :=RectLegend.Bottom+FShadowSize;
	  laBottom : Bottom:=RectLegend.Top;
	end;
	Case Self.Alignment of
	  laLeft   : Inc(Left,CalcHorizMargin);
	  laRight  : Dec(Right,CalcHorizMargin);
	  laTop    : Inc(Top,CalcVertMargin);
	  laBottom : Dec(Bottom,CalcVertMargin);
	end;
      end;
      ReCalcWidthHeight;
    end;
  end;
end;

Function TCustomChartLegend.FormattedValue(ASeries:TChartSeries; ValueIndex:Longint):String;
Var i : Integer;
begin
  if ValueIndex<>TeeAllValues then
  begin
    result:=ASeries.LegendString(ValueIndex,TextStyle);
    { eliminate breaks in string... }
    Repeat
      i:=AnsiPos(TeeLineSeparator,Result);
      if i>0 then Result[i]:=' ';
    Until i=0;
  end
  else result:='';
end;

Function TCustomChartLegend.Clicked(x,y:Integer):Integer;

Var tmpH : Integer;

  Function ClickedRow:Integer;
  var t    : Integer;
      tmp  : Integer;
  begin
    result:=-1;
    for t:=0 to NumRows-1 do
    begin
      tmp:=RectLegend.Top+1+t*tmpH;
      if (y>=tmp) and (y<=(tmp+tmpH)) then
      begin
	result:=t;
	Break;
      end;
    end;
  end;

var t    : Integer;
    tmp2 : Integer;
    tmpW : Integer;
begin
  result:=-1;
  if PtInRect(RectLegend,Classes.Point(x,y)) then { inside legend }
  begin
    if Vertical then
    begin
      if NumRows>0 then
      begin
	tmpH:=(RectLegend.Bottom-RectLegend.Top) div NumRows;
	result:=ClickedRow;
      end;
    end
    else
    begin
      if NumCols>0 then
      begin
	tmpW:=(RectLegend.Right-RectLegend.Left) div NumCols;
	tmpH:=ParentChart.Canvas.FontHeight;
	for t:=0 to NumCols-1 do
	begin
	  tmp2:=RectLegend.Left+1+t*tmpW;
	  if (x>=tmp2) and (x<=(tmp2+tmpW)) then
	  begin
	    result:=ClickedRow;
	    if result<>-1 then result:=t+NumCols*result;
	  end;
	end;
      end;
    end;
  end;
end;

Function GetNewSeriesName(AOwner:TComponent):String;
begin
  result:=TeeGetUniqueName(AOwner,TeeMsg_DefaultSeriesName);
end;

Function GetNewFunctionName(AOwner:TComponent):String;
Var tmp   : Longint;
    tmpSt : String;
begin
  tmp:=1;
  tmpSt:=TeeMsg_DefaultFunctionName+IntToStr(tmp);
  while (AOwner.FindComponent(tmpSt)<>nil) do
  begin
    Inc(tmp);
    tmpSt:=TeeMsg_DefaultFunctionName+IntToStr(tmp);
  end;
  result:=tmpSt;
end;

Procedure AssignSeries(Var OldSeries,NewSeries:TChartSeries);
Var OldName   : String;
    newPos    : Longint;
    tmpPos    : Longint;
    tmpSeries : TChartSeries;
begin
  NewSeries.Assign(OldSeries);
  With NewSeries do
  begin    { events }
    AfterDrawValues  :=OldSeries.AfterDrawValues;
    BeforeDrawValues :=OldSeries.BeforeDrawValues;
    OnAfterAdd       :=OldSeries.OnAfterAdd;
    OnBeforeAdd      :=OldSeries.OnBeforeAdd;
    OnClearValues    :=OldSeries.OnClearValues;
    OnClick          :=OldSeries.OnClick;
    OnDblClick       :=OldSeries.OnDblClick;
    OnGetMarkText    :=OldSeries.OnGetMarkText;
  end;
  OldName:=OldSeries.Name;

  While OldSeries.LinkedSeries.Count>0 do
  begin
    tmpSeries:=TChartSeries(OldSeries.LinkedSeries[0]);
    { after removing... }
    With tmpSeries.DataSources do
    begin
      if IndexOf(NewSeries)=-1 then Add(NewSeries);
      if IndexOf(OldSeries)<>-1 then Remove(OldSeries);
    end;
    NewSeries.AddLinkedSeries(tmpSeries);
    OldSeries.RemoveLinkedSeries(tmpSeries);
  end;

  { Swap Series on Chart list }
  With OldSeries.ParentChart.SeriesList do
  begin
    tmpPos:=IndexOf(OldSeries);
    newPos:=IndexOf(NewSeries);
    Exchange(tmpPos,newPos);
  end;

  OldSeries.Free;
  OldSeries:=nil;
  With NewSeries do
  begin
    Name:=OldName;
    if ( DataSource=nil ) and ( csDesigning in ComponentState ) then
	FillSampleValues(NumSampleValues);
    RefreshSeries;
  end;
end;

Function CreateNewTeeFunction(ASeries:TChartSeries; AClass:TTeeFunctionClass):TTeeFunction;
begin
  result:=AClass.Create(ASeries.Owner);
  result.ParentSeries:=ASeries;
  result.Name:=GetNewFunctionName(ASeries.Owner);
end;

Function CreateNewSeries( AOwner:TComponent;
			  AChart:TCustomAxisPanel;
			  AClass:TChartSeriesClass;
			  AFunctionClass:TTeeFunctionClass):TChartSeries;
begin
  if AOwner=nil then AOwner:=AChart;  { 4.0 }
  result:=AClass.Create(AOwner);
  result.ParentChart:=AChart;
  result.Name:=GetNewSeriesName(AOwner);
  if AFunctionClass<>nil then CreateNewTeeFunction(result,AFunctionClass);
end;

Function CloneChartSeries(ASeries:TChartSeries):TChartSeries;
Var tmp : TTeeFunctionClass;
begin
  With ASeries do
  begin
    if FunctionType=nil then tmp:=nil
			else tmp:=TTeeFunctionClass(FunctionType.ClassType);
    result:=CreateNewSeries(Owner,ParentChart,TChartSeriesClass(ClassType),tmp);
  end;
  result.Assign(ASeries);
  { add values from ASeries --> result }
  { if DataSource is not nil, values are already added in Assign }
  if result.DataSource=nil then result.AssignValues(ASeries);
end;

procedure ChangeSeriesType(Var ASeries:TChartSeries; NewType:TChartSeriesClass);
var NewSeries : TChartSeries;
begin
  if ASeries.ClassType<>NewType then { only if different classes }
  begin
    NewSeries:=CreateNewSeries(ASeries.Owner,ASeries.ParentChart,NewType,nil);
    if Assigned(newSeries) then
    begin
      AssignSeries(ASeries,newSeries);
      ASeries:=NewSeries;  { <-- change parameter }
    end;
  end;
end;

procedure ChangeAllSeriesType( AChart:TCustomChart; AClass:TChartSeriesClass );
Var t         : Integer;
    tmpSeries : TChartSeries;
begin
  for t:=0 to AChart.SeriesCount-1 do
  begin
    tmpSeries:=AChart[t];
    ChangeSeriesType(tmpSeries,AClass);
  end;
end;

Function TTeeSeriesTypes.GetSeriesType(Index:Integer):TTeeSeriesType;
begin
  result:=TTeeSeriesType(Items[Index]);
end;

Procedure RegisterTeeSeriesFunction( ASeriesClass:TChartSeriesClass;
				     AFunctionClass:TTeeFunctionClass;
				     Const ADescription,AGalleryPage:String;
				     ANumGallerySeries : Integer );
var t             : Longint;
    NewSeriesType : TTeeSeriesType;
begin
  if Assigned(ASeriesClass) then Classes.RegisterClass(ASeriesClass);
  if Assigned(AFunctionClass) then Classes.RegisterClass(AFunctionClass);
  if not Assigned(TeeSeriesTypes) then TeeSeriesTypes:=TTeeSeriesTypes.Create;
  With TeeSeriesTypes do
  for t:=0 to Count-1 do
  With SeriesType[t] do
    if (SeriesClass=ASeriesClass) and
       (FunctionClass=AFunctionClass) then Exit;
  NewSeriesType:=TTeeSeriesType.Create;
  with NewSeriesType do
  begin
    SeriesClass   := ASeriesClass;
    FunctionClass := AFunctionClass;
    Description   := ADescription;
    GalleryPage   := AGalleryPage;
    NumGallerySeries := ANumGallerySeries;
  end;
  TeeSeriesTypes.Add(NewSeriesType);
end;

{ Adds a new Series component definition for the Gallery }
Procedure RegisterTeeSeries( ASeriesClass:TChartSeriesClass;
			     Const ADescription,AGalleryPage:String;
			     ANumGallerySeries : Integer );
begin
  RegisterTeeSeriesFunction( ASeriesClass,
			     nil,
			     ADescription,
			     AGalleryPage,
			     ANumGallerySeries );
end;

Procedure RegisterTeeFunction( AFunctionClass:TTeeFunctionClass;
			       Const ADescription,AGalleryPage:String;
			       ANumGallerySeries : Integer );
begin
  RegisterTeeSeriesFunction( nil,
			     AFunctionClass,
			     ADescription,
			     AGalleryPage,
			     ANumGallerySeries);
end;

Procedure RegisterTeeBasicFunction( AFunctionClass:TTeeFunctionClass;
				    Const ADescription:String );
begin
  RegisterTeeFunction( AFunctionClass,
		       ADescription,
		       TeeMsg_GalleryFunctions,2);
end;

Function GetTeeFunctionDescription( AFunctionClass:TTeeFunctionClass):String;
var t : Longint;
begin
  result:='';
  With TeeSeriesTypes do
  for t:=0 to Count-1 do
  With SeriesType[t] do
  if FunctionClass=AFunctionClass then
  begin
    result:=Description;
    Exit;
  end;
end;

Procedure InternalUnRegister(IsSeries:Boolean; AClass:TComponentClass);
var tmp : TTeeSeriesType;
    t   : Longint;
begin
  if Assigned(TeeSeriesTypes) then
  begin
    t:=0;
    While t<TeeSeriesTypes.Count do
    begin
      tmp:=TeeSeriesTypes[t];
      if (IsSeries and (tmp.SeriesClass=AClass)) or
       ((not IsSeries) and (tmp.FunctionClass=AClass)) then
      begin
	tmp.Free;
	TeeSeriesTypes.Delete(t);
      end
      else Inc(t);
    end;
  end;
end;

Procedure UnRegisterTeeSeries(Const ASeriesList:Array of TChartSeriesClass);
var t : Integer;
begin
  for t:=Low(ASeriesList) to High(ASeriesList) do
      InternalUnRegister(True,ASeriesList[t]);
end;

Procedure UnRegisterTeeFunctions(Const AFunctionList:Array of TTeeFunctionClass);
var t : Integer;
begin
  for t:=Low(AFunctionList) to High(AFunctionList) do
      InternalUnRegister(False,AFunctionList[t]);
end;

Procedure TeeGalleryExitProc; far;
var t : Integer;
begin
  With TeeSeriesTypes do
  for t:=0 to Count-1 do SeriesType[t].Free;
  TeeSeriesTypes.Free;
  TeeSeriesTypes:=nil;
end;

{$IFNDEF D1}

{ TTeeDragObject }
constructor TTeeDragObject.Create(Const APart: TChartClickedPart);
begin
  FPart:=APart;
end;

{function TTeeDragObject.GetDragImages: TCustomImageList;
begin
  Result := FPart.GetDragImages;
end;}

{procedure TTeeDragObject.HideDragImage;
begin
  if FPart.GetDragImages <> nil then
    FPart.GetDragImages.HideDragImage;
end;

procedure TTeeDragObject.ShowDragImage;
begin
  if FPart.GetDragImages <> nil then
    FPart.GetDragImages.ShowDragImage;
end;
}
const crTeeDrag=2021;
      TeeMsg_TeeDrag = 'crTeeDrag'; { string cursor name (dont translate) }

function TTeeDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := crTeeHand
  else
    Result := crNoDrop;
end;

procedure TTeeDragObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  if not Accepted then
  begin
{    FPart.DragCanceled;}
{    Target := nil;}
  end;
{  if Assigned(FPart. FOnEndDrag) then FOnEndDrag(Self, Target, X, Y);}
{  FPart.DoEndDrag(Target, X, Y);}
end;
{$ENDIF}

initialization
  TeeSeriesTypes      :=nil;     { List of available Series/Function types }
  AnimatedZoomFactor  :=3.0;     { controls the animated zoom "speed" }
  TeeZoomMouseButton  :=mbLeft;  { button used to zoom }
  TeeScrollMouseButton:=mbRight; { button used to scroll }
  TeeZoomKeyShift     :=[];      { keys that should be pressed to start zoom }
  TeeScrollKeyShift   :=[];      { keys that should be pressed to start scrolltmo }
  RegisterClasses([ TChartLegend,TChartGradient,TChartWall,TChartTitle ]);
  RegisterTeeBasicFunction( TAddTeeFunction,      TeeMsg_FunctionAdd );
  RegisterTeeBasicFunction( TSubtractTeeFunction, TeeMsg_FunctionSubtract );
  RegisterTeeBasicFunction( TMultiplyTeeFunction, TeeMsg_FunctionMultiply );
  RegisterTeeBasicFunction( TDivideTeeFunction,   TeeMsg_FunctionDivide );
  RegisterTeeBasicFunction( THighTeeFunction,     TeeMsg_FunctionHigh );
  RegisterTeeBasicFunction( TLowTeeFunction,      TeeMsg_FunctionLow );
  RegisterTeeBasicFunction( TAverageTeeFunction,  TeeMsg_FunctionAverage );
{$IFDEF D1}
  AddExitProc(TeeGalleryExitProc);
{$ELSE}
finalization
  TeeGalleryExitProc;
{$ENDIF}
end.

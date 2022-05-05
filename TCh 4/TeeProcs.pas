{****************************************}
{   TeeChart and TeeTree VCL Library     }
{ Copyright (c) 1995-98 by David Berneda }
{        All Rights Reserved             }
{****************************************}
{$R-,S-,Q-,I-,A+,U-,X+,B-,W-,P-,V-}
{$I teedefs.inc}
{$IFNDEF D1}
{$H+}
{$ENDIF}
unit TeeProcs;

interface

Uses Classes,Messages,
     {$IFDEF D1}
     WinProcs,WinTypes,Menus,
     {$ELSE}
     Windows,
     {$ENDIF}
     TeCanvas,Printers,Clipbrd,ExtCtrls,Graphics,Controls,Forms,SysUtils, DesignIntf;

Const  MaxDefaultColors=16;
       ColorPalette:Array[1..MaxDefaultColors] of TColor=
        ( clRed,
          clGreen,
          clYellow,
          clBlue,
          clWhite,
          clGray,
          clFuchsia,
          clTeal,
          clNavy,
          clMaroon,
          clLime,
          clOlive,
          clPurple,
          clSilver,
          clAqua,
          clBlack);

   TeeDefVerticalMargin = 4;
   TeeDefHorizMargin    = 3;

   TeeMsg_ExportChartName     = 'Chart.';  { <-- dont translate }

   crTeeHand              = 2020;      { Hand cursor }
   TeeMsg_TeeHand         = 'crTeeHand'; { string cursor name (dont translate) }

   TeeLineSeparator:{$IFNDEF D1}AnsiChar{$ELSE}Char{$ENDIF} = #13;

   TeeDefault_PrintMargin = 15;

   { Printing }
   TeeNormalPrintDetail   =    0;
   TeeHighPrintDetail     = -100;

   TeeDef3DPercent        = 15;

Var
  TeeClipWhenPrinting   : Boolean;
  TeeClipWhenMetafiling : Boolean;
  TeeEraseBack          : Boolean;
  { Should Panel background to be printed ? Default: False }
  PrintTeePanel         : Boolean;

type
  ChartException=class(Exception);

  TDateTimeStep=(  dtOneSecond,
                   dtFiveSeconds,
                   dtTenSeconds,
                   dtFifteenSeconds,
                   dtThirtySeconds,
                   dtOneMinute,
                   dtFiveMinutes,
                   dtTenMinutes,
                   dtFifteenMinutes,
                   dtThirtyMinutes,
                   dtOneHour,
                   dtTwoHours,
                   dtSixHours,
                   dtTwelveHours,
                   dtOneDay,
                   dtTwoDays,
                   dtThreeDays,
                   dtOneWeek,
                   dtHalfMonth,
                   dtOneMonth,
                   dtTwoMonths,
                   dtThreeMonths,
                   dtFourMonths,
                   dtSixMonths,
                   dtOneYear,
                   dtNone );

Const
    DateTimeStep:Array[TDateTimeStep] of Double=
      ( 1.0/86400.0,  5.0/86400.0,  10.0/86400.0,
        0.25/1440.0,  0.5/1440.0,    1.0/1440.0,
        5.0/1440.0,  10.0/1440.0,    0.25/24.0,
        0.5/24.0 ,    1.0/24.0 ,  2.0/24.0 ,  6.0/24.0 ,
        12.0/24.0,    1, 2, 3, 7, 15, 30, 60, 90, 120, 182, 365, {none:} 1
      );

type
  TZoomPanningRecord=class
  public
    Active      : Boolean;
    X0,Y0,X1,Y1 : Longint;
    Procedure Check;
    Procedure Activate(x,y:Longint);
  end;

  TTeeBackImageMode=(pbmStretch,pbmTile,pbmCenter);

  TCustomTeePanel=class(TCustomPanel)
  private
    F3DPercent        : Integer;
    FApplyZOrder      : Boolean;
    FChartBounds      : TRect;
    FChartWidth       : Integer;
    FChartHeight      : Integer;
    FChartXCenter     : Integer;
    FChartYCenter     : Integer;
    FDelphiCanvas     : TCanvas;
    FHeight3D         : Integer;
    FMargins          : TRect;
    FOriginalCursor   : TCursor;
    FPanning          : TZoomPanningRecord;
    FPrinting         : Boolean;
    FPrintProportional: Boolean;
    FPrintResolution  : Integer;
    FView3D           : Boolean;
    FView3DOptions    : TView3DOptions;
    FWidth3D          : Integer;
    FZoom             : TZoomPanningRecord;
    Function GetMargin(Index:Integer):Integer;
    Function GetBufferedDisplay:Boolean;
    Procedure SetBufferedDisplay(Value:Boolean);
    procedure SetView3D(Value:Boolean);
    procedure SetView3DOptions(Value:TView3DOptions);
    Function GetMonochrome:Boolean;
    Procedure SetMonochrome(Value:Boolean);
    Procedure Set3DPercent(Value:Integer);
  protected
    InternalCanvas    : TCanvas3D;
    Function MultiLineTextWidth(S:String; Var NumLines:Integer):Integer;
    Procedure SetMargin(Index,Value:Integer);
    Procedure Loaded; override;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    Procedure InternalDraw(Const UserRectangle:TRect); virtual; abstract;
    procedure Paint; override;
  public
    AutoRepaint  : Boolean;  { when False, the Chart does not refresh }
    CancelMouse  : Boolean;  { when True, the Chart does not finish mouse events }

    { not possible to make a property, because D1 }
    ChartRect    : TRect; { the rectangle bounded by axes in pixels }
    PrintMargins : TRect; { the percent of paper printer margins }

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Procedure CalcMetaBounds( Var R:TRect; Const AChartRect:TRect;
                              Var WinWidth,WinHeight,ViewWidth,ViewHeight:Longint);
    Function CalcProportionalMargins:TRect;
    Function CanClip:Boolean;
    Procedure CanvasChanged(Sender:TObject); virtual;
    Function ChartPrintRect:TRect;
    Procedure CheckPenWidth(APen:TPen);
    Procedure CopyToClipboardBitmap;
    Procedure CopyToClipboardMetafile(Enhanced:Boolean);
    Function TeeCreateBitmap(ABackColor:TColor; Const Rect:TRect):TBitmap;
    Procedure Draw(UserCanvas:TCanvas; Const UserRect:TRect); virtual;
    Procedure DrawPanelBevels(Rect:TRect); virtual;
    Procedure DrawToMetaCanvas(ACanvas:TCanvas; Const Rect:TRect);
    Procedure DrawZoomRectangle;

    Procedure FontCanvas(SourceFont:TFont); virtual;
    Function GetCursorPos:TPoint;
    Function GetRectangle:TRect; virtual;
    procedure Invalidate; override;
    Function IsScreenHighColor:Boolean;

    Procedure Print;
    Procedure PrintLandscape;
    Procedure PrintOrientation(AOrientation:TPrinterOrientation);
    Procedure PrintPartial(Const PrinterRect:TRect);
    Procedure PrintPartialCanvas( PrintCanvas:TCanvas;
                                  Const PrinterRect:TRect);
    Procedure PrintPortrait;
    Procedure PrintRect(Const R:TRect);
    Procedure SaveToBitmapFile(Const FileName:String);
    Procedure SaveToMetafile(Const FileName:String);
    Procedure SaveToMetafileEnh(Const FileName:String);
    Procedure SaveToMetafileRect( Enhanced:Boolean; Const FileName:String;
                                  Const Rect:TRect );
    Procedure SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
    Procedure SetBrushCanvas( AColor:TColor; APattern:TBrushStyle;
                              ABackColor:TColor);
    Procedure SetColorProperty(Var Variable:TColor; Value:TColor);
    Procedure SetDoubleProperty(Var Variable:Double; Const Value:Double);
    Procedure SetLongintProperty(Var Variable:Longint; Value:Longint);
    Procedure SetIntegerProperty(Var Variable:Integer; Value:Integer);
    Procedure SetInternalCanvas(NewCanvas:TCanvas3D);
    Procedure SetStringProperty(Var Variable:String; Const Value:String);
    Procedure ReCalcWidthHeight;
    Function  TeeCreateMetafile(Enhanced:Boolean; Const Rect:TRect):TMetafile;

    { public properties }
    property ApplyZOrder:Boolean read FApplyZOrder write FApplyZOrder;
    property BufferedDisplay:Boolean read GetBufferedDisplay
                                     write SetBufferedDisplay;
    property Canvas:TCanvas3D read InternalCanvas write SetInternalCanvas;
    property ChartBounds:TRect read FChartBounds;
    property ChartHeight:Integer read FChartHeight;
    property ChartWidth:Integer read FChartWidth;
    property ChartXCenter:Integer read FChartXCenter;
    property ChartYCenter:Integer read FChartYCenter;
    property DelphiCanvas:TCanvas read FDelphiCanvas;
    property Height3D:Integer read FHeight3D write FHeight3D;
    property IPanning:TZoomPanningRecord read FPanning;
    property IZoom:TZoomPanningRecord read FZoom;
    property OriginalCursor:TCursor read FOriginalCursor write FOriginalCursor;
    property Printing:Boolean read FPrinting write FPrinting;
    property Width3D:Integer read FWidth3D write FWidth3D;

    property PrintResolution:Integer read FPrintResolution
                                     write FPrintResolution default TeeNormalPrintDetail;
    { to be published properties }
    property Chart3DPercent:Integer read F3DPercent write Set3DPercent
                                    default TeeDef3DPercent;
    property MarginLeft:Integer   index 0 read GetMargin write SetMargin default TeeDefHorizMargin;
    property MarginTop:Integer    index 1 read GetMargin write SetMargin default TeeDefVerticalMargin;
    property MarginRight:Integer  index 2 read GetMargin write SetMargin default TeeDefHorizMargin;
    property MarginBottom:Integer index 3 read GetMargin write SetMargin default TeeDefVerticalMargin;
    property Monochrome:Boolean read GetMonochrome write SetMonochrome default False;
    property PrintProportional:Boolean read FPrintProportional
                                       write FPrintProportional default True;
    property View3D:Boolean read FView3D write SetView3D default True;
    property View3DOptions:TView3DOptions read FView3DOptions write SetView3DOptions;

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
    
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  published
  end;

  TPanningMode=(pmNone,pmHorizontal,pmVertical,pmBoth);

  TCustomTeeGradient=class(TPersistent)
  private
    FDirection : TGradientDirection;
    FEndColor  : TColor;
    FOwner     : TCustomTeePanel;
    FStartColor: TColor;
    FVisible   : Boolean;
    Procedure SetDirection(Value:TGradientDirection);
    Procedure SetEndColor(Value:TColor);
    Procedure SetStartColor(Value:TColor);
    Procedure SetVisible(Value:Boolean);
  protected
  public
    Constructor Create(AOwner:TCustomTeePanel);
    Procedure Assign(Source:TPersistent); override;
    property Owner:TCustomTeePanel read FOwner write FOwner; { write for Tree }
    { to published }
    property Direction:TGradientDirection read FDirection write SetDirection default gdTopBottom;
    property EndColor:TColor read FEndColor write SetEndColor default clYellow;
    property StartColor:TColor read FStartColor write SetStartColor default clWhite;
    property Visible:Boolean read FVisible write SetVisible default False;
  end;

  TChartGradient=class(TCustomTeeGradient)
  published
    property Direction;
    property EndColor;
    property StartColor;
    property Visible;
  end;

  TCustomTeePanelExtended=class(TCustomTeePanel)
  private
    FAnimatedZoomSteps : Integer;
    FAnimatedZoom      : Boolean;
    FAllowZoom         : Boolean;
    FAllowPanning      : TPanningMode;
    FBackImage         : TPicture;
    FBackImageMode     : TTeeBackImageMode;
    FGradient          : TChartGradient;
    FZoomed            : Boolean;

    Procedure SetAnimatedZoom(Value:Boolean);
    Procedure SetAnimatedZoomSteps(Value:Integer);
    procedure SetBackImage(Value:TPicture);
    procedure SetBackImageMode(Value:TTeeBackImageMode);
    Procedure SetGradient(Value:TChartGradient);
  protected
    FOnAfterDraw       : TNotifyEvent;
    FOnScroll          : TNotifyEvent;
    FOnUndoZoom        : TNotifyEvent;
    FOnZoom            : TNotifyEvent;
    function GetPalette: HPALETTE; override;    { override the method }
    procedure DrawBitmap(Rect:TRect; Z:Integer);
    Procedure DrawImage(Const R:TRect); virtual;
    procedure FillPanelRect(Const Rect:TRect); virtual;
    procedure PanelPaint(Const UserRect:TRect); virtual;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    procedure UndoZoom; virtual;
    property Zoomed:Boolean read FZoomed write FZoomed;

    property AllowPanning:TPanningMode read FAllowPanning
				       write FAllowPanning default pmBoth;
    property AllowZoom:Boolean read FAllowZoom write FAllowZoom default True;
    property AnimatedZoom:Boolean read FAnimatedZoom
				  write SetAnimatedZoom default False;
    property AnimatedZoomSteps:Integer read FAnimatedZoomSteps
				       write SetAnimatedZoomSteps default 8;
    property BackImage:TPicture read FBackImage write SetBackImage;
    property BackImageMode:TTeeBackImageMode read FBackImageMode
					     write SetBackImageMode
					     default pbmStretch;
    property Gradient:TChartGradient read FGradient write SetGradient;
    { events }
    property OnAfterDraw:TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnScroll:TNotifyEvent read FOnScroll write FOnScroll;
    property OnUndoZoom:TNotifyEvent read FOnUndoZoom write FOnUndoZoom;
    property OnZoom:TNotifyEvent read FOnZoom write FOnZoom;
  end;

  TDraw3DPaintEvent=procedure(Sender:TObject; Const Rect:TRect) of object;

  TDraw3D=class(TCustomTeePanelExtended)
  private
    FOnPaint:TDraw3DPaintEvent;
  protected
    Procedure InternalDraw(Const UserRectangle:TRect); override;
  published
    { TCustomTeePanelExtended properties }
    property BackImage;
    property BackImageMode;
    property Gradient;
    property OnAfterDraw;

    { TCustomTeePanel properties }
    property BufferedDisplay;
    property MarginLeft;
    property MarginTop;
    property MarginRight;
    property MarginBottom;
    property Monochrome;
    property PrintProportional;
    property PrintResolution;
    property View3D;
    property View3DOptions;
    property OnPaint:TDraw3DPaintEvent read FOnPaint write FOnPaint;

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
  end;

  TChartFontObject=class(TPersistent)
  private
    FParentChart : TCustomTeePanel;
  protected
    FFont : TFont;
    Procedure SetFont(Value:TFont);
    Function IsFontStored:Boolean;
  public
    Constructor Create(AOwner: TCustomTeePanel);
    Destructor Destroy; override;
    Procedure Assign(Source:TPersistent); override;
    Procedure Repaint;
  { to be published }
    property Font:TFont read FFont write SetFont stored IsFontStored;
    property ParentChart:TCustomTeePanel read FParentChart;
  end;

{ returns one of the sample colors in ColorPalette constant array }
Function GetDefaultColor(t:Longint):TColor;

Function GetScreenLogPixels:Integer;
{$IFDEF D3}
Procedure CheckJapaneseFontSize(Var ASize:Integer);
{$ENDIF}
Function IsDefaultFont(AFont:TFont):Boolean;
Function CreateDefaultFont(ChangedEvent:TNotifyEvent):TFont;
Function GetDefaultFontSize:Integer;
Function GetDefaultFontName:String;

Function DateTimeDefaultFormat(Const AStep:Double):String;

Function DaysInMonth(Year,Month:Word):Word;

Function FindDateTimeStep(Const StepValue:Double):TDateTimeStep;
Function NextDateTimeStep(Const AStep:Double):Double;

{ returns the associated filename extension for images (BMP, WMF, etc) }
Function TeeGetImageExtension(Index:Longint):String;

{ finds ACursor and returns its name, including crTeeHand cursor }
Function TeeCursorToIdent(ACursor:Longint; Var AName:String):Boolean;
{ finds AName and returns its Cursor, including TeeHand cursor }
Function TeeIdentToCursor(Const AName:String; Var ACursor:Longint):Boolean;

{ Returns True if point T is over line:  P --> Q }
Function PointInLine(Const P:TPoint; px,py,qx,qy:Integer):Boolean;

{ Returns True if point T is over (more or less "Tolerance" pixels)
  line:  P --> Q }
Function PointInLineTolerance(Const P:TPoint; px,py,qx,qy,TolerancePixels:Integer):Boolean;

{ Returns True if point P is inside Poly polygon }
Function PointInPolygon(Const P:TPoint; Const Poly:Array of TPoint):Boolean;

{ Returns True if point P is inside the vert triangle of x0y0, midxY1, x1y0 }
Function PointInTriangle(Const P:TPoint; X0,X1,Y0,Y1:Integer):Boolean;

{ Returns True if point P is inside the horiz triangle of x0y0, x1midY, x0y0 }
Function PointInHorizTriangle(Const P:TPoint; Y0,Y1,X0,X1:Integer):Boolean;

{ Returns True if point P is inside the ellipse bounded by Rect }
Function PointInEllipse(Const P:TPoint; Const Rect:TRect):Boolean;

{ Replaces all ocurrences of "Separator" in string "St" with
  #13 (TeeLineSeparator constant)  }
Procedure TeeSplitInLines(Var St:String; Const Separator:String);

{ Returns 1 + how many times "TeeLineSeparator #13" is found
  inside string St parameter }
Function TeeNumTextLines(St:String):Integer;

{$IFNDEF D3}
Function AnsiPos(Const SubStr,St:String):Integer;
{$ENDIF}

Function DelphiToLocalFormat(Const Format:String):String;
Function LocalToDelphiFormat(Const Format:String):String;
Procedure EnableControls(Enable:Boolean; Const ControlArray:Array of TControl);

{ raises an exception if Value <0 or >360 }
Procedure TeeCheckAngle(Value:Integer; Const Description:String);

{ Round "ADate" to the nearest "AStep" value }
Function TeeRoundDate(Const ADate:TDateTime; AStep:TDateTimeStep):TDateTime;

{ Increment or Decrement "Value", for DateTime and not-DateTime }
Procedure TeeDateTimeIncrement( IsDateTime:Boolean;
                                Increment:Boolean;
                                Var Value:Double;
                                Const AnIncrement:Double;
                                tmpWhichDateTime:TDateTimeStep);

{ Load the ABitmap resource for AClass.ClassName }
Procedure TeeGetClassNameBitmap(AClass:TComponent; ABitmap:TBitmap);

{ Returns a "good" value, bigger than "OldStep", as 2..5..10..etc }
Function TeeNextStep(Const OldStep:Double):Double;

{ returns number of sections in St string separated by ";" }
Function TeeNumFields(St:String):Integer;

{ returns the index-th section in St string separated by ";" }
Function TeeExtractField(St:String; Index:Integer):String;

{ Returns a valid component name, for Delphi 1, 2, 3 & 4 and C++ 1 and 3 }
Function TeeGetUniqueName(AOwner:TComponent; Const AStartName:String):String;

implementation

Uses DsIntf,TeeConst;

{.$DEFINE MONITOR_REDRAWS}

{$IFDEF MONITOR_REDRAWS}
Const RedrawCount:Integer=0;
{$ENDIF}

{$IFDEF D1}
{$R TEERESOU.R16}
{$ELSE}
{$R TEERESOU.RES}
{$ENDIF}

Function DaysInMonth(Year,Month:Word):Word;
Const DaysMonths:Array[1..12] of Byte=(31,28,31,30,31,30,31,31,30,31,30,31);
Begin
  result:=DaysMonths[Month];
  if (Month=2) and
  {$IFDEF D5}
     IsLeapYear(Year)
  {$ELSE}
     (Year mod 4=0) and
     ( (Year mod 100<>0) or (Year mod 400=0) )
  {$ENDIF}
     then Inc(result);
End;

Function DateTimeDefaultFormat(Const AStep:Double):String;
Begin
  if AStep<=1 then result:=ShortTimeFormat
              else result:=ShortDateFormat;
end;

Function NextDateTimeStep(Const AStep:Double):Double;
var t:TDateTimeStep;
Begin
  for t:=Pred(dtOneYear) downto dtOneSecond do
  if AStep>=DateTimeStep[t] then
  Begin
    result:=DateTimeStep[Succ(t)];
    exit;
  end;
  result:=DateTimeStep[dtOneYear];
end;

Function FindDateTimeStep(Const StepValue:Double):TDateTimeStep;
var t:TDateTimeStep;
begin
  for t:=pred(High(DateTimeStep)) downto low(DateTimeStep) do
  begin
    if Abs(DateTimeStep[t]-StepValue)<DateTimeStep[dtOneSecond] then
    begin
      result:=t;
      exit;
    end;
  end;
  result:=dtNone;
end;

Function MaxDouble(Const a,b:Double):Double;
begin
  if a>b then result:=a else result:=b;
end;

Function MinDouble(Const a,b:Double):Double;
begin
  if a<b then result:=a else result:=b;
end;

Function MaxLong(a,b:Longint):Longint;
begin
  if a>b then result:=a else result:=b;
end;

Function MinLong(a,b:Longint):Longint;
begin
  if a>b then result:=b else result:=a;
end;

Procedure SwapLongint(Var a,b:Longint);
var tmp:Longint;
Begin
  Tmp:=a;  a:=b;  b:=Tmp;
End;

Procedure SwapDouble(Var a,b:Double);
var tmp:Double;
begin
  tmp:=a;  a:=b;  b:=tmp;
end;

Procedure SwapInteger(Var a,b:Integer);
var tmp:Integer;
Begin
  tmp:=a; a:=b; b:=tmp;
end;

Function GetScreenLogPixels:Integer;
var DC:HDC;
Begin
  DC:=GetDC(0);
  Result:=GetDeviceCaps(DC,LOGPIXELSY);
  ReleaseDC(0,DC);
end;

{$IFDEF D3}
Procedure CheckJapaneseFontSize(Var ASize:Integer);
begin
  if (ASize=8) and (SysLocale.PriLangID=LANG_JAPANESE) then
      ASize:=-MulDiv(DefFontData.Height,72,GetScreenLogPixels);
end;
{$ENDIF}

Function GetDefaultFontSize:Integer;
Begin
  result:=StrToInt(TeeMsg_DefaultFontSize);
{$IFDEF D3}
  CheckJapaneseFontSize(result);
{$ENDIF}
end;

Function GetDefaultFontName:String;
Begin
  result:=TeeMsg_DefaultFontName;
{$IFDEF D3}
  if (result='Arial') and (SysLocale.PriLangID=LANG_JAPANESE) then  { <-- do not translate }
      result:=DefFontData.Name;
{$ENDIF}
end;

Function IsDefaultFont(AFont:TFont):Boolean;
Begin
  With AFont do
    result:= (Size=GetDefaultFontSize) and (Name=GetDefaultFontName) and
             (Color=clBlack) and (Style=[]) {$IFDEF D3}and (CharSet=DEFAULT_CHARSET){$ENDIF};
end;

Function CreateDefaultFont(ChangedEvent:TNotifyEvent):TFont;
Begin
  result:=TFont.Create;
  With result do
  begin
    Name:=GetDefaultFontName;
    Size:=GetDefaultFontSize;
    Color:=clBlack;
    Style:=[];
    OnChange:=ChangedEvent;
  end;
end;

Function GetDefaultColor(t:Longint):TColor;
Begin
  result:=ColorPalette[1+(t mod MaxDefaultColors)];
end;

Procedure TeeCheckAngle(Value:Integer; Const Description:String);
Begin
  if (Value<0) or (Value>360) then
     Raise ChartException.CreateFmt(TeeMsg_Angle,[Description]);
end;

{ TCustomTeePanel }
Constructor TCustomTeePanel.Create(AOwner: TComponent);
begin
  InternalCanvas:=nil;
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  Width := 400;
  Height:= 250;
  F3DPercent   :=TeeDef3DPercent;
  AutoRepaint :=False;
  FApplyZOrder :=True;
  FDelphiCanvas:=inherited Canvas;
  FView3D      :=True;
  FView3DOptions:=TView3DOptions.Create(Self);
  InternalCanvas:=TTeeCanvas3D.Create;
  InternalCanvas.ReferenceCanvas:=FDelphiCanvas;
  FMargins:=Rect(TeeDefHorizMargin,TeeDefVerticalMargin,TeeDefHorizMargin,TeeDefVerticalMargin);
  FPrinting:=False;
  FPrintProportional:=True;
  FPrintResolution:=TeeNormalPrintDetail;
  PrintMargins:=Rect( TeeDefault_PrintMargin,TeeDefault_PrintMargin,
                      TeeDefault_PrintMargin,TeeDefault_PrintMargin);
  FOriginalCursor:=Cursor;
  FZoom   :=TZoomPanningRecord.Create;
  FPanning:=TZoomPanningRecord.Create;
  if TeeEraseBack then TeeEraseBack:=not (csDesigning in ComponentState);
  AutoRepaint:=True;
end;

Destructor TCustomTeePanel.Destroy;
Begin
  InternalCanvas.Free;
  InternalCanvas:=nil;
  FView3DOptions.Free;
  FZoom.Free;
  FPanning.Free;
  inherited Destroy;
end;

Procedure TCustomTeePanel.CanvasChanged(Sender:TObject);
Begin
  Invalidate;
end;

procedure TCustomTeePanel.SetView3DOptions(Value:TView3DOptions);
begin
  FView3DOptions.Assign(Value);
end;

procedure TCustomTeePanel.SetView3D(Value:Boolean);
Begin
  SetBooleanProperty(FView3D,Value);
end;

Procedure TCustomTeePanel.Draw(UserCanvas:TCanvas; Const UserRect:TRect);
Var tmpW,tmpH:Longint;
Begin
  FChartBounds:=InternalCanvas.InitWindow(UserCanvas,FView3DOptions,Color,FView3D,UserRect);
  RectSize(FChartBounds,tmpW,tmpH);
  With FChartBounds do
       ChartRect:=Rect( Left  + BorderWidth + MulDiv(MarginLeft,tmpW,100),
                        Top   + BorderWidth + MulDiv(MarginTop,tmpH,100),
                        Right - BorderWidth - MulDiv(MarginRight,tmpW,100),
                        Bottom- BorderWidth - MulDiv(MarginBottom,tmpH,100) );
  RecalcWidthHeight;
  InternalDraw(FChartBounds);

{$IFDEF MONITOR_REDRAWS}
  Inc(RedrawCount);
  InternalCanvas.TextOut(0,0,IntToStr(RedrawCount));
{$ENDIF}
  InternalCanvas.ShowImage(UserCanvas,FDelphiCanvas,UserRect);
end;

procedure TCustomTeePanel.Paint;
begin
  if (not FPrinting) and (not InternalCanvas.ReDrawBitmap) then
     Draw(FDelphiCanvas,GetClientRect);
end;

Function TCustomTeePanel.TeeCreateMetafile( Enhanced:Boolean; Const Rect:TRect ):TMetafile;
{$IFNDEF D1}
var tmpCanvas:TMetafileCanvas;
begin
  result:=TMetafile.Create;
  { bug in D3.02 ... graphics.pas metafile reduces width/height.
    Workaround:
       - Use D3.01 graphics.pas?
  }
  result.Width :=MaxLong(1,(Rect.Right-Rect.Left)-1);
  result.Height:=MaxLong(1,(Rect.Bottom-Rect.Top)-1);
  result.Enhanced:=Enhanced;
  tmpCanvas:=TMetafileCanvas.Create(result,0);
  try
    DrawToMetaCanvas(tmpCanvas,Rect);
  finally
    tmpCanvas.Free;
  end;
end;
{$ELSE}
Var tmpHandle:HMETAFILE;
    tmpCanvas:TCanvas;
begin
  tmpHandle:=CreateMetafile(nil);
  tmpCanvas:=TCanvas.Create;
  tmpCanvas.Handle:=tmpHandle;
  try
    DrawToMetaCanvas(tmpCanvas,Rect);
  finally
    tmpCanvas.Free;
  end;
  result:=TMetafile.Create;
  With result do
  begin
    Handle:=CloseMetafile(tmpHandle);
    Inch:=Screen.PixelsPerInch;
    Width :=MaxLong(1,(Rect.Right-Rect.Left)-1);
    Height:=MaxLong(1,(Rect.Bottom-Rect.Top)-1);
  end;
end;
{$ENDIF}

{$IFNDEF D1}
{$HINTS OFF}
{$ENDIF}
Procedure TCustomTeePanel.SetBrushCanvas( AColor:TColor; APattern:TBrushStyle;
                                          ABackColor:TColor);
Const BrushColors:Array[Boolean] of TColor=(clBlack,clWhite);
Var tmp:HBrush;
begin
  with Canvas do
  begin
    if (APattern<>bsSolid) and (AColor=ABackColor) then
       Brush.Color:=BrushColors[ABackColor=clBlack]
    else
       Brush.Color:=AColor;
    Brush.Style:=APattern;
    tmp:=Brush.Handle; { <-- force brush creation }
    if APattern=bsSolid then BackMode:=cbmTransparent
    else
    begin
      BackMode:=cbmOpaque;
      BackColor:=ColorToRGB(ABackColor);
    end;
  end;
end;
{$IFNDEF D1}
{$HINTS ON}
{$ENDIF}

Procedure TCustomTeePanel.FontCanvas(SourceFont:TFont);
Begin
  With InternalCanvas.Font do
  begin
    SourceFont.PixelsPerInch:=PixelsPerInch;
    Assign(SourceFont);
  end;
End;

Function TCustomTeePanel.IsScreenHighColor:Boolean;
Begin
  With InternalCanvas do
    result:= GetDeviceCaps(Handle,BITSPIXEL)*
             GetDeviceCaps(Handle,PLANES)>=15;
End;

Function TCustomTeePanel.CanClip:Boolean;
begin
  result:=((not Printing) and (not Canvas.Metafiling)) or
          (Printing and TeeClipWhenPrinting) or
          (Canvas.Metafiling and TeeClipWhenMetafiling)
end;

Procedure TCustomTeePanel.Set3DPercent(Value:Integer);
Const Max3DPercent = 100;
      Min3DPercent = 1;
Begin
  if (Value<Min3DPercent) or (Value>Max3DPercent) then
     Raise ChartException.CreateFmt(TeeMsg_3DPercent,[Min3DPercent,Max3DPercent])
  else
    SetIntegerProperty(F3DPercent,Value);
end;

Procedure TCustomTeePanel.SetLongintProperty(Var Variable:Longint; Value:Longint);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Invalidate;
  end;
end;

Procedure TCustomTeePanel.SetStringProperty(Var Variable:String; Const Value:String);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Invalidate;
  end;
end;

Procedure TCustomTeePanel.SetDoubleProperty(Var Variable:Double; Const Value:Double);
begin
  if Variable<>Value then
  begin
    Variable:=Value;
    Invalidate;
  end;
end;

Procedure TCustomTeePanel.SetColorProperty(Var Variable:TColor; Value:TColor);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Invalidate;
  end;
end;

Procedure TCustomTeePanel.SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Invalidate;
  end;
end;

Procedure TCustomTeePanel.SetIntegerProperty(Var Variable:Integer; Value:Integer);
Begin
  if Variable<>Value then
  begin
    Variable:=Value; Invalidate;
  end;
end;

Procedure TCustomTeePanel.Loaded;
begin
  inherited Loaded;
  FOriginalCursor:=Cursor;
end;

procedure TCustomTeePanel.CMMouseLeave(var Message: TMessage);
begin
  Cursor:=FOriginalCursor;
  FPanning.Active:=False;
  inherited;
end;

procedure TCustomTeePanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result:=Message.Result or DLGC_WANTARROWS;
end;

procedure TCustomTeePanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
Begin
  if TeeEraseBack then Inherited;
  Message.Result:=1;
End;

procedure TCustomTeePanel.Invalidate;
begin
  if AutoRepaint then
  begin
    if Assigned(InternalCanvas) then InternalCanvas.Invalidate;
    inherited Invalidate;
  end;
end;

Function TCustomTeePanel.MultiLineTextWidth(S:String; Var NumLines:Integer):Integer;
var i : Integer;
begin
  i:=Pos(TeeLineSeparator,s);
  if i=0 then  { optimized for speed }
  begin
    result:=Canvas.TextWidth(s);
    NumLines:=1;
  end
  else
  begin
    result:=0;
    NumLines:=0;
    While i>0 do
    begin
      result:=MaxLong(result,Canvas.TextWidth(Copy(s,1,i-1)));
      Inc(NumLines);
      Delete(s,1,i);
      i:=Pos(TeeLineSeparator,s);
    end;
    if s<>'' then
    begin
      result:=MaxLong(result,Canvas.TextWidth(s));
      Inc(NumLines);
    end;
  end;
end;

Function TCustomTeePanel.GetMargin(Index:Integer):Integer;
Begin
  with FMargins do
  Case Index of
    0: result:=Left;
    1: result:=Top;
    2: result:=Right;
  else
  { 3:} result:=Bottom;
  end;
end;

Procedure TCustomTeePanel.SetMargin(Index,Value:Integer);
Begin
  with FMargins do
  Case Index of
    0: SetIntegerProperty(Left,Value);
    1: SetIntegerProperty(Top,Value);
    2: SetIntegerProperty(Right,Value);
  else SetIntegerProperty(Bottom,Value);
  end;
end;

Function TCustomTeePanel.GetBufferedDisplay:Boolean;
begin
  result:=InternalCanvas.UseBuffer;
end;

Procedure TCustomTeePanel.SetBufferedDisplay(Value:Boolean);
begin
  InternalCanvas.UseBuffer:=Value;
end;

Procedure TCustomTeePanel.SetInternalCanvas(NewCanvas:TCanvas3D);
begin
  if Assigned(NewCanvas) then
  begin
    NewCanvas.ReferenceCanvas:=FDelphiCanvas;
    if Assigned(InternalCanvas) then
    begin
      AutoRepaint:=False;
      NewCanvas.Assign(InternalCanvas);
      AutoRepaint:=True;
      InternalCanvas.Free;
    end;
    InternalCanvas:=NewCanvas;
    Repaint;
  end;
end;

procedure TCustomTeePanel.RecalcWidthHeight;
Begin
  With ChartRect do
  begin
    if Left<FChartBounds.Left then Left:=FChartBounds.Left;
    if Top<FChartBounds.Top then Top:=FChartBounds.Top;
    if Right<Left then Right:=Left+1 else
    if Right=Left then Right:=Left+Width;
    if Bottom<Top then Bottom:=Top+1 else
    if Bottom=Top then Bottom:=Top+Height;
    FChartWidth  :=Right-Left;
    FChartHeight :=Bottom-Top;
    FChartXCenter:=(Left+Right) div 2;
    FChartYCenter:=(Top+Bottom) div 2;
  end;
end;

procedure TCustomTeePanel.DrawZoomRectangle;
Begin
  With InternalCanvas do
  Begin
    With Pen do
    begin
      Color:=ColorToRGB(Self.Color);
      if Color=clWhite then Color:=clBlack;  { when color is the same... }
      Mode :=pmNotXor;
      Style:=psSolid;
      Width:=1;
    end;
    Brush.Style:=bsClear;
    With IZoom do Rectangle(X0,Y0,X1,Y1);
  end;
end;

Function TCustomTeePanel.GetCursorPos:TPoint;
Begin
  {$IFDEF D1}
  WinProcs.GetCursorPos(result);
  {$ELSE}
  Windows.GetCursorPos(result);
  {$ENDIF}
  result:=ScreenToClient(result);
end;

Function TCustomTeePanel.GetRectangle:TRect;
begin
  result:=GetClientRect;
end;

Procedure TCustomTeePanel.DrawToMetaCanvas(ACanvas:TCanvas; Const Rect:TRect);
Var Old:Boolean;
begin
       { assume the acanvas is in MM_ANISOTROPIC mode }
  InternalCanvas.Metafiling:=True;
  Old:=BufferedDisplay;
  BufferedDisplay:=False;
  try
    Draw(ACanvas,Rect);
  finally
    BufferedDisplay:=Old;
    InternalCanvas.Metafiling:=False;
  end;
end;

Function TCustomTeePanel.GetMonochrome:Boolean;
Begin
  result:=InternalCanvas.Monochrome;
end;

Procedure TCustomTeePanel.SetMonochrome(Value:Boolean);
Begin
  InternalCanvas.Monochrome:=Value;
end;

Procedure TCustomTeePanel.Assign(Source:TPersistent);
begin
  if Source is TCustomTeePanel then
  With TCustomTeePanel(Source) do
  begin
    Self.BufferedDisplay    := BufferedDisplay;
    Self.FPrintProportional := FPrintProportional;
    Self.FPrintResolution   := FPrintResolution;
    Self.FMargins           := FMargins;
    Self.Monochrome         := Monochrome;
    Self.FView3D            := FView3D;
    Self.FView3DOptions.Assign(FView3DOptions);
    Self.F3DPercent         := F3DPercent;
    Self.Color              := Color;
  end
  else inherited Assign(Source);
end;

Procedure TCustomTeePanel.SaveToMetafile(Const FileName:String);
begin
  SaveToMetaFileRect(False,FileName,GetRectangle);
end;

Procedure TCustomTeePanel.SaveToMetafileEnh(Const FileName:String);
begin
  SaveToMetaFileRect(True,FileName,GetRectangle);
end;

{ Enhanced:Boolean }
Procedure TCustomTeePanel.SaveToMetafileRect( Enhanced:Boolean;
                                           Const FileName:String;
                                           Const Rect:TRect);
Var tmpStream:TFileStream;
    Meta:TMetafile;
Begin
  Meta:=TeeCreateMetafile(Enhanced,Rect);
  With Meta do
  try
    tmpStream:=TFileStream.Create(FileName,fmCreate);
    try
      SaveToStream(tmpStream);
    finally
      tmpStream.Free;
    end;
  finally
    Free;
  end;
End;

Function TCustomTeePanel.TeeCreateBitmap(ABackColor:TColor; Const Rect:TRect):TBitmap;
var Old  : Boolean;
begin
  result:=TBitmap.Create;
  With result do
  begin
    Width := Rect.Right-Rect.Left;
    Height:= Rect.Bottom-Rect.Top;
    if ABackColor<>clWhite then
    begin
      Canvas.Brush.Color:=ABackColor;
      Canvas.FillRect(Rect);
    end;
    Old:=BufferedDisplay;
    BufferedDisplay:=False;
    Self.Draw(Canvas,Rect);
    BufferedDisplay:=Old;
  end;
end;

Procedure TCustomTeePanel.SaveToBitmapFile(Const FileName:String);
Begin
  With TeeCreateBitmap(clWhite,GetRectangle) do
  try
    SaveToFile(FileName);
  finally
    Free;
  end;
End;

Procedure TCustomTeePanel.PrintPortrait;
Begin
  PrintOrientation(poPortrait);
end;

Procedure TCustomTeePanel.PrintLandscape;
Begin
  PrintOrientation(poLandscape);
end;

Procedure TCustomTeePanel.PrintOrientation(AOrientation:TPrinterOrientation);
Var OldOrientation:TPrinterOrientation;
Begin
  OldOrientation:=Printer.Orientation;
  Printer.Orientation:=AOrientation;
  try
    Print;
  finally
    Printer.Orientation:=OldOrientation;
  end;
end;

Procedure TCustomTeePanel.CopyToClipboardBitmap;
var tmpBitmap:TBitmap;
begin
  tmpBitmap:=TeeCreateBitmap(clWhite,GetRectangle);
  with tmpBitmap do
  try
    ClipBoard.Assign(tmpBitmap);
  finally
    tmpBitmap.Free;
  end;
end;

Procedure TCustomTeePanel.CopyToClipboardMetafile(Enhanced:Boolean);
Var Meta:TMetaFile;
begin
  Meta:=TeeCreateMetafile(Enhanced,GetRectangle);
  try
    ClipBoard.Assign(Meta);
  finally
    Meta.Free;
  end;
end;

Procedure TCustomTeePanel.CalcMetaBounds( Var R:TRect;
                                          Const AChartRect:TRect;
                                          Var WinWidth,WinHeight,
                                          ViewWidth,ViewHeight:Longint);
Var tmpRectWidth,
    tmpRectHeight:Longint;
begin
       { apply PrintResolution to the desired rectangle coordinates }
  RectSize(R,ViewWidth,ViewHeight);
  RectSize(AChartRect,tmpRectWidth,tmpRectHeight);
  WinWidth :=tmpRectWidth -MulDiv(tmpRectWidth, PrintResolution,100);
  WinHeight:=tmpRectHeight-MulDiv(tmpRectHeight,PrintResolution,100);
  With R do
  begin
    Left  :=MulDiv(Left  ,WinWidth,ViewWidth);
    Right :=MulDiv(Right ,WinWidth,ViewWidth);
    Top   :=MulDiv(Top   ,WinHeight,ViewHeight);
    Bottom:=MulDiv(Bottom,WinHeight,ViewHeight);
  end;
end;

Procedure TCustomTeePanel.PrintPartialCanvas( PrintCanvas:TCanvas;
                                              Const PrinterRect:TRect);
Var ViewWidth   : Longint;
    ViewHeight  : Longint;
    WinWidth    : Longint;
    WinHeight   : Longint;
    tmpR        : TRect;

  Procedure SetAnisotropic; { change canvas/windows metafile mode }
  Var DC : HDC;
  begin
    DC:=PrintCanvas.Handle;
    SetMapMode(DC, MM_ANISOTROPIC);
    SetWindowOrgEx(  DC, 0, 0, nil);
    SetWindowExtEx(  DC, WinWidth, WinHeight, nil);
    SetViewportExtEx(DC, ViewWidth,ViewHeight, nil);
    SetViewportOrgEx(DC, 0, 0, nil);
  end;

Begin
  tmpR:=PrinterRect;
  { check if margins inverted }
  if tmpR.Left>tmpR.Right then SwapInteger(tmpR.Left,tmpR.Right);
  if tmpR.Top>tmpR.Bottom then SwapInteger(tmpR.Top,tmpR.Bottom);

  { apply PrintResolution to dimensions }
  CalcMetaBounds(tmpR,GetRectangle,WinWidth,WinHeight,ViewWidth,ViewHeight);

  SetAnisotropic;
  FPrinting:=True;
  try
    if CanClip then ClipCanvas(PrintCanvas,tmpR);
    DrawToMetaCanvas(PrintCanvas,tmpR);
    UnClipCanvas(PrintCanvas);
  finally
    FPrinting:=False;
  end;
end;

Procedure TCustomTeePanel.PrintPartial(Const PrinterRect:TRect);
Begin
  PrintPartialCanvas(Printer.Canvas,PrinterRect);
End;

Procedure TCustomTeePanel.PrintRect(Const R:TRect);
Begin
  Printer.Title:=Name;
  Printer.BeginDoc;
  try
    PrintPartial(R);
    Printer.EndDoc;
  except
    on Exception do
    begin
      Printer.Abort;
      if Printer.Printing then Printer.EndDoc;
      Raise;
    end;
  end;
end;

Function TCustomTeePanel.CalcProportionalMargins:TRect;

  Function CalcMargin(Size1,Size2:Integer; Const ARatio:Double):Integer;
  Var tmpPrinterRatio:Double;
  begin
    tmpPrinterRatio:= GetDeviceCaps(Printer.Handle,LOGPIXELSX)/
                      GetDeviceCaps(Printer.Handle,LOGPIXELSY);
    result:=Round(100.0*(Size2-((Size1-(40.0*Size1*0.01))*ARatio*tmpPrinterRatio))/Size2) div 2;
  end;

Var tmp      : Integer;
    tmpWidth : Integer;
    tmpHeight: Integer;
begin
  With GetRectangle do
  begin
    tmpWidth:=Right-Left;
    tmpHeight:=Bottom-Top;
  end;
  if tmpWidth > tmpHeight then
  begin
    tmp:=CalcMargin(Printer.PageWidth,Printer.PageHeight,tmpHeight/tmpWidth);
    Result:=Rect(TeeDefault_PrintMargin,tmp,TeeDefault_PrintMargin,tmp);
  end
  else
  begin
    tmp:=CalcMargin(Printer.PageHeight,Printer.PageWidth,tmpWidth/tmpHeight);
    Result:=Rect(tmp,TeeDefault_PrintMargin,tmp,TeeDefault_PrintMargin);
  end;
end;

Function TCustomTeePanel.ChartPrintRect:TRect;
Var tmpLog:Array[Boolean] of Longint;

  Function InchToPixel(Horizontal:Boolean; Const Inch:Double):Integer;
  begin
    result:=Round(Inch*tmpLog[Horizontal]);
  end;

Var tmp:Double;
Begin
  if FPrintProportional then PrintMargins:=CalcProportionalMargins;
  { calculate margins in pixels and calculate the remaining rectangle in pixels }
  tmpLog[True]:=GetDeviceCaps(Printer.Handle,LOGPIXELSX);
  tmpLog[False]:=GetDeviceCaps(Printer.Handle,LOGPIXELSY);
  With result do
  Begin
    tmp   :=0.01*Printer.PageWidth/(1.0*tmpLog[True]);
    Left  :=InchToPixel(True,1.0*PrintMargins.Left*tmp);
    Right :=Printer.PageWidth-InchToPixel(True,1.0*PrintMargins.Right*tmp);

    tmp   :=0.01*Printer.PageHeight/(1.0*tmpLog[False]);
    Top   :=InchToPixel(False,1.0*PrintMargins.Top*tmp);
    Bottom:=Printer.PageHeight-InchToPixel(False,1.0*PrintMargins.Bottom*tmp);
  end;
end;

Procedure TCustomTeePanel.Print;
Begin
  PrintRect(ChartPrintRect);
end;

Procedure TCustomTeePanel.CheckPenWidth(APen:TPen);
begin
  if Printing and (APen.Style<>psSolid) and (APen.Width=1) then
     APen.Width:=0;  { <-- fixes some printer's bug (HP Laserjets?) }
end;

Procedure TCustomTeePanel.DrawPanelBevels(Rect:TRect);
Const Colors:Array[Boolean] of TColor=(clBtnHighlight,clBtnShadow);
begin
  if BevelOuter <> bvNone then
     Frame3D(Canvas.ReferenceCanvas,Rect,Colors[BevelOuter=bvLowered],Colors[BevelOuter=bvRaised],BevelWidth);
  Frame3D(Canvas.ReferenceCanvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
     Frame3D(Canvas.ReferenceCanvas, Rect, Colors[BevelInner=bvLowered],Colors[BevelInner=bvRaised], BevelWidth);
end;

{ TCustomTeePanelExtended }
Procedure TCustomTeePanelExtended.SetAnimatedZoom(Value:Boolean);
Begin
  SetBooleanProperty(FAnimatedZoom,Value);
end;

Procedure TCustomTeePanelExtended.SetAnimatedZoomSteps(Value:Integer);
Begin
  SetIntegerProperty(FAnimatedZoomSteps,Value);
end;

procedure TCustomTeePanelExtended.SetBackImage(Value:TPicture);
begin
  FBackImage.Assign(Value);
end;

procedure TCustomTeePanelExtended.SetBackImageMode(Value:TTeeBackImageMode);
Begin
  if FBackImageMode<>Value then
  begin
    FBackImageMode:=Value;
    Invalidate;
  end;
End;

Constructor TCustomTeePanelExtended.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradient :=TChartGradient.Create(Self);
  FBackImageMode:=pbmStretch;
  FBackImage:=TPicture.Create;
  FBackImage.OnChange:=CanvasChanged;
  FAnimatedZoomSteps:=8;
  FAllowZoom:=True;
  FAllowPanning:=pmBoth;
end;

Destructor TCustomTeePanelExtended.Destroy;
Begin
  FGradient.Free;
  FBackImage.Free;
  inherited Destroy;
end;

procedure TCustomTeePanelExtended.UndoZoom;
begin
  if Assigned(FOnUndoZoom) then FOnUndoZoom(Self);
  Invalidate;
  FZoomed:=False;
end;

procedure TCustomTeePanelExtended.SetGradient(Value:TChartGradient);
begin
  FGradient.Assign(Value);
end;

Procedure TCustomTeePanelExtended.Assign(Source:TPersistent);
begin
  if Source is TCustomTeePanelExtended then
  With TCustomTeePanelExtended(Source) do
  begin
    Self.FBackImage.Assign(FBackImage);
    Self.FBackImageMode   := FBackImageMode;
    Self.FGradient.Assign(FGradient);
    Self.FAllowPanning    := FAllowPanning;
    Self.FAllowZoom       := FAllowZoom;
    Self.FAnimatedZoom    := FAnimatedZoom;
    Self.FAnimatedZoomSteps:=FAnimatedZoomSteps;
  end;
  inherited Assign(Source);
end;

function TCustomTeePanelExtended.GetPalette: HPALETTE;
begin
  Result := 0;	{ default result is no palette }
  if Assigned(FBackImage) and (FBackImage.Graphic<>nil) and
     (FBackImage.Graphic is TBitmap) then	{ only bitmaps have palettes }
         Result := TBitmap(FBackImage.Graphic).Palette;	{ use it if available }
end;

procedure TCustomTeePanelExtended.DrawBitmap(Rect:TRect; Z:Integer);
var RectH      : LongInt;
    RectW      : LongInt;

    Procedure TileBitmap;
    Var tmpWidth   : LongInt;
	tmpHeight  : LongInt;
	tmpX       : LongInt;
	tmpY       : LongInt;
    begin
      tmpWidth :=BackImage.Width;
      tmpHeight:=BackImage.Height;
      { keep "painting" the wall with tiled small images... }
      if (tmpWidth>0) and (tmpHeight>0) then
      Begin
	tmpY:=0;
	while tmpY<RectH do
	begin
	  tmpX:=0;
	  while tmpX<RectW do
	  begin
	    Canvas.Draw(Rect.Left+tmpX,Rect.Top+tmpY,BackImage.Graphic);
	    Inc(tmpX,tmpWidth);
	  end;
	  Inc(tmpY,tmpHeight);
	end;
      end;
    end;

    Procedure Calc3DRect;
    begin
      With Rect do
      begin
	TopLeft:=Canvas.Calculate3DPosition(Left,Top,Z);
	BottomRight:=Canvas.Calculate3DPosition(Right,Bottom,Z);
	if not View3D then Inc(Top);
      end;
    end;

Var ShouldClip:Boolean;
Begin
  if (Z=0) or ((not View3D) or (View3DOptions.Orthogonal)) then
  if FBackImage.Graphic<>nil then
  begin
    {$IFDEF D1}
    { try to improve bmp palette on D1 }
    SelectPalette(Canvas.Handle, FBackImage.Bitmap.Palette, True);
    RealizePalette(Canvas.Handle);
    {$ENDIF}
    if FBackImageMode=pbmStretch then
    begin
      if Z>0 then Calc3DRect;
      Canvas.StretchDraw(Rect,FBackImage.Graphic);
    end
    else
    begin
      ShouldClip:=CanClip;
      if ShouldClip then
	 if Z=0 then Canvas.ClipRectangle(Rect)
		else Canvas.ClipCube(Rect,0,Width3D);
      if Z>0 then Calc3DRect;
      RectSize(Rect,RectW,RectH);
      if FBackImageMode=pbmTile then TileBitmap
      else { draw centered }
      Begin
	FillPanelRect(Rect);
	Canvas.Draw( Rect.Left+((RectW-BackImage.Width) div 2),
		     Rect.Top +((RectH-BackImage.Height) div 2),
		     BackImage.Graphic);
      end;
      if ShouldClip then Canvas.UnClipRectangle;
    end;
  end;
end;

Procedure TCustomTeePanelExtended.DrawImage(Const R:TRect);
begin
  if (FBackImage.Graphic<>nil) then DrawBitmap(R,0);
end;

procedure TCustomTeePanelExtended.PanelPaint(Const UserRect:TRect);
Var Rect : TRect;
begin
  Rect:=UserRect;
  FillPanelRect(Rect);
  DrawImage(Rect);
  if (not Printing) or PrintTeePanel then
  begin
    With Canvas.Pen do
    begin
      Style:=psSolid;
      Width:=1;
      Mode:=pmCopy;
    end;
    Canvas.Brush.Style:=bsClear;
    DrawPanelBevels(Rect);
  end;
end;

procedure TCustomTeePanelExtended.FillPanelRect(Const Rect:TRect);
Begin
  With FGradient do
  if Visible then
  begin
    if (Direction=gdTopBottom) or (Direction=gdLeftRight) then
       Canvas.GradientFill(Rect,StartColor,EndColor,Direction)
    else
       Canvas.GradientFill(Rect,EndColor,StartColor,Direction);
  end
  else
  { PrintTeePanel is a "trick" to paint Chart background also when printing }
  if (not Printing) or PrintTeePanel then
  With Canvas do
  begin
    Brush.Color:=Self.Color;
    Brush.Style:=bsSolid;
    EraseBackground(Rect);
  end;
end;

{ TCustomTeeGradient }
Constructor TCustomTeeGradient.Create(AOwner:TCustomTeePanel);
Begin
  inherited Create;
  FOwner      :=AOwner;
  FDirection  :=gdTopBottom;
  FStartColor :=clWhite;
  FEndColor   :=clYellow;
End;

Procedure TCustomTeeGradient.SetVisible(Value:Boolean);
Begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
End;

Procedure TCustomTeeGradient.SetDirection(Value:TGradientDirection);
Begin
  if FDirection<>Value then
  Begin
    FDirection:=Value;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
End;

Procedure TCustomTeeGradient.SetStartColor(Value:TColor);
Begin
  if FStartColor<>Value then
  begin
    FStartColor:=Value;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
End;

Procedure TCustomTeeGradient.SetEndColor(Value:TColor);
Begin
  if FEndColor<>Value then
  begin
    FEndColor:=Value;
    if Assigned(FOwner) then FOwner.Invalidate;
  end;
End;

Procedure TCustomTeeGradient.Assign(Source:TPersistent);
Begin
  if Source is TCustomTeeGradient then
  With TCustomTeeGradient(Source) do
  Begin
    Self.FVisible   :=FVisible;
    Self.FDirection :=FDirection;
    Self.FStartColor:=FStartColor;
    Self.FEndColor  :=FEndColor;
  end
  else inherited Assign(Source);
end;

{ TChartFontObject }
Constructor TChartFontObject.Create(AOwner: TCustomTeePanel);
Begin
  inherited Create;
  FParentChart:=AOwner;
  FFont:=CreateDefaultFont(FParentChart.CanvasChanged);
end;

Function TChartFontObject.IsFontStored:Boolean;
begin
  result:=not IsDefaultFont(FFont);
end;

Destructor TChartFontObject.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

Procedure TChartFontObject.Assign(Source:TPersistent);
Begin
  if Source is TChartFontObject then
     Self.FFont.Assign(TChartFontObject(Source).FFont)
  else
     inherited Assign(Source);
end;

Procedure TChartFontObject.SetFont(Value:TFont);
begin
  FFont.Assign(Value);
end;

Procedure TChartFontObject.Repaint;
begin
  ParentChart.Invalidate;
end;

{ TDraw3D }
Procedure TDraw3D.InternalDraw(Const UserRectangle:TRect);
begin
  PanelPaint(UserRectangle);
  RecalcWidthHeight;
  InternalCanvas.Projection(FWidth3D,FChartBounds,ChartRect);
  Canvas.ResetState;
  if Assigned(FOnPaint) then FOnPaint(Self,UserRectangle);

  if IZoom.Active then DrawZoomRectangle;
  Canvas.ResetState;
  if Assigned(FOnAfterDraw) then FOnAfterDraw(Self);
end;

{ Zoom & Scroll (Panning) }
Procedure TZoomPanningRecord.Check;
begin
  if X0>X1 Then SwapLongint(X0,X1);
  if Y0>Y1 Then SwapLongint(Y0,Y1);
end;

Procedure TZoomPanningRecord.Activate(x,y:Longint);
Begin
  X0:=x;  Y0:=y;
  X1:=x;  Y1:=y;
  Active:=True;
End;

Function TeeGetImageExtension(Index:Longint):String;
Const Extensions:Array[0..{$IFDEF TEEJPEG}4{$ELSE}3{$ENDIF}] of String=
      ('BMP','WMF','EMF','TEE'{$IFDEF TEEJPEG},'JPG'{$ENDIF});
begin
  result:=Extensions[Index];
end;

Function TeeIdentToCursor(Const AName:String; Var ACursor:Longint):Boolean;
begin
  if AName=TeeMsg_TeeHand then
  begin
    ACursor:=crTeeHand;
    result:=True;
  end
  else result:=IdentToCursor(AName,ACursor);
end;

Function TeeCursorToIdent(ACursor:Longint; Var AName:String):Boolean;
begin
  if ACursor=crTeeHand then
  begin
    AName:=TeeMsg_TeeHand;
    result:=True;
  end
  else result:=CursorToIdent(ACursor,AName);
end;

{ Returns True if point T is over (more or less "Tolerance" pixels)
  line:  P --> Q }
Function PointInLineTolerance(Const P:TPoint; px,py,qx,qy,TolerancePixels:Integer):Boolean;

  Function Dif(a,b:Integer):Boolean;
  begin
    result:=(a+TolerancePixels)<b;
  end;

begin
  if ( Abs((qy-py)*(P.x-px)-(P.y-py)*(qx-px)) >= (MaxLong(Abs(qx-px),Abs(qy-py)))) then
      result:=False
  else
  if ((Dif(qx,px) and Dif(px,P.x)) or (Dif(qy,py) and Dif(py,P.y))) then result:=False else
  if ((Dif(P.x,px) and Dif(px,qx)) or (Dif(P.y,py) and Dif(py,qy))) then result:=False else
  if ((Dif(px,qx) and Dif(qx,P.x)) or (Dif(py,qy) and Dif(qy,P.y))) then result:=False else
  if ((Dif(P.x,qx) and Dif(qx,px)) or (Dif(P.y,qy) and Dif(qy,py))) then result:=False else
     result:=True;
end;

Function PointInLine(Const P:TPoint; px,py,qx,qy:Integer):Boolean;
Begin
  result:=PointInLineTolerance(P,px,py,qx,qy,0);
end;

Function PointInPolygon(Const P:TPoint; Const Poly:Array of TPoint):Boolean;
{ slow...
Var Region:HRGN;
begin
  Region:=CreatePolygonRgn(Poly,1+High(Poly),0);
  result:=(Region>0) and PtInRegion(Region,p.x,p.y);
  DeleteObject(Region);
end;
}
Var i,j:Longint;
begin
  result:=False;
  j:=High(Poly);
  for i:=0 to High(Poly) do
  begin
    if (((( Poly[i].Y <= P.Y ) and ( P.Y < Poly[j].Y ) ) or
         (( Poly[j].Y <= P.Y ) and ( P.Y < Poly[i].Y ) )) and
          ( P.X < ( Poly[j].X - Poly[i].X ) * ( P.Y - Poly[i].Y )
          / ( Poly[j].Y - Poly[i].Y ) + Poly[i].X )) then
             result:=not result;
    j:=i;
  end;
end;

Function PointInTriangle(Const P:TPoint; X0,X1,Y0,Y1:Integer):Boolean;
begin
  result:=PointInPolygon(P,[ Point(X0,Y0),Point((X0+X1) div 2,Y1),Point(X1,Y0) ]);
end;

Function PointInHorizTriangle(Const P:TPoint; Y0,Y1,X0,X1:Integer):Boolean;
begin
  result:=PointInPolygon(P,[ Point(X0,Y0),Point(X1,(Y0+Y1) div 2),Point(X0,Y1) ]);
end;

Function PointInEllipse(Const P:TPoint; Const Rect:TRect):Boolean;
var tmpWidth,tmpHeight,
    tmpXCenter,tmpYCenter:Integer;
begin
  With Rect do
  begin
    tmpXCenter:= (Left+Right) div 2;
    tmpYCenter:= (Top+Bottom) div 2;
    tmpWidth  := Sqr(tmpXCenter-Left);
    tmpHeight := Sqr(tmpYCenter-Top);
  end;
  result:=(tmpWidth<>0) and (tmpHeight<>0) and
          ( (Sqr(1.0*P.X-tmpXCenter) / tmpWidth ) +
            (Sqr(1.0*P.Y-tmpYCenter) / tmpHeight) <= 1 );
end;

{$IFNDEF D3}
Function AnsiPos(Const SubStr,St:String):Integer;
begin
  result:=Pos(SubStr,St);
end;
{$ENDIF}

{ Replaces all ocurrences of "Separator" in string "St" with
  #13 (TeeLineSeparator constant)  }
Procedure TeeSplitInLines(Var St:String; Const Separator:String);
var i:Integer;
begin
  Repeat
    i:=AnsiPos(Separator,St);
    if i>0 then St[i]:=TeeLineSeparator;
  Until i=0;
end;

{ Returns 1 + how many times "TeeLineSeparator #13" is found
  inside string St parameter }
Function TeeNumTextLines(St:String):Integer;
var i : Integer;
begin
  result:=0;
  i:=AnsiPos(TeeLineSeparator,St);
  while i>0 do
  begin
    inc(result);
    Delete(St,1,i);
    i:=AnsiPos(TeeLineSeparator,St);
  end;
  if St<>'' then Inc(result);
end;

Function DelphiToLocalFormat(Const Format:String):String;
var t:Longint;
begin
  result:=Format;
  for t:=1 to Length(result) do
      if result[t]=',' then result[t]:=ThousandSeparator else
      if result[t]='.' then result[t]:=DecimalSeparator;
end;

Function LocalToDelphiFormat(Const Format:String):String;
var t:Longint;
begin
  result:=Format;
  for t:=1 to Length(result) do
      if result[t]=ThousandSeparator then result[t]:=',' else
      if result[t]=DecimalSeparator then result[t]:='.';
end;

Procedure EnableControls(Enable:Boolean; Const ControlArray:Array of TControl);
var t:Integer;
begin
  for t:=Low(ControlArray) to High(ControlArray) do
      ControlArray[t].Enabled:=Enable;
end;

Function TeeRoundDate(Const ADate:TDateTime; AStep:TDateTimeStep):TDateTime;
var Year  : Word;
    Month : Word;
    Day   : Word;
begin
  if ADate<=EncodeDate(1900,1,1) then result:=ADate
  else
  begin
    DecodeDate(ADate,Year,Month,Day);
    Case AStep of
       dtHalfMonth   : if Day>=15 then Day:=15
                                  else Day:=1;
       dtOneMonth,
       dtTwoMonths,
       dtThreeMonths,
       dtFourMonths,
       dtSixMonths   : Day:=1;
       dtOneYear     : begin
                         Day:=1;
                         Month:=1;
                       end;
    end;
    result:=EncodeDate(Year,Month,Day);
  end;
end;

Procedure TeeDateTimeIncrement( IsDateTime:Boolean;
                                Increment:Boolean;
                                Var Value:Double;
                                Const AnIncrement:Double;
                                tmpWhichDateTime:TDateTimeStep);
var Year  : Word;
    Month : Word;
    Day   : Word;

 Procedure DecMonths(HowMany:Word);
 begin
   Day:=1;
   if Month>HowMany then Dec(Month,HowMany)
   else
   begin
     Dec(Year);
     Month:=12-(HowMany-Month);
   end;
 end;

 Procedure IncMonths(HowMany:Word);
 begin
   Day:=1;
   Inc(Month,HowMany);
   if Month>12 then
   begin
     Inc(Year);
     Month:=Month-12;
   end;
 end;

 Procedure IncDecMonths(HowMany:Word);
 begin
   if Increment then IncMonths(HowMany)
                else DecMonths(HowMany);
 end;

begin
  if IsDateTime then
  begin
    DecodeDate(Value,year,month,day);
    Case tmpWhichDateTime of
       dtHalfMonth   : Begin
                         if Day>15 then Day:=15
                         else
                         if Day>1 then Day:=1
                         else
                         begin
                           IncDecMonths(1);
                           Day:=15;
                         end;
                       end;
       dtOneMonth    : IncDecMonths(1);
       dtTwoMonths   : IncDecMonths(2);
       dtThreeMonths : IncDecMonths(3);
       dtFourMonths  : IncDecMonths(4);
       dtSixMonths   : IncDecMonths(6);
       dtOneYear     : if Increment then Inc(year) else Dec(year);
    else
    begin
      if Increment then Value:=Value+AnIncrement
                   else Value:=Value-AnIncrement;
      Exit;
    end;
    end;
    Value:=EncodeDate(year,month,day);
  end
  else
  begin
    if Increment then Value:=Value+AnIncrement
                 else Value:=Value-AnIncrement;
  end;
end;

Function TeeNextStep(Const OldStep:Double):Double;
Begin
  if OldStep >= 10 then result := 10*TeeNextStep(0.1*OldStep)
  else
  if OldStep < 1 then result := 0.1*TeeNextStep(OldStep*10)
  else
  if OldStep < 2 then result:=2 else
  if OldStep < 5 then result:=5 else result:=10
end;

{$IFDEF D3}
type PClassBitmap=^TClassBitmap;
     TClassBitmap=Record
       AClass  : TObject;
       ABitmap : TBitmap;
     end;

function LoadClassBitmap(HInstance: Longint; Data: Pointer): Boolean;

  Function TryFindResource(Const ResName:String):Boolean;
  var tmpSt:Array[0..255] of Char;
  begin
    StrPCopy(tmpSt,ResName);
    if FindResource(HInstance, tmpSt, RT_BITMAP)<>0 then
    begin
      PClassBitmap(Data)^.ABitmap.LoadFromResourceName(HInstance,tmpSt);
      result:=True;
    end
    else result:=False;
  end;

begin
  result:=True;
  try
    With PClassBitmap(Data)^ do
    if TryFindResource(AClass.ClassName) or
       TryFindResource(AClass.ClassParent.ClassName) then
         result:=False;
  except
    on Exception do ;
  end;
end;
{$ENDIF}

Procedure TeeGetClassNameBitmap(AClass:TComponent; ABitmap:TBitmap);
{$IFDEF D3}
Var tmpBitmap:TClassBitmap;
{$ELSE}
{$IFDEF D2C1}
{$ELSE}
{$IFDEF D1}
Var tmpSt:Array[0..255] of Char;
{$ENDIF}
{$ENDIF}
{$ENDIF}
Var tmpSt:Array[0..255] of Char;
begin
  {$IFDEF D3}
  tmpBitmap.ABitmap:=ABitmap;
  tmpBitmap.AClass:=AClass;
  EnumModules(LoadClassBitmap,@tmpBitmap);
  {$ELSE}
  try
    {$IFDEF D1}
    ABitmap.Handle:=LoadBitmap(Hinstance,StrPCopy(tmpSt,AClass.ClassName));
    {$ELSE}
    if FindResource(HInstance, StrPCopy(tmpSt,AClass.ClassName), RT_BITMAP) <> 0 then
       ABitmap.LoadFromResourceName(HInstance,AClass.ClassName)
    else
    if FindResource(HInstance, StrPCopy(tmpSt,AClass.ClassParent.ClassName), RT_BITMAP) <> 0 then
       ABitmap.LoadFromResourceName(HInstance,AClass.ClassParent.ClassName)
    {$ENDIF}
  except
    on Exception do ;
  end;
  {$ENDIF}
end;

Const TeeFieldsSeparator=';';

Function TeeNumFields(St:String):Integer;
var i:Integer;
begin
  if St<>'' then
  begin
    result:=1;
    i:=AnsiPos(TeeFieldsSeparator,St);
    While i>0 do
    begin
      Inc(result);
      Delete(St,1,i);
      i:=AnsiPos(TeeFieldsSeparator,St);
    end;
  end
  else result:=0;
end;

Function TeeExtractField(St:String; Index:Integer):String;
var i:Integer;
begin
  if St<>'' then
  begin
    i:=AnsiPos(TeeFieldsSeparator,St);
    While i>0 do
    begin
      Dec(Index);
      if Index=0 then
      begin
        result:=Copy(St,1,i-1);
        exit;
      end;
      Delete(St,1,i);
      i:=AnsiPos(TeeFieldsSeparator,St);
    end;
    result:=St;
  end;
end;

{ Helper functions for runtime Series creation }
Function TeeGetUniqueName(AOwner:TComponent; Const AStartName:String):String;
Var tmp   : Longint;
    tmpSt : String;
    {$IFNDEF TEEOCX}
    tmpForm:{$IFDEF D3}TCustomForm{$ELSE}TForm{$ENDIF};
    {$ENDIF}
begin
  result:='';
  {$IFNDEF D1}
  if AOwner is {$IFDEF D3}TCustomForm{$ELSE}TForm{$ENDIF} then
  begin
    {$IFNDEF TEEOCX}
    tmpForm:={$IFDEF D3}TCustomForm{$ELSE}TForm{$ENDIF}(AOwner);
    if tmpForm.Designer<>nil then
       {$IFDEF D5}
       result := (tmpForm.Designer as IDesigner).UniqueName(AStartName);
       {$ELSE}
       //result := TFormDesigner(tmpForm.Designer).UniqueName(AStartName);   rochedo
       {$ENDIF}
    {$ENDIF}
  end;
  {$ENDIF}
  if result='' then
  begin
    {$IFDEF TEEOCX}
    tmp:=0;
    {$ELSE}
    tmp:=1;
    {$ENDIF}
    tmpSt:=AStartName+IntToStr(tmp);
    while (AOwner.FindComponent(tmpSt)<>nil) do
    begin
      Inc(tmp);
      tmpSt:=AStartName+IntToStr(tmp);
    end;
    result:=tmpSt;
  end;
end;

initialization
  TeeClipWhenPrinting   := {$IFDEF D1}False{$ELSE}True{$ENDIF};
  TeeClipWhenMetafiling := {$IFDEF D1}False{$ELSE}True{$ENDIF};
  TeeEraseBack          := {$IFDEF D1}True{$ELSE}False{$ENDIF};  { erase background before repainting panel }
  PrintTeePanel         := False; { Should Panel background to be printed ? Default: False }
  Screen.Cursors[crTeeHand]:=LoadCursor(HInstance,'TEE_CURSOR_HAND');
{$IFNDEF D1}
finalization
  Screen.Cursors[crTeeHand]:=0;
{$ENDIF}
end.

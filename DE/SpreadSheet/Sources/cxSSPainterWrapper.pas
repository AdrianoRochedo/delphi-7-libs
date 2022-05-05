{*******************************************************************}
{                                                                   }
{       Developer Express Cross platform Visual Component Library   }
{       ExpressSpreadSheet				            }
{                                                                   }
{       Copyright (c) 2001-2003 Developer Express Inc.              }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by U.S. and       }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES           }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE    }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS   }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET AND ALL           }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE      }
{   PROGRAM ONLY.                                                   }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                      }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}

unit cxSSPainterWrapper;

{$I cxSSVer.inc}

interface
uses
  SysUtils, Classes, {$IFDEF WINCLX} Windows, Graphics, {$ENDIF}
  {$IFDEF VCL}
  Windows, Graphics, cxExcelConst, cxSSTypes;
  {$ELSE}
  Qt, Types, QGraphics, QcxGraphics, QcxExcelConst, QcxSSTypes;
  {$ENDIF}



type
  {$IFDEF WINDOWS}
  TcxFontStyles = Graphics.TFontStyles;
  TcxFontCharset = Graphics.TFontCharset;
  {$ELSE}
  TcxFontStyles = QGraphics.TFontStyles;
  TcxFontCharset = QGraphics.TFontCharset;
  {$ENDIF}

  { TcxBackgroundMode }
  TcxBackgroundMode = (bmTransparent, bmOpaque);

  {TcxPaintObjects}
  TcxPaintObjects = (poBrush, poFont, poPen);
  TcxOrientation = (oHorz, oVert);

  { TcxSSTextExtents }
  TcxSSTextExtentsEx = record
     Size: TSize;
     {$IFDEF WINDOWS}
     SpaceWidth: Integer;
     {$ENDIF}
     LineHeight: Integer;
     TextWidths: array of Integer;
  end;

  { TcxCanvasWrapper }
  TcxCanvasWrapper = class
  private
    FBkColor: TColor;
    FBkMode: TcxBackgroundMode;
    FCanvas: TCanvas;
    FDefColors: array[Boolean, TcxPaintObjects] of TColor;
    FHighLightPalette: TcxExcelPalette;
    FTextColor: TColor;
    FFontHandle: TcxFontHandle;
    FPainterHandle: TcxPainterHandle;
    FPalette: Pointer;
    FSelectionColor: TColor;
    function GetDefBorderColor: TColor;
    function GetDefTextColor: TColor;
    function GetDefWindowColor: TColor;
    function GetPalette: PcxExcelPalette;
    procedure SetDefaultColor(const AValue: TColor; AObject: TcxPaintObjects);
    procedure SetDefBorderColor(const AValue: TColor);
    procedure SetDefTextColor(const AValue: TColor);
    procedure SetDefWindowColor(const AValue: TColor);
    procedure SetPalette(APalette: PcxExcelPalette);
    procedure SetSelectionColor(const AValue: TColor);
  protected
    function CheckHandle(AHandle: Integer): Boolean; overload;
    function CheckHandle(AHandle: TObject): Boolean; overload;
    procedure DrawLine(const AVertex: array of TRect;
      AStyle: TcxSSEdgeLineStyle; AOrientation: TcxOrientation);
    function GetWordFromPos(const AString: TcxString; var APos: Integer): TcxString; virtual;
    procedure HandlesNeeded; virtual;
    procedure SplitToTextBricks(const AText: TcxString; ARect: TRect;
      HAlign: TcxHorzTextAlign;  VAlign: TcxVertTextAlign; var ATextBricks: TWordExtents); virtual;
    procedure ReleaseHandles; virtual;
    class procedure CreateBrushStyles;
    class procedure CreatePenStyles;
    class procedure InitColors;
    class procedure RemoveBrushStyles;
    class procedure RemovePenStyles;
    property Palette: PcxExcelPalette read GetPalette write SetPalette;
  public
    constructor Create(APalette: PcxExcelPalette); virtual;
    destructor Destroy; override;
    procedure BeginPaint(ACanvas: TCanvas); overload;
    procedure BeginPaint(APalette: PcxExcelPalette; ACanvas: TCanvas); overload;
    procedure CalculateTextExtents(const AText: TcxString;const ATextRect: TRect;
      AHorzAlign: TcxHorzTextAlign; AVertAlign: TcxVertTextAlign;
      AWordBreak: Boolean; var ATextParams: TcxTextParameters);
    procedure DrawText(const AText: TcxString; const ARect: TRect);
    procedure DrawTriangleGlyph(const ARect: TRect; AColor: TColor;
      ALeftToRight: Boolean; IsDrawLine: Boolean);
    procedure EndPaint; virtual;
    procedure ExcludeClipRect(const ARect: TRect); overload;
    procedure ExcludeClipRect(X1, Y1, X2, Y2: Integer); overload;
    procedure ExcludeClipRgn(const APoints: array of TPoint);
    procedure ExDrawText(const AClipRect: TRect;
      const AextTextParams: TcxTextParameters); virtual;
    procedure FillRect(const ARect: TRect; AStyle: TcxSSFillStyle;
      const ABkColor, AFgColor: TColor); overload;
    procedure FillRect(const ARect: TRect; AStyle: TcxSSFillStyle;
      const ABkColor, AFgColor: Word; IsSelected: Boolean); overload;
    procedure FrameRect(const ARect: TRect; AColor: TColor;
      IsSelected: Boolean = False); overload;
    procedure FrameRect(const ARect: TRect; const ATopColor,  ABottomColor: TColor;
      IsSelected: Boolean = False); overload;
    class function GetNativeColor(const AColor: TColor): Integer; register;
    function GetTextExtent(const AText: TcxString): TPoint;
    procedure InvertRect(const ARect: TRect);
    procedure Line(const AVertex: TRect; AStyle: TcxSSEdgeLineStyle;
      AOrientation: TcxOrientation; ABkColor, AFgColor: TColor); overload;
    procedure Line(const AVertex: array of TRect; AStyle: TcxSSEdgeLineStyle;
      AOrientation: TcxOrientation; ABkColor, AFgColor: Word; IsSelected: Boolean); overload;
    procedure PaletteChanged; virtual;
    procedure Polygon(const APoints: array of TPoint; const AColor: TColor);
    procedure Rectangle(const ARect: TRect; const ABrush: TcxFillBrushHandle);
    function RectIsVisible(ARect: TRect): Boolean;
    procedure SelectFont(AFont: TFont); overload;
    {$IFDEF WINCLX}
    procedure SelectFont(const AFont: QFontH); overload;
    {$ENDIF}
    procedure SelectFont(const AFont: TcxFontHandle;
      ACheckSettings: Boolean = True); overload;
    procedure SelectFont(const AFont: TcxFontHandle; AColor: Word); overload;
    procedure SetBkMode(AMode: TcxBackgroundMode);
    procedure SetSingleText(const AText: TcxString; const ATextRect: TRect;
      var ATextSettings: TcxTextParameters; AVertCenterAlign: Boolean = True); virtual;
    procedure SetBkColor(AColor: TColor); overload;
    procedure SetBkColor(AColor: Word; IsSelected: Boolean;
      AObject: TcxPaintObjects); overload;
    procedure SetTextColor(AColor: TColor); overload;
    procedure SetTextColor(AColor: Word; IsSelected: Boolean;
      AObject: TcxPaintObjects); overload;
    function TextWidth(const AText: TcxString): Integer;
    function TextHeight(const AText: TcxString): Integer;
    class function MixColors(ASelColor, ASrcColor: TColor): TColor;
    property BorderColor: TColor read GetDefBorderColor write SetDefBorderColor;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Handle: TcxPainterHandle read FPainterHandle;
    property TextColor: TColor read GetDefTextColor write SetDefTextColor;
    property WindowColor: TColor read GetDefWindowColor write SetDefWindowColor;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
  end;

const
  cxPenWidth: array[TcxSSEdgeLineStyle] of Byte =
    (1, 1, 2, 1, 1, 3, 3, 1, 2, 1, 2, 1, 2, 2, 0);
  cxHalfPenWidth: array[TcxSSEdgeLineStyle] of Byte =
    (1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 2, 0);
  {$IFNDEF WINDOWS}
  cxTopAlign    = Integer(AlignmentFlags_AlignTop);
  cxAlignLeft   = cxTopAlign or Integer(AlignmentFlags_AlignLeft);
  cxAlignRight  = cxTopAlign or Integer(AlignmentFlags_AlignRight);
  cxAlignCenter = cxTopAlign or Integer(AlignmentFlags_AlignHCenter);
  {$ENDIF}

{$IFNDEF WINDOWS}
const
  clBtnShadow = clMid;
{$ENDIF}

  cxBlackColor: Integer = clBlack;
  cxWhiteColor: Integer = clWhite;
  cxBtnFaceColor: Integer = clBtnFace;
  cxBtnShadowColor: Integer = clBtnShadow;
  cxBtnHighLightColor: Integer = clBtnHighLight;

  function ColorToRGB(const AColor: TColor): Integer;

  function RectHeight(const ARect: TRect): Integer;
  function RectWidth(const ARect: TRect): Integer;

implementation
type
  PPoints = ^TPoints;
  TPoints = array[0..MaxInt div SizeOf(TPoint) - 1] of TPoint;
  TCanvasAccess = class(TCanvas);

  TRGB = packed record
    R, G, B, A: Byte;
  end;

var
  APenBrushes: array[0..9, TcxOrientation] of TcxFillBrushHandle;
  DefaultPens: array[0..16, TcxOrientation] of TcxFillBrushHandle;
  DefaultBrushes: array[TcxSSFillStyle] of TcxFillBrushHandle;

const
  BrushReference: Integer = 0;
  PenReference: Integer = 0;
  EmptyHandle: {$IFDEF WINDOWS} Integer = 0 {$ELSE} Pointer = nil {$ENDIF};

  BrushPatterns: array[TcxSSFillStyle, 0..7] of Word =
  (($00FF, $00FF, $00FF, $00FF, $00FF, $00FF, $00FF, $00FF),
   ($00AA, $0000, $0055, $0000, $00AA, $0000, $0055, $0000),
   ($00AA, $0055, $00AA, $0055, $00AA, $0055, $00AA, $0055),
   ($00FF, $0055, $00FF, $00AA, $00FF, $0055, $00FF, $00AA),
   ($00FF, $0077, $00FF, $00DD, $00FF, $0077, $00FF, $00DD),
   ($00FF, $0077, $00FF, $00FF, $00FF, $00DD, $00FF, $00FF),
   ($00FF, $0000, $0000, $00FF, $00FF, $0000, $0000, $00FF),
   ($0066, $0066, $0066, $0066, $0066, $0066, $0066, $0066),
   ($0033, $0099, $00CC, $0066, $0033, $0099, $00CC, $0066),
   ($0099, $0033, $0066, $00CC, $0099, $0033, $0066, $00CC),
   ($0033, $0033, $00CC, $00CC, $0033, $0033, $00CC, $00CC),
   ($0011, $0011, $0044, $0044, $0011, $0011, $0044, $0044),
   ($00FF, $0000, $00FF, $00FF, $00FF, $0000, $00FF, $00FF),
   ($0077, $0077, $0077, $0077, $0077, $0077, $0077, $0077),
   ($0077, $00BB, $00DD, $00EE, $0077, $00BB, $00DD, $00EE),
   ($00BB, $0077, $00EE, $00DD, $00BB, $0077, $00EE, $00DD),
   ($0077, $0000, $0077, $0077, $0077, $0000, $0077, $0077),
   ($00AA, $0077, $00AA, $00DD, $00AA, $0077, $00AA, $00DD));

function ColorToRGB(const AColor: TColor): Integer;
begin
  {$IFDEF WINDOWS}
  Result := Graphics.ColorToRGB(AColor)
  {$ELSE}
  Result := QGraphics.ColorToRGB(AColor);
  {$ENDIF}
end;

function RectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
  if Result < 0 then
    Result := 0;
end;

function RectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
  if Result < 0 then
    Result := 0;
end;

{$IFNDEF VCL}
function RGBToBGR(AColor: Integer): Integer;
begin
  with TRGB(AColor) do
    Result := RGB(B, G, R);
end;
{$ENDIF}

{$IFNDEF WINDOWS}
procedure ValidTextRect(ACanvas: TCanvas; X, Y: Integer; const AClipRect: TRect;
  const AText: WideString; HorzAlign: TcxHorzTextAlign);
var
  AClipRgn: QRegionH;
  ARectRgn: QRegionH;
  AHandle: QPainterH;
  ARect: TRect;
const
  ATextAlign: array[TcxHorzTextAlign] of Integer =
    (cxAlignLeft, cxAlignLeft, cxAlignCenter, cxAlignRight, cxAlignLeft, cxAlignLeft);
begin
  ARect := AClipRect;
  ARect.Top := Y;
  if Pointer(AText) <> nil then
  begin
    TCanvasAccess(ACanvas).RequiredState([csFontValid, csHandleValid]);
    TCanvasAccess(ACanvas).Start;
    AHandle := ACanvas.Handle;
    AClipRgn := QPainter_clipRegion(AHandle);
    if QRegion_IsEmpty(AClipRgn) then
      AClipRgn := QRegion_Create(@cxMaxViewRect, QRegionRegionType_Rectangle)
    else
      AClipRgn := QRegion_Create(AClipRgn);
    try
      ARectRgn := QRegion_Create(@AClipRect, QRegionRegionType_Rectangle);
      try
        QRegion_Intersect(AClipRgn, ARectRgn, ARectRgn);
        if not (QRegion_IsEmpty(ARectRgn) or QRegion_IsNull(ARectRgn)) then
        begin
          QPainter_setClipRegion(AHandle, ARectRgn);
          QPainter_SetBackgroundMode(AHandle, BGMODE_TRANSPARENTMODE);
          QPainter_DrawText(AHandle, @ARect, ATextAlign[HorzAlign],
            PWideString(@AText), -1, @ARect, nil);
        end;
       finally
         QRegion_Destroy(ARectRgn);
       end;
    finally
      TCanvasAccess(ACanvas).Stop;
      QPainter_setClipRegion(AHandle, AClipRgn);
      QRegion_Destroy(AClipRgn);
    end;
  end;
end;
{$ENDIF}

{ TcxCanvasWrapper }
constructor TcxCanvasWrapper.Create(APalette: PcxExcelPalette);
begin
  HandlesNeeded;
  FPalette := APalette;
  FSelectionColor := GetNativeColor(clHighLight);
  WindowColor := GetNativeColor(clWindow);
  PaletteChanged;
  BorderColor := clBtnFace;
end;

destructor TcxCanvasWrapper.Destroy;
begin
  ReleaseHandles;
  inherited Destroy;
end;

procedure TcxCanvasWrapper.BeginPaint(ACanvas: TCanvas);

{$IFDEF WINDOWS}
  function GetHandle(AHandle: HDC): HDC; overload;
  begin
    Result := AHandle;
  end;
  {$IFDEF WINCLX}
  function GetHandle(AHandle: QPainterH): HDC; overload;
  begin
    Result := QPainter_Handle(AHandle)
  end;
  {$ENDIF}
{$ELSE}
  function GetHandle(AHandle: QPainterH): QPainterH;
  begin
    Result := AHandle
  end;
{$ENDIF}
begin
  if ACanvas <> nil then
    FPainterHandle := GetHandle(ACanvas.Handle)
  else
    FPainterHandle := EmptyHandle;
  FCanvas := ACanvas;
  SetBkMode(bmTransparent);
  {$IFDEF WINDOWS}
  Windows.SetTextColor(FPainterHandle, ColorToRGB(FTextColor));
  Windows.SetBkColor(FPainterHandle, ColorToRGB(FBkColor));
  SelectObject(FPainterHandle, FFontHandle);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.BeginPaint(APalette: PcxExcelPalette; ACanvas: TCanvas);
begin
  if APalette <> nil then
  begin
    FPalette := APalette;
    PaletteChanged;
  end;
  BeginPaint(ACanvas);
end;

procedure TcxCanvasWrapper.CalculateTextExtents(const AText: TcxString;
  const ATextRect: TRect; AHorzAlign: TcxHorzTextAlign; AVertAlign: TcxVertTextAlign;
  AWordBreak: Boolean; var ATextParams: TcxTextParameters);
  
begin
  with ATextParams do
  begin
    WordBreak := AWordBreak;
    HorzAlign := AHorzAlign;
    VertAlign := AVertAlign;
    FontHandle := FFontHandle;
    case AHorzAlign of
      haGENERAL, haLEFT, haFILL, haJUSTIFY:
        XPos := ATextRect.Left + 3;
      haCENTER:
        XPos := (ATextRect.Left + ATextRect.Right) shr 1;
      haRIGHT:
        XPos := ATextRect.Right - 3;
    end;
    if (AHorzAlign = haFill) or (not AWordBreak) then
    begin
       SetLength(TextBricks, 1);
       with TextBricks[0] do
       begin
         case AVertAlign of
          vaTOP, vaJUSTIFY:
            YPos := ATextRect.Top;
          vaCENTER:
            YPos := (ATextRect.Bottom + ATextRect.Top - TextHeight(AText)) shr 1;
          vaBOTTOM:
            YPos := ATextRect.Bottom - TextHeight(AText) {$IFNDEF WINDOWS} + 2 {$ENDIF};
         end;
         Text := AText;
       end;
    end
    else
      if AWordBreak then
        SplitToTextBricks(AText, ATextRect, AHorzAlign, AVertAlign, TextBricks);
   end;
end;

procedure TcxCanvasWrapper.DrawText(const AText: TcxString; const ARect: TRect);
{$IFDEF WINDOWS}
var
  H: Integer;
{$ENDIF}
begin
  if not RectIsVisible(ARect) then Exit;
  SetBkMode(bmTransparent);
  {$IFDEF WINDOWS}
  SetTextAlign(FPainterHandle, TA_Bottom or TA_Center);
  H := TextHeight(AText);
  with ARect do
    ExtTextOut(FPainterHandle, (Right + Left) shr 1, (Top + Bottom + H) shr 1,
      ETO_CLIPPED, @ARect, PChar(AText), Length(AText), nil);
  {$ELSE}
  Canvas.Font.Color := FTextColor;
  ValidTextRect(Canvas, ARect.Left, ARect.Top, ARect, AText, haCenter);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.DrawTriangleGlyph(const ARect: TRect; AColor: TColor;
  ALeftToRight: Boolean; IsDrawLine: Boolean);
var
  H, W: Integer;
  AOfs, AWOfs: Byte;
  APolygon: array[0..2] of TPoint;
  I: Integer;
  AR: TRect;

const
  AOffset: array[Boolean] of ShortInt = (-1, 0);

begin
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  AOfs := (H div 3) shr 1 - 1;
  H := H div 3;
  AWOfs := (W - (H + 2)) shr 1;
  APolygon[0] := Point(AWOfs, AOfs);
  APolygon[1] := Point(AWOfs, AOfs + H * 2 + 2);
  APolygon[2] := Point(AWOfs + H + 1, AOfs + H + 1);
  for I := 0 to 2 do
  begin
    Inc(APolygon[I].Y, ARect.Top);
    if ALeftToRight then
      Inc(APolygon[I].X, ARect.Left)
    else
      APolygon[I].X := ARect.Right - APolygon[I].X - 2;
    if IsDrawLine then
      if ALeftToRight then
        Dec(APolygon[I].X, 1)
      else
        Inc(APolygon[I].X, 1);
  end;
  if IsDrawLine then
  begin
    AR := Rect(APolygon[2].X, APolygon[0].Y + 1,  APolygon[2].X, APolygon[1].Y - 1);
    OffsetRect(AR, AOffset[ALeftToRight], 0);
    FillRect(AR, fsSolid, clBlack, clBlack);
  end;
  Canvas.Pen.Style := psClear;
  Polygon(APolygon, clBlack);
end;

procedure TcxCanvasWrapper.EndPaint;
begin
  FCanvas := nil;
  {$IFDEF WINDOWS}
  FFontHandle := EmptyHandle;
  {$ENDIF}
end;

procedure TcxCanvasWrapper.ExcludeClipRect(const ARect: TRect);
begin
  with ARect do
    ExcludeClipRect(Left, Top, Right, Bottom);
end;

procedure TcxCanvasWrapper.ExcludeClipRect(X1, Y1, X2, Y2: Integer);
{$IFNDEF WINDOWS}
var
  ClipRgn, Rgn: QRegionH;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Windows.ExcludeClipRect(FPainterHandle, X1, Y1, X2 + 1, Y2 + 1);
  {$ELSE}
  ClipRgn := QPainter_clipRegion(FPainterHandle);
  if QRegion_isEmpty(ClipRgn) or QRegion_isNull(ClipRgn) then
    ClipRgn := QRegion_create(0, 0, MaxInt div 20, MaxInt div 20,
      QRegionRegionType_Rectangle);
  Rgn := QRegion_create(X1, Y1, X2 - X1 + 1, Y2 - Y1 + 1,
    QRegionRegionType_Rectangle);
  QRegion_subtract(ClipRgn, ClipRgn, Rgn);
  QRegion_destroy(Rgn);
  QPainter_setClipRegion(FPainterHandle, ClipRgn);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.ExcludeClipRgn(const APoints: array of TPoint);
var
{$IFDEF WINDOWS}
  ARegion: TcxRegionHandle;
{$ELSE}
  ClipRgn, Rgn: QRegionH;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  ARegion := CreatePolygonRgn(APoints, Length(APoints), Alternate);
  Windows.ExtSelectClipRgn(FPainterHandle, ARegion, RGN_DIFF);
  DeleteObject(ARegion);
  {$ELSE}
  ClipRgn := QPainter_clipRegion(FPainterHandle);
  if QRegion_isEmpty(ClipRgn) or QRegion_isNull(ClipRgn) then
    ClipRgn := QRegion_create(0, 0, MaxInt div 20, MaxInt div 20,
      QRegionRegionType_Rectangle);
  Rgn := QRegion_create(@APoints, True);
  QRegion_subtract(ClipRgn, ClipRgn, Rgn);
  QRegion_destroy(Rgn);
  QPainter_setClipRegion(FPainterHandle, ClipRgn);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.ExDrawText(const AClipRect: TRect;
 const AextTextParams: TcxTextParameters);
var
  I: Integer;
const
  AHorzTextAlign: array[TcxHorzTextAlign] of Word =
  {$IFDEF WINDOWS}
    (TA_LEFT, TA_LEFT, TA_CENTER, TA_RIGHT, TA_LEFT, TA_LEFT);
  {$ELSE}
    (cxAlignLeft, cxAlignLeft, cxAlignHCenter, cxAlignRight, cxAlignLeft, cxAlignLeft);
  {$ENDIF}
begin
  if not RectIsVisible(AClipRect) then Exit;
  if Length(AextTextParams.TextBricks) > 0 then
  begin
    with AextTextParams do
    begin
      SelectFont(FontHandle);
      if FontColor <> $FFFF then
        SetTextColor(FontColor, False, poFont);
      SetBkMode(bmTransparent);
      {$IFDEF WINDOWS}
      SetTextAlign(FPainterHandle, AHorzTextAlign[HorzAlign] or TA_TOP);
      for I := 0 to Length(TextBricks) - 1 do
       if Length(TextBricks[I].Text) > 0 then
          with TextBricks[I] do
          begin
            if HorzAlign = haJustify then
              SetTextJustification(FPainterHandle, BreakExtra, BreakCount);
            ExtTextOut(FPainterHandle, XPos, YPos - 2, ETO_CLIPPED, @AClipRect,
              @Text[1], Length(Text), nil);
          end;
       if HorzAlign = haJustify then
         SetTextJustification(FPainterHandle, 0, 1);
      {$ELSE}
      Canvas.Font.Color := FTextColor;
      for I := 0 to Length(TextBricks) - 1 do
       if Length(TextBricks[I].Text) > 0 then
          with TextBricks[I] do
            ValidTextRect(Canvas, XPos, YPos - 2, AClipRect, Text, HorzAlign);
      {$ENDIF}
    end;
  end;
end;

procedure TcxCanvasWrapper.FillRect(const ARect: TRect; AStyle: TcxSSFillStyle;
  const ABkColor, AFgColor: TColor);
begin
  if RectIsVisible(ARect) then
  begin
    SetBkColor(ColorToRgb(ABkColor));
    SetTextColor(ColorToRgb(AFgColor));
    Rectangle(ARect, DefaultBrushes[AStyle]);
  end;
end;

procedure TcxCanvasWrapper.FillRect(const ARect: TRect; AStyle: TcxSSFillStyle;
  const ABkColor, AFgColor: Word; IsSelected: Boolean);
begin
  SetBkColor(ABkColor, IsSelected, poBrush);
  if AFgColor < 55 then
    SetTextColor(AFgColor, IsSelected, poBrush)
  else
  begin
    if IsSelected then
      SetTextColor(MixColors(SelectionColor, clBlack))
    else
      SetTextColor(clBlack);
  end;
  {$IFDEF LINUX}
  QBrush_SetColor(DefaultBrushes[AStyle], QColor(FTextColor));
  SetBkMode(bmOpaque);
  {$ENDIF}
  Rectangle(ARect, DefaultBrushes[AStyle]);
end;

procedure TcxCanvasWrapper.FrameRect(const ARect: TRect; AColor: TColor;
  IsSelected: Boolean = False);
begin
  if not RectIsVisible(ARect) then Exit;
  with ARect do
  begin
    if IsSelected then
      AColor := MixColors(FSelectionColor, AColor);
    {$IFDEF WINDOWS}
    AColor := Windows.SetBkColor(FPainterHandle, ColorToRgb(AColor));
    Windows.FrameRect(FPainterHandle,
      Rect(Left, Top, Right + 1, Bottom + 1),  DefaultBrushes[fsSolid]);
    Windows.SetBkColor(FPainterHandle, AColor);
    {$ELSE}
    FrameRect(ARect, AColor, AColor, False)
    {$ENDIF}
  end;
end;

procedure TcxCanvasWrapper.FrameRect(const ARect: TRect;
  const ATopColor, ABottomColor: TColor; IsSelected: Boolean = False);
{$IFDEF WINDOWS}
var
  AC: Integer;
{$ENDIF}

begin
  if not RectIsVisible(ARect) then Exit;
  with ARect do
  begin
    {$IFDEF WINDOWS}
    AC := Windows.SetBkColor(FPainterHandle, ColorToRgb(ATopColor));
    Windows.FillRect(FPainterHandle,
      Rect(Left, Top, Right, Top + 1), DefaultBrushes[fsSolid]);
    Windows.FillRect(FPainterHandle,
      Rect(Left, Top, Left + 1, Bottom + 1), DefaultBrushes[fsSolid]);
    Windows.SetBkColor(FPainterHandle, ColorToRgb(ABottomColor));
    Windows.FillRect(FPainterHandle,
      Rect(Right, Top, Right + 1, Bottom + 1), DefaultBrushes[fsSolid]);
    Windows.FillRect(FPainterHandle,
      Rect(Left + 1, Bottom, Right, Bottom + 1), DefaultBrushes[fsSolid]);
    Windows.SetBkColor(FPainterHandle, ColorToRgb(AC));
    {$ELSE}
    SetBkMode(bmOpaque);
    QPainter_SetBackgroundColor(FPainterHandle, QColor(ColorToRGB(ATopColor)));
    QPainter_FillRect(FPainterHandle, Left, Top,
      Right - Left, 1, DefaultBrushes[fsSolid]);
    QPainter_FillRect(FPainterHandle, Left, Top, 1,
      Bottom - Top + 1, DefaultBrushes[fsSolid]);
    QPainter_SetBackgroundColor(FPainterHandle, QColor(ColorToRGB(ABottomColor)));
    QPainter_FillRect(FPainterHandle, Right, Top, 1,
      Bottom - Top + 1, DefaultBrushes[fsSolid]);
    QPainter_FillRect(FPainterHandle, Left + 1, Bottom,
      Right - Left, 1, DefaultBrushes[fsSolid]);
    SetBkMode(bmTransparent);
    {$ENDIF}
  end;
end;

class function TcxCanvasWrapper.GetNativeColor(const AColor: TColor): Integer;
begin
  {$IFDEF VCL}
  Result := Graphics.ColorToRgb(AColor);
  {$ELSE}
  Result := QGraphics.ColorToRgb(AColor);
  if AColor < 0 then
    Result := RGBToBGR(Result)
  else
    Result := Result and $FFFFFF;
   {$ENDIF}
end;

function TcxCanvasWrapper.GetTextExtent(const AText: TcxString): TPoint;
{$IFDEF WINDOWS}
var
  DC: TcxPainterHandle;
{$ENDIF}

begin
  FillChar(Result, SizeOf(Result), 0);
  {$IFDEF WINDOWS}
  DC := GetDC(0);
  try
    FFontHandle := SelectObject(DC, FFontHandle);
    GetTextExtentPoint32(DC, @AText[1], Length(AText), TSize(Result));
    FFontHandle := SelectObject(DC, FFontHandle);
  finally
    ReleaseDC(0, DC);
  end;
  {$ELSE}
  if CheckHandle(Canvas) then
  begin
    SelectFont(FFontHandle);
    Result := TPoint(Canvas.TextExtent(AText));
  end;
  {$ENDIF}
end;

procedure TcxCanvasWrapper.InvertRect(const ARect: TRect);
begin
  {$IFDEF WINDOWS}
  with ARect do
    Windows.InvertRect(FPainterHandle, Rect(Left, Top, Right, Bottom));
  {$ELSE}
  with ARect do
    FillRect(Rect(Left, Top, Right - 1, Bottom - 1), fsSolid, clBlack, clBlack);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.Line(const AVertex: TRect; AStyle: TcxSSEdgeLineStyle;
  AOrientation: TcxOrientation; ABkColor, AFgColor: TColor);
begin
  SetBkColor(ABkColor);
  SetTextColor(AFgColor);
  DrawLine(AVertex, AStyle, AOrientation);
end;

procedure TcxCanvasWrapper.Line(const AVertex: array of TRect; AStyle: TcxSSEdgeLineStyle;
  AOrientation: TcxOrientation; ABkColor, AFgColor: Word; IsSelected: Boolean);
begin
  if AStyle = lsDefault then
    SetTextColor(BorderColor)
  else
  begin
    if AFgColor > 55 then
      SetTextColor(GetNativeColor(clWindowText), IsSelected, poPen)
    else
      SetTextColor(AFgColor, IsSelected, poPen);
    SetBkColor(ABkColor, IsSelected, poBrush);
  end;
  DrawLine(AVertex, AStyle, AOrientation);
end;

procedure TcxCanvasWrapper.PaletteChanged;
var
  I: Integer;
begin
  for I := 0 to High(TcxExcelPalette) do
    FHighLightPalette[I] := MixColors(FSelectionColor, Palette^[I]);
  for I := 0 to Byte(High(FDefColors[False])) do
   FDefColors[True, TcxPaintObjects(I)] :=
     MixColors(FSelectionColor, FDefColors[True, TcxPaintObjects(I)]);
end;

procedure TcxCanvasWrapper.Polygon(const APoints: array of TPoint; const AColor: TColor);
begin
  SetTextColor(AColor);
  SetBkColor(AColor);
  Canvas.Brush.Color := AColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Polygon(APoints {$IFNDEF VCL}, True, 0, Length(APoints) {$ENDIF});
end;

procedure TcxCanvasWrapper.Rectangle(const ARect: TRect; const ABrush: TcxFillBrushHandle);
begin
  if not RectIsVisible(ARect) then Exit;
  {$IFDEF WINDOWS}
  with ARect do
    Windows.FillRect(FPainterHandle, Rect(Left, Top, Right + 1, Bottom + 1), ABrush);
  {$ELSE}
  SetBkMode(bmOpaque);
  QBrush_SetColor(ABrush, QColor(FTextColor));
  QPainter_SetBackgroundColor(FPainterHandle, QColor(FBkColor));
  with ARect do
    QPainter_FillRect(FPainterHandle, Left, Top, Right - Left + 1, Bottom - Top + 1, ABrush);
  SetBkMode(bmTransparent);
  {$ENDIF}
end;

function TcxCanvasWrapper.RectIsVisible(ARect: TRect): Boolean;
begin
{$IFDEF WINDOWS}
  Result := True;
{$ELSE}
  Result := QPainter_HasClipping(FPainterHandle);
  InflateRect(ARect, 1, 1); 
  if Result then
    Result := QRegion_Contains(QPainter_ClipRegion(FPainterHandle), PRect(@ARect));
{$ENDIF}
end;

procedure TcxCanvasWrapper.SetBkMode(AMode: TcxBackgroundMode);
begin
  FBkMode := AMode;
{$IFDEF WINDOWS}
  Windows.SetBkMode(FPainterHandle, Byte(AMode));
{$ELSE}
  if CheckHandle(FPainterHandle) then
    QPainter_SetBackgroundMode(FPainterHandle, BGMode(AMode));
{$ENDIF}
end;

procedure TcxCanvasWrapper.SetSingleText(const AText: TcxString; const ATextRect: TRect;
  var ATextSettings: TcxTextParameters; AVertCenterAlign: Boolean = True);
begin
  with ATextSettings do
  begin
    SetLength(TextBricks, 1);
    TextBricks[0].Text := AText;
    FontHandle := FFontHandle;
    HorzAlign := haCenter;
    if not AVertCenterAlign then
      VertAlign := vaBottom
    else
      VertAlign := vaCenter;
    XPos := (ATextRect.Left + ATextRect.Right) shr 1;
    if AVertCenterAlign then
      TextBricks[0].YPos :=
        (ATextRect.Bottom + ATextRect.Top - TextHeight(AText)) shr 1
    else
      TextBricks[0].YPos :=
        (ATextRect.Bottom - TextHeight(AText)) {$IFNDEF WINDOWS} + 2 {$ENDIF};
  end;
end;

procedure TcxCanvasWrapper.SelectFont(const AFont: TcxFontHandle;
  ACheckSettings: Boolean = True);
begin
  {$IFDEF WINDOWS}
  if (not ACheckSettings) or (FFontHandle <> AFont) then
  begin
    FFontHandle := AFont;
    SelectObject(FPainterHandle, FFontHandle);
  end;
  {$ELSE}
  FFontHandle := AFont;
  if Canvas <> nil then
  begin
    with Canvas.Font do
    begin
      Weight := fwNormal;
      Size := FFontHandle.Size;
      Charset := FFontHandle.Charset;
      Style := FFontHandle.Style;
      Name := FFontHandle.Name;
    end;
  end;
  {$ENDIF}
end;

procedure TcxCanvasWrapper.SelectFont(AFont: TFont);
begin
  SetTextColor(GetNativeColor(AFont.Color));
  {$IFDEF WINDOWS}
  FFontHandle :=
    {$IFNDEF WINCLX} AFont.Handle {$ELSE} QFont_Handle(AFont.Handle) {$ENDIF};
  SelectObject(FPainterHandle, FFontHandle);
  {$ELSE}
  if CheckHandle(Canvas) then
  begin
    Canvas.Font.Assign(AFont);
    FFontHandle.Name := AFont.Name;
    FFontHandle.Size := AFont.Size;
    FFontHandle.Style := AFont.Style;
    FFontHandle.Charset := AFont.Charset;
  end;
  {$ENDIF}
end;

{$IFDEF WINCLX}
procedure TcxCanvasWrapper.SelectFont(const AFont: QFontH);
begin
  SelectFont(QFont_handle(AFont));
end;
{$ENDIF}

procedure TcxCanvasWrapper.SelectFont(const AFont: TcxFontHandle; AColor: Word);
begin
  SelectFont(AFont);
  SetTextColor(AColor, False, poFont)
end;

procedure TcxCanvasWrapper.SetBkColor(AColor: TColor);
begin
  FBkColor := ColorToRgb(AColor);
  {$IFDEF WINDOWS}
  Windows.SetBkColor(FPainterHandle, FBkColor);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.SetBkColor(AColor: Word;
  IsSelected: Boolean; AObject: TcxPaintObjects);
var
  AColorValue: TColor;
begin
  if AColor <= 55 then
  begin
    if not IsSelected then
      AColorValue := Palette^[AColor]
    else
      AColorValue := FHighLightPalette[AColor];
  end
  else
    AColorValue := FDefColors[IsSelected, AObject];
  SetBkColor(AColorValue);
end;

procedure TcxCanvasWrapper.SetTextColor(AColor: TColor);
begin
  FTextColor := ColorToRgb(AColor);
  {$IFDEF WINDOWS}
  Windows.SetTextColor(FPainterHandle, FTextColor);
  {$ENDIF}
end;

procedure TcxCanvasWrapper.SetTextColor(AColor: Word;
  IsSelected: Boolean; AObject: TcxPaintObjects);
var
  AColorValue: TColor;
begin
  if AColor <= 55 then
  begin
    if not IsSelected then
      AColorValue := Palette^[AColor]
    else
      AColorValue := FHighLightPalette[AColor]
  end
  else
    AColorValue := FDefColors[IsSelected, AObject];
  SetTextColor(AColorValue);
end;

function TcxCanvasWrapper.TextWidth(const AText: TcxString): Integer;
begin
  Result := GetTextExtent(AText).X;
end;

function TcxCanvasWrapper.TextHeight(const AText: TcxString): Integer;
begin
  Result := GetTextExtent(AText).Y;
end;

class function TcxCanvasWrapper.MixColors(ASelColor, ASrcColor: TColor): TColor;

  function GetLightValue(ASrcValue, ASelValue: Byte): Integer;
  const
    Alfa = $5C;
    Alfa2 = $FF - $5C;
  begin
    Result := Round((ASelValue * Alfa + ASrcValue * Alfa2) / 255);
  end;

begin
  ASrcColor := ColorToRGB(ASrcColor);
  ASelColor := ColorToRGB(ASelColor);
  Result := GetLightValue(GetRValue(ASrcColor), GetRValue(ASelColor)) or
   (GetLightValue(GetGValue(ASrcColor), GetGValue(ASelColor)) shl 8) or
      (GetLightValue(GetBValue(ASrcColor), GetBValue(ASelColor)) shl 16);
end;

function TcxCanvasWrapper.CheckHandle(AHandle: Integer): Boolean;
begin
  Result := AHandle <> 0;
end;

function TcxCanvasWrapper.CheckHandle(AHandle: TObject): Boolean;
begin
  Result := AHandle <> nil;
end;

procedure TcxCanvasWrapper.DrawLine(const AVertex: array of TRect;
  AStyle: TcxSSEdgeLineStyle; AOrientation: TcxOrientation);
var
  ARect: TRect;
  I: Byte;
const
  AOffset: array[TcxOrientation, 0..1] of Byte = ((0, 1), (1, 0));
begin
  case AStyle of
    lsDefault, lsThin, lsDashed, lsDotted, lsThick, lsHair, lsMediumDashed,
    lsMedium, lsDashDot, lsMediumDashDot, lsDashDotDot, lsMediumDashDotDot, lsNone:
      Rectangle(AVertex[0], DefaultPens[Byte(AStyle), AOrientation]);
    lsDouble:
    begin
      if Length(AVertex) <> 1 then
        for I := 0 to 2 do
          Rectangle(AVertex[I], APenBrushes[Byte(I = 1), AOrientation])
      else
      begin
        ARect := AVertex[0];
        if AOrientation = oHorz then
          ARect.Bottom := ARect.Top
        else
          ARect.Right := ARect.Left;
        for I := 0 to 2 do
        begin
          OffsetRect(ARect, AOffset[AOrientation, 0], AOffset[AOrientation, 1]);
          Rectangle(ARect, APenBrushes[Byte(I = 1), AOrientation])
        end;
      end;
    end;
    lsSlantedDashDot:
    begin
      if AOrientation = oHorz then
        with AVertex[0] do
          ARect := Rect(Left, Top, Right, Top)
      else
        with AVertex[0] do
          ARect := Rect(Left, Top, Left, Bottom);
      Rectangle(ARect, DefaultPens[Byte(AStyle),  AOrientation]);
      OffsetRect(ARect, AOffset[AOrientation, 0], AOffset[AOrientation, 1]);
      Rectangle(ARect, APenBrushes[9, AOrientation]);
    end;
  end;
end;

function TcxCanvasWrapper.GetWordFromPos(const AString: TcxString;
  var APos: Integer): TcxString;

  function IsDelimiterChar: Boolean;
  begin
    Result := (APos <= Length(AString)) and (Byte(AString[APos]) <= 32);
  end;

var
  IsDelimiterStr: Boolean;
  AStartPos: Integer;
begin
  AStartPos := APos;
  IsDelimiterStr := IsDelimiterChar;
  while (APos <= Length(AString)) and (IsDelimiterChar = IsDelimiterStr) do
  begin
    if Byte(AString[APos]) in [9, 10, 13] then Break;
    Inc(APos);
  end;
  Result := Copy(AString, AStartPos, APos - AStartPos);
end;

procedure TcxCanvasWrapper.HandlesNeeded;
begin
  CreateBrushStyles;
  CreatePenStyles;
end;

procedure TcxCanvasWrapper.SplitToTextBricks(const AText: TcxString; ARect: TRect;
  HAlign: TcxHorzTextAlign; VAlign: TcxVertTextAlign; var ATextBricks: TWordExtents);
var
  AHeight, AWidth: Integer;
  AWordCount, APos, LineHeight, DY: Integer;
  {$IFDEF WINDOWS}
  DC: TcxPainterHandle;
  AFont: HFont;
  {$ENDIF}

  {$IFNDEF WINDOWS}
  function Copy(const AString: TcxString; StartPos, StrLen: Integer): TcxString;
  begin
    SetLength(Result, StrLen);
    Move(AString[StartPos], Result[1], StrLen shl 1);
  end;
  {$ENDIF}

  function ExTextExtent(StrPtr: PcxString; StrLen: Integer): TSize;
  {$IFNDEF WINDOWS}
  var
     W: Widestring;
  {$ENDIF}
  begin
    {$IFDEF WINDOWS}
    GetTextExtentPoint32(DC, StrPtr, StrLen, Result);
    {$ELSE}
    SetLength(W, StrLen);
    Move(StrPtr^, W[1], StrLen shl 1);
    Result := TSize(GetTextExtent(W));
    {$ENDIF}
  end;

  procedure AdjustWords(var AText: TcxString);
  var
    APos: Integer;
  begin
    APos := Pos(' ', AText);
    if APos > 0 then
    begin
      while ExTextExtent(@AText[1], Length(AText)).cx <= AWidth do
      begin
        while AText[APos] <> ' ' do
        begin
          Inc(APos);
          if APos >= Length(AText) then
            APos := 1;
        end;
        while (AText[APos] = ' ') and (APos < Length(AText)) do
          Inc(APos);
        Insert(' ', AText, APos);
        if APos < Length(AText) then
          Inc(APos)
        else
          APos := 1;
      end;
    end
    else
      while TextWidth(AText) <= AWidth do AText := AText + ' ';
  end;

  procedure ScanWord(const AText: TcxString; var StartPos: Integer; var TextBrick: TcxTextBrick);
  var
    I: Integer;
    BreakPos: Integer;
    Count: Integer;
    W: Integer;
    EndPos: Integer;
  begin
    BreakPos := 0;
    TextBrick.Text := '';
    Count := 0;
    EndPos := StartPos;
    for I := StartPos to Length(AText) do
    begin
      Inc(EndPos);
      if (AText[I] = #10) then
        Break
      else
      begin
        Inc(Count);
        if AText[I] = ' ' then
           BreakPos := Count;
        W := ExTextExtent(@AText[StartPos], Count).CX;
        if W > AWidth then
        begin
          if (I > BreakPos) and (Count > 1) then
          begin
            Dec(Count);
            Dec(EndPos);
          end;
          if BreakPos <> 0 then
          begin
            Count := BreakPos;
            EndPos := StartPos + Count;
          end;
          Break;
        end;
      end;
    end;
    while (Count > 0) and (AText[StartPos + Count] = ' ') do Dec(Count);
    begin
      with TextBrick do
      begin
        TextBrick.Text := Copy(AText, StartPos, Count);
        if HAlign = haJustify then
        begin
          {$IFDEF WINDOWS}
          BreakCount := 0;
          BreakExtra := 1;
          for I := 1 to Length(Text) do
            if Text[I] = ' ' then Inc(BreakCount);
          if BreakCount > 0 then
          begin
            while AWidth > ExTextExtent(@AText[StartPos], Count).CX do
            begin
              Inc(BreakExtra);
              SetTextJustification(DC, BreakExtra, BreakCount);
            end;
          end;
          {$ELSE}
          AdjustWords(TextBrick.Text)
          {$ENDIF}
        end;
      end;
      StartPos := EndPos;
    end;
  end;

begin
  {$IFDEF WINDOWS}
  DC := GetDC(0);
  AFont := SelectObject(DC, FFontHandle);
  {$ENDIF}
  AWordCount := 0;
  try
    AWidth := ARect.Right - ARect.Left - 2;
    AHeight := ARect.Bottom - ARect.Top - 2;
    APos := 1;
    LineHeight := TextHeight('Wg');
    while APos <= Length(AText) do
    begin
      Inc(AWordCount);
      SetLength(ATextBricks, AWordCount);
      ScanWord(AText, APos, ATextBricks[AWordCount - 1]);
      ATextBricks[AWordCount - 1].YPos := LineHeight * (AWordCount - 1) + ARect.Top;
    end;
  finally
    {$IFDEF WINDOWS}
    SelectObject(DC, AFont);
    ReleaseDC(0, DC);
    {$ENDIF}
  end;
  if AWordCount > 0 then
  begin
    if (VAlign <> vaTop) and (AWordCount * LineHeight < AHeight) then
    begin
      case VAlign of
        vaCENTER:
        begin
          DY := (AHeight - AWordCount * LineHeight) shr 1;
          for APos := 0 to AWordCount - 1 do
            Inc(ATextBricks[APos].YPos, DY);
        end;
        vaBOTTOM:
        begin
          for APos := 0 to AWordCount - 1 do
            ATextBricks[APos].YPos := ARect.Bottom - (AWordCount - APos) * LineHeight;
        end;
        vaJUSTIFY:
        if AWordCount > 1 then
        begin
          DY := Round((AHeight - (AWordCount * LineHeight)) / (AWordCount - 1));
          for APos := 1 to AWordCount - 1 do
            Inc(ATextBricks[APos].YPos, DY * APos);
        end;
      end;
    end;
  end;
//  2.1746082936777702243371855880354e-4
end;

procedure TcxCanvasWrapper.ReleaseHandles;
begin
  RemoveBrushStyles;
  RemovePenStyles;
end;

class procedure TcxCanvasWrapper.CreateBrushStyles;

  {$IFNDEF WINDOWS}
  procedure LX_CreatePatterns;
  var
    I: TcxSSFillStyle;
    J, K: Integer;
    ABrush: TBrush;
    ABitmap: TBitmap;
  begin
    for I := Low(TcxSSFillStyle) to High(TcxSSFillStyle) do
    begin
      ABitmap := TBitmap.Create;
      ABitmap.Width := 8;
      ABitmap.Height := 8;
      ABitmap.Monochrome := True;
      for J := 0 to 7 do
        for K := 0 to 7 do
        begin
          if ((BrushPatterns[I, J] and (1 shl K)) <> 0) then
            ABitmap.Canvas.Pen.Color := ColorToRGB(clBlack)
          else
            ABitmap.Canvas.Pen.Color := ColorToRGB(clWhite);
          ABitmap.Canvas.DrawPoint(J, K);
        end;
      ABrush := TBrush.Create;
      ABrush.Bitmap := ABitmap;
      DefaultBrushes[I] := QBrush_Create(ABrush.Handle);
      ABitmap.Free;
      ABrush.Free;
    end;
  end;
  {$ENDIF}

{$IFDEF WINDOWS}
var
  I: TcxSSFillStyle;
  ABitmap: HBitmap;
  {$ENDIF}
begin
  Inc(BrushReference);
  if BrushReference = 1 then
  begin
    {$IFDEF WINDOWS}
    for I := Low(TcxSSFillStyle) to High(TcxSSFillStyle) do
    begin
      ABitmap := CreateBitmap(8, 8, 1, 1, @BrushPatterns[I]);
      DefaultBrushes[I] := CreatePatternBrush(ABitmap);
      DeleteObject(ABitmap);
    end;
    {$ELSE}
    LX_CreatePatterns;
    {$ENDIF}
  end;
end;

class procedure TcxCanvasWrapper.CreatePenStyles;
const
  APatterns: array[0..9] of string =
  ('11111111', '00000000', '11101110',
   '11001100',  '10101010',  '111111111000',
   '111111111000111000', '111000111000111111111000',
   '011111111111011111', '111111111100111100');

  ABrushNumbers: array[0..16] of Byte =
    (0, 0, 0, 2, 3, 0, 0, 4, 5, 6, 6, 7, 7, 8, 1, 9, 1);

  {$IFDEF WINDOWS}
  procedure WN_CreatePens;
  var
    K: TcxOrientation;
    I, J, L: Integer;
    AColor: Integer;
    DC: HDC;
    ABitmap: HBitmap;
  begin
    DC := CreateCompatibleDC(0);
    for K := oHorz to oVert do
    begin
      for I := 0 to High(APatterns) do
      begin
        if K = oHorz then
          ABitmap := CreateBitmap(Length(APatterns[I]), 8, 1, 1, nil)
        else
          ABitmap := CreateBitmap(8, Length(APatterns[I]), 1, 1, nil);
        ABitmap := SelectObject(DC, ABitmap);
        for L := 0 to 8 do
        begin
          for J := 0 to Length(APatterns[I]) - 1 do
          begin
            if APatterns[I][J + 1] = '1' then
              AColor := ColorToRGB(clBlack)
            else
              AColor := ColorToRGB(clWhite);
            if K = oHorz then
              SetPixel(DC, J, L, AColor)
            else
              SetPixel(DC, L, J, AColor);
          end;
        end;
        ABitmap := SelectObject(DC, ABitmap);
        APenBrushes[I, K] := CreatePatternBrush(ABitmap);
        DeleteObject(ABitmap);
      end;
    end;
    DeleteDC(DC);
  end;
  {$ELSE}
  procedure LX_CreatePens;

    function CreateBitmap(AWidth, AHeight: Integer): TBitmap;
    begin
      Result := TBitmap.Create;
      with Result do
      begin
        Width := AWidth;
        Height := AHeight;
        Monochrome := True;
      end;
    end;

  var
    ABrush: TBrush;
    ABitmap: TBitmap;
    K: TcxOrientation;
    I, J, L: Integer;


  begin
    for K := oHorz to oVert do
    begin
      for I := 0 to High(APatterns) do
      begin
        if K = oHorz then
          ABitmap := CreateBitmap(Length(APatterns[I]), 8)
        else
          ABitmap := CreateBitmap(8, Length(APatterns[I]));
        for L := 0 to 8 do
        begin
          for J := 0 to Length(APatterns[I]) - 1 do
          begin
            if APatterns[I][J + 1] = '1' then
              ABitmap.Canvas.Pen.Color := ColorToRGB(clWhite)
            else
              ABitmap.Canvas.Pen.Color := ColorToRGB(clBlack);
            if K = oHorz then
              ABitmap.Canvas.DrawPoint(J, L)
            else
              ABitmap.Canvas.DrawPoint(L, J)
          end;
        end;
        ABrush := TBrush.Create;
        ABrush.Bitmap := ABitmap;
        APenBrushes[I, K] := QBrush_Create(ABrush.Handle);
        ABitmap.Free;
        ABrush.Free;
      end;
    end;
  end;
  {$ENDIF}

var
  K: TcxOrientation;
  I: Integer;

begin
  Inc(PenReference);
  if PenReference = 1 then
  begin
    {$IFDEF WINDOWS}
    WN_CreatePens;
    {$ELSE}
    LX_CreatePens;
    {$ENDIF}
    for K := oHorz to oVert do
      for I := 0 to High(DefaultPens) do
        DefaultPens[I, K] := APenBrushes[ABrushNumbers[I], K];
  end;
end;

class procedure TcxCanvasWrapper.InitColors;
begin
  cxWhiteColor := GetNativeColor(clWhite); 
  cxBlackColor := GetNativeColor(clBlack);
  cxBtnFaceColor := GetNativeColor(clBtnFace);
  cxBtnShadowColor := GetNativeColor(clBtnShadow);
  cxBtnHighLightColor := GetNativeColor(clBtnHighLight);
end;

class procedure TcxCanvasWrapper.RemoveBrushStyles;
var
  I: TcxSSFillStyle;
begin
  Dec(BrushReference);
  if BrushReference = 0 then
    for I := Low(TcxSSFillStyle) to High(TcxSSFillStyle) do
    {$IFDEF WINDOWS}
      DeleteObject(DefaultBrushes[I]);
    {$ELSE}
      QBrush_Destroy(DefaultBrushes[I]);
    {$ENDIF}
end;

class procedure TcxCanvasWrapper.RemovePenStyles;
var
  I: Integer;
  K: TcxOrientation;
begin
  Dec(PenReference);
  if PenReference = 0 then
    for K := oHorz to oVert do
      for I := 0 to High(APenBrushes) do
      begin
        {$IFDEF WINDOWS}
        DeleteObject(APenBrushes[I, K]);
        {$ELSE}
        QBrush_Destroy(APenBrushes[I, K])
        {$ENDIF}
      end;
end;

function TcxCanvasWrapper.GetDefBorderColor: TColor;
begin
  Result := FDefColors[False, poPen]
end;

function TcxCanvasWrapper.GetDefTextColor: TColor;
begin
  Result := FDefColors[False, poFont]
end;

function TcxCanvasWrapper.GetDefWindowColor: TColor;
begin
  Result := FDefColors[False, poBrush]
end;

function TcxCanvasWrapper.GetPalette: PcxExcelPalette;
begin
  Result := FPalette; 
end;

procedure TcxCanvasWrapper.SetDefaultColor(const AValue: TColor;
  AObject: TcxPaintObjects);
begin
  FDefColors[False, AObject] := ColorToRGB(AValue);
  FDefColors[True, AObject] := MixColors(FSelectionColor, AValue);
end;

procedure TcxCanvasWrapper.SetDefBorderColor(const AValue: TColor);
begin
  SetDefaultColor(AValue, poPen);
end;

procedure TcxCanvasWrapper.SetDefTextColor(const AValue: TColor);
begin
  SetDefaultColor(AValue, poFont);
end;

procedure TcxCanvasWrapper.SetDefWindowColor(const AValue: TColor);
begin
  SetDefaultColor(AValue, poBrush);
end;

procedure TcxCanvasWrapper.SetPalette(APalette: PcxExcelPalette);
begin
  FPalette := APalette;
  PaletteChanged; 
end;

procedure TcxCanvasWrapper.SetSelectionColor(const AValue: TColor);
begin
  if FSelectionColor <> ColorToRGB(AValue) then
  begin
    FSelectionColor := AValue;
    PaletteChanged;
  end;
end;

initialization
  TcxCanvasWrapper.InitColors;
end.

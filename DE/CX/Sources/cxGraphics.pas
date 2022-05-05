
{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2003 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxGraphics;

{$I cxVer.inc}

interface

uses
{$IFDEF VCL}
  Windows,
{$ELSE}
  Types, Qt,
{$ENDIF}
  Classes, Controls, Graphics, ImgList;

const
  cxAlignLeft = 1;
  cxAlignRight = 2;
  cxAlignHCenter = 4;
  cxAlignTop = 8;
  cxAlignBottom = 16;
  cxAlignVCenter = 32;
  cxAlignCenter = 36;
  cxSingleLine = 64;
  cxDontClip = 128;
  cxExpandTabs = 256;
  cxShowPrefix = 512;
  cxWordBreak = 1024;
  cxShowEndEllipsis = 2048;
  cxDontPrint = 4096;

type
  IcxFontListener = interface
    ['{B144DD7E-0B27-439A-B908-FC3ACFE6A2D3}']
    procedure Changed(Sender: TObject; AFont: TFont);
  end;

  TcxBorder = (bLeft, bTop, bRight, bBottom);
  TcxBorders = set of TcxBorder;

const
  cxBordersAll = [bLeft, bTop, bRight, bBottom];

type
  TBrushHandle = {$IFDEF VCL}HBRUSH{$ELSE}QBrushH{$ENDIF};

  TRectArray = array of TRect;

  TcxColorPart = -100..100;

  TcxGridLines = (glBoth, glNone, glVertical, glHorizontal);

  TcxViewParams = record
    Bitmap: TBitmap;
    Color: TColor;
    Font: TFont;
    TextColor: TColor;
  end;

  { TcxRegion }

  TcxRegionHandle = {$IFDEF VCL}HRGN{$ELSE}QRegionH{$ENDIF};
  TcxRegionOperation = (roSet, roAdd, roSubtract, roIntersect);

  TcxRegion = class  {6}
  private
    FHandle: TcxRegionHandle;
    function GetIsEmpty: Boolean;
  protected
    procedure DestroyHandle;
  public
    constructor Create(AHandle: TcxRegionHandle); overload; virtual;
    constructor Create(const ABounds: TRect); overload;
    constructor Create; overload;
    constructor Create(ALeft, ATop, ARight, ABottom: Integer); overload;
    destructor Destroy; override;
    procedure Combine(ARegion: TcxRegion; AOperation: TcxRegionOperation;
      ADestroyRegion: Boolean = True);
    property Handle: TcxRegionHandle read FHandle write FHandle;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { TcxCanvas }

  TcxCanvas = class
  private
    FCanvas: TCanvas;

    function GetBrush: TBrush;
    function GetCopyMode: TCopyMode;
    function GetFont: TFont;
    function GetHandle: {$IFDEF VCL}HDC{$ELSE}QPainterH{$ENDIF};
    function GetPen: TPen;
    procedure SetBrush(Value: TBrush);
    procedure SetCopyMode(Value: TCopyMode);
    procedure SetFont(Value: TFont);
    procedure SetPen(Value: TPen);
    procedure SetPixel(X, Y: Integer; Value: TColor);
  public
    constructor Create(ACanvas: TCanvas); virtual;

    procedure CopyRect(const Dest: TRect; ACanvas: TCanvas; const Source: TRect);
    procedure Draw(X, Y: Integer; Graphic: TGraphic);
    procedure DrawComplexFrame(const R: TRect; ALeftTopColor, ARightBottomColor: TColor;
      ABorders: TcxBorders = [bLeft, bTop, bRight, bBottom]; ABorderWidth: Integer = 1);
    procedure DrawEdge(const R: TRect; ASunken, AOuter: Boolean;
      ABorders: TcxBorders = [bLeft, bTop, bRight, bBottom]);
    procedure DrawFocusRect(const R: TRect);
    procedure DrawGlyph(X, Y: Integer; AGlyph: TBitmap; AEnabled: Boolean = True;
      ABackgroundColor: TColor = clNone{; ATempCanvas: TCanvas = nil});
    procedure DrawImage(Images: TCustomImageList; X, Y, Index: Integer;
      Enabled: Boolean = True);
    procedure DrawTexT(const Text: string; R: TRect; Flags: Integer;
      Enabled: Boolean = True);
    procedure FillRect(const R: TRect); overload;
    procedure FillRect(const R: TRect; ABitmap: TBitmap); overload;
    function FontHeight(AFont: TFont): Integer;
    procedure FrameRect(const R: TRect; Color: TColor = clDefault;
      ALineWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll);
    procedure InvertFrame(const R: TRect; ABorderSize: Integer);
    procedure InvertRect(const R: TRect);
    procedure LineTo(X, Y: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure Polygon(const Points: array of TPoint);
    procedure Polyline(const Points: array of TPoint);
    function TextExtent(const Text: string): TSize; overload;
    procedure TextExtent(const Text: string; var R: TRect; Flags: Integer); overload;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;

    procedure SetBrushColor(Value: TColor);

  {$IFDEF VCL}
    procedure GetTextStringsBounds(Text: string; R: TRect; Flags: Integer;
      Enabled: Boolean; var ABounds: TRectArray);
  {$ENDIF}

    // clipping
    procedure ExcludeClipRect(const R: TRect);
    procedure IntersectClipRect(const R: TRect);
    function GetClipRegion: TcxRegion;
    procedure SetClipRegion(ARegion: TcxRegion; AOperation: TcxRegionOperation;
      ADestroyRegion: Boolean = True);
    function RectVisible(const R: TRect): Boolean;

    property Brush: TBrush read GetBrush write SetBrush;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property CopyMode: TCopyMode read GetCopyMode write SetCopyMode;
    property Font: TFont read GetFont write SetFont;
    property Handle: {$IFDEF VCL}HDC{$ELSE}QPainterH{$ENDIF} read GetHandle;
    property Pen: TPen read GetPen write SetPen;
    property Pixels[X, Y: Integer]: TColor write SetPixel;
  end;

  { TcxScreenCanvas }

  TcxScreenCanvas = class(TcxCanvas)
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
  end;

{$IFDEF VCL}
function cxFlagsToDTFlags(Flags: Integer): Integer;
{$ELSE}
function ColorToRGB(Color: TColor): Longint;
function GetRValue(Color: Integer): Byte;
function GetGValue(Color: Integer): Byte;
function GetBValue(Color: Integer): Byte;
function RGB(R, G, B: Byte): Integer;
{$ENDIF}

function GetRealColor(AColor: TColor): TColor;

// light colors
function GetLightColor(ABtnFaceColorPart, AHighlightColorPart, AWindowColorPart: TcxColorPart): TColor;
function GetLightBtnFaceColor: TColor;
function GetLightDownedColor: TColor;
function GetLightDownedSelColor: TColor;
function GetLightSelColor: TColor;

implementation

uses
  Menus;

type
  TCanvasAccess = class(TCanvas);

{$IFDEF VCL}

function cxFlagsToDTFlags(Flags: Integer): Integer;
begin
  Result := DT_NOPREFIX;
  if cxAlignLeft and Flags <> 0 then
    Result := Result or DT_LEFT;
  if cxAlignRight and Flags <> 0 then
    Result := Result or DT_RIGHT;
  if cxAlignHCenter and Flags <> 0 then
    Result := Result or DT_CENTER;
  if cxAlignTop and Flags <> 0 then
    Result := Result or DT_TOP;
  if cxAlignBottom and Flags <> 0 then
    Result := Result or DT_BOTTOM;
  if cxAlignVCenter and Flags <> 0 then
    Result := Result or DT_VCENTER;
  if cxSingleLine and Flags <> 0 then
    Result := Result or DT_SINGLELINE;
  if cxDontClip and Flags <> 0 then
    Result := Result or DT_NOCLIP;
  if cxExpandTabs and Flags <> 0 then
    Result := Result or DT_EXPANDTABS;
  if cxShowPrefix and Flags <> 0 then
    Result := Result and not DT_NOPREFIX;
  if cxWordBreak and Flags <> 0 then
    Result := Result or DT_WORDBREAK or DT_EDITCONTROL;
  if cxShowEndEllipsis and Flags <> 0 then
    Result := Result or DT_END_ELLIPSIS;
  if cxDontPrint and Flags <> 0 then
    Result := Result or DT_CALCRECT;
end;

{$ELSE}

function ColorToRGB(Color: TColor): Longint;
var
  R, G, B: Byte;
begin
  Result := QGraphics.ColorToRGB(Color);
  if Color < 0 then
  begin
    R := GetRValue(Result);
    G := GetGValue(Result);
    B := GetBValue(Result);
    Result := RGB(B, G, R);
  end;  
end;

function GetRValue(Color: Integer): Byte;
begin
  Result := Byte(Color);
end;

function GetGValue(Color: Integer): Byte;
begin
  Result := Byte(Color shr 8);
end;

function GetBValue(Color: Integer): Byte;
begin
  Result := Byte(Color shr 16);
end;

function RGB(R, G, B: Byte): Integer;
begin
  Result := (R or (G shl 8) or (B shl 16));
end;

{$ENDIF}

function GetRealColor(AColor: TColor): TColor;
{$IFDEF VCL}
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetNearestColor(DC, AColor);
  ReleaseDC(0, DC);
end;
{$ELSE}
begin
  Result := AColor;
end;
{$ENDIF}

function GetLightColor(ABtnFaceColorPart, AHighlightColorPart, AWindowColorPart: TcxColorPart): TColor;
var
  ABtnFaceColor, AHighlightColor, AWindowColor: TColor;

  function GetLightIndex(ABtnFaceValue, AHighlightValue, AWindowValue: Byte): Integer;
  begin
    Result :=
      MulDiv(ABtnFaceValue, ABtnFaceColorPart, 100) +
      MulDiv(AHighlightValue, AHighlightColorPart, 100) +
      MulDiv(AWindowValue, AWindowColorPart, 100);
    if Result < 0 then Result := 0;
    if Result > 255 then Result := 255;
  end;

begin
  ABtnFaceColor := ColorToRGB(clBtnFace);
  AHighlightColor := ColorToRGB(clHighlight);
  AWindowColor := ColorToRGB(clWindow);
  if (ABtnFaceColor = 0) or (ABtnFaceColor = $FFFFFF) then
    Result := AHighlightColor
  else
    Result := RGB(
      GetLightIndex(GetRValue(ABtnFaceColor), GetRValue(AHighlightColor), GetRValue(AWindowColor)),
      GetLightIndex(GetGValue(ABtnFaceColor), GetGValue(AHighlightColor), GetGValue(AWindowColor)),
      GetLightIndex(GetBValue(ABtnFaceColor), GetBValue(AHighlightColor), GetBValue(AWindowColor)));
end;

function GetLightBtnFaceColor: TColor;

  function GetLightValue(Value: Byte): Byte;
  begin
    Result := Value + MulDiv(255 - Value, 16, 100);
  end;

begin
  Result := ColorToRGB(clBtnFace);
  Result := RGB(
    GetLightValue(GetRValue(Result)),
    GetLightValue(GetGValue(Result)),
    GetLightValue(GetBValue(Result)));
  Result := GetRealColor(Result);
end;

function GetLightDownedColor: TColor;
begin
  Result := GetRealColor(GetLightColor(11, 9, 73));
end;

function GetLightDownedSelColor: TColor;
begin
  Result := GetRealColor(GetLightColor(14, 44, 40));
end;

function GetLightSelColor: TColor;
begin
  Result := GetRealColor(GetLightColor(-2, 30, 72));
end;

{ TcxRegion }

constructor TcxRegion.Create(AHandle: TcxRegionHandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

constructor TcxRegion.Create(const ABounds: TRect);
var
  AHandle: TcxRegionHandle;
begin
{$IFDEF VCL}
  AHandle := CreateRectRgnIndirect(ABounds);
{$ELSE}
  AHandle := QRegion_create(@ABounds, QRegionRegionType_Rectangle);
{$ENDIF}
  Create(AHandle);
end;

constructor TcxRegion.Create;
begin
{$IFDEF VCL}
  Create(0, 0, 0, 0);
{$ELSE}
  Create(QRegion_create);
{$ENDIF}
end;

constructor TcxRegion.Create(ALeft, ATop, ARight, ABottom: Integer);
begin
  Create(Rect(ALeft, ATop, ARight, ABottom));
end;

destructor TcxRegion.Destroy;
begin
  DestroyHandle;
  inherited;
end;

function TcxRegion.GetIsEmpty: Boolean;
{$IFDEF VCL}
var
  R: TRect;
{$ENDIF}  
begin
{$IFDEF VCL}
  Result := GetRgnBox(FHandle, R) = NULLREGION;
{$ELSE}
  Result := QRegion_isEmpty(FHandle);
{$ENDIF}
end;

procedure TcxRegion.DestroyHandle;
begin
{$IFDEF VCL}
  if FHandle <> 0 then
  begin
    DeleteObject(FHandle);
    FHandle := 0;
  end;
{$ELSE}
  if FHandle <> nil then
  begin
    QRegion_destroy(FHandle);
    FHandle := nil;
  end;  
{$ENDIF}
end;

procedure TcxRegion.Combine(ARegion: TcxRegion; AOperation: TcxRegionOperation;
  ADestroyRegion: Boolean = True);
{$IFDEF VCL}
const
  Modes: array[TcxRegionOperation] of Integer = (RGN_COPY, RGN_OR, RGN_DIFF, RGN_AND);
{$ENDIF}
begin
{$IFDEF VCL}
  CombineRgn(FHandle, FHandle, ARegion.Handle, Modes[AOperation]);
{$ELSE}
  case AOperation of
    roSet:
      {};
    roAdd:
      QRegion_unite(FHandle, FHandle, ARegion.Handle);
    roSubtract:
      QRegion_subtract(FHandle, FHandle, ARegion.Handle);
    roIntersect:
      QRegion_intersect(FHandle, FHandle, ARegion.Handle);
  end;
{$ENDIF}
  if ADestroyRegion then ARegion.Free;
end;

{ TcxCanvas }

constructor TcxCanvas.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

function TcxCanvas.GetBrush: TBrush;
begin
  Result := Canvas.Brush;
end;

function TcxCanvas.GetCopyMode: TCopyMode;
begin
  Result := Canvas.CopyMode;
end;

function TcxCanvas.GetFont: TFont;
begin
  Result := Canvas.Font;
end;

function TcxCanvas.GetHandle: {$IFDEF VCL}HDC{$ELSE}QPainterH{$ENDIF};
begin
  Result := Canvas.Handle;
end;

function TcxCanvas.GetPen: TPen;
begin
  Result := Canvas.Pen;
end;

procedure TcxCanvas.SetBrush(Value: TBrush);
begin
  Canvas.Brush := Value;
end;

procedure TcxCanvas.SetCopyMode(Value: TCopyMode);
begin
  Canvas.CopyMode := Value;
end;

procedure TcxCanvas.SetFont(Value: TFont);
begin
  Canvas.Font := Value;
end;

procedure TcxCanvas.SetPen(Value: TPen);
begin
  Canvas.Pen := Value;
end;

procedure TcxCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
{$IFDEF VCL}
  Canvas.Pixels[X, Y] := Value;
{$ELSE}
  SetBrushColor(Value);
  FillRect(Rect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

procedure TcxCanvas.CopyRect(const Dest: TRect; ACanvas: TCanvas;
  const Source: TRect);
begin
{$IFNDEF VCL}
  if IsRectEmpty(Dest) or IsRectEmpty(Source) then Exit;
{$ENDIF}
  Canvas.CopyRect(Dest, ACanvas, Source);
end;

procedure TcxCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  Canvas.Draw(X, Y, Graphic);
end;

procedure TcxCanvas.DrawComplexFrame(const R: TRect;
  ALeftTopColor, ARightBottomColor: TColor; ABorders: TcxBorders;
  ABorderWidth: Integer);
var
  ABorder: TcxBorder;

  function GetBorderColor: TColor;
  begin
    if ABorder in [bLeft, bTop] then
      Result := ALeftTopColor
    else
      Result := ARightBottomColor;
  end;

  function GetBorderBounds: TRect;
  begin
    Result := R;
    with Result do
      case ABorder of
        bLeft:
          Right := Left + ABorderWidth;
        bTop:
          Bottom := Top + ABorderWidth;
        bRight:
          Left := Right - ABorderWidth;
        bBottom:
          Top := Bottom - ABorderWidth;
      end;
  end;

begin
  for ABorder := Low(ABorder) to High(ABorder) do
    if ABorder in ABorders then
    begin
      SetBrushColor(GetBorderColor);
      FillRect(GetBorderBounds);
    end;
end;

procedure TcxCanvas.DrawEdge(const R: TRect; ASunken, AOuter: Boolean;
  ABorders: TcxBorders);
begin
  if ASunken then
    if AOuter then
      DrawComplexFrame(R, clBtnShadow, clBtnHighlight, ABorders)
    else
      DrawComplexFrame(R, cl3DDkShadow{clBtnText}, cl3DLight{clBtnFace}, ABorders)
  else
    if AOuter then
      DrawComplexFrame(R, cl3DLight{clBtnFace}, cl3DDkShadow{clBtnText}, ABorders)
    else
      DrawComplexFrame(R, clBtnHighlight, clBtnShadow, ABorders);
end;

procedure TcxCanvas.DrawFocusRect(const R: TRect);
begin
  with TCanvasAccess(Canvas) do
  begin
    Font.Color := clBlack;
    SetBrushColor(clWhite);
    RequiredState([csFontValid]);
    DrawFocusRect(R);
  end;
end;

procedure TcxCanvas.DrawGlyph(X, Y: Integer; AGlyph: TBitmap; AEnabled: Boolean = True;
  ABackgroundColor: TColor = clNone{; ATempCanvas: TCanvas = nil});
var
{$IFDEF VCL}
  APrevBrushStyle: TBrushStyle;
{$ENDIF}  
  AImageList: TImageList;
  ABitmap: TBitmap;
begin
{$IFDEF VCL}
  if AEnabled {and (ATempCanvas = nil)} then
  begin
    APrevBrushStyle := Brush.Style;
    if ABackgroundColor = clNone then
      Brush.Style := bsClear
    else
      Brush.Color := ABackgroundColor;
    Canvas.BrushCopy(Bounds(X, Y, AGlyph.Width, AGlyph.Height), AGlyph,
      Rect(0, 0, AGlyph.Width, AGlyph.Height), AGlyph.TransparentColor);
    Brush.Style := APrevBrushStyle;
    Exit;
  end;
{$ENDIF}

  AImageList := nil;
  ABitmap := nil;
  try
    AImageList := TImageList.Create(nil);
    AImageList.Width := AGlyph.Width;
    AImageList.Height := AGlyph.Height;
    if ABackgroundColor <> clNone then
      //if ATempCanvas = nil then
      begin
        ABitmap := TBitmap.Create;
        ABitmap.Width := AImageList.Width;
        ABitmap.Height := AImageList.Height;
        with ABitmap.Canvas do
        begin
          Brush.Color := ABackgroundColor;
          FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
        end;
      end
      {else
        with ATempCanvas do
        begin
          Brush.Color := ABackgroundColor;
          FillRect(Bounds(X, Y, AGlyph.Width, AGlyph.Height));
        end};

    if AGlyph.TransparentMode = tmFixed then
      AImageList.AddMasked(AGlyph, AGlyph.TransparentColor)
    else
      AImageList.AddMasked(AGlyph, clDefault);

    if ABitmap <> nil then
    begin
      AImageList.Draw(ABitmap.Canvas, 0, 0, 0{$IFNDEF VCL}, itMask{$ENDIF}, AEnabled); // ??? itMask TODO
      Draw(X, Y, ABitmap);
    end
    else
      (*if ATempCanvas <> nil then
        AImageList.Draw(ATempCanvas, X, Y, 0{$IFNDEF VCL}, itMask{$ENDIF}, AEnabled) // ??? itMask TODO
      else*)
        AImageList.Draw(Canvas, X, Y, 0{$IFNDEF VCL}, itMask{$ENDIF}, AEnabled); // ??? itMask TODO
  finally
    ABitmap.Free;
    AImageList.Free;
  end;        
end;

procedure TcxCanvas.DrawImage(Images: TCustomImageList; X, Y, Index: Integer;
  Enabled: Boolean = True);
begin
  if (0 <= Index) and (Index < Images.Count) then
    Images.Draw(Canvas, X, Y, Index, {$IFNDEF VCL}itImage, {$ENDIF}Enabled);
end;

procedure TcxCanvas.DrawText(const Text: string; R: TRect; Flags: Integer;
  Enabled: Boolean);
var
{$IFDEF VCL}
  AUseDrawText: Boolean;
{$ENDIF}  
  PrevBrushStyle: TBrushStyle;
  PrevFontColor: TColor;

  procedure ProcessFlags;
  var
    ASize: TSize;
  {$IFDEF VCL}
    ASizeR: TRect;
  {$ENDIF}
  begin
    ASize := TextExtent(Text);
    if (ASize.cx < R.Right - R.Left) and (ASize.cy < R.Bottom - R.Top) then
      Flags := Flags or cxDontClip;
  {$IFDEF VCL}
    if AUseDrawText then
    begin
      if (Flags and cxSingleLine = 0) and (Flags and (cxAlignBottom or cxAlignVCenter) <> 0) then
      begin
        ASizeR := Rect(0, 0, R.Right - R.Left - Ord(not Enabled), 0);
        TextExtent(Text, ASizeR, Flags);
        case Flags and (cxAlignBottom or cxAlignVCenter) of
          cxAlignBottom:
            R.Top := R.Bottom - (ASizeR.Bottom - ASizeR.Top + Ord(not Enabled));
          cxAlignVCenter:
            R.Top := (R.Top + R.Bottom - (ASizeR.Bottom - ASizeR.Top)) div 2;
        end;
      end;
      Flags := cxFlagsToDTFlags(Flags);
    end
    else
    begin
      if ASize.cx < R.Right - R.Left then
        case Flags and (cxAlignLeft or cxAlignRight or cxAlignHCenter) of
          cxAlignRight:
            R.Left := R.Right - ASize.cx - Ord(not Enabled);
          cxAlignHCenter:
            R.Left := (R.Left + R.Right - ASize.cx) div 2;
        end;
      if ASize.cy < R.Bottom - R.Top then
        case Flags and (cxAlignTop or cxAlignBottom or cxAlignVCenter) of
          cxAlignBottom:
            R.Top := R.Bottom - ASize.cy - Ord(not Enabled);
          cxAlignVCenter:
            R.Top := (R.Top + R.Bottom - ASize.cy) div 2;
        end;
      if Flags and cxDontClip = 0 then
        Flags := ETO_CLIPPED
      else
        Flags := 0;
    end;
  {$ELSE}
    Flags := Flags and not cxShowEndEllipsis;
  {$ENDIF}
  end;

  procedure DoDrawText;
  begin
  {$IFDEF VCL}
    if AUseDrawText then
      Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, Flags)
    else
      ExtTextOut(Canvas.Handle, R.Left, R.Top, Flags, @R, PChar(Text), Length(Text), nil);
  {$ELSE}
    Canvas.TextRect(R, R.Left, R.Top, Text, Flags);
  {$ENDIF}
  end;

begin
  if Length(Text) = 0 then Exit;
{$IFDEF VCL}
  AUseDrawText := (Flags and cxSingleLine = 0) or
    (Flags and (cxShowPrefix or cxShowEndEllipsis) <> 0);
{$ENDIF}
  ProcessFlags;
  PrevBrushStyle := Brush.Style;
  PrevFontColor := Font.Color;
  if not Enabled then
  begin
    with R do
    begin
      Inc(Left);
      Inc(Top);
    end;
    Brush.Style := bsClear;
    Font.Color := clBtnHighlight;
    DoDrawText;
    OffsetRect(R, -1, -1);
    Font.Color := clBtnShadow;
  end;
  DoDrawText;
  if Brush.Style <> PrevBrushStyle then
    Brush.Style := PrevBrushStyle;
  Font.Color := PrevFontColor;
end;

procedure TcxCanvas.FillRect(const R: TRect);
begin
  Canvas.FillRect(R);
end;

procedure TcxCanvas.FillRect(const R: TRect; ABitmap: TBitmap);
var
  ABitmapSize, AOffset: TPoint;
  AFirstCol, AFirstRow, ALastCol, ALastRow, I, J: Integer;
  ABitmapRect, ACellRect: TRect;
begin  {4}
  if ABitmap.Empty then Exit;
  with ABitmapSize, ABitmap do
  begin
    X := Width;
    Y := Height;
  end;
  with R, ABitmapSize do
  begin
    AFirstCol := Left div X;
    AFirstRow := Top div Y;
    ALastCol := Right div X - Ord(Right mod X = 0);
    ALastRow := Bottom div Y - Ord(Bottom mod Y = 0);
    for J := AFirstRow to ALastRow do
      for I := AFirstCol to ALastCol do
      begin
        AOffset.X := I * X;
        AOffset.Y := J * Y;
        ACellRect := Bounds(AOffset.X, AOffset.Y, X, Y);
        IntersectRect(ACellRect, ACellRect, R);
        ABitmapRect := ACellRect;
        OffsetRect(ABitmapRect, -AOffset.X, -AOffset.Y);
        CopyRect(ACellRect, ABitmap.Canvas, ABitmapRect);
      end;
  end;
end;
(*var
  ABitmapSize: TPoint;
  AColCount, ARowCount, I, J: Integer;
  ABitmapRect, ACellRect: TRect;
begin  {4}
  if ABitmap.Empty then Exit;
  with ABitmapSize, ABitmap do
  begin
    X := Width;
    Y := Height;
  end;
  with R, ABitmapSize do
  begin
    AColCount := (Right - Left) div X + Ord((Right - Left) mod X <> 0);
    ARowCount := (Bottom - Top) div Y + Ord((Bottom - Top) mod Y <> 0);
    for J := 0 to ARowCount - 1 do
      for I := 0 to AColCount - 1 do
      begin
        ACellRect := Bounds(Left + I * X, Top + J * Y, X, Y);
        if ACellRect.Right > Right then ACellRect.Right := Right;
        if ACellRect.Bottom > Bottom then ACellRect.Bottom := Bottom;
        ABitmapRect := ACellRect;
        OffsetRect(ABitmapRect, -ABitmapRect.Left, -ABitmapRect.Top);
        CopyRect(ACellRect, ABitmap.Canvas, ABitmapRect);
      end;
  end;
end;*)

function TcxCanvas.FontHeight(AFont: TFont): Integer;
begin
  Font := AFont;
  Result := TextHeight('Qq');
end;

procedure TcxCanvas.FrameRect(const R: TRect; Color: TColor = clDefault;
  ALineWidth: Integer = 1; ABorders: TcxBorders = cxBordersAll);
begin
  if Color <> clDefault then
  begin
    SetBrushColor(Color);
  {$IFNDEF VCL}
    Brush.Style := bsSolid;  // bug in Qt
  {$ENDIF}  
  end;
  with R do
  begin
    if bLeft in ABorders then
      FillRect(Rect(Left, Top, Left + ALineWidth, Bottom));
    if bRight in ABorders then
      FillRect(Rect(Right - ALineWidth, Top, Right, Bottom));
    if bTop in ABorders then
      FillRect(Rect(Left, Top, Right, Top + ALineWidth));
    if bBottom in ABorders then
      FillRect(Rect(Left, Bottom - ALineWidth, Right, Bottom));
  end;
end;

procedure TcxCanvas.InvertFrame(const R: TRect; ABorderSize: Integer);
begin
  with R do
  begin
    InvertRect(Rect(Left, Top, Left + ABorderSize, Bottom));
    InvertRect(Rect(Right - ABorderSize, Top, Right, Bottom));
    InvertRect(Rect(Left + ABorderSize, Top,
      Right - ABorderSize, Top + ABorderSize));
    InvertRect(Rect(Left + ABorderSize, Bottom - ABorderSize,
      Right - ABorderSize, Bottom));
  end;
end;

procedure TcxCanvas.InvertRect(const R: TRect);
begin
  with Canvas do
  begin
    CopyMode := cmDstInvert;
    CopyRect(R, Canvas, R);
    CopyMode := cmSrcCopy;
  end;
end;

procedure TcxCanvas.LineTo(X, Y: Integer);
begin
  Canvas.LineTo(X, Y);
end;

procedure TcxCanvas.MoveTo(X, Y: Integer);
begin
  Canvas.MoveTo(X, Y);
end;

procedure TcxCanvas.Polygon(const Points: array of TPoint);
begin
  Canvas.Polygon(Points{$IFNDEF VCL}, False, 0, -1{$ENDIF});
end;

procedure TcxCanvas.Polyline(const Points: array of TPoint);
begin
  Canvas.Polyline(Points);
end;

function TcxCanvas.TextExtent(const Text: string): TSize;
begin
{$IFDEF VCL}
  TCanvasAccess(Canvas).RequiredState([csHandleValid, csFontValid]);
  Result.cX := 0;
  Result.cY := 0;
  GetTextExtentPoint(Handle, PChar(Text), Length(Text), Result);
{$ELSE}
  Result := Canvas.TextExtent(Text, 0);
{$ENDIF}
end;

procedure TcxCanvas.TextExtent(const Text: string; var R: TRect; Flags: Integer);
{$IFDEF VCL}
var
  RWidth, RHeight, TextWidth, TextHeight: Integer;

  procedure CalcRSizes(var AWidth, AHeight: Integer);
  begin
    with R do
    begin
      AWidth := Right - Left;
      AHeight := Bottom - Top;
    end;
  end;

  procedure AlignR;
  begin
    if Flags and DT_CENTER <> 0 then
      OffsetRect(R, (RWidth - TextWidth) div 2, 0)
    else
      if Flags and DT_RIGHT <> 0 then
        OffsetRect(R, RWidth - TextWidth, 0);
    if Flags and DT_VCENTER <> 0 then
      OffsetRect(R, 0, (RHeight - TextHeight) div 2)
    else
      if Flags and DT_BOTTOM <> 0 then
        OffsetRect(R, 0, RHeight - TextHeight);
  end;

begin
  CalcRSizes(RWidth, RHeight);
  Flags := cxFlagsToDTFlags(Flags);
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R,
    Flags and not DT_VCENTER or DT_CALCRECT);
  CalcRSizes(TextWidth, TextHeight);
  AlignR;
end;
{$ELSE}
begin
  Canvas.TextExtent(Text, R, Flags);
end;
{$ENDIF}

function TcxCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

function TcxCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

procedure TcxCanvas.SetBrushColor(Value: TColor);
begin
  if Brush.Color <> Value then
    Brush.Color := Value;
end;

{$IFDEF VCL}

procedure TcxCanvas.GetTextStringsBounds(Text: string; R: TRect; Flags: Integer;
  Enabled: Boolean; var ABounds: TRectArray);
var
  AAlignHorz, AAlignVert, AMaxCharCount: Integer;
  ATextR: TRect;
  AStringSize: TSize;

  procedure PrepareRects;
  begin
    if not Enabled then
      with R do
      begin
        Dec(Right);
        Dec(Bottom);
      end;
    ATextR := R;
    TextExtent(Text, ATextR, Flags);
    case AAlignVert of
      cxAlignBottom:
        OffsetRect(ATextR, 0, R.Bottom - ATextR.Bottom);
      cxAlignVCenter:
        OffsetRect(ATextR, 0, (R.Bottom - ATextR.Bottom) div 2);
    end;
  end;

  procedure CheckMaxCharCount;

    function ProcessSpecialChars: Boolean;
    const
      SpecialChars = [#10, #13];
    var
      I, ACharCount: Integer;
    begin
      Result := False;
      for I := 1 to AMaxCharCount do
        if Text[I] in SpecialChars then
        begin
          AMaxCharCount := I - 1;
          ACharCount := 1;
          if (I < Length(Text)) and
            (Text[I + 1] in SpecialChars) and (Text[I] <> Text[I + 1]) then
            Inc(ACharCount);
          Delete(Text, I, ACharCount);
          Result := True;
          Break;
        end;
    end;

    procedure ProcessSpaces;
    var
      I: Integer;
    begin
      if AMaxCharCount < Length(Text) then
        for I := AMaxCharCount + 1 downto 1 do
          if Text[I] = ' ' then
          begin
            if I < AMaxCharCount then
            begin
              AMaxCharCount := I;
              if AAlignHorz <> cxAlignLeft then
              begin
                Delete(Text, I, 1);
                Dec(AMaxCharCount);
              end;
            end;  
            Break;
          end;
    end;

  begin
    if not ProcessSpecialChars then
      ProcessSpaces;
  end;

  procedure GetStringSize;
  begin
    if AMaxCharCount = 0 then
      AStringSize.cx := 0
    else
      GetTextExtentPoint(Handle, PChar(Copy(Text, 1, AMaxCharCount)),
        AMaxCharCount, AStringSize);
  end;

  function GetBounds: TRect;
  begin
    Result := ATextR;
    with Result, AStringSize do
    begin
      case AAlignHorz of
        cxAlignLeft:
          Right := Left + cx;
        cxAlignRight:
          Left := Right - cx;
        cxAlignHCenter:
          begin
            Left := (Left + Right - cx) div 2;
            Right := Left + cx;
          end;
      end;
      Bottom := Top + cy;
    end;
    ATextR.Top := Result.Bottom;
  end;

begin
  if Flags and cxShowPrefix <> 0 then
  begin
    Text := StripHotKey(Text);
    Flags := Flags and not cxShowPrefix;
  end;
  AAlignHorz := Flags and (cxAlignLeft or cxAlignRight or cxAlignHCenter);
  AAlignVert := Flags and (cxAlignTop or cxAlignBottom or cxAlignVCenter);
  PrepareRects;
  repeat
    GetTextExtentExPoint(Handle, PChar(Text), Length(Text), R.Right - R.Left,
      @AMaxCharCount, nil, AStringSize);
    CheckMaxCharCount;
    GetStringSize;
    SetLength(ABounds, High(ABounds) + 2);
    ABounds[High(ABounds)] := GetBounds;
    Delete(Text, 1, AMaxCharCount);
  until Text = '';
end;

{$ENDIF}

procedure TcxCanvas.ExcludeClipRect(const R: TRect);
begin
{$IFDEF VCL}
  with R do
    Windows.ExcludeClipRect(Handle, Left, Top, Right, Bottom);
{$ELSE}
  if not IsRectEmpty(R) then
    SetClipRegion(TcxRegion.Create(R), roSubtract);
{$ENDIF}
end;

procedure TcxCanvas.IntersectClipRect(const R: TRect);
{$IFDEF VCL}
begin
  with R do
    Windows.IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
end;
{$ELSE}
begin
  if IsRectEmpty(R) then Exit;
  SetClipRegion(TcxRegion.Create(R), roIntersect);
end;
{$ENDIF}

function TcxCanvas.GetClipRegion: TcxRegion;
begin
  Result := TcxRegion.Create;
{$IFDEF VCL}
  if GetClipRgn(Handle, Result.Handle) = 0 then
    SetRectRgn(Result.Handle, 0, 0, MaxInt div 20, MaxInt div 20);
{$ELSE}
  if QPainter_hasClipping(Handle) then
    QRegion_unite(Result.Handle, Result.Handle, Canvas.GetClipRegion)
  else
  begin
    Result.DestroyHandle;
    Result.Handle := QRegion_create(0, 0, MaxInt div 20, MaxInt div 20,
      QRegionRegionType_Rectangle);
  end;
{$ENDIF}
end;

procedure TcxCanvas.SetClipRegion(ARegion: TcxRegion; AOperation: TcxRegionOperation;
  ADestroyRegion: Boolean = True);
var
  AClipRegion: TcxRegion;
begin
  if AOperation = roSet then
  {$IFDEF VCL}
    SelectClipRgn(Handle, ARegion.Handle)
  {$ELSE}
    QPainter_setClipRegion(Handle, ARegion.Handle)
  {$ENDIF}
  else
  begin
    AClipRegion := GetClipRegion;
    AClipRegion.Combine(ARegion, AOperation, False);
    SetClipRegion(AClipRegion, roSet);
  end;
  if ADestroyRegion then ARegion.Free;
end;

function TcxCanvas.RectVisible(const R: TRect): Boolean;
begin
{$IFDEF VCL}
  Result := Windows.RectVisible(Handle, R);
{$ELSE}
  Result := not QPainter_hasClipping(Handle);
  if Result then
    Result := not IsRectEmpty(R)
  else
    Result := QRegion_contains(QPainter_clipRegion(Handle), PRect(@R));
{$ENDIF}
end;

{ TScreenCanvas }

type
  TScreenCanvas = class({$IFDEF VCL}TCanvas{$ELSE}TQtCanvas{$ENDIF})
  private
  {$IFNDEF VCL}
    FWidget: QOpenWidgetH;
    FWidgetFlags: Integer;
  {$ENDIF}
  {$IFDEF VCL}
    procedure FreeHandle;
  protected
    procedure CreateHandle; override;
  {$ENDIF}
  public
  {$IFNDEF VCL}
    constructor Create;
  {$ENDIF}
    destructor Destroy; override;
  end;

{$IFNDEF VCL}

constructor TScreenCanvas.Create;
begin
  inherited;
  FWidget := QOpenWidgetH(QApplication_desktop);
  FWidgetFlags := QOpenWidget_getWFlags(FWidget);
  QOpenWidget_setWFlags(FWidget, FWidgetFlags or Integer(WidgetFlags_WPaintUnclipped));
  QtHandle := FWidget;
  Start;
end;

{$ENDIF}

destructor TScreenCanvas.Destroy;
begin
{$IFDEF VCL}
  FreeHandle;
{$ELSE}
  Stop;
  QOpenWidget_setWFlags(FWidget, FWidgetFlags);
{$ENDIF}
  inherited;
end;

{$IFDEF VCL}

procedure TScreenCanvas.FreeHandle;
begin
  ReleaseDC(0, Handle);
  Handle := 0;
end;

procedure TScreenCanvas.CreateHandle;
begin
  Handle := GetDC(0);
end;

{$ENDIF}

{ TcxScreenCanvas }

constructor TcxScreenCanvas.Create;
begin
  inherited Create(TScreenCanvas.Create);
end;

destructor TcxScreenCanvas.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

end.

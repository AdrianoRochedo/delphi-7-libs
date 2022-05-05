unit TBXDock2003;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB2Dock, TB2Common, TBX, TBXThemes, TBXUxThemes, extctrls, TBXDefaultTheme,
  TBXOffice2003Theme, TBXOfficeXPGradientTheme;

type
  TTBOffice2003Dock = class(TTBDock)
  private
    function DrawGradient: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    function UsingBackground: Boolean; override;
    procedure DrawBackground (DC: HDC; const DrawRect: TRect); override;
    procedure Resize; override;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

type TGradDir = (tGLeftRight, tGTopBottom);

procedure Register;

implementation

const
  REBARPartFiller0  = 0;

procedure Register;
begin
  RegisterComponents('Toolbar2000', [TTBOffice2003Dock]);
end;

{ TTBOffice2003Dock }

procedure FillGradient(const DC: HDC; const ARect: TRect;
                       const StartColor, EndColor: TColor;
                       const Direction: TGradDir);
var
 rc1, rc2, gc1, gc2,
 bc1, bc2, Counter: Integer;
 Brush: HBrush;
begin
  rc1 := GetRValue(ColorToRGB(StartColor));
  gc1 := GetGValue(ColorToRGB(StartColor));
  bc1 := GetBValue(ColorToRGB(StartColor));
  rc2 := GetRValue(ColorToRGB(EndColor));
  gc2 := GetGValue(ColorToRGB(EndColor));
  bc2 := ColorToRGB(GetBValue(EndColor));

  if Direction = tGTopBottom then
  for Counter := ARect.Top to ARect.Bottom - 1 do
  begin
    Brush:= CreateSolidBrush(
      RGB(Byte(rc1 + (((rc2 - rc1) * (ARect.Top + Counter)) div ARect.Bottom)),
          Byte(gc1 + (((gc2 - gc1) * (ARect.Top + Counter)) div ARect.Bottom)),
          Byte(bc1 + (((bc2 - bc1) * (ARect.Top + Counter)) div ARect.Bottom))));
    FillRect(DC, Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom - Counter + 1), Brush);
    DeleteObject(Brush);
  end else
  for Counter := ARect.Left to ARect.Right - 1 do
  begin
    Brush := CreateSolidBrush(
      RGB(Byte(rc1 + (((rc2 - rc1) * (ARect.Left + Counter)) div ARect.Right)),
          Byte(gc1 + (((gc2 - gc1) * (ARect.Left + Counter)) div ARect.Right)),
          Byte(bc1 + (((bc2 - bc1) * (ARect.Left + Counter)) div ARect.Right))));
    FillRect(DC, Rect(ARect.Left, ARect.Top, ARect.Right - Counter +1, ARect.Bottom), Brush);
    DeleteObject(Brush);
  end;
end;

procedure TTBOffice2003Dock.DrawBackground(DC: HDC; const DrawRect: TRect);
var
  R : TRect;
begin
  if DrawGradient then
  begin
    R := DrawRect;
    R.Left := R.Left + Left;
    R.Top := R.Top + Top;
    R.Right := R.Left + Width;
    R.Bottom := R.Top + Height;
    FillGradient(DC, R, RGB(245, 244, 242), ColorToRGB(clBtnFace), TGLeftRight);
  end;
end;

function TTBOffice2003Dock.DrawGradient : Boolean;
begin
  Result:= (CurrentTheme is TTBXOffice2003Theme) or (CurrentTheme is TTBXOfficeXPGradientTheme);
end;

procedure TTBOffice2003Dock.Resize;
var
  I : Integer;
begin
  inherited;
  if DrawGradient then
  begin
    Invalidate;
    for I := 0 to ToolbarCount - 1 do
      PostMessage(Toolbars[I].Handle, WM_MOVE, 0, 0);
  end;
end;

function TTBOffice2003Dock.UsingBackground: Boolean;
begin
  Result:= inherited UsingBackground or DrawGradient;
end;

end.

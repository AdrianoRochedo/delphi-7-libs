unit Liquid_Indicator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,Math;

const
  CM_NEEDREPAINT =cm_Base+101;  

type
  Tpoints = array of Tpoint;
  Tintegers =array of Integer;
  TIAIndicatorKind = (ik7segment,ik5x7matrix,ik9segment);
  TIABevelKind = (bkUP,bkDown,bkNone);
  TIAIndicatorStyle = (isThin,isNormal,isThick);
  TIAMouseAction = (maNone,maColor,maWidth,maBoth);
  TIATransparentMode =(tmNone,tmBackOnly,tmAll);
  TIAColorScheme = (csSilver,csInvSilver,csWhite,csBlack,csPlasma,csInvPlasma,csTIAN);
  TIAMaterialShadowMode =(msmDark,msmLight,msmNone);
  TIAMaterialMode = (mmStretch,mmTile);
  TMouseInOutEvent = procedure(sender : Tobject) of object;

  TLiquid_Indicator = class(TCustomControl)
  private
    { Private declarations }
    FSignNumber : Integer;
    FIndicatorWidth : Integer;
    FIndicatorHeight : Integer;
    FIndicatorAngle : Integer;
    FIndicatorGap : Integer;
    FIndicatorString : String;
    FIndicatorKind : TIAIndicatorKind;
    FInnerBevel,
    FOuterBevel : TIABevelKind;
    FInnerBevelWidth,
    FOuterBevelWidth : Integer;
    FBevelColor,
    FBackColor,
    FNumberForeColor,
    FNumberBackColor : TColor;
    FIndicatorStyle : TIAIndicatorStyle;
    FonMouseEnter : TMouseInOutEvent;
    FonMouseLeave  : TMouseInOutEvent;
    FNumberForeColor1 : TColor;
    FMouseAction : TIAMouseAction;
    FTransparentMode : TIATransparentMode;
    FColorScheme : TIAColorScheme;
    FBackMaterial : TBitmap;
    FBackMaterialD : TBitmap;
    FBackMaterialL : TBitmap;
    FBackMaterialLD : TBitmap;
    FForeMaterial : TBitmap;
    FMaterialShadowMode : TIAMaterialShadowMode;
    FMaterialMode : TIAMaterialMode;
    FForeMaterialMode : TIAMaterialMode;

    Moused : Boolean;
    Imloaded : boolean;

    CanvasBitmap : TBitmap;
    ClipRGN : HRGN;

    BlackPolyPoly,
    GrayPOlyPoly : Tpoints;
    blackVertex,
    grayVertex : Tintegers;
    BlackN,
    GrayN : integer;


    procedure Getme24D(var B : TBitmap;N : integer);
    procedure Getme24L(var B : TBitmap;N : integer);
    procedure SETLD;
    procedure WMSIZE(var Message: TMessage); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMNEEDREPAINT(var Message: TMessage); message CM_NEEDREPAINT;
    procedure ShiftPoly(var points : array of Tpoint;dx,dy : integer);
    procedure PolyRotate(var points : Tpoints;x,y,Angle : integer);
    procedure PolyZrotate(var points : Tpoints;Angle : integer);
    procedure GetIndicatorRgn;
    procedure GetIndicatorImageRgns;
    procedure GetSinglePolyPoly(R : Trect;Sign : Char;Pointed : Boolean;var p1,p2 :Tpoints;
                                var v1,v2 :Tintegers;var n1,n2 :integer);
    procedure AddPolytoPPoly(var p2 : Tpoints;
                 var v2 :Tintegers;var p :array of Tpoint);
    procedure Addpolypoly(var p1,p2 : Tpoints;var V1,V2 : Tintegers;var n1,n2 :integer);             
    procedure GetsinglePoly7segment(r : TRect;i : integer;var p : TPoints);
    procedure GetsinglePoly5x7matrix(r : trect;i,j : integer; var p : TPoints);
    function GetLColor(Value : Tcolor): Tcolor;
    function GetDColor(Value : Tcolor): Tcolor;
    procedure DrawLD(var B : TBitmap);


  protected
    { Protected declarations }
    procedure Paint;override;
    procedure SETSignNumber(Value : Integer); virtual;
    procedure SETIndicatorWidth(Value : Integer);  virtual;
    procedure SETIndicatorHeight(Value : Integer); virtual;
    procedure SETIndicatorAngle(Value : Integer); virtual;
    procedure SETIndicatorGap(Value : Integer); virtual;
    procedure SETIndicatorString(Value : string);virtual;
    procedure SETIndicatorKind(Value : TIAIndicatorKind);virtual;
    procedure SETInnerBevel(Value : TIABevelKind); virtual;
    procedure SETOuterBevel(Value : TIABevelKind); virtual;
    procedure SETInnerBevelWidth(Value : Integer); virtual;
    procedure SETOuterBevelWidth(Value : Integer); virtual;
    procedure SETBevelColor(Value : TColor); virtual;
    procedure SETBackColor(Value : TColor); virtual;
    procedure SETNumberForeColor(Value : TColor); virtual;
    procedure SETNumberForeColor1(Value : TColor); virtual;
    procedure SETNumberBackColor(Value : TColor); virtual;
    procedure SETIndicatorStyle(Value : TIAIndicatorStyle);virtual;
    procedure SETMouseAction(Value : TIAMouseAction); virtual;
    procedure SETTransparentMode(Value : TIATransparentMode);virtual;
    procedure SETColorScheme(Value : TIAColorScheme); virtual;
    procedure SETBackMaterial(Value : TBitmap); virtual;
    procedure SETForeMaterial(Value : TBitmap); virtual;
    procedure SETMaterialShadowMode(Value : TIAMaterialShadowMode); virtual;
    procedure SETMaterialMode(Value : TIAMaterialMode); virtual;
    procedure SETForeMaterialMode(Value : TIAMaterialMode); virtual;
    procedure loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property ColorScheme : TIAColorScheme read FColorScheme write SETColorScheme default csSilver;
    property SignNumber : integer read FSignNumber write SetSignNumber default 10;
    property IndicatorWidth : integer read FIndicatorWidth write SetIndicatorWidth default 172;
    property IndicatorHeight : integer read FIndicatorHeight write SetIndicatorHeight default 36;
    property IndicatorAngle : integer read FIndicatorAngle write SetIndicatorAngle default 0;
    property IndicatorGap : integer read FIndicatorGap write SetIndicatorGap default 2;
    property Indicatorstring : string read FIndicatorstring write SETIndicatorstring;
    property IndicatorKind : TIAIndicatorKind read FIndicatorKind write SETIndicatorKind default ik7segment;
    property InnerBevel : TIABevelKind read FInnerBevel write SETInnerBevel default bkDown;
    property OuterBevel : TIABevelKind read FOuterBevel write SETOuterBevel default bkUp;
    property InnerBevelWidth : integer read FInnerBevelWidth write SETInnerBevelWidth default 2;
    property OuterBevelWidth : integer read FOuterBevelWidth write SETOuterBevelWidth default 2;
    property BevelColor : TColor read FBevelColor write SETBevelColor default $00DDDDDD;
    property BackColor : TColor read FBackColor write SETBackColor default $00DDDDDD;
    property NumberForeColor : TColor read FNumberForeColor write SETNumberForeColor default $00363636;
    property NumberForeColor1 : TColor read FNumberForeColor1 write SETNumberForeColor1 default clRed;
    property NumberBackColor : TColor read FNumberBackColor write SETNumberBackColor default $00D3D3D3;
    property IndicatorStyle : TIAIndicatorStyle read FIndicatorStyle write SETIndicatorStyle default isNormal;
    property MouseAction : TIAMouseAction read FMouseAction write SETMouseAction default maNone;
    property TransparentMode : TIATransparentMode read FTransparentMode write SETTransparentMode default tmNone;

    property BackMaterial : TBitmap read FBackMaterial write SETBackMaterial;
    property ForeMaterial : TBitmap read FForeMaterial write SETForeMaterial;
    property MaterialShadowMode : TIAMaterialShadowMode read FMaterialShadowMode write SETMaterialShadowMode
                                   default msmDark;
    property MaterialMode : TIAMaterialMode read FMaterialMode write SETMaterialMode default mmStretch;
    property ForeMaterialMode : TIAMaterialMode read FForeMaterialMode write SETForeMaterialMode
                                   default mmStretch;

    property onMouseEnter  : TMouseInOutEvent read FonMouseEnter write FonMouseEnter;
    property onMouseExit   : TMouseInOutEvent read FonMouseLeave  write FonMouseLeave;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ShowHint;
  end;

procedure Register;

implementation
procedure TLiquid_Indicator.DrawLD(var B : TBitmap);
var
 i,j : integer;
begin
  case FMaterialMode of
    mmStretch :
      begin
        CanvasBitmap.Canvas.StretchDraw(rect(0,0,CanvasBitmap.Width,CanvasBitmap.Height),b);
      end;
    mmTile :
      begin
        if B.Width<>0 then
        for i:=0 to CanvasBitmap.Width div B.Width do
          for j:=0 to CanvasBitmap.Height div b.Height do
            begin
              CanvasBitmap.Canvas.Draw(i*b.width,j*b.height,b);
            end;
      end;
  end;      
end;
procedure TLiquid_Indicator.Getme24D(var B : TBitmap;N : integer);
var
  i,j : integer;
  pB : PByteArray;
begin
  for i:=0 to B.Height-1 do
   begin
     pb:=B.ScanLine[i];
     for j:=0 to 3*B.Width-1 do
       begin
         if pb[j]<n then pb[j]:=0 else dec(pb[j],n);
       end;
   end;

end;

procedure TLiquid_Indicator.Getme24l(var B : TBitmap;N : integer);
var
  i,j : integer;
  pB : PByteArray;
begin
  for i:=0 to B.Height-1 do
   begin
     pb:=B.ScanLine[i];
     for j:=0 to 3*B.Width-1 do
       begin
         if pb[j]>(255-n) then pb[j]:=255 else inc(pb[j],n);
       end;
   end;

end;
procedure TLiquid_Indicator.SetLd;
begin
  FBackMaterialD.assign(FBackMaterial);
  FBackMaterialL.assign(FBackMaterial);
  FBackMaterialLD.assign(FBackMaterial);
  FBackMaterialD.PixelFormat:=pf24bit;
  FBackMaterialL.PixelFormat:=pf24bit;
  FBackMaterialLD.PixelFormat:=pf24bit;
  FBackMaterialD.canvas.draw(0,0,FBackMaterial);
  FBackMaterialL.canvas.draw(0,0,FBackMaterial);
  FBackMaterialLD.canvas.draw(0,0,FBackMaterial);
        Getme24D(FBackMaterialD,64);
        Getme24L(FBackMaterialL,64);
        Case FMaterialShadowMode of
          msmDark: Getme24D(FBackMaterialLD,32);
          msmLight: Getme24L(FBackMaterialLD,32);
        end;
end;
//+++++++++++++++++++++++++
function TLiquid_Indicator.GetLColor(Value : Tcolor): Tcolor;
var
 R,G,B : Integer;
 V : Tcolor;

begin
  V:=ColortoRgb(Value);
  R:=(integer(V) shl 24) shr 24;
  G:=(integer(V) shl 16) shr 24;
  B:=(integer(V) shl 8)  shr 24;
  R:=R+32;G:=G+32;B:=B+32;
  if R>255 then R:=255;
  if G>255 then G:=255;
  if B>255 then B:=255;
  Result:=RGB(R,G,B);
end;
function TLiquid_Indicator.GetDColor(Value : Tcolor): Tcolor;
var
 R,G,B : Integer;
  V : Tcolor;

begin
  V:=ColortoRgb(Value);
  R:=(integer(V) shl 24) shr 24;
  G:=(integer(V) shl 16) shr 24;
  B:=(integer(V) shl 8)  shr 24;
  R:=R-32;G:=G-32;B:=B-32;
  if R<0 then R:=0;
  if G<0 then G:=0;
  if B<0 then B:=0;
  Result:=RGB(R,G,B);
end;
procedure TLiquid_Indicator.SETSignNumber(Value : Integer);
begin
  if (Value>0) and (Value<>FSignNumber) then
    begin
      fSignNumber:=Value;
      if (imloaded) or (csDesigning  in	componentstate) then
      begin
      GetIndicatorRgn;
      GetIndicatorImageRgns;
      invalidate;
      end;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorWidth(Value : Integer);
var
  Nw,Nh,dx,dy : integer;
  rgn : hrgn;
begin
  if (Value>0) and (Value<>FIndicatorWidth) then
    begin
      fIndicatorWidth:=Value;
      if (imloaded) or (csDesigning  in	componentstate) then
      begin
      dx:=(parent.width-parent.ClientWidth) div 2;
      dy:=parent.Height-parent.ClientHeight-dx;
      Nw:=round(abs(fIndicatorWidth*cos(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*sin(fIndicatorAngle*pi/1800)));
      Nh:=round(abs(fIndicatorWidth*sin(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*cos(fIndicatorAngle*pi/1800)));
      rgn:=createrectrgn(0,0,width,height);
      getwindowrgn(self.handle,rgn);
      offsetrgn(rgn,left+dx,top+dy);
      MoveWindow(self.handle,Left,Top,Nw,Nh,false);
      GetIndicatorRgn;
      invalidatergn(self.parent.handle,rgn,true);
      deleteobject(rgn);
      GetIndicatorImageRgns;
      parent.invalidate;
      end;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorHeight(Value : Integer);
var
  Nw,Nh,dx,dy : integer;
  rgn : hrgn;
begin
  if (Value>0) and (Value<>FIndicatorHeight) then
    begin
      fIndicatorHeight:=Value;
      if (imloaded) or (csDesigning  in	componentstate) then
      begin
      dx:=(parent.width-parent.ClientWidth) div 2;
      dy:=parent.Height-parent.ClientHeight-dx;
      Nw:=round(abs(fIndicatorWidth*cos(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*sin(fIndicatorAngle*pi/1800)));
      Nh:=round(abs(fIndicatorWidth*sin(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*cos(fIndicatorAngle*pi/1800)));
      rgn:=createrectrgn(0,0,width,height);
      getwindowrgn(self.handle,rgn);
      offsetrgn(rgn,left+dx,top+dy);
      MoveWindow(self.handle,Left,Top,Nw,Nh,false);
      GetIndicatorRgn;
      invalidatergn(self.parent.handle,rgn,true);
      deleteobject(rgn);
      GetIndicatorImageRgns;
      parent.invalidate;
      end;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorAngle(Value : Integer);
var
  Nw,Nh : integer;
begin
  if (Value<>FIndicatorAngle) then
    begin
      fIndicatorAngle:=Value;
      if (imloaded) or (csDesigning  in	componentstate) then
      begin
      Nw:=round(abs(fIndicatorWidth*cos(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*sin(fIndicatorAngle*pi/1800)));
      Nh:=round(abs(fIndicatorWidth*sin(fIndicatorAngle*pi/1800))+abs(fIndicatorHeight*cos(fIndicatorAngle*pi/1800)));
      MoveWindow(self.handle,Left,Top,Nw,Nh,true);
      GetIndicatorRgn;
      GetIndicatorImageRgns;
      invalidate;
      end;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorGap(Value : Integer);
begin
  if (Value<>FIndicatorGap) then
    begin
      fIndicatorGap:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorString(Value : string);
begin
  if Value<>FIndicatorString then
    begin
      FIndicatorString:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorKind(Value : TIAIndicatorKind);
begin
  if Value<>FIndicatorKind then
    begin
      FIndicatorKind:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETInnerBevel(Value : TIABevelKind);
begin
  if Value<>FInnerBevel then
    begin
      FInnerBevel:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETOuterBevel(Value : TIABevelKind);
begin
  if Value<>FOuterBevel then
    begin
      FOuterBevel:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETInnerBevelWidth(Value : Integer);
begin
  if (Value>0) and (Value<>FInnerBevelWidth) then
    begin
      FInnerBevelWidth:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETOuterBevelWidth(Value : Integer);
begin
  if (Value>0) and (Value<>FOuterBevelWidth) then
    begin
      FOuterBevelWidth:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETBevelColor(Value : TColor);
begin
  if Value<>FBevelColor then
    begin
      FBevelColor:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETBackColor(Value : TColor);
begin
  if Value<>FBackColor then
    begin
      FBackColor:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETNumberForeColor(Value : TColor);
begin
  if Value<>FNumberForeColor then
    begin
      FNumberForeColor:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETNumberForeColor1(Value : TColor);
begin
  if Value<>FNumberForeColor1 then
    begin
      FNumberForeColor1:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETNumberBackColor(Value : TColor);
begin
  if Value<>FNumberBackColor then
    begin
      FNumberBackColor:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETIndicatorStyle(Value : TIAIndicatorStyle);
begin
  if Value<>FIndicatorStyle then
    begin
      FIndicatorStyle:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETMouseAction(Value : TIAMouseAction);
begin
  if Value<>FMouseAction then
    begin
      FMouseAction:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETTransparentMode(Value : TIATransparentMode);
begin
  if Value<>FTransparentMode then
    begin
      FTransparentMode:=Value;
      Case Value of
        tmNone : ControlStyle:=Controlstyle+[csOpaque];
        tmBackOnly,tmAll :  ControlStyle:=Controlstyle-[csOpaque];
      end;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETColorScheme(Value : TIAColorScheme);
begin
  FColorScheme:=Value;
  if (ImLoaded) or ((csDesigning  in componentstate)) then
    begin
      case FColorScheme of
        csSilver :
          begin
            FBackColor       :=$00dddddd;
            FBevelColor      :=$00dddddd;
            FNumberBackColor :=$00d3d3d3;
            FNumberForeColor :=$00363636;
            FNumberForeColor1:=clRed;
          end;
        csInvSilver :
          begin
            FBackColor       :=clBlack;
            FBevelColor      :=$00dddddd;
            FNumberBackColor :=$00363636;
            FNumberForeColor :=$00dddddd;
            FNumberForeColor1:=clRed;
          end;
        csWhite :
          begin
            FBackColor       :=clWhite;
            FBevelColor      :=$00dddddd;
            FNumberBackColor :=clWhite;
            FNumberForeColor :=clBlack;
            FNumberForeColor1:=clred;
          end;
        csBlack :
          begin
            FBackColor       :=clBlack;
            FBevelColor      :=clGray;
            FNumberBackColor :=clBlack;
            FNumberForeColor :=clWhite;
            FNumberForeColor1:=$000080ff;
          end;
        csPlasma :
          begin
            FBackColor       :=clMaroon;
            FBevelColor      :=clMaroon;
            FNumberBackColor :=$00400080;
            FNumberForeColor :=$0048bfff;
            FNumberForeColor1:=$0080ffff;
          end;
        csInvPlasma :
          begin
            FBackColor       :=$0048bfff;
            FBevelColor      :=$0048bfff;
            FNumberBackColor :=$0057A3F7;
            FNumberForeColor :=clMaroon;
            FNumberForeColor1:=$002747FC;
          end;
        csTIAN :
          begin
            FBackColor       :=$00A00000;
            FBevelColor      :=$00A00000;
            FNumberBackColor :=$00804000;
            FNumberForeColor :=$00Daf376;
            FNumberForeColor1:=clYellow;
          end;            

      end;//Case...

      postmessage(self.handle,CM_NEEDREPAINT,0,0);   
    end;
end;
procedure TLiquid_Indicator.SETBackMaterial(Value : TBitmap);
begin
  FBackMaterial.Assign(Value);
  SetLD;
  GetIndicatorImageRgns;
  invalidate;
end;
procedure TLiquid_Indicator.SETForeMaterial(Value : TBitmap);
begin
  FForeMaterial.Assign(Value);
  GetIndicatorImageRgns;
  invalidate;
end;
procedure TLiquid_Indicator.SETMaterialShadowMode(Value : TIAMaterialShadowMode);
begin
  if Value<>FMaterialShadowMode then
    begin
      FMaterialShadowMode:=Value;
      SetLD;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETMaterialMode(Value : TIAMaterialMode);
begin
  if Value<>FMaterialMode then
    begin
      FMaterialMode:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.SETForeMaterialMode(Value : TIAMaterialMode);
begin
  if Value<>FForeMaterialMode then
    begin
      FForeMaterialMode:=Value;
      GetIndicatorImageRgns;
      invalidate;
    end;
end;
procedure TLiquid_Indicator.CMMouseEnter(var Message: TMessage);
begin
  Moused:=true;
  postmessage(self.handle,CM_NEEDREPAINT,0,0);
  inherited;
  if assigned(onMouseenter) then onMouseenter(self);
end;
procedure TLiquid_Indicator.CMMouseLeave(var Message: TMessage);
begin
  Moused:=false;
  postmessage(self.handle,CM_NEEDREPAINT,0,0);
  inherited;
  if assigned(onMouseexit) then onMouseExit(self);
end;
procedure TLiquid_Indicator.CMNEEDREPAINT(var Message: TMessage);
begin
  inherited;
  GetIndicatorImageRgns;
  invalidate;
end;
procedure TLiquid_Indicator.WMSIZE(var Message: TMessage);
var
  cs,sn : double;
  Nw,Nh,dx,dy : integer;
  rgn : hrgn;
begin
  inherited;
  cs:=cos(fIndicatorAngle*pi/1800);
  sn:=sin(fIndicatorAngle*pi/1800);
  if abs(sn)<1e-4 then
    begin
      fIndicatorHeight:=height;
      fIndicatorWidth:=width;
      GetIndicatorRgn;
      postmessage(self.handle,CM_NEEDREPAINT,0,0);
    end else
    begin
      if abs(cs)<1e-4 then
        begin
          fIndicatorHeight:=width;
          fIndicatorWidth:=height;
          GetIndicatorRgn;
          GetIndicatorImageRgns;
          invalidate;
        end else
        begin
          dx:=(parent.width-parent.ClientWidth) div 2;
          dy:=parent.Height-parent.ClientHeight-dx;
          Nw:=round(abs(fIndicatorWidth*cos(fIndicatorAngle*pi/1800))+
                    abs(fIndicatorHeight*sin(fIndicatorAngle*pi/1800)));
          Nh:=round(abs(fIndicatorWidth*sin(fIndicatorAngle*pi/1800))+
                    abs(fIndicatorHeight*cos(fIndicatorAngle*pi/1800)));
          rgn:=createrectrgn(0,0,width,height);
          getwindowrgn(self.handle,rgn);
          offsetrgn(rgn,left+dx,top+dy);
          MoveWindow(self.handle,Left,Top,Nw,Nh,false);
          invalidatergn(self.parent.handle,rgn,true);
          deleteobject(rgn);
          parent.invalidate;
          postmessage(self.handle,CM_NEEDREPAINT,0,0);
        end;
    end;
end;
procedure TLiquid_Indicator.PolyRotate(var points : Tpoints;x,y,Angle : integer);
var
  i : integer;
  dx,dy,R,alfa : double;
begin
  PolyZrotate(points,Angle);
  R:=sqrt(sqr(x)+sqr(y));
  if round(r)<>0 then
    begin
      if x<>0 then
        begin
          alfa:=Angle*pi/1800+arctan2(-y/1000.0,x/1000.0);
        end else
        begin
          if y>0 then alfa:=Angle*pi/1800-pi/2.0
                 else alfa:=Angle*pi/1800+pi/2.0;
        end;
      dx:=x-r*cos(alfa);
      dy:=y+r*sin(alfa);
    end else
    begin
      dx:=0;
      dy:=0;
    end;;
  for i:=0 to High(points) do
    begin
      inc(points[i].x,round(dx));
      inc(points[i].y,round(dy));
    end;
end;
procedure TLiquid_Indicator.PolyZrotate(var points : Tpoints;Angle : integer);
var
i : integer;
R,Alfa : Double;
begin
  for i:=0 to High(points) do
    begin
      R:=sqrt(sqr(points[i].x)+sqr(points[i].y));
      if round(r)<>0 then
      begin
        if points[i].x<>0 then
        begin
          alfa:=Angle*pi/1800+arctan2(-points[i].y/1000.0,points[i].x/1000.0);
        end else
        begin
          if points[i].y>0 then alfa:=Angle*pi/1800-pi/2.0
                           else alfa:=Angle*pi/1800+pi/2.0;
        end;
        points[i].x:=round(r*cos(alfa));
        points[i].y:=-round(r*sin(alfa));
      end;
    end;
end;
procedure TLiquid_Indicator.ShiftPoly(var points : array of Tpoint;dx,dy : integer);
var
i: integer;
begin
  for i:=0 to High(Points) do
    begin
      points[i].x:=points[i].x+dx;
      points[i].y:=points[i].y+dy;
    end;
end;
procedure TLiquid_Indicator.GetIndicatorImageRgns;
var
 i,j,w,h,k,dgap,quoter,octo,dw : integer;
 p1,p2 : Tpoints;
 v1,v2 : Tintegers;
 n1,n2 : integer;
 sign : char;
 rgn : Hrgn;
 ssin,ccos : double;
 p : Tpoints;
 s : string;
begin
  deleteobject(ClipRgn);
  if FTransparentmode<>tmNone then ClipRgn:=createRectRgn(0,0,0,0);
  setlength(blackPolyPoly,0);
  setlength(Graypolypoly,0);
  setlength(Blackvertex,0);
  setlength(GrayVertex,0);
  BlackN:=0;
  GrayN:=0;
  dgap:=indicatorGap;
  if outerbevel<>bkNone then inc(dgap,fouterbevelwidth);
  if innerbevel<>bkNone then inc(dgap,finnerbevelwidth);
  h:=indicatorheight-2*dgap;
  if (h<=0) or (h>indicatorheight) then h:=indicatorheight;
  w:=(indicatorwidth-2*dgap) div signnumber;
  if (w<=0) or (w>(indicatorwidth div signnumber)) then w:=indicatorwidth div signnumber;

  if (indicatorkind=ik7segment) or (indicatorkind=ik9segment) then
    begin
      Sign:='.';k:=1;
      for i:=1 to length(IndicatorString) do
        begin
          if k>signnumber then break;
          if (i=1) and ((indicatorString[i]='.') or (indicatorString[i]=',')) then
            begin
              inc(k);
            end else
            begin
              if (sign='.') or (sign=',') then
                begin
                  Sign:=indicatorString[i];
                  if (i=length(IndicatorString)) or ((sign='.') or (sign=',')) then
                    begin
                      inc(k);
                     end;
                end else
                begin
                  if ((indicatorString[i]='.') or (indicatorString[i]=',')) then
                    begin
                      inc(k);
                      Sign:=',';
                    end else
                    begin
                      inc(k);
                      Sign:=indicatorString[i];
                      if k>signnumber then break;
                      if (i=length(IndicatorString)) then
                        begin
                          inc(k);
                        end;
                    end;
                end;
            end;
        end;
      s:='';
      if (length(indicatorString)>0) and ((indicatorString[1]='.') or (indicatorString[1]=',')) then  s:=s+' ';
      for i:=1 to signnumber-k+1 do begin s:=s+' '; end;



      s:=s+indicatorString;
      //==  
      Sign:='.';k:=1;
      for i:=1 to length(s) do
        begin
          if k>signnumber then break;
          if (i=1) and ((s[i]='.') or (s[i]=',')) then
            begin
              GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,true,p1,p2,v1,v2,n1,n2);
              inc(k);
              Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
              Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
            end else
            begin
              if (sign='.') or (sign=',') then
                begin
                  Sign:=s[i];
                  if (i=length(s)) or ((sign='.') or (sign=',')) then
                    begin
                      GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,false,p1,p2,v1,v2,n1,n2);
                      inc(k);
                      Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
                      Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
                    end;
                end else
                begin
                  if ((s[i]='.') or (s[i]=',')) then
                    begin
                      GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,true,p1,p2,v1,v2,n1,n2);
                      inc(k);
                      Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
                      Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
                      Sign:=',';
                    end else
                    begin
                      GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,false,p1,p2,v1,v2,n1,n2);
                      inc(k);
                      Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
                      Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
                      Sign:=s[i];
                      if k>signnumber then break;
                      if (i=length(s)) then
                        begin
                          GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,false,p1,p2,v1,v2,n1,n2);
                          inc(k);
                          Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
                          Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
                        end;
                    end;
                end;
            end;
        end;
    end else
    begin
      k:=1;i:=0;
       while (k<=FSignNumber) do
         begin
           inc(i);
           if length(IndicatorString)>=FSignNumber then
             begin
               sign:=indicatorstring[i];
             end else
             begin
               if i<=(FSignNumber-length(IndicatorString)) then
                 begin
                   sign:=' ';
                 end else
                 begin
                   sign:=indicatorstring[i-(FSignNumber-length(IndicatorString))];
                   if (i-(FSignNumber-length(IndicatorString)))<>length(IndicatorString) then
                 end;
             end;// length(IndicatorString)>=FSignNumber then



            GetSinglePolyPoly(rect((k-1)*w,0,k*w,h),sign,false,p1,p2,v1,v2,n1,n2);
            inc(k);
            Addpolypoly(BlackPolypoly,p1,BlackVertex,v1,BlackN,n1);
            Addpolypoly(GrayPolypoly,p2,GrayVertex,v2,GrayN,n2);
          end;//while;

    
    end;
  ShiftPoly(BlackPolypoly,dgap,dgap);
  ShiftPoly(GrayPolypoly,dgap,dgap);
  PolyRotate(BlackPolypoly,FIndicatorWidth div 2,FIndicatorHeight div 2,FIndicatorAngle);
  ShiftPoly(BlackPolypoly,(width-FIndicatorWidth) div 2,(Height-FIndicatorHeight) div 2);
  PolyRotate(GrayPolypoly,FIndicatorWidth div 2,FIndicatorHeight div 2,FIndicatorAngle);
  ShiftPoly(GrayPolypoly,(width-FIndicatorWidth) div 2,(Height-FIndicatorHeight) div 2);

  CanvasBitmap.FreeImage;
  CanvasBitmap.Width:=width;
  CanvasBitmap.Height:=height;

   with CanvasBitmap.Canvas do
    begin

      CanvasBitmap.Canvas.brush.Color:=FBackColor;
      if FBackMaterial.Width=0 then
        begin
          CanvasBitmap.Canvas.fillrect(rect(0,0,CanvasBitmap.Width,CanvasBitmap.Height));
        end else
        begin
          DrawLD(FBackMaterial);
        end;  
      //Draw outer Bevel;
      ssin:=sin(FIndicatorAngle*pi/1800);ccos:=cos(FIndicatorAngle*pi/1800);
      quoter:=0;
      if (ccos>=0) and (SSin>=0) then quoter:=1;
      if (ccos>=0) and (SSin<=0) then quoter:=4;
      if (ccos<=0) and (SSin>=0) then quoter:=2;
      if (ccos<=0) and (SSin<=0) then quoter:=3;
      case quoter of
        1:
          begin
            if ssin>(1/sqrt(2)) then octo:=2 else octo:=1;
          end;
        2:
          begin
            if ssin>(1/sqrt(2)) then octo:=3 else octo:=4;
          end;
        3:
          begin
            if ssin<(-1/sqrt(2)) then octo:=6 else octo:=5;
          end;
        4:
          begin
            if ssin<(-1/sqrt(2)) then octo:=7 else octo:=8;
          end;
        else octo:=0;
      end;

      setlength(p,16);
      p[0].x:=0;                                p[0].y:=0;
      p[1].x:=FIndicatorWidth;                  p[1].y:=0;
      p[2].x:=FIndicatorWidth-FOuterBevelWidth; p[2].y:=FOuterBevelWidth;
      p[3].x:=FOuterBevelWidth;                 p[3].y:=FOuterBevelWidth;
      p[4].x:=0;                                p[4].y:=0;
      p[5].x:=0;                                p[5].y:=FIndicatorHeight;
      p[6].x:=FOuterBevelWidth;                 p[6].y:=FIndicatorHeight-FOuterBevelWidth;
      p[7].x:=FOuterBevelWidth;                 p[7].y:=FOuterBevelWidth;
      p[8].x:=0;                                p[8].y:=FIndicatorHeight;
      p[9].x:=FOuterBevelWidth;                 p[9].y:=FIndicatorHeight-FOuterBevelWidth;
      p[10].x:=FIndicatorWidth-FOuterBevelWidth;p[10].y:=FIndicatorHeight-FOuterBevelWidth;
      p[11].x:=FIndicatorWidth;                 p[11].y:=FIndicatorHeight;
      p[12].x:=FIndicatorWidth;                 p[12].y:=FIndicatorHeight;
      p[13].x:=FIndicatorWidth-FOuterBevelWidth;p[13].y:=FIndicatorHeight-FOuterBevelWidth;
      p[14].x:=FIndicatorWidth-FOuterBevelWidth;p[14].y:=FOuterBevelWidth;
      p[15].x:=FIndicatorWidth;                 p[15].y:=0;
      PolyRotate(p,FIndicatorWidth div 2,FIndicatorHeight div 2,FIndicatorAngle);
      ShiftPoly(p,(width-FIndicatorWidth) div 2,(Height-FIndicatorHeight) div 2);
      case FOuterBevel of
        bkUp :
          begin
            case octo of
              1,8:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;  
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              2,3:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              4,5:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              6,7:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                     if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;

            end;    
          end;
        bkDown :
          begin
            case octo of
              1,8:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              2,3:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              4,5:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              6,7:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;

            end;
          end;
      end;      
     //-----------------------
     if FOuterBevel<>bkNone then dw:=FOuterBevelwidth else dw:=0;
      p[0].x:=dw;                                p[0].y:=dw;
      p[1].x:=FIndicatorWidth-dw;                  p[1].y:=dw;
      p[2].x:=FIndicatorWidth-FInnerBevelWidth-dw; p[2].y:=FInnerBevelWidth+dw;
      p[3].x:=FInnerBevelWidth+dw;                 p[3].y:=FInnerBevelWidth+dw;
      p[4].x:=dw;                                p[4].y:=dw;
      p[5].x:=dw;                                p[5].y:=FIndicatorHeight-dw;
      p[6].x:=FInnerBevelWidth+dw;                 p[6].y:=FIndicatorHeight-FInnerBevelWidth-dw;
      p[7].x:=FInnerBevelWidth+dw;                 p[7].y:=FInnerBevelWidth+dw;
      p[8].x:=dw;                                p[8].y:=FIndicatorHeight-dw;
      p[9].x:=FInnerBevelWidth+dw;                 p[9].y:=FIndicatorHeight-FInnerBevelWidth-dw;
      p[10].x:=FIndicatorWidth-FInnerBevelWidth-dw;p[10].y:=FIndicatorHeight-FInnerBevelWidth-dw;
      p[11].x:=FIndicatorWidth-dw;                 p[11].y:=FIndicatorHeight-dw;
      p[12].x:=FIndicatorWidth-dw;                 p[12].y:=FIndicatorHeight-dw;
      p[13].x:=FIndicatorWidth-FInnerBevelWidth-dw;p[13].y:=FIndicatorHeight-FInnerBevelWidth-dw;
      p[14].x:=FIndicatorWidth-FInnerBevelWidth-dw;p[14].y:=FInnerBevelWidth+dw;
      p[15].x:=FIndicatorWidth-dw;                 p[15].y:=dw;
      PolyRotate(p,FIndicatorWidth div 2,FIndicatorHeight div 2,FIndicatorAngle);
      ShiftPoly(p,(width-FIndicatorWidth) div 2,(Height-FIndicatorHeight) div 2);
     case FInnerBevel of
        bkUp :
          begin
            case octo of
              1,8:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              2,3:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              4,5:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              6,7:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;

            end;    
          end;
        bkDown :
          begin
            case octo of
              1,8:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              2,3:
                begin
                      CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              4,5:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;
              6,7:
                begin
                     CanvasBitmap.Canvas.Brush.Color:=getDcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[4],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[8],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialD);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      CanvasBitmap.Canvas.Brush.Color:=getLcolor(FBevelColor);
                      rgn:=CreatePolygonRgn(p[12],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                      rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
                      if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
                      if FBackMaterial.Width=0 then
                        begin
                          fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
                        end else
                        begin
                          selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
                          DrawLD(FBackMaterialL);
                          selectClipRGN(CanvasBitmap.Canvas.Handle,0);
                        end;
                      deleteobject(rgn);
                end;

            end;
          end;
      end;  

     ////-----------------------
      if BlackPolyPoly<>nil then
        begin
        //  TIAMouseAction = (maNone,maColor,maWidth,maBoth);
          if Moused and ((FMouseaction=maColor) or (FMouseAction=maBoth)) then
          CanvasBitmap.Canvas.brush.Color:=FNumberForeColor1 else
          CanvasBitmap.Canvas.brush.Color:=FNumberForeColor;
          rgn:=createPolyPolygonRgn(BlackPolyPoly[0],BlackVertex[0],BlackN,ALTERNATE);
          if FTransparentmode<>tmNone then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
          if (FForeMaterial.Width=0) or
             ( Moused and ((FMouseaction=maColor) or (FMouseAction=maBoth))) then
            begin
              fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
            end else
            begin
              selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
              //------
 


              case FForeMaterialMode of
                 mmStretch :
                   begin
                     CanvasBitmap.Canvas.StretchDraw(
                      rect(0,0,CanvasBitmap.Width,CanvasBitmap.Height),FForeMaterial);
                   end;
                 mmTile :
                   begin
                     for i:=0 to CanvasBitmap.Width div FForeMaterial.Width do
                       for j:=0 to CanvasBitmap.Height div FForeMaterial.Height do
                         begin
                           CanvasBitmap.Canvas.Draw(i*FForeMaterial.width,j*FForeMaterial.height,FForeMaterial);
                         end;
                   end;
              end;

              //------
              selectClipRGN(CanvasBitmap.Canvas.Handle,0);
            end;  
          deleteobject(rgn);
        end;
      if GrayPolyPoly<>nil then
        begin
          CanvasBitmap.Canvas.brush.Color:=FNumberBackColor;
          rgn:=createPolyPolygonRgn(GrayPolyPoly[0],GrayVertex[0],GrayN,ALTERNATE);
          if FTransparentmode=tmBackOnly then combinergn(ClipRgn,ClipRgn,rgn,RGN_OR);
          if FBackMaterial.Width=0 then
            begin
              fillrgn(CanvasBitmap.Canvas.handle,rgn,CanvasBitmap.Canvas.brush.handle);
            end else
            begin
              selectClipRGN(CanvasBitmap.Canvas.Handle,RGN);
              DrawLD(FBackMaterialLD);
              selectClipRGN(CanvasBitmap.Canvas.Handle,0);
            end;  
          deleteobject(rgn);
        end;
     
    end;
end;
procedure TLiquid_Indicator.GetIndicatorRgn;
var
  p : Tpoints;
  rgn : HRGN;
begin
  setlength(p,4);
  p[0].x:=0;p[0].y:=0;
  p[1].x:=FIndicatorWidth;p[1].y:=0;
  p[2].x:=FIndicatorWidth;p[2].y:=FIndicatorHeight;
  p[3].x:=0;p[3].y:=FIndicatorHeight;
  PolyRotate(p,FIndicatorWidth div 2,FIndicatorHeight div 2,FIndicatorAngle);
  ShiftPoly(p,(width-FIndicatorWidth) div 2,(Height-FIndicatorHeight) div 2);

  rgn:=CreatePolygonRgn(p[0],4,ALTERNaTE);
  if assigned(parent) then  setwindowrgn(self.handle,rgn,True) else deleteobject(Rgn);
end;
procedure TLiquid_Indicator.Paint;

begin
  inherited paint;
  SelectClipRgn(self.Canvas.Handle,ClipRGN);

  with self.Canvas do
    begin
      self.Canvas.draw(0,0,canvasbitmap);
    end;
end;
procedure TLiquid_Indicator.loaded;
begin
  inherited Loaded;
  Imloaded:=true;
  SetLD;
  GetIndicatorRgn;
  GetIndicatorImageRgns;
  invalidate;
end;

constructor TLiquid_Indicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csOpaque];
  FSignNumber:=10;
  FIndicatorWidth:=172;
  FIndicatorHeight:=36;
  FIndicatorAngle:=0;
  FIndicatorGap:=2;
  FIndicatorKind:=ik7segment;
  FInnerBevel:=bkDown;
  FOuterBevel:=bkUp;
  FInnerBevelWidth:=2;
  FOuterBevelWidth:=2;
  FBevelColor:=$00DDDDDD;
  FBackColor:=$00DDDDDD;
  FNumberForeColor:=$00363636;
  FNumberForeColor1:=clRed;
  FNumberBackColor:=$00d3d3d3;
  FColorScheme:=csSilver;
  FIndicatorStyle:=isNormal;
  FTransparentMode:=tmNone;
  FMouseAction:=maNone;
  Imloaded:=false;
  FBackMaterial:=TBitmap.create;
  FBackMaterialL:=TBitmap.create;
  FBackMaterialD:=TBitmap.create;
  FBackMaterialLD:=TBitmap.create;
  FForeMaterial:=TBitmap.Create;
  FMaterialShadowMode:=msmDark;
  FMaterialMode:=mmStretch;
  FForeMaterialMode:=mmStretch;


  canvasbitmap:=TBitmap.Create;
  canvasBitmap.PixelFormat:=pf32bit;
  canvasbitmap.Width:=width;
  canvasbitmap.Height:=height;
  width:=FIndicatorWidth;
  height:=FIndicatorHeight;
  GetIndicatorRgn;
  GetIndicatorImageRgns;
end;
destructor TLiquid_Indicator.Destroy;
begin
  deleteobject(ClipRGN);
  canvasbitmap.free;
  FForeMaterial.free;
  FBackMaterial.free;
  FBackMaterialLD.free;
  FBackMaterialD.free;
  FBackMaterialL.free;
  
  inherited destroy;
end;
procedure TLiquid_Indicator.Addpolypoly(var p1,p2 : Tpoints;var V1,V2 : Tintegers;var n1,n2 :integer);
var
  tempP : array of Tpoint;
  tempV : array of integer;
  j : integer;
begin
  setlength(tempP,length(p1)+length(p2));
  setlength(tempV,length(v2)+length(v1));
  for j:=0 to length(p1)-1 do
    begin
      tempP[j]:=p1[j];
    end;
  for j:=length(p1) to  length(p1)+length(p2)-1 do
    begin
      tempP[j]:=p2[j-length(p1)];
    end;
  for j:=0 to length(v1)-1 do
    begin
      tempV[j]:=v1[j];
    end;
  for j:=0 to length(v2)-1 do
    begin
      tempV[j+length(v1)]:=v2[j];
    end;  
 setlength(p1,length(tempP));
 setlength(v1,length(tempV));
  for j:=0 to length(tempP)-1 do
    begin
      p1[j]:=tempP[j];
    end;
  for j:=0 to length(tempV)-1 do
    begin
      v1[j]:=tempV[j];
    end;
  n1:=n1+n2;
end;
procedure TLiquid_Indicator.AddPolytoPPoly(var p2 : Tpoints;
                 var v2 :Tintegers;var p :array of Tpoint);
var
  tempP : array of Tpoint;
  tempV : array of integer;
  j : integer;
begin
  setlength(tempP,length(p)+length(p2));
  setlength(tempV,length(v2)+1);
  for j:=0 to length(p2)-1 do
    begin
      tempP[j]:=p2[j];
    end;
  for j:=length(p2) to  length(p)+length(p2)-1 do
    begin
      tempP[j]:=p[j-length(p2)];
    end;
  for j:=0 to length(v2)-1 do
    begin
      tempV[j]:=v2[j];
    end;
 tempV[length(v2)]:=length(p);
 setlength(p2,length(tempP));
 setlength(v2,length(tempV));
  for j:=0 to length(tempP)-1 do
    begin
      p2[j]:=tempP[j];
    end;
  for j:=0 to length(tempV)-1 do
    begin
      v2[j]:=tempV[j];
    end;
end;
procedure TLiquid_Indicator.GetSinglePolyPoly(R : Trect;Sign : Char;Pointed : Boolean;var p1,p2 :Tpoints;
                                var v1,v2 :Tintegers;var n1,n2 :integer);
var
  i,j : integer;
  matrix : string;
  p : Tpoints;
  m5x7 : array [1..7] of string;

begin
  setlength(p1,0);
  setlength(p2,0);
  setlength(v1,0);
  setlength(v2,0);
  n1:=0;n2:=0;

  case IndicatorKind of
    ik7segment :
      begin
        case Sign of
          '1' : matrix:='0001100';
          '2' : matrix:='1011011';
          '3' : matrix:='0011111';
          '4' : matrix:='0101101';
          '5' : matrix:='0110111';
          '6' : matrix:='1110111';
          '7' : matrix:='0011100';
          '8' : matrix:='1111111';
          '9' : matrix:='0111111';
          '0' : matrix:='1111110';
          'A' : matrix:='1111101';
          'o' : matrix:='1000111';
          'b' : matrix:='1100111';
          'd' : matrix:='1001111';
          '-' : matrix:='0000001';
          '_' : matrix:='0000010';
          'r' : matrix:='1000001';
          'O' : matrix:='1111110';
          'C' : matrix:='1110010';
          'E' : matrix:='1110011';
          'c' : matrix:='1000011';
          'F' : matrix:='1110001';
          'G' : matrix:='1110110';
          'H' : matrix:='1101101';
          'h' : matrix:='1100101';
          'L' : matrix:='1100010';
          'P' : matrix:='1111001';
          'U' : matrix:='1101110';
          'u' : matrix:='1000110';
          'Y' : matrix:='0101111';
          'n' : matrix:='1000101';
          't' : matrix:='1100011';
          'l' : matrix:='1100000';
          'i' : matrix:='1000000';
          'J' : matrix:='1001110';
          'j' : matrix:='0000110';

          else  matrix:='0000000';
        end;//case Sign...
        if Pointed then matrix:=matrix+'1' else matrix:=matrix+'0';
        if (Sign='.') or (sign=',') then matrix:='00000001';
        for i:=1 to 8 do
          begin
            GetsinglePoly7segment(r,i,p);
            if p<>nil then shiftpoly(p,r.left,r.top);
                    case matrix[i] of
              '0' :
                begin
                  AddPolytoPPoly(p2,v2,p);
                  inc(n2);
                end;
              '1' :
                begin
                  AddPolytoPPoly(p1,v1,p);
                  inc(n1);
                end;
            end;      

          end;

      end; // ik7segment
    ik9segment :
      begin
        case Sign of
          '1' : matrix:='000110000';
          '2' : matrix:='101101100';
          '3' : matrix:='001111100';
          '4' : matrix:='010110100';
          '5' : matrix:='011011100';
          '6' : matrix:='111011100';
          '7' : matrix:='001110000';
          '8' : matrix:='111111100';
          '9' : matrix:='011111100';
          '0' : matrix:='111111000';
          'A' : matrix:='111110100';
          'o' : matrix:='100011100';
          'b' : matrix:='110011100';
          'd' : matrix:='100111100';
          '-' : matrix:='000000100';
          '_' : matrix:='000001000';
          'r' : matrix:='100000100';
          'O' : matrix:='111111000';
          'C' : matrix:='111001000';
          'E' : matrix:='111001100';
          'c' : matrix:='100001100';
          'F' : matrix:='111000100';
          'G' : matrix:='111011000';
          'H' : matrix:='110110100';
          'h' : matrix:='110010100';
          'L' : matrix:='110001000';
          'P' : matrix:='111100100';
          'U' : matrix:='110111000';
          'u' : matrix:='100011000';
          'Y' : matrix:='010111100';
          'n' : matrix:='100010100';
          't' : matrix:='110001100';
          'l' : matrix:='110000000';
          'i' : matrix:='100000000';
          'J' : matrix:='100111000';
          'j' : matrix:='000011000';
          ':' : matrix:='000000011';
          else  matrix:='000000000';
        end;//case Sign...
        if Pointed then matrix:=matrix+'1' else matrix:=matrix+'0';
        if (Sign='.') or (sign=',') then matrix:='0000000001';
        for i:=1 to 10 do
          begin
            GetsinglePoly7segment(r,i,p);
            if p<>nil then shiftpoly(p,r.left,r.top);
                    case matrix[i] of
              '0' :
                begin
                  AddPolytoPPoly(p2,v2,p);
                  inc(n2);
                end;
              '1' :
                begin
                  AddPolytoPPoly(p1,v1,p);
                  inc(n1);
                end;
            end;      

          end;

      end; // ik9segment
    ik5x7matrix :
      begin
        case Sign of
          'A' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='11111';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='10001';
              end;
           '.' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00110';
                m5x7[7]:='00110';
              end;
           ',' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00110';
                m5x7[6]:='00110';
                m5x7[7]:='00010';
              end;
            '1' :
              begin
                m5x7[1]:='00010';
                m5x7[2]:='00110';
                m5x7[3]:='00010';
                m5x7[4]:='00010';
                m5x7[5]:='00010';
                m5x7[6]:='00010';
                m5x7[7]:='00111';
              end;
            '2' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='00001';
                m5x7[4]:='00010';
                m5x7[5]:='00100';
                m5x7[6]:='01000';
                m5x7[7]:='11111';
              end;
            '3' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='00001';
                m5x7[4]:='00110';
                m5x7[5]:='00001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            '4' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='01111';
                m5x7[5]:='00001';
                m5x7[6]:='00001';
                m5x7[7]:='00001';
              end;
            '5' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='11110';
                m5x7[5]:='00001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            '6' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10000';
                m5x7[4]:='11110';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            '7' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='00001';
                m5x7[3]:='00001';
                m5x7[4]:='00010';
                m5x7[5]:='00010';
                m5x7[6]:='00010';
                m5x7[7]:='00010';
              end;
            '8' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='01110';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            '9' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='01111';
                m5x7[5]:='00001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            '0' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
              end;
            'B' :
              begin
                m5x7[1]:='11110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='11110';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='11110';
            end;
            'C' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10000';
                m5x7[4]:='10000';
                m5x7[5]:='10000';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'D' :
              begin
                m5x7[1]:='11110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='11110';
            end;
            'E' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='11110';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='11111';
            end;
            'F' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='11110';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='10000';
            end;
            'G' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10000';
                m5x7[4]:='10000';
                m5x7[5]:='10011';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'H' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='11111';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='10001';
            end;
            'I' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='00100';
                m5x7[3]:='00100';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='01110';
            end;
            'J' :
              begin
                m5x7[1]:='00011';
                m5x7[2]:='00001';
                m5x7[3]:='00001';
                m5x7[4]:='00001';
                m5x7[5]:='00001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'K' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10010';
                m5x7[3]:='10100';
                m5x7[4]:='11000';
                m5x7[5]:='10100';
                m5x7[6]:='10010';
                m5x7[7]:='10001';
            end;
            'L' :
              begin
                m5x7[1]:='10000';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='10000';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='11111';
            end;
            'M' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='11011';
                m5x7[3]:='10101';
                m5x7[4]:='10101';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='10001';
            end;
            'N' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='11001';
                m5x7[3]:='11001';
                m5x7[4]:='10101';
                m5x7[5]:='10111';
                m5x7[6]:='10011';
                m5x7[7]:='10001';
            end;
            'O' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'P' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='11110';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='10000';
            end;
            'Q' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10101';
                m5x7[6]:='10011';
                m5x7[7]:='01111';
            end;
            'R' :
              begin
                m5x7[1]:='11110';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='11110';
                m5x7[5]:='10100';
                m5x7[6]:='10010';
                m5x7[7]:='10001';
            end;
            'S' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='10000';
                m5x7[4]:='01110';
                m5x7[5]:='00001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'T' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='00100';
                m5x7[3]:='00100';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='00100';
            end;
            'U' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'V' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='01010';
                m5x7[7]:='00100';
            end;
            'W' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='10001';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10101';
                m5x7[6]:='10101';
                m5x7[7]:='01010';
            end;
            'X' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='01010';
                m5x7[3]:='01110';
                m5x7[4]:='00100';
                m5x7[5]:='01110';
                m5x7[6]:='01010';
                m5x7[7]:='10001';
            end;
            'Y' :
              begin
                m5x7[1]:='10001';
                m5x7[2]:='01010';
                m5x7[3]:='00100';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='01110';
            end;
            'Z' :
              begin
                m5x7[1]:='11111';
                m5x7[2]:='00001';
                m5x7[3]:='00010';
                m5x7[4]:='00100';
                m5x7[5]:='01000';
                m5x7[6]:='10000';
                m5x7[7]:='11111';
            end;
            '-' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00000';
                m5x7[4]:='01110';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '+' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00100';
                m5x7[4]:='01110';
                m5x7[5]:='00100';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            ':' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00110';
                m5x7[3]:='00110';
                m5x7[4]:='00000';
                m5x7[5]:='00110';
                m5x7[6]:='00110';
                m5x7[7]:='00000';
            end;
            ';' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00110';
                m5x7[3]:='00110';
                m5x7[4]:='00000';
                m5x7[5]:='00110';
                m5x7[6]:='00110';
                m5x7[7]:='00010';
            end;
            '!' :
              begin
                m5x7[1]:='00110';
                m5x7[2]:='00110';
                m5x7[3]:='00110';
                m5x7[4]:='00110';
                m5x7[5]:='00000';
                m5x7[6]:='00110';
                m5x7[7]:='00110';
            end;
            '?' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10001';
                m5x7[3]:='00001';
                m5x7[4]:='00010';
                m5x7[5]:='00000';
                m5x7[6]:='00110';
                m5x7[7]:='00110';
            end;
            '*' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='10101';
                m5x7[3]:='01110';
                m5x7[4]:='11111';
                m5x7[5]:='01110';
                m5x7[6]:='10101';
                m5x7[7]:='00000';
            end;
            '/' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00001';
                m5x7[3]:='00010';
                m5x7[4]:='00100';
                m5x7[5]:='01000';
                m5x7[6]:='10000';
                m5x7[7]:='00000';
            end;
            '\' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='10000';
                m5x7[3]:='01000';
                m5x7[4]:='00100';
                m5x7[5]:='00010';
                m5x7[6]:='00001';
                m5x7[7]:='00000';
            end;
            '=' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='00000';
                m5x7[5]:='01110';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '_' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='11111';
            end;
            '(' :
              begin
                m5x7[1]:='00100';
                m5x7[2]:='01000';
                m5x7[3]:='01000';
                m5x7[4]:='01000';
                m5x7[5]:='01000';
                m5x7[6]:='01000';
                m5x7[7]:='00100';
            end;
            ')' :
              begin
                m5x7[1]:='00100';
                m5x7[2]:='00010';
                m5x7[3]:='00010';
                m5x7[4]:='00010';
                m5x7[5]:='00010';
                m5x7[6]:='00010';
                m5x7[7]:='00100';
            end;
            '%' :
              begin
                m5x7[1]:='11000';
                m5x7[2]:='11001';
                m5x7[3]:='00010';
                m5x7[4]:='00100';
                m5x7[5]:='01000';
                m5x7[6]:='10011';
                m5x7[7]:='00011';
            end;
            '&' :
              begin
                m5x7[1]:='00110';
                m5x7[2]:='01001';
                m5x7[3]:='10010';
                m5x7[4]:='01100';
                m5x7[5]:='01101';
                m5x7[6]:='10010';
                m5x7[7]:='01101';
            end;
            '^' :
              begin
                m5x7[1]:='00100';
                m5x7[2]:='01010';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '$' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10101';
                m5x7[3]:='10100';
                m5x7[4]:='01110';
                m5x7[5]:='00101';
                m5x7[6]:='10101';
                m5x7[7]:='01110';
            end;
            '#' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='01010';
                m5x7[3]:='11111';
                m5x7[4]:='01010';
                m5x7[5]:='11111';
                m5x7[6]:='01010';
                m5x7[7]:='00000';
            end;
            '@' :
              begin
                m5x7[1]:='01110';
                m5x7[2]:='10011';
                m5x7[3]:='10101';
                m5x7[4]:='10111';
                m5x7[5]:='10000';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            '~' :
              begin
                m5x7[1]:='01010';
                m5x7[2]:='10100';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '''':
              begin
                m5x7[1]:='01100';
                m5x7[2]:='11000';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '"' :
              begin
                m5x7[1]:='01010';
                m5x7[2]:='10100';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
            '<' :
              begin
                m5x7[1]:='00010';
                m5x7[2]:='00100';
                m5x7[3]:='01000';
                m5x7[4]:='10000';
                m5x7[5]:='01000';
                m5x7[6]:='00100';
                m5x7[7]:='00010';
            end;
            '>' :
              begin
                m5x7[1]:='01000';
                m5x7[2]:='00100';
                m5x7[3]:='00010';
                m5x7[4]:='00001';
                m5x7[5]:='00010';
                m5x7[6]:='00100';
                m5x7[7]:='01000';
            end;
            '[' :
              begin
                m5x7[1]:='11100';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='10000';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='11100';
            end;
            ']' :
              begin
                m5x7[1]:='00111';
                m5x7[2]:='00001';
                m5x7[3]:='00001';
                m5x7[4]:='00001';
                m5x7[5]:='00001';
                m5x7[6]:='00001';
                m5x7[7]:='00111';
            end;
            '{' :
              begin
                m5x7[1]:='00110';
                m5x7[2]:='01000';
                m5x7[3]:='01000';
                m5x7[4]:='10000';
                m5x7[5]:='01000';
                m5x7[6]:='01000';
                m5x7[7]:='00110';
            end;
            '}' :
              begin
                m5x7[1]:='01100';
                m5x7[2]:='00010';
                m5x7[3]:='00010';
                m5x7[4]:='00001';
                m5x7[5]:='00010';
                m5x7[6]:='00010';
                m5x7[7]:='01100';
            end;
            '|' :
              begin
                m5x7[1]:='00100';
                m5x7[2]:='00100';
                m5x7[3]:='00100';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='00100';
            end;
            'a' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='00001';
                m5x7[5]:='01111';
                m5x7[6]:='10001';
                m5x7[7]:='01111';
            end;
            'b' :
              begin
                m5x7[1]:='10000';
                m5x7[2]:='10000';
                m5x7[3]:='11110';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='11110';
            end;
            'c' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10001';
                m5x7[5]:='10000';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'd' :
              begin
                m5x7[1]:='00001';
                m5x7[2]:='00001';
                m5x7[3]:='01111';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01111';
            end;
            'e' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10001';
                m5x7[5]:='11110';
                m5x7[6]:='10000';
                m5x7[7]:='01111';
            end;
             'f' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10000';
                m5x7[5]:='11100';
                m5x7[6]:='10000';
                m5x7[7]:='10000';
            end;
            'g' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10001';
                m5x7[5]:='01111';
                m5x7[6]:='00001';
                m5x7[7]:='00110';
            end;
            'h' :
              begin
                m5x7[1]:='10000';
                m5x7[2]:='10000';
                m5x7[3]:='10000';
                m5x7[4]:='11110';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='10001';
            end;
             'i' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00100';
                m5x7[3]:='00000';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='01110';
            end;
            'j' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00010';
                m5x7[3]:='00000';
                m5x7[4]:='00010';
                m5x7[5]:='00010';
                m5x7[6]:='00010';
                m5x7[7]:='01100';
            end;
            'k' :
              begin
                m5x7[1]:='10000';
                m5x7[2]:='10000';
                m5x7[3]:='10010';
                m5x7[4]:='10100';
                m5x7[5]:='11000';
                m5x7[6]:='10100';
                m5x7[7]:='10010';
            end;
             'l' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00100';
                m5x7[3]:='01100';
                m5x7[4]:='00100';
                m5x7[5]:='00100';
                m5x7[6]:='00100';
                m5x7[7]:='01110';
            end;
            'm' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01010';
                m5x7[4]:='10101';
                m5x7[5]:='10101';
                m5x7[6]:='10101';
                m5x7[7]:='10101';
            end;
            'n' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='11110';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='10001';
            end;
             'o' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
            'p' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='11110';
                m5x7[4]:='10001';
                m5x7[5]:='11110';
                m5x7[6]:='10000';
                m5x7[7]:='10000';
            end;
            'q' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01111';
                m5x7[4]:='10001';
                m5x7[5]:='01111';
                m5x7[6]:='00001';
                m5x7[7]:='00001';
            end;
             'r' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01110';
                m5x7[4]:='10001';
                m5x7[5]:='10000';
                m5x7[6]:='10000';
                m5x7[7]:='10000';
            end;
            's' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='01111';
                m5x7[4]:='10000';
                m5x7[5]:='01110';
                m5x7[6]:='00001';
                m5x7[7]:='11110';
            end;
            't' :
              begin
                m5x7[1]:='10000';
                m5x7[2]:='10000';
                m5x7[3]:='11100';
                m5x7[4]:='10000';
                m5x7[5]:='10000';
                m5x7[6]:='10001';
                m5x7[7]:='01110';
            end;
             'u' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='10011';
                m5x7[7]:='01101';
            end;
            'v' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='10001';
                m5x7[6]:='01010';
                m5x7[7]:='00100';
            end;
            'w' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='10001';
                m5x7[4]:='10101';
                m5x7[5]:='10101';
                m5x7[6]:='10101';
                m5x7[7]:='01010';
            end;
             'x' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='10001';
                m5x7[4]:='01010';
                m5x7[5]:='00100';
                m5x7[6]:='01010';
                m5x7[7]:='10001';
            end;
            'y' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='10001';
                m5x7[4]:='10001';
                m5x7[5]:='01111';
                m5x7[6]:='00001';
                m5x7[7]:='00110';
            end;
            'z' :
              begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='11111';
                m5x7[4]:='00010';
                m5x7[5]:='00100';
                m5x7[6]:='01000';
                m5x7[7]:='11111';
            end;
          else
            begin
                m5x7[1]:='00000';
                m5x7[2]:='00000';
                m5x7[3]:='00000';
                m5x7[4]:='00000';
                m5x7[5]:='00000';
                m5x7[6]:='00000';
                m5x7[7]:='00000';
            end;
        end;//case Sign...

          for i:=1 to 7 do
            for j:=1 to 5 do
              begin
                GetsinglePoly5x7matrix(r,i,j,p);
                if p<>nil then shiftpoly(p,r.left,r.top);
                case m5x7[i][j] of
                  '0' :
                     begin
                       AddPolytoPPoly(p2,v2,p);
                       inc(n2);
                     end;
                   '1' :
                     begin
                       AddPolytoPPoly(p1,v1,p);
                       inc(n1);
                     end;
                end;
              end;
         

      end;
  end;//case  IndicatorKind....
end;
procedure TLiquid_Indicator.GetsinglePoly5x7matrix(r : trect;i,j : integer; var p : TPoints);
var
  w,h,dx : integer;
  dw,dh,rdx,rdy : double;
  r1 : Trect;
begin

  setlength(p,4);
  w:=r.Right-r.Left;
  dw:=w/8;

  rdx:=0;
  rdy:=0;
  r1:=r;
  dx:=-(trunc(dw/2)+1);
  inflaterect(r1,-(trunc(dw/2)+1),0);
  w:=r1.Right-r1.Left;
  h:=r1.Bottom-r1.Top;
  dw:=w/5;
  dh:=h/7;
  
   case FindicatorStyle of
    isThin :
      begin
        if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        begin
          rdx:=dw/2-1;
          rdy:=dh/2-1;
        end else
        begin
          rdx:=2*dw/3-1;
          rdy:=2*dh/3-1;
        end;
        if rdx<1.5 then rdx:=1.5;
        if rdy<1.5 then rdy:=1.5;
      end;
    isNormal:
      begin
         if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        begin
          rdx:=2*dw/3-1;
          rdy:=2*dh/3-1;
        end else
        begin
          rdx:=3*dw/4-1;
          rdy:=3*dh/4-1;
        end;
        if rdx<2.5 then rdx:=2.5;
        if rdy<2.5 then rdy:=2.5;
      end;
    isThick :
      begin
         if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        begin
          rdx:=3*dw/4-1;
          rdy:=3*dh/4-1;
        end else
        begin
          rdx:=dw-1;
          rdy:=dh-1;
        end;
        if rdx<3.5 then rdx:=3.5;
        if rdy<3.5 then rdy:=3.5;
      end;
  end;
  p[0].x:=trunc(-dx+(j-1)*dw);p[0].y:=trunc((i-1)*dh);
  p[1].x:=trunc(-dx+(j-1)*dw)+trunc(rdx);p[1].y:=trunc((i-1)*dh);
  p[2].x:=trunc(-dx+(j-1)*dw)+trunc(rdx);p[2].y:=trunc((i-1)*dh)+trunc(rdy);
  p[3].x:=trunc(-dx+(j-1)*dw);p[3].y:=trunc((i-1)*dh)+trunc(rdy);
end;
procedure TLiquid_Indicator.GetsinglePoly7segment(r : TRect;i : integer;var p : TPoints);
var
  w,h : integer;
  dw,dh,rd : double;
begin
  setlength(p,6);
  w:=r.Right-r.Left;
  h:=r.Bottom-r.Top;
  dw:=w/8;
  dh:=h/8;
  rd:=0;
  //  TIAMouseAction = (maNone,maColor,maWidth,maBoth);
  //if Moused and ((FMouseaction=maColor) or (FMouseAction=maBoth)) then
  case FindicatorStyle of
    isThin :
      begin
        if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        rd:=min(w/6,h/6) else rd:=min(w/8,h/8);
        if rd<2 then rd:=2;
      end;
    isNormal:
      begin
        if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        rd:=min(w/5,h/5) else rd:=min(w/6,h/6);
        if rd<3 then rd:=3;
      end;
    isThick :
      begin
        if Moused and ((FMouseaction=maWidth) or (FMouseAction=maBoth)) then
        rd:=min(w/4.5,h/4.5) else rd:=min(w/5,h/5);
        if rd<4 then rd:=4;
      end;
  end;       
  case i of
    1:
      begin
        p[0].x:=trunc(2*dw/2);p[0].y:=trunc(14*dh /2+rd/2)-1;
        p[1].x:=trunc(2*dw/2-rd/2);p[1].y:=trunc(14*dh /2-rd/2)-1;
        p[2].x:=trunc(3*dw/2-rd/2);p[2].y:=trunc(8*dh /2+rd/2);
        p[3].x:=trunc(3*dw/2);p[3].y:=trunc(8*dh /2);
        p[4].x:=trunc(3*dw/2+rd/2);p[4].y:=trunc(8*dh /2+rd/2);
        p[5].x:=trunc(2*dw/2+rd/2);p[5].y:=trunc(14*dh /2-rd/2)-1;
      end;
    2:
      begin
        p[0].x:=trunc(3*dw /2);p[0].y:=trunc(8*dh /2)-1;
        p[1].x:=trunc(3*dw /2-rd/2);p[1].y:=trunc(8*dh /2-rd/2)-1;
        p[2].x:=trunc(4*dw /2-rd/2);p[2].y:=trunc(2*dh /2+rd/2);
        p[3].x:=trunc(4*dw /2);p[3].y:=trunc(2*dh /2-rd/2);
        p[4].x:=trunc(4*dw /2+rd/2);p[4].y:=trunc(2*dh /2+rd/2);
        p[5].x:=trunc(3*dw /2+rd/2);p[5].y:=trunc(8*dh /2-rd/2)-1;
      end;
    3:
      begin
        p[0].x:=trunc(4*dw /2)+1;p[0].y:=trunc(2*dh /2-rd/2);
        p[1].x:=trunc(4*dw /2+rd/2)+1;p[1].y:=trunc(2*dh /2-rd/2);
        p[2].x:=trunc(13*dw /2-rd/2)-1;p[2].y:=trunc(2*dh /2-rd/2);
        p[3].x:=trunc(13*dw /2)-1;p[3].y:=trunc(2*dh /2-rd/2);
        p[4].x:=trunc(13*dw /2-rd/2)-1;p[4].y:=trunc(2*dh /2+rd/2);
        p[5].x:=trunc(4*dw /2+rd/2)+1;p[5].y:=trunc(2*dh /2+rd/2);
      end;
    4:
      begin
        p[0].x:=trunc(13*dw /2);p[0].y:=trunc(2*dh /2-rd/2);
        p[1].x:=trunc(13*dw /2+rd/2);p[1].y:=trunc(2*dh /2+rd/2);
        p[2].x:=trunc(12*dw /2+rd/2);p[2].y:=trunc(8*dh /2-rd/2)-1;
        p[3].x:=trunc(12*dw /2);p[3].y:=trunc(8*dh /2)-1;
        p[4].x:=trunc(12*dw /2-rd/2);p[4].y:=trunc(8*dh /2-rd/2)-1;
        p[5].x:=trunc(13*dw /2-rd/2);p[5].y:=trunc(2*dh /2+rd/2);
      end;
    5:
      begin
        p[0].x:=trunc(12*dw /2);p[0].y:=trunc(8*dh /2);
        p[1].x:=trunc(12*dw /2+rd/2);p[1].y:=trunc(8*dh /2+rd/2);
        p[2].x:=trunc(11*dw /2+rd/2);p[2].y:=trunc(14*dh /2-rd/2)-1;
        p[3].x:=trunc(11*dw /2);p[3].y:=trunc(14*dh /2+rd/2)-1;
        p[4].x:=trunc(11*dw /2-rd/2);p[4].y:=trunc(14*dh /2-rd/2)-1;
        p[5].x:=trunc(12*dw /2-rd/2);p[5].y:=trunc(8*dh /2+rd/2);
      end;
    6:
      begin
        p[0].x:=trunc(2*dw /2)+1;p[0].y:=trunc(14*dh /2+rd/2);
        p[1].x:=trunc(2*dw /2+rd/2)+1;p[1].y:=trunc(14*dh/2-rd/2);
        p[2].x:=trunc(11*dw /2-rd/2)-1;p[2].y:=trunc(14*dh/2-rd/2);
        p[3].x:=trunc(11*dw /2)-1;p[3].y:=trunc(14*dh/2+rd/2);
        p[4].x:=trunc(11*dw /2-rd/2)-1;p[4].y:=trunc(14*dh/2+rd/2);
        p[5].x:=trunc(2*dw /2+rd/2)+1;p[5].y:=trunc(14*dh/2+rd/2);
      end;
    7:
      begin
        p[0].x:=trunc(3*dw /2)+1;p[0].y:=trunc(8*dh /2);
        p[1].x:=trunc(3*dw /2+rd/2)+1;p[1].y:=trunc(8*dh /2-rd/2);
        p[2].x:=trunc(12*dw/2-rd/2)-1;p[2].y:=trunc(8*dh /2-rd/2);
        p[3].x:=trunc(12*dw/2)-1;     p[3].y:=trunc(8*dh /2);
        p[4].x:=trunc(12*dw/2-rd/2)-1;p[4].y:=trunc(8*dh /2+rd/2);
        p[5].x:=trunc(3*dw /2+rd/2)+1;p[5].y:=trunc(8*dh /2+rd/2);
      end;
    8:
      begin
        case FIndicatorKind of
          ik7segment :
            begin
              p[0].x:=trunc(11*dw /2+rd/4)+1;p[0].y:=trunc(14*dh/2);
              p[1].x:=trunc(11*dw /2+2*rd/4)+1;p[1].y:=trunc(14*dh/2-rd/2);
              p[2].x:=trunc(11*dw /2+5*rd/4);p[2].y:=trunc(14*dh/2-rd/2);
              p[3].x:=trunc(11*dw /2+6*rd/4);p[3].y:=trunc(14*dh/2);
              p[4].x:=trunc(11*dw /2+5*rd/4);p[4].y:=trunc(14*dh/2+rd/2);
              p[5].x:=trunc(11*dw /2+2*rd/4)+1;p[5].y:=trunc(14*dh/2+rd/2);
            end;
          ik9Segment :
            begin
              p[0].x:=trunc(4*dw-rd/3);      p[0].y:=trunc(2.5*dh+rd*2/3);
              p[1].x:=trunc(4*dw-2*rd/3);        p[1].y:=trunc(2.5*dh);
              p[2].x:=trunc(4*dw-rd/3);      p[2].y:=trunc(2.5*dh-rd*2/3);
              p[3].x:=trunc(4*dw+rd/3);      p[3].y:=trunc(2.5*dh-rd*2/3);
              p[4].x:=trunc(4*dw+2*rd/3);        p[4].y:=trunc(2.5*dh);
              p[5].x:=trunc(4*dw+rd/3);      p[5].y:=trunc(2.5*dh+rd*2/3);
            end;  
        end;
      end;
    9:
      begin
        case FIndicatorKind of
          ik7segment :
            begin
            end;
          ik9Segment :
            begin
              p[0].x:=trunc(3.5*dw-rd/3);      p[0].y:=trunc(5.5*dh+rd*2/3);
              p[1].x:=trunc(3.5*dw-2*rd/3);        p[1].y:=trunc(5.5*dh);
              p[2].x:=trunc(3.5*dw-rd/3);      p[2].y:=trunc(5.5*dh-rd*2/3);
              p[3].x:=trunc(3.5*dw+rd/3);      p[3].y:=trunc(5.5*dh-rd*2/3);
              p[4].x:=trunc(3.5*dw+2*rd/3);        p[4].y:=trunc(5.5*dh);
              p[5].x:=trunc(3.5*dw+rd/3);      p[5].y:=trunc(5.5*dh+rd*2/3);
            end;
        end;
      end;
   10:
      begin
        case FIndicatorKind of
          ik7segment :
            begin

            end;
          ik9Segment :
            begin
              p[0].x:=trunc(11*dw /2+rd/4)+1;p[0].y:=trunc(14*dh/2);
              p[1].x:=trunc(11*dw /2+2*rd/4)+1;p[1].y:=trunc(14*dh/2-rd/2);
              p[2].x:=trunc(11*dw /2+5*rd/4);p[2].y:=trunc(14*dh/2-rd/2);
              p[3].x:=trunc(11*dw /2+6*rd/4);p[3].y:=trunc(14*dh/2);
              p[4].x:=trunc(11*dw /2+5*rd/4);p[4].y:=trunc(14*dh/2+rd/2);
              p[5].x:=trunc(11*dw /2+2*rd/4)+1;p[5].y:=trunc(14*dh/2+rd/2);
            end;
        end;
      end;
    else
      begin

      end;
  end;
end;
procedure Register;
begin
  RegisterComponents('AD-2', [TLiquid_Indicator]);
end;

end.

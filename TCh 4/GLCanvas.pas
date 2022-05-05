{******************************************}
{      TeeChart Pro OpenGL Canvas          }
{   Copyright (c) 1998 by David Berneda    }
{         All Rights Reserved              }
{******************************************}
{$I teedefs.inc}
unit GLCanvas;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, OpenGL2, Controls, TeCanvas;

type
    GLMat=Array[0..3] of GLFloat;

    TGLCanvas = class(TCanvas3D)
    private
      { Private declarations }
      FBackColor     : TColor;
      FBackMode      : TCanvasBackMode;
      FDepth         : Integer;
      FTextAlign     : Integer;
      FUseMaterial   : Boolean;

      FWidth         : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
      FHeight        : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
      FXCenter       : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
      FYCenter       : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};

      FOnInit        : TNotifyEvent;

      { internal }
      TheFontHandle  : Integer;
      FDC            : HDC;
      HRC            : HGLRC;
      Palette        : HPALETTE;
      FontOffset     : Integer;
      FX             : Integer;
      FY             : Integer;
      FZ             : Integer;
      IFontCreated   : Boolean;

      Procedure TeeVertex2D(x,y:Integer);
      Procedure TeeVertex3D(x,y,z:Integer);
      Procedure TeeNormal(x,y,z:Integer);
      Procedure SetPenColor;

      Procedure InitMatrix;
      Procedure DoProjection;
      Procedure SetMaterial(AColor:TColor);
    protected
      { Protected declarations }
      Procedure InitFont;
    public
      { Public declarations }
      Constructor Create;
      Destructor Destroy; override;
      Procedure Repaint;

      { 2d }
      procedure SetPixel(X, Y: Integer; Value: TColor); override;
      Function GetSupports3DText:Boolean; override;
      Function GetSupportsFullRotation:Boolean; override;
      Function GetTextAlign:TCanvasTextAlign; override;
      Function GetUseBuffer:Boolean; override;
      Procedure SetUseBuffer(Value:Boolean); override;
      Function GetHandle:HDC; override;

      { 3d }
      procedure SetPixel3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Value: TColor); override;

      Procedure SetBackMode(Mode:TCanvasBackMode); override;
      Function GetMonochrome:Boolean; override;
      Procedure SetMonochrome(Value:Boolean); override;
      Procedure SetBackColor(Color:TColor); override;
      Function GetBackMode:TCanvasBackMode; override;
      Function GetBackColor:TColor; override;
      Procedure SetTextAlign(Align:TCanvasTextAlign); override;

      procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
      procedure Draw(X, Y: Integer; Graphic: TGraphic); override;
      procedure EraseBackground(const Rect: TRect); override;
      procedure FillRect(const Rect: TRect); override;
      procedure Frame3D( Rect: TRect; TopColor,BottomColor: TColor;
                         Width: Integer); override;
      procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
      procedure LineTo(X,Y:Integer); override;
      procedure MoveTo(X,Y:Integer); override;
      procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
      procedure Rectangle(X0,Y0,X1,Y1:Integer); override;
      procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); override;
      procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); override;
      Procedure TextOut(X,Y:Integer; const Text:String); override;
      Procedure DoRectangle(Const Rect:TRect); override;
      Procedure DoHorizLine(X0,X1,Y:Integer); override;
      Procedure DoVertLine(X,Y0,Y1:Integer); override;
      procedure ClipRectangle(Const Rect:TRect); override;
      procedure ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer); override;
      procedure UnClipRectangle; override;
      Procedure GradientFill( Const Rect:TRect;
                              StartColor,EndColor:TColor;
                              Direction:TGradientDirection); override;
      procedure RotateLabel(x,y:Integer; Const St:String; RotDegree:Integer); override;
      procedure RotateLabel3D(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
                              Const St:String; RotDegree:Integer); override;
      Procedure Invalidate; override;
      Procedure Line(X0,Y0,X1,Y1:Integer); override;
      Procedure Polygon(const Points: array of TPoint); override;
      { 3d }
      Procedure Calculate2DPosition(Var x,y:Integer; z:Integer); override;
      Function Calculate3DPosition(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}):TPoint; override;
      Procedure Projection(MaxDepth:Integer; const Bounds,Rect:TRect); override;
      Function InitWindow( DestCanvas:TCanvas;
                           A3DOptions:TView3DOptions;
                           ABackColor:TColor;
                           Is3D:Boolean;
                           Const UserRect:TRect):TRect; override;
      Procedure ShowImage(DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect); override;
      Function ReDrawBitmap:Boolean; override;
      Procedure Arrow( Filled:Boolean;
                       Const FromPoint,ToPoint:TPoint;
                       ArrowWidth,ArrowHeight,Z:Integer); override;
      Procedure Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean); override;
      procedure Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; DarkCover:Boolean); override;
      procedure EllipseWithZ(X1, Y1, X2, Y2, Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      Procedure RectangleZ(Left,Top,Bottom,Z0,Z1:Integer); override;
      Procedure RectangleY(Left,Top,Right,Z0,Z1:Integer); override;
      procedure FrontPlaneBegin; override;
      procedure FrontPlaneEnd; override;
      Procedure HorizLine3D(Left,Right,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      procedure LineTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      Procedure LineWithZ(X0,Y0,X1,Y1,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      procedure MoveTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      procedure Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                       Const StartAngle,EndAngle:Double;
                       DarkSides,DrawSides:Boolean); override;
      procedure Plane3D(Const A,B:TPoint; Z0,Z1:Integer); override;
      procedure PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer); override;
      procedure PlaneFour3D(Points:TFourPoints; Z0,Z1:Integer); override;
      procedure PolygonWithZ(const Points: array of TPoint; Z:Integer);
      procedure Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean); override;
      Procedure RectangleWithZ(Const Rect:TRect; Z:Integer); override;
      Procedure TextOut3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; const Text:String); override;
      procedure TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer); override;
      Procedure VertLine3D(X,Top,Bottom,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
      Procedure ZLine3D(X,Y,Z0,Z1:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;

      procedure Sphere(x,y,z,Radius:Integer);

      property OnInit:TNotifyEvent read FOnInit write FOnInit;
      property UseMaterial:Boolean read FUseMaterial write FUseMaterial;
    published
    end;

Procedure ColorToGL(AColor:TColor; Var C:GLMat);

implementation

Function MinInteger(a,b:Integer):Integer;
begin
  if a>b then result:=b else result:=a;
end;

Procedure ColorToGL(AColor:TColor; Var C:GLMat);
begin
  AColor:=ColorToRGB(AColor);
  C[0]:=(1+GetRValue(AColor))/128.0-1.0;
  C[1]:=(1+GetGValue(AColor))/128.0-1.0;
  C[2]:=(1+GetBValue(AColor))/128.0-1.0;
  C[3]:=1;
end;

{ TGLCanvas }
Constructor TGLCanvas.Create;
begin
  inherited Create;
  IFontCreated:=False;
  FX:=0;
  FY:=0;
  FZ:=0;
//  FPerspective:=50;
  TheFontHandle:=-1;
  FTextAlign:=TA_LEFT;
end;

Destructor TGLCanvas.Destroy;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(hrc);
  if Palette<>0 then DeleteObject(Palette);
  inherited Destroy;
end;

Procedure TGLCanvas.SetMaterial(AColor:TColor);
var tmp:GLMat;
begin
  ColorToGL(AColor,tmp);
  if FUseMaterial then
     glMaterialfv(GL_FRONT{_AND_BACK}, GL_AMBIENT{_AND_DIFFUSE}, @tmp)
  else
     glColor3f(tmp[0],tmp[1],tmp[2]);
end;

Procedure TGLCanvas.Calculate2DPosition(Var x,y:Integer; z:Integer);
begin
      { nothing yet }
end;

Function TGLCanvas.Calculate3DPosition(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}):TPoint;
begin
  result:=Point(x,y);
end;

Procedure TGLCanvas.Projection(MaxDepth:Integer; const Bounds,Rect:TRect);
begin
  RectSize(Bounds,FWidth,FHeight);
  RectCenter(Rect,FXCenter,FYCenter);
  FDepth:=MaxDepth;
  DoProjection;
  InitMatrix;
end;

Procedure TGLCanvas.InitMatrix;
Const Scale=1.0/100.0;
var B:GLMat;
begin
  ColorToGL(FBackColor,B);
  glClearColor(B[0]/2.0+0.5,B[1]/2.0+0.5,B[2]/2.0+0.5,1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  if Assigned(FOnInit) then FOnInit(Self);

  glScalef(Scale,Scale,Scale);
  With View3DOptions do
  begin
    glTranslatef(0,0,-40000.0/Zoom);
    glRotatef(Tilt, 0, 0, 1);
    glRotatef(-Elevation, 1, 0, 0);
    glRotatef(Rotation, 0, 1, 0);
    glTranslatef(HorizOffset,0,0);
    glTranslatef(-FXCenter,0,0);
    glTranslatef(0,FYCenter-VertOffset,0);
  end;
end;

Procedure TGLCanvas.TeeVertex2D(x,y:Integer);
begin
  glVertex2i(x,-y);
end;

Procedure TGLCanvas.TeeVertex3D(x,y,z:Integer);
begin
  glVertex3i(x,-y,-z);
end;

Procedure TGLCanvas.TeeNormal(x,y,z:Integer);
begin
  if y<>0 then y:=-y;
  if (x=0) and (y=0) and (z=0) then z:=1;
  glNormal3i(x,y,-z);
end;

Procedure TGLCanvas.Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean);
var x0f,y0f,z0f,x1f,y1f,z1f:Integer;
begin
  x0f:= Left;
  y0f:=-Top;
  z0f:=-z0;
  x1f:= Right;
  y1f:=-Bottom;
  z1f:=-z1;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_QUADS);
    SetMaterial(Brush.Color);
      TeeNormal(0,0,z0);
       glVertex3i( x0f, y0f, z0f);
       glVertex3i( x1f, y0f, z0f);
       glVertex3i( x1f, y1f, z0f);
       glVertex3i( x0f, y1f, z0f);
      TeeNormal( 0, 0, z1);
       glVertex3i( x0f,  y0f, z1f);
       glVertex3i( x0f,  y1f, z1f);
       glVertex3i( x1f,  y1f, z1f);
       glVertex3i( x1f,  y0f, z1f);
      TeeNormal(Right,  0,  0);
       glVertex3i( x1f, y0f, z0f);
       glVertex3i( x1f, y0f, z1f);
       glVertex3i( x1f, y1f, z1f);
       glVertex3i( x1f, y1f, z0f);
      TeeNormal(Left,  0,  0);
       glVertex3i( x0f, y0f, z0f);
       glVertex3i( x0f, y1f, z0f);
       glVertex3i( x0f, y1f, z1f);
       glVertex3i( x0f, y0f, z1f);
      TeeNormal( 0, Top,  0);
       glVertex3i( x1f, y0f, z1f);
       glVertex3i( x1f, y0f, z0f);
       glVertex3i( x0f, y0f, z0f);
       glVertex3i( x0f, y0f, z1f);
      TeeNormal( 0, Bottom,  0);
       glVertex3i( x1f, y1f, z1f);
       glVertex3i( x0f, y1f, z1f);
       glVertex3i( x0f, y1f, z0f);
       glVertex3i( x1f, y1f, z0f);
    glEnd;
  end;
  if Pen.Style<>psClear then
  begin
    SetPenColor;
    glBegin(GL_LINE_LOOP);
      TeeNormal(0,0,z0);
      glVertex3i( x0f{-1}, y0f{-1}, z0f{-1});
      glVertex3i( x1f{+1}, y0f{-1}, z0f{-1});
      glVertex3i( x1f{+1}, y1f{+1}, z0f{-1});
      glVertex3i( x0f{-1}, y1f{+1}, z0f{-1});
    glEnd;

    glBegin(GL_LINE_LOOP);
      TeeNormal(0,0,z1);
       glVertex3i( x0f,  y0f, z1f);
       glVertex3i( x0f,  y1f, z1f);
       glVertex3i( x1f,  y1f, z1f);
       glVertex3i( x1f,  y0f, z1f);
    glEnd;

    glBegin(GL_LINE_LOOP);
      TeeNormal(Right,  0,  0);
       glVertex3i( x1f, y0f, z0f);
       glVertex3i( x1f, y0f, z1f);
       glVertex3i( x1f, y1f, z1f);
       glVertex3i( x1f, y1f, z0f);
    glEnd;

    glBegin(GL_LINE_LOOP);
      TeeNormal(Left,  0,  0);
       glVertex3i( x0f, y0f, z0f);
       glVertex3i( x0f, y1f, z0f);
       glVertex3i( x0f, y1f, z1f);
       glVertex3i( x0f, y0f, z1f);
    glEnd;

    glBegin(GL_LINE_LOOP);
      TeeNormal( 0, Top,  0);
       glVertex3i( x1f, y0f, z1f);
       glVertex3i( x1f, y0f, z0f);
       glVertex3i( x0f, y0f, z0f);
       glVertex3i( x0f, y0f, z1f);
    glEnd;

    glBegin(GL_LINE_LOOP);
      TeeNormal( 0, Bottom,  0);
       glVertex3i( x1f, y1f, z1f);
       glVertex3i( x0f, y1f, z1f);
       glVertex3i( x0f, y1f, z0f);
       glVertex3i( x1f, y1f, z0f);
    glEnd;
  end;
end;

{procedure SetDCPixelFormat(DC:HDC; Var Palette:HPalette; ToBitmap:Boolean); forward;}

type PointFloat=record x,y:Double; end;
{     GLYPHMETRICSFLOAT=record
       gmfBlackBoxX,
       gmfBlackBoxY:Double;
       gmfptGlyphOrigin:PointFloat;
       gmfCellIncX,
       gmfCellIncY:Double;
     end;}

Procedure TGLCanvas.InitFont;
var Old,hFont:THandle;
//    agmf:Array[0..255] of GLYPHMETRICSFLOAT ;
  {$IFNDEF D2C1}
    FontMode:Integer;
  {$ENDIF}
begin
  hFont := CreateFont(8, 0, 0, 0, {FW_THIN}FW_DONTCARE,
		      0, 0, 0, ANSI_CHARSET,
		      OUT_DEFAULT_PRECIS,
                      CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY{DRAFT_QUALITY},
		      DEFAULT_PITCH or FF_DONTCARE{FIXED_PITCH or FF_MODERN},
                      'Arial');

  Old:=SelectObject (FDC, hFont);
  fontOffset:=1000;

  glDeleteLists(fontOffset,128-32+1);

  {$IFNDEF D2C1}
//  FontMode:=WGL_FONT_POLYGONS;
  FontMode:=WGL_FONT_LINES;
  wglUseFontOutlines( FDC,32, 128-32+1,fontOffset,20.90,0,FontMode,nil{@agmf});
//  wglUseFontBitmaps(FDC,32,128-32+1,fontoffset);
  {$ENDIF}
  DeleteObject(SelectObject(FDC,Old));
  IFontCreated:=True;
end;

Function TGLCanvas.InitWindow( DestCanvas:TCanvas;
                               A3DOptions:TView3DOptions;
                               ABackColor:TColor;
                               Is3D:Boolean;
                               Const UserRect:TRect):TRect;
begin
  if View3DOptions<>A3DOptions then
  begin
    View3DOptions:=A3DOptions;
    FDC := GetDC((View3DOptions.Parent as TWinControl).Handle);
    InitOpenGL;
    hrc:=CreateRenderingContext(FDC,[opDoubleBuffered],24,0);
    ActivateRenderingContext(FDC,HRC);
  end;
  FX:=0;
  FY:=0;
  SetCanvas(DestCanvas);

  glEnable(GL_DITHER);
//    glEnable(GL_LINE_SMOOTH);
  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

//  glEnable(GL_POLYGON_OFFSET_LINE);
//  glPolygonOffset(2,2);

  FBackColor:=ABackColor;
  result:=UserRect;
end;

Procedure TGLCanvas.DoProjection;
const FarZ=3000;
begin
  glViewport(0, 0, FWidth, FHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  gluPerspective(45,    // Field-of-view angle
                 FWidth/FHeight,  // Aspect ratio of viewing volume
                 0.1,               // Distance to near clipping plane
                 FarZ);           // Distance to far clipping plane
end;

Procedure TGLCanvas.ShowImage(DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect);
begin
  glFlush;
  SwapBuffers(FDC);
end;

Function TGLCanvas.ReDrawBitmap:Boolean;
begin
  result:=False;
end;

procedure TGLCanvas.Rectangle(X0,Y0,X1,Y1:Integer);
begin
  if Brush.Style<>bsClear then FillRect(Rect(X0,Y0,X1,Y1));
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    glNormal3i(0,0,1);
    SetPenColor;
    TeeVertex2D(X0,Y0);
    TeeVertex2D(X1,Y0);
    TeeVertex2D(X1,Y1);
    TeeVertex2D(X0,Y1);
    glEnd;
  end;
end;

procedure TGLCanvas.SetTextAlign(Align:Integer);
begin
  FTextAlign:=Align;
end;

procedure TGLCanvas.MoveTo(X, Y: Integer);
begin
  FX:=X;
  FY:=Y;
end;

procedure TGLCanvas.Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean);
begin
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SetMaterial(Brush.Color);
    TeeNormal(0,0,z0);
    TeeVertex3D(Left,Bottom,z0);
    TeeVertex3D(Right,Bottom,z0);
    TeeVertex3D((Left+Right) div 2,Top,(z0+z1) div 2);
    TeeVertex3D(Right,Bottom,z1);
    TeeVertex3D(Right,Bottom,z0);
    glEnd;
  end;
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_STRIP);
    SetPenColor;
    TeeNormal(0,0,z0);
    TeeVertex3D(Left,Bottom,z0);
    TeeVertex3D(Right,Bottom,z0);
    TeeVertex3D((Left+Right) div 2,Top,(z0+z1) div 2);
    TeeVertex3D(Right,Bottom,z1);
    glEnd;
  end;
end;

procedure TGLCanvas.Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; DarkCover:Boolean);
Var c:PGLUQuadricObj;
    tmpSize,Radius:Integer;
begin
  glPushMatrix;
  if Vertical then
  begin
    Radius:=MinInteger((Right-Left) div 2,(Z1-Z0) div 2);
    glTranslatef(Left+(Right-Left) div 2,-Top,-z0-Radius);
    tmpSize:=Bottom-Top;
    glRotatef(90,1,0,0);
  end
  else
  begin
    Radius:=MinInteger((Bottom-Top) div 2,(Z1-Z0) div 2);
    glTranslatef(Left,-Top+(Bottom-Top) div 2,-z0-Radius);
    tmpSize:=Right-Left;
    glRotatef(90,0,1,0);
  end;
  SetMaterial(Brush.Color);
  c:=gluNewQuadric;
  gluCylinder(c,Radius,Radius,tmpSize,18,6);
  gluDeleteQuadric(c);
  c:=gluNewQuadric;
  gluDisk(c,0,Radius,18,6);
  gluDeleteQuadric(c);
  glPopMatrix;
end;

procedure TGLCanvas.Sphere(x,y,z,Radius:Integer);
Var c:PGLUQuadricObj;
begin
  glPushMatrix;
  c:=gluNewQuadric;
  TeeNormal(0, 0, 1);
   glTranslatef(x,y,z);
   SetMaterial(Brush.Color);
   gluSphere(c,Radius,18,18);
   gluDeleteQuadric(c);
  glPopMatrix;
end;

procedure TGLCanvas.SetPenColor;
begin
  SetMaterial(Pen.Color);
//  glLineStipple(1, 31234);  {<-- mask =psdot, psdash, etc }
  glLineWidth( Pen.Width );
end;

procedure TGLCanvas.LineTo(X, Y: Integer);
begin
  glBegin(GL_LINES);
    SetPenColor;
    TeeVertex2D(FX,FY);
    TeeVertex2D(X,Y);
  glEnd;
  FX:=X;
  FY:=Y;
end;

procedure TGLCanvas.ClipRectangle(Const Rect:TRect);
begin
end;

procedure TGLCanvas.ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer);
begin
end;

procedure TGLCanvas.UnClipRectangle;
begin
end;

function TGLCanvas.GetBackColor:TColor;
begin
  result:=clWhite;
end;

procedure TGLCanvas.SetBackColor(Color:TColor);
begin
end;

procedure TGLCanvas.SetBackMode(Mode:TCanvasBackMode);
begin
end;

Function TGLCanvas.GetMonochrome:Boolean;
begin
  result:=False;
end;

Procedure TGLCanvas.SetMonochrome(Value:Boolean);
begin
end;

procedure TGLCanvas.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
end;

procedure TGLCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
end;

Procedure TGLCanvas.GradientFill( Const Rect:TRect;
                                  StartColor,EndColor:TColor;
                                  Direction:TGradientDirection);
begin
  { temporary solution }
  SetMaterial(StartColor);
  DoRectangle(Rect);
end;

Procedure TGLCanvas.RectangleY(Left,Top,Right,Z0,Z1:Integer);
begin
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_QUADS);
    TeeNormal(0,Top,0);
    SetMaterial(Brush.Color);
    TeeVertex3D(Left, Top,Z0);
    TeeVertex3D(Right,Top,Z0);
    TeeVertex3D(Right,Top,Z1);
    TeeVertex3D(Left, Top,Z1);
    glEnd;
  end;
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    TeeNormal( 0, Top, 0);
    SetPenColor;
    TeeVertex3D(Left, Top,Z0);
    TeeVertex3D(Right,Top,Z0);
    TeeVertex3D(Right,Top,Z1);
    TeeVertex3D(Left, Top,Z1);
    glEnd;
  end;
end;

Procedure TGLCanvas.RectangleWithZ(Const Rect:TRect; Z:Integer);
begin
  With Rect do
  begin
    if Pen.Style<>psClear then
    begin
      glBegin(GL_LINE_LOOP);
      TeeNormal( 0,  0, Z);
      SetPenColor;
      TeeVertex3D(Left, Top,Z);
      TeeVertex3D(Right,Top,Z);
      TeeVertex3D(Right,Bottom,Z);
      TeeVertex3D(Left, Bottom,Z);
      glEnd;
    end;
    if Brush.Style<>bsClear then
    begin
      glBegin(GL_QUADS);
      TeeNormal( 0,  0, Z);
      SetMaterial(Brush.Color);
      TeeVertex3D(Left, Top,Z);
      TeeVertex3D(Left, Bottom,Z);
      TeeVertex3D(Right,Bottom,Z);
      TeeVertex3D(Right,Top,Z);
      glEnd;
    end;
  end;
end;

Procedure TGLCanvas.RectangleZ(Left,Top,Bottom,Z0,Z1:Integer);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    TeeNormal( Left,  0, 0);
    SetPenColor;
    TeeVertex3D(Left,Top,Z0);
    TeeVertex3D(Left,Bottom,Z0);
    TeeVertex3D(Left,Bottom,Z1);
    TeeVertex3D(Left,Top,Z1);
    glEnd;
  end;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_QUADS);
    TeeNormal(Left,  0, 0);
    SetMaterial(Brush.Color);
    TeeVertex3D(Left,Top,Z0);
    TeeVertex3D(Left,Bottom,Z0);
    TeeVertex3D(Left,Bottom,Z1);
    TeeVertex3D(Left,Top,Z1);
    glEnd;
  end;
end;

procedure TGLCanvas.FillRect(const Rect: TRect);
begin
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_POLYGON);
      TeeNormal( 0, 0, 1);
      SetMaterial(Brush.Color);
      With Rect do
      begin
        TeeVertex2D(Right,Bottom);
        TeeVertex2D(Right,Top);
        TeeVertex2D(Left, Top);
        TeeVertex2D(Left, Bottom);
      end;
    glEnd;
  end;
end;

procedure TGLCanvas.Frame3D( Rect: TRect; TopColor, BottomColor: TColor;
                             Width: Integer);
begin
  FillRect(Rect);
end;

procedure TGLCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  EllipseWithZ(X1,Y1,X2,Y2,0);
end;

procedure TGLCanvas.EllipseWithZ(X1, Y1, X2, Y2, Z: {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
Const PiStep=pi/10.0;
var t,XC,YC,XR,YR:Integer;
    tmpSin,tmpCos:Extended;
begin
  XR:=(X2-X1) div 2;
  YR:=(Y2-Y1) div 2;
  XC:=(X1+X2) div 2;
  YC:=(Y1+Y2) div 2;
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    SetPenColor;
    TeeNormal(0,0,z);
    for t:=0 to 18 do
    begin
      SinCos(t*piStep,tmpSin,tmpCos);
      TeeVertex3D(XC+Trunc(XR*tmpSin),YC+Trunc(YR*tmpCos),Z);
    end;
    glEnd;
  end;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_TRIANGLE_FAN);
    SetMaterial(Brush.Color);
    TeeNormal(0,0,z);
    TeeVertex3D(XC,YC,Z);
    for t:=0 to 20 do
    begin
      SinCos(t*piStep,tmpSin,tmpCos);
      TeeVertex3D(XC+Trunc(XR*tmpSin),YC+Trunc(YR*tmpCos),Z);
    end;
    glEnd;
  end;
end;

procedure TGLCanvas.FrontPlaneBegin;
Const Scale=1.0/100.0;
begin
  glPushMatrix;
  glLoadIdentity;
  glScalef(Scale,Scale,Scale);
  glTranslatef(-FXCenter,FYCenter,0);
end;

procedure TGLCanvas.FrontPlaneEnd;
begin
  glPopMatrix;
end;

procedure TGLCanvas.SetPixel3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Value: TColor);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_POINT);
    Pen.Color:=Value;
    SetPenColor;
    TeeVertex3D(X,Y,Z);
    glEnd;
  end;
end;

procedure TGLCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_POINT);
    Pen.Color:=Value;
    SetPenColor;
    TeeVertex2D(X,Y);
    glEnd;
  end;
end;

procedure TGLCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
//  gluPartialDisk
end;

procedure TGLCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
//  gluPartialDisk
end;

procedure TGLCanvas.Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                           Const StartAngle,EndAngle:Double;
                           DarkSides,DrawSides:Boolean);

Var piStep:Double;
    tmpSin,tmpCos:Extended;
    Radius:Integer;

  Function ToDegree(Const Value:Double):Double;
  begin
    result:=Value*180.0/pi;
  end;

  Procedure DrawPieSlice(z:Integer);

    Procedure DrawSlice;
    var t:Integer;
    begin
      TeeNormal(0,0,z);
      TeeVertex3D(0,0,z);
      for t:=0 to 20 do
      begin
        SinCos(StartAngle+(t*piStep),tmpSin,tmpCos);
        TeeVertex3D(Trunc(Radius*tmpSin),Trunc(Radius*tmpCos),z);
      end;
    end;

  begin
    if Pen.Style<>psClear then
    begin
      glBegin(GL_LINE_LOOP);
      SetPenColor;
      DrawSlice;
      glEnd;
    end;
    if Brush.Style<>bsClear then
    begin
      glBegin(GL_TRIANGLE_FAN);
      SetMaterial(Brush.Color);
      DrawSlice;
      glEnd;
    end;
  end;

  Procedure DrawCover;
  var t,x,y:Integer;
  begin
    glBegin(GL_QUAD_STRIP);
    SetMaterial(Brush.Color);
    for t:=0 to 20 do
    begin
      SinCos(StartAngle+(t*piStep),tmpSin,tmpCos);
      X:=Trunc(Radius*tmpSin);
      Y:=Trunc(Radius*tmpCos);
      TeeVertex3D(X,Y,0);
      TeeVertex3D(X,Y,z1-z0);
    end;
    glEnd;
  end;

begin
  glPushMatrix;
  glTranslatef(XCenter,-YCenter,0);
  Radius:=MaxLong(XRadius,YRadius);
  piStep:=(EndAngle-StartAngle)/20.0;
  DrawCover;
  if DrawSides then
  begin
    SinCos(StartAngle,tmpSin,tmpCos);
    Plane3D(Point(0,0),Point(Round(Radius*tmpSin),Round(Radius*tmpCos)),Z0,Z1);
    SinCos(EndAngle,tmpSin,tmpCos);
    Plane3D(Point(0,0),Point(Round(Radius*tmpSin),Round(Radius*tmpCos)),Z0,Z1);
  end;
  DrawPieSlice(z0);
  DrawPieSlice(z1);
  glPopMatrix;
end;

procedure TGLCanvas.Polygon(const Points: array of TPoint);
begin
  PolygonWithZ(Points,0);
end;

procedure TGLCanvas.PlaneFour3D(Points:TFourPoints; Z0,Z1:Integer);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    SetPenColor;
    With Points[0] do TeeVertex3D(x,y,z0);
    With Points[1] do TeeVertex3D(x,y,z0);
    With Points[2] do TeeVertex3D(x,y,z1);
    With Points[3] do TeeVertex3D(x,y,z1);
    glEnd;
  end;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_POLYGON);
    TeeNormal(0,0,z0);
    SetMaterial(Brush.Color);
    With Points[0] do TeeVertex3D(x,y,z0);
    With Points[1] do TeeVertex3D(x,y,z0);
    With Points[2] do TeeVertex3D(x,y,z1);
    With Points[3] do TeeVertex3D(x,y,z1);
    glEnd;
  end;
end;

procedure TGLCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  Rectangle(X1,Y1,X2,Y2);
end;

Procedure TGLCanvas.Repaint;
begin
  if Assigned(View3DOptions) then View3DOptions.Repaint;
end;

Procedure TGLCanvas.Invalidate;
begin
      { nothing to do }
end;

Procedure TGLCanvas.TextOut3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; const Text:String);
var tmp:TSize;
    tmpLength:Integer;
    s:String;
begin
  glPushMatrix;
  if not IFontCreated then InitFont;

  glListBase(FontOffset-32);

  s:=Text;
  tmpLength:=Length(s);
  GetTextExtentPoint(FDC, PChar(s), tmpLength, tmp);
  if (FTextAlign and TA_CENTER)=TA_CENTER then
    x:=x-(tmp.Cx div 2)
  else
  if (FTextAlign and TA_RIGHT)=TA_RIGHT then
    x:=x-2*(tmp.Cx div 3);

  if (FTextAlign and TA_BOTTOM)<>TA_BOTTOM then
    y:=y+2*(tmp.Cy div 3);

  glTranslatef( x,-y,-z+1);
  glScalef(Font.Size*1.5,Font.Size*1.5,1);

  SetMaterial(Font.Color);
  glCallLists(1+tmpLength, GL_UNSIGNED_BYTE, PChar(Text));
  glPopMatrix;
end;

Procedure TGLCanvas.TextOut(X,Y:Integer; const Text:String);
begin
  TextOut3D(x,y,0,Text);
end;

procedure TGLCanvas.MoveTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  FX:=X;
  FY:=Y;
  FZ:=Z;
end;

procedure TGLCanvas.LineTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  glBegin(GL_LINES);
    SetPenColor;
    TeeVertex3D(FX,FY,FZ);
    TeeVertex3D(x,y,z);
  glEnd;
  FX:=X;
  FY:=Y;
  FZ:=Z;
end;

procedure TGLCanvas.PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    SetPenColor;
    TeeNormal(0,0,z);
    TeeVertex3D(P1.X,P1.Y,Z);
    TeeVertex3D(P2.X,P2.Y,Z);
    TeeVertex3D(P3.X,P3.Y,Z);
    TeeVertex3D(P4.X,P4.Y,Z);
    TeeVertex3D(P1.X,P1.Y,Z);
    glEnd;
  end;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_QUADS);
    SetMaterial(Brush.Color);
    TeeNormal(0,0,z);
    TeeVertex3D(P1.X,P1.Y,Z);
    TeeVertex3D(P2.X,P2.Y,Z);
    TeeVertex3D(P3.X,P3.Y,Z);
    TeeVertex3D(P4.X,P4.Y,Z);
    TeeVertex3D(P1.X,P1.Y,Z);
    glEnd;
  end;
end;

procedure TGLCanvas.Plane3D(Const A,B:TPoint; Z0,Z1:Integer);
begin
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    SetPenColor;
    TeeNormal(0,0,z0);
    TeeVertex3D(A.X,A.Y,Z0);
    TeeVertex3D(B.X,B.Y,Z0);
    TeeVertex3D(B.X,B.Y,Z1);
    TeeVertex3D(A.X,A.Y,Z1);
    glEnd;
  end;
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_QUADS);
    TeeNormal(0,0,z0);
    SetMaterial(Brush.Color);
    TeeVertex3D(A.X,A.Y,Z0);
    TeeVertex3D(B.X,B.Y,Z0);
    TeeVertex3D(B.X,B.Y,Z1);
    TeeVertex3D(A.X,A.Y,Z1);
    glEnd;
  end;
end;

Function TGLCanvas.GetSupports3DText:Boolean;
begin
  result:=True;
end;

Function TGLCanvas.GetSupportsFullRotation:Boolean;
begin
  result:=True;
end;

Function TGLCanvas.GetTextAlign:TCanvasTextAlign;
begin
  result:=TA_LEFT;
end;

Function TGLCanvas.GetUseBuffer:Boolean;
begin
  result:=False;
end;

Procedure TGLCanvas.SetUseBuffer(Value:Boolean);
begin
end;

Function TGLCanvas.GetHandle:HDC;
begin
  result:=FDC;
end;

Procedure TGLCanvas.DoRectangle(Const Rect:TRect);
begin
  With Rect do Rectangle(Left,Top,Right,Bottom)
end;

Procedure TGLCanvas.DoHorizLine(X0,X1,Y:Integer);
begin
  MoveTo(X0,Y);
  LineTo(X1,Y);
end;

Procedure TGLCanvas.DoVertLine(X,Y0,Y1:Integer);
begin
  MoveTo(X,Y0);
  LineTo(X,Y1);
end;

procedure TGLCanvas.RotateLabel3D(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Const St:String; RotDegree:Integer);
begin
  RotateLabel(x,y,St,RotDegree);
end;

procedure TGLCanvas.RotateLabel(x,y:Integer; Const St:String; RotDegree:Integer);
begin
  TextOut(X,Y,St);
end;

Procedure TGLCanvas.Line(X0,Y0,X1,Y1:Integer);
begin
  MoveTo(X0,Y0);
  LineTo(X1,Y1);
end;

procedure TGLCanvas.EraseBackground(const Rect: TRect);
begin
      { nothing... }
end;

Procedure TGLCanvas.HorizLine3D(Left,Right,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  MoveTo3D(Left,Y,Z);
  LineTo3D(Right,Y,Z);
end;

Procedure TGLCanvas.VertLine3D(X,Top,Bottom,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  MoveTo3D(X,Top,Z);
  LineTo3D(X,Bottom,Z);
end;

Procedure TGLCanvas.ZLine3D(X,Y,Z0,Z1:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  MoveTo3D(X,Y,Z0);
  LineTo3D(X,Y,Z1);
end;

Procedure TGLCanvas.Arrow( Filled:Boolean;
                           Const FromPoint,ToPoint:TPoint;
                           ArrowWidth,ArrowHeight,Z:Integer);
begin
end;

Procedure TGLCanvas.LineWithZ(X0,Y0,X1,Y1,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  MoveTo3D(X0,Y0,Z);
  LineTo3D(X1,Y1,Z);
end;

procedure TGLCanvas.PolygonWithZ(const Points: array of TPoint; Z:Integer);
var t:Integer;
begin
  if Brush.Style<>bsClear then
  begin
    glBegin(GL_POLYGON);
    TeeNormal(0,0,1);
    SetMaterial(Brush.Color);
    for t:=Low(Points) to High(Points) do
      With Points[t] do TeeVertex3D( x, y, z);
    glEnd;
  end;
  if Pen.Style<>psClear then
  begin
    glBegin(GL_LINE_LOOP);
    TeeNormal(0,0,1);
    SetMaterial(Pen.Color);
    for t:=Low(Points) to High(Points) do
      With Points[t] do TeeVertex3D( x, y, z);
    glEnd;
  end;
end;

procedure TGLCanvas.TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer);
begin
  PolygonWithZ([P1,P2,P3],Z);
end;

Function TGLCanvas.GetBackMode:TCanvasBackMode;
begin
  result:=FBackMode;
end;

end.

// GLzBuffer
{: ZBuffer retrieval and computations.<p> 

   See readme.txt in the Demos/SpecialsFX/Shadows directory.<br>
   By Ren? Lindsay.<p>

	<b>History : </b><font size=-1><ul>
      <li>05/02/02 - EG  - Fixed glTex[Sub]Image calls 
      <li>20/11/01 - EG  - Removed warnings (axed out... hope I didn't broke anything)
      <li>17/10/01 - Lin - Added Xres and Yres...makes shadow texture size independent from viewer.
                           Calculations now use z-depth in stead of world distance
                           - more acurate, and 15% faster.
      <li>27/09/01 - Lin - Bypass the GLScene Material.texture.image, and send the shadow
                           texture directly to OpenGL. This increased speed by almost 20%
      <li>25/09/01 - Lin - Add Optimise property to specify faster rastering methods
      <li>07/09/01 - Lin - Restructure zBuffer code, to support the new TGLMemoryViewer
      <li>06/09/01 - Lin - Created TZShadows object, for casting shadows
      <li>30/08/01 - Lin - More speed + bugfixes
      <li>24/07/01 - Lin - Greatly improved speed
      <li>07/07/01 - Lin - Added PixelToWorld, WorldToPixel, and PixelToDistance
      <li>01/07/01 - Lin - Precalculate the corner vectors in GetDepthBuffer,
                            to speed up FastVectorToScreen and FastScreenToVector
      <li>28/06/01 - Lin - First operational code
      <li>26/06/01 - Lin - Creation of zBuffer class
	</ul></font>
}


   //--------These formulas are the key to making use of the z-Buffer--------
   //
   // dst (d): world distance
   // dov    : depth of view (distance between Far-plane and Near-plane)
   // np     : near plane
   // fp     : far plane (dov+np)
   //
   //------------------------
   //dst:=(fp*np)/(fp-z*dov);  //calc from z-buffer value to frustrum depth
   //z  :=(1-np/d)/(1-np/fp);  //calc from frustrum depth to z-buffer value
   //------------------------  z:=1-(fp/d-1)/(fp/np-1); //old FtoZ
   //------------------------------------------------------------------------


unit GLzBuffer;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Classes, Graphics, GLMisc, OpenGL12, GLScene, Geometry, GLGraphics,
     Dialogs, SysUtils, GLObjects, GLBitmapFont, xopengl, GLTexture, GLWin32Viewer;

type
  TZArray = array [0..MaxInt shr 3] of Single;

  PZArray = ^TZArray;
//  PSingle = ^Single;
  TOptimise=(opNone,op4in1,op9in1,op16in1);

  TGLzBuffer = class (TPersistent)
  private
    FData : PZArray;
    FWidth, FHeight : Integer;
    FDataSize : Integer;

    ang1,ang2,scal,c1,s1,c2,s2,vw,vh :single;  //VectorToScreen variables;
    lt,rt,lb,rb :TAffineVector;                //ScreenToVector corner vectors;
    UpVec, riVec :TAffineVector;

    ltW,rtW,lbW,rbW :TAffineVector;            //ScreenToVector corner vectors;(Warped)
    UpVecW, riVecW :TAffineVector;

    dov, np, fp, NpFp, OneMinNp_Fp :single;                       //Calc Variables;

    cam: TGLCamera;

    procedure DoCalcVectors;
  protected
    procedure SetWidth(val : Integer);
    procedure SetHeight(const val : Integer);

  public
    SceneViewer : TGLSceneViewer;
    MemoryViewer : TGLMemoryViewer;
    Buffer :TGLSceneBuffer;


    Normal :TAffineVector;                    //Absolute direction of camera

    //constructor Create(viewer:TGLSceneViewer);
    constructor Create;
    destructor Destroy; override;
    Procedure LinkToViewer(viewer:TGLSceneViewer); overload;
    Procedure LinkToViewer(viewer:TGLMemoryViewer); overload;
    function GetDepthBuffer(CalcVectors :Boolean;ContextIsActive:boolean) :PZArray;

    function GetPixelzDepth(x,y:integer) :Single;
    function PixelToDistance_OLD(x,y:integer) :Single;
    function PixelToDistance(x,y:integer) :Single;
    property Width : Integer read FWidth write SetWidth;
    property Height : Integer read FHeight write SetHeight;
    property DataSize : Integer read FDataSize;
    property Data : PZArray read FData;

    procedure Refresh;
    function FastScreenToVector(x,y :integer):TAffineVector;
    function FastVectorToScreen(vec :TAffineVector):TAffineVector;
    function PixelToWorld_OLD(x,y :integer):TAffineVector;
    function PixelToWorld(x,y :integer):TAffineVector;
    function WorldToPixel(aPoint :TAffineVector;var pixX,pixY:integer;var pixZ:single):boolean;
    function WorldToPixelZ(aPoint :TAffineVector;var pixX,pixY:integer;var pixZ:single):boolean;
  end;

   // TZShadows
   //
   TZShadows = class (TGLCustomSceneObject)
        private
         FViewer      :TGLSceneViewer;
         FCaster      :TGLMemoryViewer;
         FDepthFade   :Boolean;
         FFrustShadow :Boolean;
         FSkyShadow   :Boolean;
         FOptimise    :TOptimise;

         bmp32:TGLBitmap32;

         FWidth  :integer;
         FHeight :integer;
         FXRes   :integer;
         FYRes   :integer;
         Fsoft   :boolean;
         FTolerance :single;

         FColor :TGLColor;
         SCol :TGLPixel32;

         stepX, stepY :single;

         FTexturePrepared : Boolean;

        protected
         function  GetViewer : TGLSceneViewer;
         procedure SetViewer(const val : TGLSceneViewer);
         function  GetCaster :TGLMemoryViewer;
         procedure SetCaster(const val :TGLMemoryViewer);
         procedure CalcShadowTexture(var rci : TRenderContextInfo);
         function  HardSet(res_X,res_Y :integer):TGLPixel32;
         function  HardTest(x,y:integer):TGLPixel32;
         function  SoftTest(x,y:integer):TGLPixel32;
         procedure SetXRes(const val :integer);
         procedure SetYRes(const val :integer);
	public
          ViewerZBuf :TGLzBuffer;
          CasterZBuf :TGLzBuffer;
    	  constructor Create(AOwner: TComponent); override;
          destructor  destroy; override;
          procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
        published
          property Viewer      :TGLSceneViewer  read GetViewer write SetViewer;
          property Caster      :TGLMemoryViewer read GetCaster write SetCaster;
          property FrustShadow :Boolean read FFrustShadow write FFrustShadow;
          property SkyShadow   :Boolean read FSkyShadow write FSkyShadow;
          property Optimise    :TOptimise read FOptimise write FOptimise;
          property Width       :integer read FWidth write FWidth;// default 64;
          property Height      :integer read FHeight write FHeight;// default 64;
          property Color       :TGLColor read FColor write FColor;
          property Xres        :integer read FXRes write SetXRes;// default 64;
          property Yres        :integer read FYRes write SetYRes;// default 64;
          property Soft        :Boolean read Fsoft write Fsoft;
          property Tolerance   :single read FTolerance write FTolerance;
          property Material;
          property ObjectsSorting;
          property Visible;

//          property Width default 128;
//          property Height default 128;

          property DepthFade :Boolean read FDepthFade write FDepthFade;
          function CastShadow :boolean;

   end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

implementation

//---Create---
{
constructor TGLzBuffer.Create(viewer:TGLSceneViewer);
begin
 inherited Create;
 FWidth:=0;
 FHeight:=0;
 FDataSize:=0;

// viewer:=nil;
// cam:=nil;
 self.viewer:=viewer;
 self.buffer:=Viewer.buffer;
// self.cam:=viewer.cam;
 cam:=nil;
 self.DoCalcVectors;
end;
}

constructor TGLzBuffer.Create;
begin
 inherited Create;

 self.FWidth:=0;
 self.FHeight:=0;
 self.FDataSize:=0;
 self.cam:=nil;
 self.SceneViewer:=nil;
 self.MemoryViewer:=nil;
 self.buffer:=nil;
 // self.DoCalcVectors;
end;

Procedure TGLzBuffer.LinkToViewer(viewer:TGLSceneViewer);// overload;
begin
 If ((FWidth<>Viewer.width)or(FHeight<>Viewer.height)) then begin
    FWidth :=Viewer.width;
    FHeight:=Viewer.height;
    FDataSize:=FWidth*FHeight*4;
    ReallocMem(FData,FDataSize);
 end;
 cam:=Viewer.camera;
 SceneViewer:=Viewer;
 Buffer:=Viewer.Buffer;
 self.DoCalcVectors;
end;

Procedure TGLzBuffer.LinkToViewer(viewer:TGLMemoryViewer);// overload;
begin
 If ((FWidth<>Viewer.width)or(FHeight<>Viewer.height)) then begin
    FWidth :=Viewer.width;
    FHeight:=Viewer.height;
    FDataSize:=FWidth*FHeight*4;
    ReallocMem(FData,FDataSize);
 end;
 cam:=Viewer.camera;
 MemoryViewer:=Viewer;
 Buffer:=Viewer.Buffer;
 self.DoCalcVectors;
end;



//---Destroy---
destructor TGLzBuffer.Destroy;
begin
 FreeMem(FData);
 inherited Destroy;
end;

//---Width---
procedure TGLzBuffer.SetWidth(val : Integer);
begin
   if val<>FWidth then begin
      Assert(val>=0);
      FWidth:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

//---Height---
procedure TGLzBuffer.SetHeight(const val : Integer);
begin
   if val<>FHeight then begin
      Assert(val>=0);
      FHeight:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

function TGLzBuffer.GetDepthBuffer(CalcVectors :Boolean;ContextIsActive:boolean) :PZArray;
begin
{
   If ((FWidth<>Viewer.width)or(FHeight<>Viewer.height)) then begin
      FWidth :=Viewer.width;
      FHeight:=Viewer.height;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData,FDataSize);
   end;
}
   if ContextIsActive=True then begin
      glReadPixels(0,0, FWidth, FHeight, GL_DEPTH_COMPONENT, GL_FLOAT, FData);
   end else begin
      Buffer.RenderingContext.Activate;
      Try
        glReadPixels(0,0, FWidth, FHeight, GL_DEPTH_COMPONENT, GL_FLOAT, FData);
      Finally
        Buffer.RenderingContext.Deactivate;
      end;
   end;

   If CalcVectors=True then begin
      //self.Viewer:=viewer;
      DoCalcVectors;
   end;
   Result:=FData;
end;
{
function TGLzBuffer.GetPix :Single;
var pix :single;
begin
  glReadPixels(1,1, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @pix);
  Result:=pix;
end;
}
function TGLzBuffer.GetPixelzDepth(x,y:integer) :Single;
begin
 if ((x<0)or(x>FWidth)or(y<0)or(y>FWidth)) then result:=0
 else  result:=FData[x+(FHeight-y)*FWidth];
end;

function TGLzBuffer.PixelToDistance_OLD(x,y:integer) :Single;
 var z, dst, camAng,wrpdst :single;
    vec :TAffineVector;
begin
 if ((x<0)or(x>FWidth)or(y<0)or(y>FWidth)) then result:=0
 else begin
   z:=FData[x+(FHeight-y)*FWidth];     //fetch pixel z-depth
   dst:=(NpFp)/(fp-z*dov);             //calc from z-buffer value to frustrum depth
   vec:=FastScreenToVector(x,y);
   camAng:=VectorAngleCosine(normal,vec);
   wrpdst:=dst/camAng;                 //compensate for flat frustrum face
   result:=wrpdst;
 end;
end;

function TGLzBuffer.PixelToDistance(x,y:integer) :Single;
 var z, dst :single;
    xx,yy,zz :single;
    fy:integer;
begin
 if ((x<0)or(x>=FWidth)or(y<0)or(y>=FHeight)) then result:=0
 else begin
   fy:=FHeight-y;
   z:=FData[x+fy*FWidth];     //fetch pixel z-depth
   if z<1 then begin
    dst:=(NpFp)/(fp-z*dov);             //calc from z-buffer value to frustrum depth
    xx:=(lbW[0] + riVecW[0]*x + UpVecW[0]*fy) ;
    yy:=(lbW[1] + riVecW[1]*x + UpVecW[1]*fy) ;
    zz:=(lbW[2] + riVecW[2]*x + UpVecW[2]*fy) ;
    result:=sqrt(xx*xx+yy*yy+zz*zz)*dst;
   end else result:=0;
 end;
end;

procedure TGLzBuffer.Refresh;
begin
// if assigned(SceneViewer)  then GetDepthBuffer(SceneViewer,True,False);
// if assigned(MemoryViewer) then GetDepthBuffer(Memoryviewer,True,False);
 if assigned(Buffer) then GetDepthBuffer(True,False);

end;


procedure TGLzBuffer.DoCalcVectors;
var  axs :TAffineVector;
     Hnorm, hcvec :TVector;
     vec :TAffineVector;
     w,h :integer;
     wrp :single;
begin
  if not assigned(Buffer) then exit;
  if not assigned(cam) then begin
     showMessage('No Camera!');
     exit;
  end;
  if (FWidth=0) then showMessage('No Width!!!!!');
  if not assigned(cam) then exit;
//  if not assigned(Viewer.Camera) then exit;
//  if not assigned(Viewer.Camera.TargetObject) then exit;

//-----------For ScreenToVector-------------
 w:=FWidth;
 h:=FHeight;
 setVector(vec,0,0,0);     lb:=buffer.ScreenToVector(vec);    // same as cvec...optimise?
 setVector(vec,w,0,0);     rb:=buffer.ScreenToVector(vec);
 setVector(vec,0,h,0);     lt:=buffer.ScreenToVector(vec);
 setVector(vec,w,h,0);     rt:=buffer.ScreenToVector(vec);
//------------Set Camera values-------------
 normal:=VectorLerp(lb,rt,0.5);
 upVec :=VectorSubtract(lt,lb);
 riVec :=VectorSubtract(rb,lb);
// cam:=viewer.Camera;
 dov:=Cam.DepthOfView;
 np :=Cam.NearPlane;
 fp :=Cam.NearPlane+dov;
 NpFp:=np*fp;
 OneMinNp_Fp:=1-np/fp;
//-----------For VectorToScreen-------------
{
  cam :=Viewer.Camera.Position.AsAffineVector;
  targ:=Viewer.Camera.TargetObject.Position.AsAffineVector;
  norm:=VectorSubtract(targ,cam);     //---Camera Normal vector---
  MakeVector(hnorm,norm);
}
  MakeVector(hnorm,normal);

  MakeVector(hcVec,lb);                //---Corner Vector---
  ang1:=ArcTan2(Hnorm[0],Hnorm[2]);
  SetVector(axs,0,1,0);
  RotateVector(hnorm,axs,ang1);
  RotateVector(hcvec,axs,ang1);

  ang2:=ArcTan2(Hnorm[1],Hnorm[2]);
  SetVector(axs,1,0,0);
  RotateVector(hcvec,axs,-ang2);

  hcvec[0]:=hcvec[0]/hcvec[2];
  vw:=Fwidth/2;
  vh:=Fheight/2;
  scal:=vw/hcvec[0];
  SinCos(-ang1, s1, c1);
  SinCos(-ang2, s2, c2);
//------------------------------------------
//--------------------2-----------------
 vec:=self.FastScreenToVector(0,1);
 wrp:=VectorAngleCosine(normal,vec);
 ltW:=VectorNormalize(lt);
 rtW:=VectorNormalize(rt);
 lbW:=VectorNormalize(lb);
 rbW:=VectorNormalize(rb);
 ltW:=VectorScale(ltW,1/wrp);
 rtW:=VectorScale(rtW,1/wrp);
 lbW:=VectorScale(lbW,1/wrp);
 rbW:=VectorScale(rbW,1/wrp);
 upVecW :=VectorSubtract(ltW,lbW); upVecW :=VectorScale(upVecW,1/Fheight);
 riVecW :=VectorSubtract(rbW,lbW); riVecW :=VectorScale(riVecW,1/Fwidth);
//--------------------------------------
end;

function TGLzBuffer.FastScreenToVector(x,y :integer):TAffineVector;
  var w,h :integer;
      Rlerp,Ulerp :single;
begin
 w:=FWidth;
 h:=FHeight;
 Rlerp:=x/w;
 Ulerp:=(h-y)/h;
 result[0]:=lb[0] + riVec[0]*Rlerp + UpVec[0]*Ulerp;
 result[1]:=lb[1] + riVec[1]*Rlerp + UpVec[1]*Ulerp;
 result[2]:=lb[2] + riVec[2]*Rlerp + UpVec[2]*Ulerp;
end;

function TGLzBuffer.FastVectorToScreen(Vec :TAffineVector):TAffineVector;
 var v0, v1, x,y,z : Single;
 begin
   x:=vec[0];   y:=vec[1];   z:=vec[2];
   v0:=x;
   x:=c1*v0+s1*z;
   z:=c1*z-s1*v0;            //Rotate around Y-axis
   v1:=y;
   y:=c2*v1+s2*z;
   z:=c2*z-s2*v1;            //Rotate around X-axis
   Result[0]:=Round(-x/z*scal+vw);
   Result[1]:=Round( y/z*scal+vh);
{
  MakeVector(hpvec,vec);
  SetVector(axs,0,1,0);
  RotateVector(hpvec,axs,ang1);
  SetVector(axs,1,0,0);
  RotateVector(hpvec,axs,-ang2);
  Result[0]:=Round(-hpvec[0]/hpvec[2]*scal+vw);
  Result[1]:=Round( hpvec[1]/hpvec[2]*scal+vh);
  Result[2]:=0;
}
 end;
//-----------------------------------------------------------------


function TGLzBuffer.PixelToWorld_OLD(x,y :integer):TAffineVector;
var z, dst, camAng,wrpdst :single;
    vec, campos :TAffineVector;
begin
 z:=GetPixelzDepth(x,y);
 dst:=(NpFp)/(fp-z*dov);  //calc from z-buffer value to frustrum depth
 vec:=FastScreenToVector(x,y);
 NormalizeVector(vec);
 camAng:=VectorAngleCosine(normal,vec);                  //
 wrpdst:=dst/camAng;                                     //compensate for flat frustrum face
 SetVector( campos  ,Cam.AbsolutePosition);              //--camera position--
 result:=VectorCombine(campos,vec,1,wrpdst);
end;

function TGLzBuffer.PixelToWorld(x,y :integer):TAffineVector;
var z, dst :single;
    fy :integer;
    camvec :TVector;
begin
 if ((x>0)or(x<FWidth)or(y>0)or(y<FWidth)) then begin
  fy:=FHeight-y;
  z:=FData[x+fy*FWidth];
  dst:=(NpFp)/(fp-z*dov);  //calc from z-buffer value to frustrum depth
  camvec:=cam.AbsolutePosition;
  result[0]:=(lbW[0] + riVecW[0]*x + UpVecW[0]*fy) *dst +camvec[0];
  result[1]:=(lbW[1] + riVecW[1]*x + UpVecW[1]*fy) *dst +camvec[1];
  result[2]:=(lbW[2] + riVecW[2]*x + UpVecW[2]*fy) *dst +camvec[2];
 end else begin
  result[0]:=0;
  result[1]:=0;
  result[2]:=0;
 end;
end;


function TGLzBuffer.WorldToPixel(aPoint :TAffineVector;var pixX,pixY:integer;var pixZ:single):boolean;
var camPos :TVector;
    x,y,z, v0,v1 ,zscal:single;
begin
   //---Takes x,y,z world coordinate.
   //---Result is true if pixel lies within view frustrum
   //---returns canvas pixel x,y coordinate, and the world distance
   result:=false;
   campos:=cam.AbsolutePosition;
   x:=apoint[0]-camPos[0];
   y:=apoint[1]-camPos[1];
   z:=apoint[2]-camPos[2];      //get vector from camera to world point
   v0:=x;
   x:=c1*v0+s1*z;
   z:=c1*z-s1*v0;            //Rotate around Y-axis
   v1:=y;
   y:=c2*v1+s2*z;
   z:=c2*z-s2*v1;            //Rotate around X-axis
   if z>0 then begin
      zscal:=scal/z;
      pixX:=Round(-x*zscal+vw);
      pixY:=Round( y*zscal+vh);
      pixZ:=sqrt(x*x+y*y+z*z);
      If (pixX>0)and(pixX<FWidth)and(pixY>0)and(pixY<FHeight) then Result:=true;
   end else begin           //ignore anything that is behind the camera
      pixX:=0;
      pixY:=0;
      pixZ:=0;
   end;

{
  SetVector(campos,viewer.Camera.AbsolutePosition);
  pntVec:=VectorSubtract(aPoint,campos);
  Result:=FastVectorToScreen(pntVec);     //---x,y coordinate
  Result[2]:=VectorLength(pntVec);        //---distance
}
end;

function TGLzBuffer.WorldToPixelZ(aPoint :TAffineVector;var pixX,pixY:integer;var pixZ:single):boolean;
var camPos :TVector;
    x,y,z, v0,v1 ,zscal:single;
begin
   //---Takes x,y,z world coordinate.
   //---Result is true if pixel lies within view frustrum
   //---returns canvas pixel x,y coordinate, and CALCULATES the z-buffer distance
   result:=false;
   campos:=cam.AbsolutePosition;
   x:=apoint[0]-camPos[0];
   y:=apoint[1]-camPos[1];
   z:=apoint[2]-camPos[2];      //get vector from camera to world point
   v0:=x;
   x:=c1*v0+s1*z;
   z:=c1*z-s1*v0;            //Rotate around Y-axis
   v1:=y;
   y:=c2*v1+s2*z;
   z:=c2*z-s2*v1;            //Rotate around X-axis
   if z>0 then begin
      zscal:=scal/z;
      pixX:=Round(-x*zscal+vw);
      pixY:=Round( y*zscal+vh);
//      pixZ:=sqrt(x*x+y*y+z*z);
//------z:=(1-np/z)/(1-np/fp);------
//      pixZ:=(1-np/z)/(1-np/fp);
      pixZ:=(1-np/z)/OneMinNp_Fp;
      If (pixX>0)and(pixX<FWidth)and(pixY>0)and(pixY<FHeight) then Result:=true;
   end else begin           //ignore anything that is behind the camera
      pixX:=0;
      pixY:=0;
      pixZ:=0;
   end;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX









// ------------------
// ------------------ TZShadows ------------------
// ------------------

// Create
//
constructor TZShadows.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
//   Width:=64;
//   Height:=64;
//   Inited:=False;

//   Material.Texture.ImageClassName:='Blank Image';

{
   material.Texture.Disabled:=False;
   Material.Texture.TextureMode:=tmModulate;
   Material.BlendingMode:=bmTransparency;
   Material.Texture.TextureWrap:=twNone;
}


   bmp32:=TGLBitmap32.Create;
   FColor:=TGLColor.Create(Self);
//   FShadowColor.DefaultColor:=
//   SetShadowColor(FShadowColor);
//   ShowMessage(floatToStr(FShadowColor.Alpha));

   self.FXRes:=64;
   self.FYRes:=64;
   self.Tolerance:=0.003;

end;

//---Destroy---
destructor TzShadows.Destroy;
begin
 ViewerZBuf.Free;
 CasterZBuf.Free;
 bmp32.Free;
 FColor.Free;
 inherited Destroy;
end;



// DoRender
//
procedure TZShadows.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var vx, vy, vx1, vy1 : Single;
begin
   if not assigned(FViewer) then exit;
   if not assigned(FCaster) then exit;
   if not assigned(CasterZBuf) then exit; //only render if a shadow has been cast
   if Scene.CurrentGLCamera<>FViewer.Camera then exit;  //only render in view-camera
   if not assigned(ViewerZBuf) then begin  //Create viewer zbuffer
      ViewerZBuf:=TGLZBuffer.Create;
      ViewerZBuf.LinkToViewer(FViewer);
   end;
   ViewerZBuf.Refresh;

   if FWidth >rci.viewPortSize.cx then Fwidth :=rci.viewPortSize.cx;
   if FHeight>rci.viewPortSize.cy then FHeight:=rci.viewPortSize.cy;

   bmp32.Width:= FXRes;
   bmp32.Height:=FYRes;
   //-----------------------
   CalcShadowTexture(rci);
   //-----------------------
   if not Material.Texture.IsHandleAllocated then
      FTexturePrepared:=False;
   Material.Apply(rci);

//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

   glTexParameteri(GL_Texture_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_Texture_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST); //GL_LINEAR
//   glTexImage2D(GL_TEXTURE_2D,0,1,FXRes,FYRes,0,GL_ALPHA,GL_UNSIGNED_BYTE,@bmp32.data[0]);
   if not FTexturePrepared then begin
      glTexImage2D(GL_TEXTURE_2D,0,4,FXRes,FYRes,0,GL_RGBA,GL_UNSIGNED_BYTE,@bmp32.data[0]);
      FTexturePrepared:=True;
   end else
      glTexSubImage2D(GL_TEXTURE_2D,0,0,0,FXRes,FYRes,GL_RGBA,GL_UNSIGNED_BYTE,@bmp32.data[0]);
{
   glEnable(GL_TEXTURE_2D);
   glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);    //Transparency
   glEnable(GL_BLEND);
}
   NotifyChange(Self);
//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


   // Prepare matrices
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
   glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
   glTranslatef(Position.X-rci.viewPortSize.cx*0.5,
                rci.viewPortSize.cy*0.5-Position.Y, Position.Z);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LIGHTING);
   vx:=0;   vx1:=vx+FWidth;
   vy:=0;   vy1:=vy-FHeight;
   // issue quad
	glBegin(GL_QUADS);
      glNormal3fv(@YVector);
      xglTexCoord2f(0, 0);  glVertex2f( vx, vy1);
      xglTexCoord2f(1, 0);  glVertex2f(vx1, vy1);
      xglTexCoord2f(1, 1);  glVertex2f(vx1,  vy);
      xglTexCoord2f(0, 1);  glVertex2f( vx,  vy);
	glEnd;
   // restore state
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
   Material.UnApply(rci);
   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;

Procedure TZShadows.CalcShadowTexture(var rci : TRenderContextInfo);
var pix,p0,p1,p2,p3,p4:TGLPixel32;
    pixa :PGLPixel32Array;
    x,y,w,h :integer;
    xy :integer;
    fx,fy :single;
begin
 pixa:=bmp32.data;
// w:=bmp32.width;
// h:=bmp32.height;

// w:=self.Width;
// h:=self.Height;
 w:=fXres;
 h:=fYres;

 stepX:=Self.Width/fXres;
 stepY:=self.Height/fYres;

 SCol.r:=Round(FColor.Red*255);
 SCol.g:=Round(FColor.green*255);
 SCol.b:=Round(FColor.Blue*255);
 SCol.a:=Round(FColor.Alpha*255);

 //-----------No optimising-----------
 if FOptimise=opNone then begin
{
  y:=1;While y<h do begin
    x:=0;While x<w do begin
      pix:=HardTest(x,y);
      pixa[x+(h-y)*w]:=pix;
    x:=x+1;end;
  y:=y+1;end;
}
  fy:=1;
  y:=1;While y<fYres do begin
    fx:=0;
    x:=0;While x<fXres do begin
      pix:=HardTest(Round(fx),Round(fy));
      pixa[x+(fYres-y)*fXres]:=pix;
      fx:=fx+stepX;
      x:=x+1;
    end;//x
    fy:=fy+stepY;
    y:=y+1;
  end;//y

 end else
 //-------Optimise 4in1--------
 if FOptimise=op4in1 then begin
  for x:=0 to fXres-1 do HardSet(x,1);
  for x:=0 to fXres-1 do HardSet(x,fYres);
  for y:=1 to fYres-1 do HardSet(0,y);
  for y:=1 to fYres-1 do HardSet(fXres-1,y);
  y:=3;
  While y<fYres do begin
    x:=2;
    p1:=HardSet(x-1,y-2);
        HardSet(x-1,y-1);
    p0:=HardSet(x-1,y);
    While x<fXres do begin
      pix:=HardSet(x,y);
      if (pix.a=p1.a)and(pix.a=p0.a) then begin
         pixa[x-1+(fYres-y)*fXres]:=pix;
         pixa[x-1+(fYres-(y-1))*fXres]:=pix;
      end else begin
         HardSet(x-1,y);
         HardSet(x-1,y-1);
      end;
      p2:=SoftTest(x+1,y-2);
      if (pix.a=p2.a) then pixa[x+(fYres-(y-1))*fXres]:=pix
                      else HardSet(x,y-1);
      p1:=p2;
      p0:=pix;
     x:=x+2;
    end;
    y:=y+2;
  end;
 end else
 //-------Optimise 9in1--------
 if FOptimise=op9in1 then begin
  for x:=0 to fXres-1 do HardSet(x,1);
  for x:=0 to fXres-1 do HardSet(x,fYres);
  for x:=0 to fXres-1 do HardSet(x,fYres-1);
  for y:=1 to fYres-1 do HardSet(fXres-1,y);
  for y:=1 to fYres-1 do HardSet(fXres-2,y);

  y:=4;
  While y<=fYres do begin
    x:=3;
    p1:=HardSet(x-3,y-3);
    p2:=HardSet(x  ,y-3);
    p3:=HardSet(x-3,y  );
    While x<=fXres do begin
      p2:=SoftTest(x,y-3);
      p4:=HardSet(x,y);
      if ((p1.a=p2.a)and(p3.a=p4.a)and(p2.a=p4.a)) then begin
         xy:=x+(fYres-(y-3))*fXres;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
         xy:=x+(fYres-(y-2))*fXres;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
         xy:=x+(fYres-(y-1))*fXres;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
      end else begin
          HardSet(x-2,y-3);
          HardSet(x-1,y-3);
          HardSet(x-3,y-2);
          HardSet(x-2,y-2);
          HardSet(x-1,y-2);
          HardSet(x-3,y-1);
          HardSet(x-2,y-1);
          HardSet(x-1,y-1);
      end;
      p1:=p2;
      p3:=p4;

     x:=x+3;
    end;
    y:=y+3;
  end;

 end else
 //-----------16in1 optimising-----------
 if FOptimise=op16in1 then begin
  for x:=0 to fXres-1 do HardSet(x,fYres);
  for x:=0 to fXres-1 do HardSet(x,fYres-1);
  for x:=0 to fXres-1 do HardSet(x,fYres-2);
  for x:=0 to fXres-1 do HardSet(x,fYres-3);
  y:=5;
  While y<=fYres do begin
    x:=4;
    p1:=HardSet(x-4,y-4);
    p2:=HardSet(x  ,y-4);
    p3:=HardSet(x-4,y  );
    While x<=fXres do begin
      p2:=SoftTest(x,y-4);
      p4:=HardSet(x,y);
      //p4.r:=255;
      if ((p1.a=p2.a)and(p3.a=p4.a)and(p2.a=p4.a)) then begin
         xy:=x+(h-(y-4))*w;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
         xy:=x+(h-(y-3))*w;
          pixa[xy-4]:=p4;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
         xy:=x+(h-(y-2))*w;
          pixa[xy-4]:=p4;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
         xy:=x+(h-(y-1))*w;
          pixa[xy-4]:=p4;
          pixa[xy-3]:=p4;
          pixa[xy-2]:=p4;
          pixa[xy-1]:=p4;
      end else begin
          HardSet(x-3,y-4);
          HardSet(x-2,y-4);
          HardSet(x-1,y-4);
          HardSet(x-4,y-3);
          HardSet(x-3,y-3);
          HardSet(x-2,y-3);
          HardSet(x-1,y-3);
          HardSet(x-4,y-2);
          HardSet(x-3,y-2);
          HardSet(x-2,y-2);
          HardSet(x-1,y-2);
          HardSet(x-4,y-1);
          HardSet(x-3,y-1);
          HardSet(x-2,y-1);
          HardSet(x-1,y-1);
      end;
      p1:=p2;
      p3:=p4;
    x:=x+4;
    end;
  y:=y+4;
  end;
 end;
end;

function TZShadows.HardSet(res_X,res_Y :integer):TGLPixel32;
 var pix :TGLPixel32;
begin
 pix:=HardTest(Round(res_X*stepX) , Round(res_Y*stepY));
// pix:=HardTest(Round(res_X) , Round(res_Y));
 bmp32.data[res_X+(fYres-res_Y)*fXres]:=pix;
 result:=pix;
end;

function TZShadows.HardTest(x,y:integer):TGLPixel32;
var  pix :TGLPixel32;
//     width,height :integer;

    coord:TAffineVector;
    dist :real;
    pixX,pixY :integer;
    pixZ:single;
    IsInFrust :Boolean;
    ilum :Integer;

    d2,d4,d6,d8 :single;
    shad :Integer;
begin
     //---test pixel for shadow---
      ilum:=0;
      if ViewerZBuf.GetPixelzDepth(x,y)<1 then begin
         coord:=ViewerZBuf.PixelToWorld(x,y);
         IsInFrust:=CasterZBuf.WorldToPixelZ(coord,pixX,pixY,pixZ);
         //---Lighting---
         if FDepthFade=True then begin
            ilum:=Round(pixZ*SCol.a*10)-SCol.a*9;
            if ilum<0 then ilum:=0;
            //if ilum>255 then ilum:=255;
            if ilum>SCol.a then ilum:=SCol.a;
         end;
         //--------------
         //---  ilum=light  ------  SCol.a=shade  ------
         if isInFrust=False then begin
             if FFrustShadow then pix.a:=SCol.a
                             else pix.a:=ilum;
         end else begin //---soft shadows---
            if FSoft=True then begin
               d2:=CasterZBuf.Data[(pixX-1)+(pixY  )*Caster.Width];
               d4:=CasterZBuf.Data[(pixX  )+(pixY-1)*Caster.Width];
               d6:=CasterZBuf.Data[(pixX  )+(pixY+1)*Caster.Width];
               d8:=CasterZBuf.Data[(pixX+1)+(pixY  )*Caster.Width];
               if ((pixZ-d2)>FTolerance) then Shad:=     SCol.a else Shad:=     ilum;
               if ((pixZ-d4)>FTolerance) then Shad:=Shad+SCol.a else Shad:=Shad+ilum;
               if ((pixZ-d6)>FTolerance) then Shad:=Shad+SCol.a else Shad:=Shad+ilum;
               if ((pixZ-d8)>FTolerance) then Shad:=Shad+SCol.a else Shad:=Shad+ilum;
               pix.a:=Shad div 4;
            end else begin //---hard shadows---
               dist:=CasterZBuf.Data[pixX+pixY*Caster.Width];
               if ((pixZ-dist)>FTolerance) then Shad:=SCol.a else Shad:=ilum;
               pix.a:=Shad;
            end;
         end;
      end else begin
         if FSkyShadow then pix.a:=SCol.a
                       else pix.a:=ilum;
      end;
      pix.r:=SCol.r;   //0
      pix.g:=SCol.g;   //0
      pix.b:=SCol.b;   //0
      result:=pix;
//      bmp32.data[x+(iheight-y)*iwidth]:=pix;    // OPTIMISE used this to save directly when HardTesting
end;

function TZShadows.SoftTest(x,y:integer):TGLPixel32;
begin
// result:=bmp32.data[x+(iheight-y)*iwidth];
  result:=bmp32.data[x+(fYres-y)*fXres];
end;


function TZShadows.GetViewer :TGLSceneViewer;
begin
 result:=FViewer;
end;

procedure TZShadows.SetViewer(const val :TGLSceneViewer);
begin
 FViewer:=Val;
 Width:=FViewer.Width;
 Height:=FViewer.Height;
end;

function TZShadows.GetCaster :TGLMemoryViewer;
begin
 result:=FCaster;
end;

procedure TZShadows.SetCaster(const val :TGLMemoryViewer);
begin
 FCaster:=Val;
end;


Function TZShadows.CastShadow :Boolean;
begin
   if Caster<>nil then begin
      if not assigned(CasterZBuf) then begin
         CasterZBuf:=TGLZBuffer.Create;
         CasterZBuf.LinkToViewer(FCaster);
      end;
      try
         FCaster.Render;
      except
         Caster:=nil; // prevents further attempts
         raise;
      end;
      CasterZBuf.Refresh;
      Result:=False;
   end else Result:=True;
end;

procedure TZShadows.SetXRes(const val :integer);
var dst :integer;
begin
 dst:=2; While val>=dst do dst:=dst*2;
 FXRes:=dst div 2;
 FTexturePrepared:=False;
// FXRes:=val;

{
 dst:=2; While val>=dst do begin
   dst:=(dst shl 1);
   FXRes:=(dst shr 1);
 end;
}
end;

procedure TZShadows.SetYRes(const val :integer);
var dst :integer;
begin
 dst:=2; While val>=dst do dst:=dst*2;
 FYRes:=dst div 2;
 FTexturePrepared:=False;
// FYRes:=val;


{
 dst:=2; While val>=dst do begin
   dst:=(dst shl 1);
   FYRes:=(dst shr 1);
 end;
}
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// class registrations
   RegisterClasses([TZShadows]);

end.

// GLMirror
{: Implements a basic, stencil-based mirror (as in Mark Kilgard's demo).<p>

   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.<p>

	<b>History : </b><font size=-1><ul>
      <li>07/12/01 - Egg - Creation
   </ul></font>
}
unit GLMirror;

interface

uses Classes, GLScene, Geometry, OpenGL12, GLMisc, GLTexture;

type

   // TMirrorOptions
   //
   TMirrorOption = (moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer);
   TMirrorOptions = set of TMirrorOption;

const
   cDefaultMirrorOptions = [moUseStencil];

type

   // TGLMirror
   //
   {: A simple plane mirror.<p>
      This mirror requires a stencil buffer!<p>
      The object is a mix between a plane and a proxy object, in that the plane
      defines the mirror's surface, while the proxy part is used to reference
      the objects that should be mirrored (it is legal to self-mirror, but no
      self-mirror visuals will be rendered).<p>
      It is strongly recommended to read and understand the explanations in the
      materials/mirror demo before using this component. }
	TGLMirror = class (TGLSceneObject)
	   private
			{ Private Declarations }
         FRendering : Boolean;
         FMirrorObject : TGLBaseSceneObject;
			FWidth, FHeight : TGLFloat;
         FMirrorOptions : TMirrorOptions;

		protected
			{ Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetMirrorObject(const val : TGLBaseSceneObject);
         procedure SetMirrorOptions(const val : TMirrorOptions);
         procedure ClearZBufferArea;

		   procedure SetHeight(AValue: TGLFloat);
		   procedure SetWidth(AValue: TGLFloat);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
		   procedure BuildList(var rci : TRenderContextInfo); override;

		   procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

		published
			{ Public Declarations }
         {: Selects the object to mirror.<p>
            If nil, the whole scene is mirrored. }
         property MirrorObject : TGLBaseSceneObject read FMirrorObject write SetMirrorObject;
         {: Controls rendering options.<p>
            <ul>
            <li>moUseStencil: mirror area is stenciled, prevents reflected
               objects to be visible on the sides of the mirror
            <li>moOpaque: mirror is opaque (ie. painted with background color)
            <li>moMirrorPlaneClip: a ClipPlane is defined when reflecting objects
               to prevent reflections from popping out of the mirror
            <li>moClearZBuffer: mirror area's ZBuffer is cleared so that background
               objects don't interfere with reflected objects (reflected objects
               must be rendered AFTER the mirror in the hierarchy)
            </ul>
         }
         property MirrorOptions : TMirrorOptions read FMirrorOptions write SetMirrorOptions default cDefaultMirrorOptions;

			property Height: TGLFloat read FHeight write SetHeight;
         property Width: TGLFloat read FWidth write SetWidth;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLMirror ------------------
// ------------------

// Create
//
constructor TGLMirror.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FMirrorOptions:=cDefaultMirrorOptions;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
   Material.BlendingMode:=bmTransparency;
end;

// DoRender
//
procedure TGLMirror.DoRender(var rci : TRenderContextInfo;
                          renderSelf, renderChildren : Boolean);
var
   oldProxySubObject : Boolean;
   refMat : TMatrix;
   clipPlane : TDoubleHmgPlane;
   bgColor : TColorVector;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      oldProxySubObject:=rci.proxySubObject;
      rci.proxySubObject:=True;

      if VectorDotProduct(VectorSubtract(rci.cameraPosition, AbsolutePosition), AbsoluteDirection)>0 then begin
      
         glPushAttrib(GL_ENABLE_BIT);

         // "Render" stencil mask
         if MirrorOptions<>[] then begin
            if (moUseStencil in MirrorOptions) then begin
               glClearStencil(1);
               glEnable(GL_STENCIL_TEST);
               glStencilFunc(GL_ALWAYS, 0, 0);
               glStencilOp(GL_ZERO, GL_KEEP, GL_ZERO);
            end;
            if (moOpaque in MirrorOptions) then begin
               bgColor:=ConvertWinColor(Scene.CurrentBuffer.BackgroundColor);
               SetGLMaterialColors(GL_FRONT, @bgColor, @clrBlack, @clrBlack, @clrBlack, 0);
               UnSetGLState(rci.currentStates, stTexture2D);
            end else begin
               glColorMask(False, False, False, False);
            end;
            glDepthMask(False);

            BuildList(rci);

            glDepthMask(True);

            if (moUseStencil in MirrorOptions) then begin
               glStencilFunc(GL_EQUAL, 0, 1);
               glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
            end;

            if (moClearZBuffer in MirrorOptions) then
               ClearZBufferArea;

            if not (moOpaque in MirrorOptions) then
               glColorMask(True, True, True, True);
         end;

         // Mirror lights
         glPushMatrix;
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
         refMat:=MakeReflectionMatrix(AffineVectorMake(AbsolutePosition),
                                      AffineVectorMake(AbsoluteDirection));
         glMultMatrixf(@refMat);
         Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

         // mirror geometry and render master
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);

         glDisable(GL_CULL_FACE);
         glEnable(GL_NORMALIZE);

         if moMirrorPlaneClip in MirrorOptions then begin
            glEnable(GL_CLIP_PLANE0);
            SetPlane(clipPlane, PlaneMake(AffineVectorMake(AbsolutePosition),
                                          VectorNegate(AffineVectorMake(AbsoluteDirection))));
            glClipPlane(GL_CLIP_PLANE0, @clipPlane);
         end;

         if Assigned(FMirrorObject) then begin
            if FMirrorObject.Parent<>nil then
               glMultMatrixf(PGLFloat(FMirrorObject.Parent.AbsoluteMatrixAsAddress));
            glMultMatrixf(@refMat);
            FMirrorObject.DoRender(rci, renderSelf, RenderChildren);
         end else begin
            glMultMatrixf(@refMat);
            Scene.Objects.DoRender(rci, renderSelf, RenderChildren);
         end;

         if moMirrorPlaneClip in MirrorOptions then begin
            glDisable(GL_CLIP_PLANE0);
         end;

         if moUseStencil in MirrorOptions then begin
            glDisable(GL_STENCIL_TEST);
         end;

         // Restore to "normal"
         glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);
         Scene.SetupLights(Scene.CurrentBuffer.LimitOf[limLights]);

         glPopAttrib;
         glPopMatrix;
         ResetGLMaterialColors;
         ResetGLCurrentTexture;

         rci.proxySubObject:=oldProxySubObject;

         // start rendering self
         if renderSelf then begin
            Material.Apply(rci);
            BuildList(rci);
            Material.UnApply(rci);
         end;
         
      end;
      
      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);

      if Assigned(FMirrorObject) then
         FMirrorObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
end;

// BuildList
//
procedure TGLMirror.BuildList(var rci : TRenderContextInfo);
var
   hw, hh : TGLFloat;
begin
   hw:=FWidth*0.5;
   hh:=FHeight*0.5;
   glNormal3fv(@YVector);
   glBegin(GL_QUADS);
      glVertex3f( hw,  hh, 0);
      glVertex3f(-hw,  hh, 0);
      glVertex3f(-hw, -hh, 0);
      glVertex3f( hw, -hh, 0);
   glEnd;
end;

// BuildList
//
procedure TGLMirror.ClearZBufferArea;
var
   worldMat : TMatrix;
   p : TAffineVector;
begin
   with Scene.CurrentBuffer do begin
      glPushMatrix;
      worldMat:=Self.AbsoluteMatrix;
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0, Width, 0, Height, 1, -1);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glDepthFunc(GL_ALWAYS);

      glBegin(GL_QUADS);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(Self.Width*0.5, Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.99);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width*0.5, Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.99);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width*0.5, -Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.99);
         p:=WorldToScreen(VectorTransform(AffineVectorMake(Self.Width*0.5, -Self.Height*0.5, 0), worldMat));
         glVertex3f(p[0], p[1], 0.99);
      glEnd;

      glDepthFunc(GL_LESS);
      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
   end;
end;

// Notification
//
procedure TGLMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (AComponent = FMirrorObject) then
      MirrorObject:=nil;
   inherited;
end;

// SetMirrorObject
//
procedure TGLMirror.SetMirrorObject(const val : TGLBaseSceneObject);
begin
   if FMirrorObject<>val then begin
      if Assigned(FMirrorObject) then
         FMirrorObject.RemoveFreeNotification(Self);
      FMirrorObject:=val;
      if Assigned(FMirrorObject) then
         FMirrorObject.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetWidth
//
procedure TGLMirror.SetWidth(AValue : TGLFloat);
begin
   if AValue<>FWidth then begin
      FWidth:=AValue;
	   StructureChanged;
   end;
end;

// SetHeight
//
procedure TGLMirror.SetHeight(AValue : TGLFloat);
begin
   if AValue<>FHeight then begin
      FHeight:=AValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLMirror.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLMirror) then begin
      FWidth:=TGLMirror(Source).FWidth;
      FHeight:=TGLMirror(Source).FHeight;
      FMirrorOptions:=TGLMirror(Source).FMirrorOptions;
      MirrorObject:=TGLMirror(Source).MirrorObject;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLMirror.AxisAlignedDimensions: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth)*Scale.DirectX,
                      0.5*Abs(FHeight)*Scale.DirectY, 0);
end;

// SetMirrorOptions
//
procedure TGLMirror.SetMirrorOptions(const val : TMirrorOptions);
begin
   if FMirrorOptions<>val then begin
      FMirrorOptions:=val;
      StructureChanged;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLMirror]);

end.

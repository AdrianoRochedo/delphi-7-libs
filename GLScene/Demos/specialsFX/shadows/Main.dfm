?
 TMAINFM 0?$  TPF0TMainFmMainFmLeft_Top@Width2Height?CaptionMainFmColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1Left Top WidthHeight	AlignmenttaCenterAutoSizeCaption	Main ViewColor??? Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold ParentColor
ParentFont  TLabelLabel2LeftTop WidthHeight	AlignmenttaCenterAutoSizeCaptionLightsource Point of viewColor??? Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold ParentColor
ParentFont  TPanelPanel2LeftTopWidthHeightTabOrder  TGLSceneViewerCasterLeftTopWidth Height Camera	GLCamera2OnMouseDownCasterMouseDownOnMouseMoveCasterMouseMove	OnMouseUpCasterMouseUp   TPanelPanel1Left TopWidthHeightTabOrder TGLSceneViewerViewerLeftTopWidth? Height? Camera	GLCamera1OnMouseDownViewerMouseDownOnMouseMoveViewerMouseMove	OnMouseUpViewerMouseUp   TPanelPanel3Left Top(WidthHeight)TabOrder TLabelLabel4LeftTopWidthHHeightCaptionCamera DistanceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  TLabelTimeLblLeft? TopWidthHeightCaptionTimeFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  TLabelLabel6Left? TopWidthHeightCaptionXresFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  TLabelLabel7Left? TopWidthHeightCaptionYresFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  	TTrackBarDistanceBarLeftTopWidthsHeightHint4Moves the main camera closer/further from the teapotMaxMinOrientationtrHorizontalParentShowHint	FrequencyPositionSelEndSelStartShowHint	TabOrder TabStopThumbLength
	TickMarkstmBottomRight	TickStyletsAutoOnChangeDistanceBarChange  TButtonCastBtnLeft? TopWidth9HeightHint}Measure the time it takes in s/100 to render the lightsource z-buffer, generate the shadow texture and render the main view. CaptionCastTabOrderOnClickCastBtnClick  	TComboBoxXresBoxLeft? TopWidth)HeightHint5Adjust the X-resolution of the shadow overlay textureFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ItemHeight
ParentFontParentShowHintShowHint	TabOrderText256OnChangeXResBoxChangeItems.Strings5122561286432   	TComboBoxYresBoxLeft? TopWidth)HeightHint5Adjust the Y-resolution of the shadow overlay textureFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ItemHeight
ParentFontParentShowHintShowHint	TabOrderText256OnChangeYresBoxChangeItems.Strings5122561286432    TPanelPanel4LeftTop(WidthHeight)TabOrder TLabelLabel3LeftTopWidthHHeightCaptionCamera DistanceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  TLabelLabel5Left? TopWidthHeightCaptionFocusFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  	TTrackBarDistanceBar2LeftTopWidthsHeightHint/Moves the light closer/further from the teapot.MaxMinOrientationtrHorizontalParentShowHint	FrequencyPosition	SelEndSelStartShowHint	TabOrder TabStopThumbLength
	TickMarkstmBottomRight	TickStyletsAutoOnChangeDistanceBar2Change  	TTrackBarFocalLeft? TopWidthsHeightHintPAdjust the Focal length of the lightsource camera, to adjust the lightbeam widthMax,MinOrientationtrHorizontalParentShowHint	Frequency
Position2SelEnd SelStart ShowHint	TabOrderTabStopThumbLength
	TickMarkstmBottomRight	TickStyletsAutoOnChangeFocalChange   TPanelPanel5Left TopXWidthyHeightaTabOrder 	TCheckBoxFrustBoxLeftTopWidthiHeightCaptionFrustrum ShadowChecked	State	cbCheckedTabOrder OnClickFrustBoxClick  	TCheckBox	RotateBoxLeftTopHWidthiHeightCaptionRotate the TorusTabOrderOnClickRotateBoxClick  	TCheckBoxShadowOnBoxLeftTopWidthaHeightCaption
Shadows OnChecked	State	cbCheckedTabOrderOnClickShadowOnBoxClick  	TCheckBoxSoftBoxLeftTop8WidthIHeightHintzTests 4 pixels on the lightsource z-buffer, in stead of 1, to calculate shadow brightness, and give soft edges to shadows.Caption
Soft EdgesParentShowHintShowHint	TabOrderOnClickSoftBoxClick  	TCheckBox
SkyShadBoxLeftTop(WidthaHeightCaption
Sky ShadowTabOrderOnClickSkyShadBoxClick   TPanelPanel6Left?TopXWidthyHeightaTabOrder TLabelLabel9LeftTop@Width;HeightCaptionShadow AlphaFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFont  	TCheckBoxFadeBoxLeftTopWidthaHeightCaptionDepth of view fadeFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style 
ParentFontParentShowHintShowHint	TabOrder OnClickFadeBoxClick  	TTrackBardovBarLeftTopWidthsHeightHintAdjust depth of viewMaxdMinOrientationtrHorizontalParentShowHint	FrequencyPosition2SelEndSelStartShowHint	TabOrderTabStopThumbLength
	TickMarkstmBottomRight	TickStyletsAutoOnChangedovBarChange  	TTrackBarAlphaBarLeftTopJWidthsHeightHintAdjust the darkness of shadowsMax MinOrientationtrHorizontalParentShowHint	Frequency
Position? SelEndSelStartShowHint	TabOrderTabStopThumbLength
	TickMarkstmBottomRight	TickStyletsAutoOnChangeAlphaBarChange   TMemoMemo1Left? TopXWidth)HeightaFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameSmall Fonts
Font.Style Lines.StringsDThis application casts and draws shadows, using the Z-Buffer method.@Left-click and drag on the viewers, to rotate the camera angles.FAnything that is not visible from the lightsource viewer(Right) is in  shadow in the main viewer.(Left) FWhen Depth Fade is on, the light fades as it approaches the far plane.5Shadows can be set to have either hard or soft edges.FThe area outside the lightsource view, can be set to be light or dark. 
ParentFontTabOrder  TGLSceneGLScene1LeftTop  
TDummyCubeObjectsCubeSize       ??? THeightFieldHeightField1Direction.Coordinates
         ??        Position.Coordinates
          ?      ??Scale.Coordinates
      A   A  ??    Up.Coordinates
             ??    Material.MaterialLibraryGLMaterialLibrary1Material.LibMaterialNameTilesXSamplingScale.Min       ???XSamplingScale.Max       ???XSamplingScale.Step     
ף??YSamplingScale.Min       ???YSamplingScale.Max       ???YSamplingScale.Step     
ף??OptionshfoTextureCoordinateshfoTwoSided   TCubeCube1Position.Coordinates
   ????          ??Scale.Coordinates
   ???=   @   @    Material.MaterialLibraryGLMaterialLibrary1Material.LibMaterialNamePlaneMat  TTorusTorus1Material.Texture.TextureMode
tmModulateMajorRadius     ?̌??MinorRadius     ?????  TTeapotTeapot1Scale.Coordinates
   ????????????    Material.MaterialLibraryGLMaterialLibrary1Material.LibMaterialNameBeigeMarble   	TZShadowsShadows1ViewerViewerCasterMemViewFrustShadow		SkyShadowOptimiseop9in1Width? Height? Color.Color
                  ?Xres Yres Soft	Tolerance     ?????Material.BlendingModebmTransparencyMaterial.Texture.TextureMode
tmModulateMaterial.Texture.TextureWraptwNoneMaterial.Texture.Disabled	DepthFade  	TGLCamera	GLCamera1DepthOfView       ?@FocalLength       ?@TargetObjectObjectsPosition.Coordinates
             ?@  ??LeftTop?   	TGLCamera	GLCamera2DepthOfView       ?@FocalLength       ?@TargetObjectObjectsPosition.Coordinates
     @@  ?@  ?@  ??LeftTop?  TGLLightSourceGLLightSource1Ambient.Color
   ???>???>???>  ??ConstAttenuation       ???
SpotCutOff       ?@    TGLMaterialLibraryGLMaterialLibrary1	MaterialsName	ShadowMat&Material.FrontProperties.Ambient.Color
                 ??&Material.FrontProperties.Diffuse.Color
                  ?'Material.FrontProperties.Emission.Color
     ??  ??  ??  ??Material.BlendingModebmTransparencyMaterial.Texture.ImageClassNameTGLBlankImage NamePlaneMatMaterial.Texture.ImageClassNameTGLBlankImage NameTilesMaterial.Texture.TextureMode
tmModulate NameBeigeMarbleMaterial.Texture.TextureMode
tmModulate NameMarbleMaterial.Texture.TextureMode
tmModulate  Left0Top   TGLMemoryViewerMemViewCamera	GLCamera2Buffer.ContextOptionsroNoColorBuffer Buffer.LightingLeft? Top   TAsyncTimerAsyncTimer1Enabled	Interval(OnTimerAsyncTimer1TimerThreadPrioritytpTimeCriticalLeftTopP  TGLCadencerGLCadencer1LeftTop?    
// GLSpaceText
{: Win32 specific Context.<p>

	<b>History : </b><font size=-1><ul>
      <li>30/01/02 - EG - Text Alignment (S?ren M?hlbauer),
                          TFontManager now GLContext compliant (RenderToBitmap ok!) 
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	   <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLSpaceText;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Classes, GLScene, Graphics, OpenGL12, GLTexture, GLContext;

type

   // TSpaceTextCharRange
   //
   TSpaceTextCharRange = (stcrAlphaNum, stcrNumbers, stcrAll);

   // TGLTextHorzAdjust
   //
   TGLTextHorzAdjust = (haLeft, haCenter, haRight, haAligned, haCentrically, haFitIn);

   // TGLTextVertAdjust
   //
   TGLTextVertAdjust = (vaTop, vaCenter, vaBottom, vaBaseLine);

   // TGLTextAdjust
   //
   TGLTextAdjust = class(TPersistent)
      private
			{ Private Declarations }
         FHorz: TGLTextHorzAdjust;
         FVert: TGLTextVertAdjust;
         FOnChange: TNotifyEvent;
         procedure SetHorz(const Value: TGLTextHorzAdjust);
         procedure SetVert(const Value: TGLTextVertAdjust);

      public
			{ public Declarations }
         constructor Create;
         procedure Assign(Source: TPersistent); override;

         property OnChange: TNotifyEvent read FOnChange write FOnChange;

      published
			{ Published Declarations }
         property Horz: TGLTextHorzAdjust read FHorz write SetHorz default haLeft;
         property Vert: TGLTextVertAdjust read FVert write SetVert default vaBaseLine;
   end;

   // holds an entry in the font manager list (used in TSpaceText)
   PFontEntry        = ^TFontEntry;
   TFontEntry        = record
                         Name      : String;
                         FVirtualHandle : TGLVirtualHandle;
                         Styles    : TFontStyles;
                         Extrusion : Single;
                         RefCount  : Integer;
                         allowedDeviation : Single;
                         firstChar, lastChar : Integer;
                         glyphMetrics : array [0..255] of TGlyphMetricsFloat;
                       end;

   // TSpaceText
   //
   {: Renders a text in 3D. }
   TSpaceText = class (TGLSceneObject)
      private
			{ Private Declarations }
         FFont       : TFont;
         FText       : String;
         FExtrusion  : Single;
         FAllowedDeviation : Single;
         FCharacterRange : TSpaceTextCharRange;
         FAdjust : TGLTextAdjust;
         FAspectRatio : Single;
         FOblique : Single;
         FTextHeight : Single;
         procedure SetCharacterRange(const val : TSpaceTextCharRange);
         procedure SetAllowedDeviation(const val : Single);
         procedure SetExtrusion(AValue: Single);
         procedure SetFont(AFont: TFont);
         procedure SetText(AText: String);
         procedure SetAdjust(const value : TGLTextAdjust);
         procedure SetAspectRatio(const value : Single);
         procedure SetOblique(const value : Single);
         procedure SetTextHeight(const value : Single);

		protected
			{ Protected Declarations }
         FTextFontEntry : PFontEntry;
         FontChanged : Boolean;
         procedure DestroyHandles; override;
         procedure OnFontChange(sender : TObject);
         procedure GetFirstAndLastChar(var firstChar, lastChar : Integer);

		public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

         function TextWidth(const str : String = '') : Single;
         function TextMaxHeight(const str : String = '') : Single;
         function TextMaxUnder(const str : String = '') : Single;
         procedure TextMetrics(const str : String; var width, maxHeight, maxUnder : Single);

		published
			{ Published Declarations }
         {: Adjusts the 3D font extrusion.<p>
            If Extrusion=0, the characters will be flat (2D), values >0 will
            give them a third dimension. }
         property Extrusion: Single read FExtrusion write SetExtrusion;
         property Font: TFont read FFont write SetFont;
         property Text: String read FText write SetText;
         {: Quality related, see Win32 help for wglUseFontOutlines }
         property AllowedDeviation : Single read FAllowedDeviation write SetAllowedDeviation;
         {: Character range to convert.<p>
            Converting less characters saves time and memory... }
         property CharacterRange : TSpaceTextCharRange read FCharacterRange write SetCharacterRange default stcrAll;
         property AspectRatio : Single read FAspectRatio write SetAspectRatio;
         property TextHeight : Single read FTextHeight write SetTextHeight;
         property Oblique : Single read FOblique write SetOblique;
         property Adjust : TGLTextAdjust read FAdjust write SetAdjust;
    end;

   // TFontManager
   //
   {: Manages a list of fonts for which display lists were created. }
   TFontManager = class(TList)
	   private
			{ Private Declarations }
         FCurrentBase : Integer;

	   protected
			{ Protected Declarations }
         procedure VirtualHandleAlloc(sender : TGLVirtualHandle; var handle : Integer);
         procedure VirtualHandleDestroy(sender : TGLVirtualHandle; var handle : Integer);

	   public
			{ Public Declarations }
         constructor Create;
         destructor Destroy; override;
         
         function FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                           FAllowedDeviation : Single;
                           FFirstChar, FLastChar : Integer) : PFontEntry;
         function GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                              allowedDeviation : Single;
                              firstChar, lastChar : Integer) : PFontEntry;
         procedure Release(entry : PFontEntry);
   end;

function FontManager : TFontManager;
procedure ReleaseFontManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

var
	vFontManager : TFontManager;

// FontManager
//
function FontManager : TFontManager;
begin
	if not Assigned(vFontManager) then
		vFontManager:=TFontManager.Create;
	Result:=vFontManager;
end;

// ReleaseFontManager
//
procedure ReleaseFontManager;
begin
   if Assigned(vFontManager) then begin
      vFontManager.Free;
      vFontManager:=nil;
   end;
end;

// ------------------
// ------------------ TGLTextAdjust ------------------
// ------------------

// Create
//
constructor TGLTextAdjust.Create;
begin
   inherited;
   FHorz:=haLeft;
   FVert:=vaBaseLine;
end;

// Assign
//
procedure TGLTextAdjust.Assign(source : TPersistent);
begin
   if Source is TGLTextAdjust then begin
      FHorz:=TGLTextAdjust(source).Horz;
      FVert:=TGLTextAdjust(source).Vert;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end else inherited Assign(Source);
end;

// SetHorz
//
procedure TGLTextAdjust.SetHorz(const value : TGLTextHorzAdjust);
begin
   if FHorz<>value then begin
      FHorz:=value;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end;
end;

// SetVert
//
procedure TGLTextAdjust.SetVert(const value : TGLTextVertAdjust);
begin
   if value<>FVert then begin
      FVert:=value;
      if Assigned(FOnChange) then
         FOnChange(Self);
   end;
end;

// ------------------
// ------------------ TSpaceText ------------------
// ------------------

// Create
//
constructor TSpaceText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FFont:=TFont.Create;
   FFont.Name:='Arial';
   FontChanged:=True;
   FExtrusion:=0;
   CharacterRange:=stcrAll;
   FFont.OnChange:=OnFontChange;
   FAdjust:=TGLTextAdjust.Create;
   FAdjust.OnChange:=OnFontChange;
end;

// Destroy
//
destructor TSpaceText.Destroy;
begin
   FAdjust.OnChange:=nil;
   FAdjust.Free;
   FFont.OnChange:=nil;
   FFont.Free;
   inherited Destroy;
end;

// TextMetrics
//
procedure TSpaceText.TextMetrics(const str : String; var width, maxHeight, maxUnder : Single);
var
   i, firstChar, lastChar : Integer;
   buf : String;
   gmf : TGlyphMetricsFloat;
begin
   width:=0;
   maxUnder:=0;
   maxHeight:=0;
   if Assigned(FTextFontEntry) then begin
      GetFirstAndLastChar(firstChar, lastChar);
      if str='' then
         buf:=FText
      else buf:=str;
      for i:=1 to Length(buf) do begin
         gmf:=FTextFontEntry.GlyphMetrics[Integer(buf[i])-firstChar];
         width:=width+gmf.gmfCellIncX;
         if gmf.gmfptGlyphOrigin.y>maxHeight then
            maxHeight:=gmf.gmfptGlyphOrigin.y;
         if gmf.gmfptGlyphOrigin.y-gmf.gmfBlackBoxY<maxUnder then
            maxUnder:=gmf.gmfptGlyphOrigin.y-gmf.gmfBlackBoxY;
      end;
   end;
end;

// TextWidth
//
function TSpaceText.TextWidth(const str : String = '') : Single;
var
   mh, mu : Single;
begin
   TextMetrics(str, Result, mh, mu);
end;

// TextMaxHeight
//
function TSpaceText.TextMaxHeight(const str : String = '') : Single;
var
   w, mu : Single;
begin
   TextMetrics(str, w, Result, mu);
end;

// TextMaxUnder
//
function TSpaceText.TextMaxUnder(const str : String = '') : Single;
var
   w, mh : Single;
begin
   TextMetrics(str, w, mh, Result);
end;

// BuildList
//
procedure TSpaceText.BuildList(var rci : TRenderContextInfo);
var
   textL, maxUnder, maxHeight : Single;
   charScale : Single;
begin
   if Length(FText)>0 then begin
      glPushMatrix;

      if FAspectRatio<>0 then
         glScalef(FAspectRatio, 1, 1);
      if FOblique<>0 then
         glRotatef(FOblique, 0, 0, 1);

      if (FAdjust.Horz<>haLeft) or (FAdjust.Vert<>vaBaseLine) or (FTextHeight<>0) then begin
         TextMetrics('', textL, maxHeight, maxUnder);
         if FTextHeight<>0 then begin
            charScale:=FTextHeight/MaxHeight;
            glScalef(CharScale,CharScale,1);
         end;
         case FAdjust.Horz of
            haLeft : ; // nothing
            haCenter : glTranslatef(-textL*0.5, 0, 0);
            haRight :  glTranslatef(-textL, 0, 0);
         end;
         case FAdjust.Vert of
            vaBaseLine : ; // nothing;
            vaBottom : glTranslatef(0, abs(maxUnder), 0);
            vaCenter : glTranslatef(0, abs(maxUnder)*0.5-maxHeight*0.5, 0);
            vaTop :    glTranslatef(0, -maxHeight, 0);
         end;
      end;

      glPushAttrib(GL_POLYGON_BIT);
      case FCharacterRange of
        stcrAlphaNum : glListBase(FTextFontEntry.FVirtualHandle.Handle - 32);
        stcrNumbers :  glListBase(FTextFontEntry.FVirtualHandle.Handle - Integer('0'));
      else
        glListBase(FTextFontEntry.FVirtualHandle.Handle);
      end;
      glCallLists(Length(FText), GL_UNSIGNED_BYTE, PChar(FText));
      glPopAttrib;

      glPopMatrix;
   end;
end;

// DoDestroyList
//
procedure TSpaceText.DestroyHandles;
begin
   ReleaseFontManager;
   inherited;
end;

// GetFirstAndLastChar
//
procedure TSpaceText.GetFirstAndLastChar(var firstChar, lastChar : Integer);
begin
   case FCharacterRange of
      stcrAlphaNum : begin
         firstChar:=32;
         lastChar:=127;
      end;
      stcrNumbers : begin
         firstChar:=Integer('0');
         lastChar:=Integer('9');
      end;
   else
      // stcrAll
      firstChar:=0;
      lastChar:=255;
   end;
end;

// DoRender
//
procedure TSpaceText.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var
	firstChar, lastChar : Integer;
begin
   if FText<>'' then begin
   	if FontChanged or (FTextFontEntry.FVirtualHandle.Handle=0) then with FFont do begin
	   	FontManager.Release(FTextFontEntry);
         GetFirstAndLastChar(firstChar, lastChar);
   		FTextFontEntry:=FontManager.GetFontBase(Name, Style, FExtrusion,
	   					      							 FAllowedDeviation, firstChar, lastChar);
		   FontChanged:=False;
   	end;
   end;
	inherited;
end;

// SetExtrusion
//
procedure TSpaceText.SetExtrusion(AValue: Single);
begin
   Assert(AValue>=0, 'Extrusion must be >=0');
	if FExtrusion<>AValue then begin
		FExtrusion:=AValue;
      OnFontChange(nil);
	end;
end;

// SetAllowedDeviation
//
procedure TSpaceText.SetAllowedDeviation(const val : Single);
begin
	if FAllowedDeviation<>val then begin
		FAllowedDeviation:=val;
      OnFontChange(nil);
	end;
end;

// SetCharacterRange
//
procedure TSpaceText.SetCharacterRange(const val : TSpaceTextCharRange);
begin
	if FCharacterRange<>val then begin
		FCharacterRange:=val;
      OnFontChange(nil);
	end;
end;

// SetFont
//
procedure TSpaceText.SetFont(AFont: TFont);
begin
   FFont.Assign(AFont);
   OnFontChange(nil);
end;

// OnFontChange
//
procedure TSpaceText.OnFontChange(sender : TObject);
begin
   FontChanged:=True;
   StructureChanged;
end;

// SetText
//
procedure TSpaceText.SetText(AText: String);
begin
   if FText<>AText then begin
      FText:=AText;
      StructureChanged;
   end;
end;

// SetAdjust
//
procedure TSpaceText.SetAdjust(const value : TGLTextAdjust);
begin
   FAdjust.Assign(Value);
   StructureChanged;
end;

// SetAspectRatio
//
procedure TSpaceText.SetAspectRatio(const value : Single);
begin
   if FAspectRatio<>value then begin
      FAspectRatio:=value;
      StructureChanged;
   end;
end;

// SetOblique
//
procedure TSpaceText.SetOblique(const value : Single);
begin
   if FOblique<>Value then begin
      FOblique:=value;
      StructureChanged;
   end;
end;

// SetTextHeight
//
procedure TSpaceText.SetTextHeight(const value : Single);
begin
   if value<>FTextHeight then begin
      FTextHeight:=value;
      StructureChanged;
   end;
end;

// ------------------
// ------------------ TFontManager ------------------
// ------------------

// Create
//
constructor TFontManager.Create;
begin
   inherited;
end;

// Destroy
//
destructor TFontManager.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      TFontEntry(Items[i]^).FVirtualHandle.Free;
      FreeMem(Items[i], SizeOf(TFontEntry));
   end;
   inherited Destroy;
end;

// VirtualHandleAlloc
//
procedure TFontManager.VirtualHandleAlloc(sender : TGLVirtualHandle; var handle : Integer);
begin
   handle:=FCurrentBase;
end;

// VirtualHandleDestroy
//
procedure TFontManager.VirtualHandleDestroy(sender : TGLVirtualHandle; var handle : Integer);
begin
   if handle<>0 then
      glDeleteLists(handle, sender.Tag);
end;

// FindFond
//
function TFontManager.FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
										 FAllowedDeviation : Single;
										 FFirstChar, FLastChar : Integer) : PFontEntry;
var
	i : Integer;
begin
	Result:=nil;
	// try to find an entry with the required attributes
	for I :=0 to Count-1 do with TFontEntry(Items[I]^) do
		if (CompareText(Name, AName) = 0) and (Styles = FStyles)
				and (Extrusion = FExtrusion) and (allowedDeviation=FAllowedDeviation)
				and (firstChar=FFirstChar)	and (lastChar=FLastChar) then begin
			// entry found
			Result:=Items[I];
			Break;
		end;
end;

// GetFontBase
//
function TFontManager.GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
											 allowedDeviation : Single;
											 firstChar, lastChar : Integer) : PFontEntry;
var
   NewEntry : PFontEntry;
	MemDC    : HDC;
	AFont    : TFont;
   nbLists  : Integer;
begin
   NewEntry:=FindFont(AName, FStyles, FExtrusion, allowedDeviation, firstChar, lastChar);
   if Assigned(NewEntry) then begin
	   Inc(NewEntry^.RefCount);
      Result:=NewEntry;
   end else Result:=nil;
   if Result=nil then begin
      // no entry found, or entry was purged
      nbLists:=lastChar-firstChar+1;
      if not Assigned(newEntry) then begin
         // no entry found, so create one
         New(NewEntry);
         NewEntry^.Name:=AName;
         NewEntry^.FVirtualHandle:=TGLVirtualHandle.Create;
         NewEntry^.FVirtualHandle.OnAllocate:=VirtualHandleAlloc;
         NewEntry^.FVirtualHandle.OnDestroy:=VirtualHandleDestroy;
         NewEntry^.FVirtualHandle.Tag:=nbLists;
         NewEntry^.Styles:=FStyles;
         NewEntry^.Extrusion:=FExtrusion;
         NewEntry^.RefCount:=1;
         NewEntry^.firstChar:=firstChar;
         NewEntry^.lastChar:=lastChar;
         NewEntry^.allowedDeviation:=allowedDeviation;
         Add(NewEntry);
      end;
      // create a font to be used while display list creation
      AFont:=TFont.Create;
      MemDC:=CreateCompatibleDC(0);
      try
         AFont.Name:=AName;
         AFont.Style:=FStyles;
         SelectObject(MemDC, AFont.Handle);
         FCurrentBase:=glGenLists(nbLists);
		   if FCurrentBase = 0 then
			   raise Exception.Create('FontManager: no more display lists available');
         NewEntry.FVirtualHandle.AllocateHandle;
		   if not OpenGL12.wglUseFontOutlines(MemDC, firstChar, nbLists,
                                            FCurrentBase, allowedDeviation,
                                            FExtrusion, WGL_FONT_POLYGONS,
                                            @NewEntry^.GlyphMetrics) then
   		  	raise Exception.Create('FontManager: font creation failed');
      finally
		   AFont.Free;
         DeleteDC(MemDC);
      end;
      Result:=NewEntry;
   end;
end;

// Release
//
procedure TFontManager.Release(entry : PFontEntry);
begin
   if Assigned(entry) then begin
      Dec(entry^.RefCount);
      if entry^.RefCount=0 then begin
         entry.FVirtualHandle.Free;
         Remove(entry);
         Dispose(entry)
      end;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClass(TSpaceText);

finalization

   ReleaseFontManager;

end.

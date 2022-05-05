{**************************************}
{   TeeChart Pro Charting Library      }
{   Custom Series Example: TImageBar   }
{**************************************}
{$I teedefs.inc}
unit ImageBar;

interface

Uses WinTypes, WinProcs, Classes, Graphics, Series, Chart;

{ This unit implements a custom TeeChart Series.
  The TImageBarSeries is a normal BarSeries with an optional Image to
  be displayed on each Bar point, stretched or tiled.
  Only rectangular Bar style is allowed.
  The bitmap image is not "mapped" to Bars in 3D mode with
  rotation or elevation. Only 2D and 3D with Orthogonal projection.
}
type TImageBarSeries=class(TBarSeries)
     private
       FImage:TPicture;
       FImageTiled:Boolean;
       Procedure SetImage(Value:TPicture);
       Procedure SetImageTiled(Value:Boolean);
       Procedure DrawTiledImage(AImage:TPicture; Const R:TRect; StartFromTop:Boolean);
     public
       Constructor Create(AOwner:TComponent); override;
       Destructor Destroy; override;
       Procedure DrawBar(BarIndex,StartPos,EndPos:Longint); override;
       Function GetEditorClass:String; override;	
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
     published
       property Image:TPicture read FImage write SetImage;
       property ImageTiled:Boolean read FImageTiled write SetImageTiled default False;
     end;

{ used also at ImaPoint.pas unit }
Procedure LoadBitmapFromResourceName(ABitmap:TBitmap; const ResName: string);

implementation

Uses TeeProcs,SysUtils,TeCanvas,TeeProco;  { <-- needed only for the "Samples" constant }

{ This resource file contains the default bitmap image (Bulb.bmp) }
{$IFDEF D1}
{$R TEEIMABA.R16}
{$ELSE}
{$R TEEIMABA.RES}
{$ENDIF}

{ This function loads a bitmap from a resource linked to the executable }
Procedure LoadBitmapFromResourceName(ABitmap:TBitmap; const ResName: string);
{$IFDEF D1}
var tmpSt:Array[0..255] of char;
{$ENDIF}
begin
  {$IFDEF D1}
  ABitmap.Handle:=LoadBitmap(Hinstance,StrPCopy(tmpSt,ResName));
  {$ELSE}
  ABitmap.LoadFromResourceName(HInstance,ResName);
  {$ENDIF}
end;

{ overrided constructor to create the Image property }
Constructor TImageBarSeries.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FImage:=TPicture.Create;
  FImage.OnChange:=CanvasChanged;
  LoadBitmapFromResourceName(FImage.Bitmap,'TeeMoney');  { <-- load default }
end;

Destructor TImageBarSeries.Destroy;
begin
  FImage.Free;  { <-- remember to destroy private properties }
  inherited Destroy;
end;

Procedure TImageBarSeries.SetImage(Value:TPicture);
begin
  FImage.Assign(Value);
end;

Procedure TImageBarSeries.SetImageTiled(Value:Boolean);
begin
  SetBooleanProperty(FImageTiled,Value);
end;

{ Add two bars only to the gallery }
Procedure TImageBarSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(2);
  ParentChart.View3DOptions.Orthogonal:=True;
end;

{ This method draws an image in tiled mode }
Procedure TImageBarSeries.DrawTiledImage( AImage:TPicture;
                                          Const R:TRect;
                                          StartFromTop:Boolean );
Var tmpX      : Integer;
    tmpY      : Integer;
    tmpWidth  : Integer;
    tmpHeight : Integer;
    RectH     : Longint;
    RectW     : Longint;
    tmpRect   : TRect;
begin
  tmpWidth :=AImage.Width;
  tmpHeight:=AImage.Height;
  if (tmpWidth>0) and (tmpHeight>0) then
  Begin
    ParentChart.Canvas.ClipRectangle(R);
    RectSize(R,RectW,RectH);
    tmpY:=0;
    while tmpY<RectH do
    begin
      tmpX:=0;
      while tmpX<RectW do
      begin
        if StartFromTop then
           tmpRect:=Rect(R.Left,R.Top+tmpY,R.Right,R.Top+tmpY+tmpHeight)
        else
           tmpRect:=Rect(R.Left,R.Bottom-tmpY-tmpHeight,R.Right,R.Bottom-tmpY);
        ParentChart.Canvas.StretchDraw(tmpRect,AImage.Graphic);
        Inc(tmpX,tmpWidth);
      end;
      Inc(tmpY,tmpHeight);
    end;
    ParentChart.Canvas.UnClipRectangle;
  end;
end;

Procedure TImageBarSeries.DrawBar(BarIndex,StartPos,EndPos:Longint);
Var R   : TRect;
    tmp : Integer;
begin
  { first thing to do is to call the inherited DrawBar method of TBarSeries }
  inherited DrawBar(BarIndex, StartPos, EndPos );

  if FImage.Graphic<>nil then { <-- if non empty image... }
  Begin
    { Calculate the exact rectangle, removing borders }
    R:=BarBounds;
    if R.Bottom<R.Top then SwapInteger(R.Top,R.Bottom);
    if BarPen.Visible then
    begin
      tmp:=BarPen.Width;
      if (tmp>1) and ((tmp mod 2)=0) then Dec(tmp);
      Inc(R.Left,tmp);
      Inc(R.Top,tmp);
      if not ParentChart.View3D then
      begin
        Dec(R.Right);
        Dec(R.Bottom);
      end;
    end;
    With ParentChart.Canvas,R do
    begin
      TopLeft    :=Calculate3DPosition(Left,Top,StartZ);
      BottomRight:=Calculate3DPosition(Right,Bottom,StartZ);
    end;
    { Draw the image }
    if FImageTiled then { tiled }
       DrawTiledImage(FImage,R,BarBounds.Bottom<BarBounds.Top)
    else { stretched }
       ParentChart.Canvas.StretchDraw(R,FImage.Graphic);
  end;
end;

Function TImageBarSeries.GetEditorClass:String; 
begin
  result:='TImageBarSeriesEditor';
end;

{ Series/Functions Registration/Un-Registration }
Procedure ImageBarExitProc; far;
begin
  UnRegisterTeeSeries([ TImageBarSeries ]);
end;

initialization
  RegisterTeeSeries( TImageBarSeries, 'ImageBar', TeeMsg_GallerySamples, 1 );
{$IFDEF D1}
  AddExitProc(ImageBarExitProc);
{$ELSE}
finalization
  ImageBarExitProc;
{$ENDIF}
end.

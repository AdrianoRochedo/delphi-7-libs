{**********************************************}
{   TImagePointSeries and TDeltaPointSeries    }
{   Copyright (c) 1997-98 by David Berneda     }
{      Series Developer Kit Example            }
{**********************************************}
{$I teedefs.inc}
unit ImaPoint;

interface

Uses WinTypes,WinProcs,SysUtils,Classes,Graphics,Teengine,Chart,Series,TeCanvas;

{  This unit contains two sample Series components:

   TImagePointSeries --  A point Series displaying images
   TDeltaPointSeries --  An ImagePoint Series displaying a different
                         image depending the previous point value.
}

Type { ImagePoint Series definition.... }
     TCustomImagePointSeries=class;
     TGetImageEvent=Procedure( Sender:TCustomImagePointSeries;
                               ValueIndex:Integer;
                               Picture:TPicture) of object;

     TCustomImagePointSeries=class(TPointSeries)
     private
       FImagePoint:TPicture;
       FOnGetImage:TGetImageEvent;
       procedure SetImagePoint(Value:TPicture);
     protected
       procedure DrawValue(ValueIndex:Longint); override;
     public
       Constructor Create(AOwner:TComponent); override;
       Destructor Destroy; override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
       property ImagePoint:TPicture read FImagePoint write SetImagePoint;
       property OnGetImage:TGetImageEvent read FOnGetImage
                                          write FOnGetImage;
     end;

     TImagePointSeries=class(TCustomImagePointSeries)
     published
       property ImagePoint;
       property OnGetImage;
     end;

     TDeltaImageStyle=(disSmiles, disHands);

     { DeltaPoint Series definition.... }
     TDeltaPointSeries=class(TCustomImagePointSeries)
     private
       FEqualImage,
       FGreaterImage,
       FLowerImage   : TPicture;
       FImageStyle : TDeltaImageStyle;
       procedure SetEqualImage(Value:TPicture);
       procedure SetGreaterImage(Value:TPicture);
       procedure SetLowerImage(Value:TPicture);
       procedure SetImageStyle(Value:TDeltaImageStyle);
     public
       Constructor Create(AOwner:TComponent); override;
       Destructor Destroy; override;
     protected
       procedure DrawValue(ValueIndex:Longint); override;
     published
       property ImageStyle:TDeltaImageStyle read FImageStyle write
                                            SetImageStyle default disSmiles;
       property EqualImage:TPicture read FEqualImage write SetEqualImage;
       property GreaterImage:TPicture read FGreaterImage write SetGreaterImage;
       property LowerImage:TPicture read FLowerImage write SetLowerImage;
       { inherited from TCustomImagePointSeries }
       property OnGetImage;
     end;

implementation

Uses ImageBar,TeeProco;  { <-- needed only for the "Samples" constant }

{$IFDEF D1}
{$R TEEIMAPO.R16}
{$ELSE}
{$R TEEIMAPO.RES}
{$ENDIF}

{ overrided constructor to create the ImagePoint property }
Constructor TCustomImagePointSeries.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FImagePoint:=TPicture.Create;
  FImagePoint.OnChange:=CanvasChanged;
  LoadBitmapFromResourceName(FImagePoint.Bitmap,'TeeDefaultImage');
  Pointer.ChangeHorizSize(FImagePoint.Width);
  Pointer.ChangeVertSize(FImagePoint.Height);
end;

{ overrided DrawValue to draw an Image for each point }
procedure TCustomImagePointSeries.DrawValue(ValueIndex:Longint);
var tmpX : Integer;
    tmpY : Integer;
    tmpW : Integer;
    tmpH : Integer;
    R    : TRect;
begin
  if FImagePoint.Graphic=nil then inherited DrawValue(ValueIndex)
  else
  With ParentChart,Canvas do
  begin
    { trigger the OnGetImage event if assigned... }
    if Assigned(FOnGetImage) then OnGetImage(Self,ValueIndex,FImagePoint);

    { draw the image... }
    tmpX:=CalcXPos(ValueIndex);
    tmpY:=CalcYPos(ValueIndex);

    tmpW:=Pointer.HorizSize div 2;
    tmpH:=Pointer.VertSize div 2;

    R:=Rect( tmpX-tmpW,tmpY-tmpH,tmpX+tmpW,tmpY+tmpH );
    With ParentChart.Canvas,R do
    begin
      TopLeft    :=Calculate3DPosition(Left,Top,StartZ);
      BottomRight:=Calculate3DPosition(Right,Bottom,StartZ);
      StretchDraw(R,FImagePoint.Graphic);
    end;
  end;
end;

procedure TCustomImagePointSeries.SetImagePoint(Value:TPicture);
begin
  FImagePoint.Assign(Value);   { <-- set new property values }
  if Value<>nil then
  begin
    if Value.Width>0 then Pointer.HorizSize:=Value.Width;
    if Value.Height>0 then Pointer.VertSize:=Value.Height;
  end;
end;

Destructor TCustomImagePointSeries.Destroy;
begin
  FImagePoint.Free;  { <-- remember to destroy private properties }
  inherited Destroy;
end;

Procedure TCustomImagePointSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  ParentChart.View3DOptions.Orthogonal:=True;
end;

{ TDeltaPointSeries }
Constructor TDeltaPointSeries.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FEqualImage:=TPicture.Create;
  FEqualImage.OnChange:=CanvasChanged;
  FGreaterImage:=TPicture.Create;
  FGreaterImage.OnChange:=CanvasChanged;
  FLowerImage:=TPicture.Create;
  FLowerImage.OnChange:=CanvasChanged;
  ImageStyle:=disSmiles;
end;

Destructor TDeltaPointSeries.Destroy;
begin
  FEqualImage.Free;
  FLowerImage.Free;
  FGreaterImage.Free;
  inherited Destroy;
end;

{ overrided DrawValue to draw an Image for each point }
procedure TDeltaPointSeries.DrawValue(ValueIndex:Longint);
Var tmp,tmpPrevious:Double;
begin
  if ValueIndex=0 then FImagePoint.Assign(FEqualImage)
  else
  begin
    tmpPrevious:=MandatoryValueList[ValueIndex-1];
    tmp:=MandatoryValueList[ValueIndex];
    if tmp>tmpPrevious then FImagePoint.Assign(FGreaterImage) else
    if tmp<tmpPrevious then FImagePoint.Assign(FLowerImage) else
                            FImagePoint.Assign(FEqualImage);
  end;
  if FImagePoint.Width>0 then Pointer.ChangeHorizSize(FImagePoint.Width);
  if FImagePoint.Height>0 then Pointer.ChangeVertSize(FImagePoint.Height);
  inherited DrawValue(ValueIndex);
end;

procedure TDeltaPointSeries.SetEqualImage(Value:TPicture);
begin
  FEqualImage.Assign(Value);
end;

procedure TDeltaPointSeries.SetGreaterImage(Value:TPicture);
begin
  FGreaterImage.Assign(Value);
end;

procedure TDeltaPointSeries.SetLowerImage(Value:TPicture);
begin
  FLowerImage.Assign(Value);
end;

procedure TDeltaPointSeries.SetImageStyle(Value:TDeltaImageStyle);
begin
  FImageStyle:=Value;
  if Value=disSmiles then
  begin
    LoadBitmapFromResourceName(FEqualImage.Bitmap,  'TeeEqualSmile');
    LoadBitmapFromResourceName(FGreaterImage.Bitmap,'TeeGreaterSmile');
    LoadBitmapFromResourceName(FLowerImage.Bitmap,  'TeeLowerSmile');
  end
  else
  begin
    LoadBitmapFromResourceName(FEqualImage.Bitmap,  'TeeEqualHand');
    LoadBitmapFromResourceName(FGreaterImage.Bitmap,'TeeGreaterHand');
    LoadBitmapFromResourceName(FLowerImage.Bitmap,  'TeeLowerHand');
  end;
end;

{ Series/Functions Registration/Un-Registration }
Procedure ImagePointExitProc; far;
begin
  UnRegisterTeeSeries([ TImagePointSeries,TDeltaPointSeries ]);
end;

initialization
  RegisterTeeSeries( TImagePointSeries, 'ImagePoint', TeeMsg_GallerySamples, 1 );
  RegisterTeeSeries( TDeltaPointSeries, 'DeltaPoint', TeeMsg_GallerySamples, 1 );
{$IFDEF D1}
  AddExitProc(ImagePointExitProc);
{$ELSE}
finalization
  ImagePointExitProc;
{$ENDIF}
end.

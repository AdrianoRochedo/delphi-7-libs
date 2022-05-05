unit RAImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TRAImage = class(TCustomControl)
  private
    FPicture: TPicture;
    FBorderStyle: TBorderStyle;
    FStretch: Boolean;
    FCenter: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default true;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Picture: TPicture read FPicture write SetPicture;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
 {$IFDEF RA_D4H}
  public
    property DockManager;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock; 
 {$ENDIF RA_D4H}
  end;

implementation

constructor TRAImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 105;
  Height := 105;
  TabStop := True;
  ParentColor := true;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBorderStyle := bsSingle;
  FCenter := True;
end;

destructor TRAImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TRAImage.GetPalette: HPALETTE;
begin
  if FPicture.Graphic is TBitmap then
    Result := TBitmap(FPicture.Graphic).Palette
  else Result := 0;
end;

procedure TRAImage.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRAImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TRAImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TRAImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TRAImage.Paint;
var
  H : Integer;
  R : TRect;
  DrawPict : TPicture;
begin
  with Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    DrawPict := TPicture.Create;
    H := 0;
    try
      DrawPict.Assign(FPicture);
      if Focused and (DrawPict.Graphic is TBitmap) and (DrawPict.Bitmap.Palette <> 0) then begin
        H := SelectPalette(Handle, DrawPict.Bitmap.Palette, False);
        RealizePalette(Handle);
      end;
      if Stretch then
        if (DrawPict.Graphic = nil) or DrawPict.Graphic.Empty then
          FillRect(ClientRect)
        else
          StretchDraw(ClientRect, DrawPict.Graphic)
      else begin
        SetRect(R, 0, 0, DrawPict.Width, DrawPict.Height);
        if Center then OffsetRect(R, (ClientWidth - DrawPict.Width) div 2,
          (ClientHeight - DrawPict.Height) div 2);
        StretchDraw(R, DrawPict.Graphic);
        ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
        FillRect(ClientRect);
        SelectClipRgn(Handle, 0);
      end;
    finally
      if H <> 0 then SelectPalette(Handle, H, True);
      DrawPict.Free;
    end;
    if (GetParentForm(Self).ActiveControl = Self) and
      not (csDesigning in ComponentState) then DrawFocusRect(ClientRect);
  end;
end;

procedure TRAImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TRAImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FBorderStyle = bsSingle then
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
end;

procedure TRAImage.CMEnter(var Message: TCMEnter);
begin
  Invalidate; { Draw the focus marker }
  inherited;
end;

procedure TRAImage.CMExit(var Message: TCMExit);
begin
  Invalidate; { Erase the focus marker }
  inherited;
end;

procedure TRAImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

end.

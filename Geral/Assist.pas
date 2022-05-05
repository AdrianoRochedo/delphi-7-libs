unit Assist;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  THintDirection = (hdUpRight, hdUpLeft, hdDownRight, hdDownLeft);

  TAssistente = class(TForm)
    Timer: TTimer;
    Title: TLabel;
    Sep1: TBevel;
    Edit: TEdit;
    Sep2: TBevel;
    btnOk: TButton;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FHintDirection:      THintDirection;
    B:                   TBitmap;  {Fundo da janela}
    FTextDY:             Integer;  {Separação do texto na vertical}            
    procedure Desenha;
    procedure DesenhaBalao;
    procedure CheckUpLeft(Spot: TPoint);
    procedure CheckUpRight(Spot: TPoint);
    procedure CheckDownRight(Spot: TPoint);
    procedure CheckDownLeft(Spot: TPoint);
    procedure SelectProperHintDirection(ARect: TRect);
    procedure Paint; override;
    Function  GetText: TStrings;
    Procedure SetText(SL: TStrings);
    Procedure DrawText(Canvas: TCanvas);
    Procedure GetTextLenght(SL: TStrings; Var Size: TSize; Var Dy: Integer);
  public
    Lines        : TStrings;
    UseCursorPos : Boolean;
    ShowEditBox  : Boolean;
    Procedure Active(L, T: Integer);
    Procedure ShowText;
    Property Text : TStrings Read GetText Write SetText;
  end;

  Var Assistente: TAssistente;

implementation
uses SysUtilsEx;

{$R *.DFM}

const
   SHADOW_WIDTH   =  6;
   N_PIXELS       =  5;
   TopText        =  34;
var
   UpRect, DownRect:     TRect;
   TextRect:             TRect;
   SelectHintDirection:  THintDirection;
   ShowPos:              TPoint;
   ScreenDC:             HDC;
   AddNum:               Integer;

Function  TAssistente.GetText: TStrings;
Begin
  Result := Lines;
End;

Procedure TAssistente.DrawText(Canvas: TCanvas);
var Dy, i: Integer;
Begin
  Dy := TextRect.Top;
  For i := 0 to Lines.Count-1 do
    Begin
    Canvas.TextOut(TextRect.Left, Dy, Lines[i]);
    Inc(Dy, FTextDy);
    End;
End;

Procedure TAssistente.SetText(SL: TStrings);
Begin
  Lines.Assign(SL);
  ShowText;
End;

Procedure TAssistente.ShowText;
Begin
  Canvas.FillRect(TextRect);
  DrawText(Canvas);
End;

procedure TAssistente.Paint;
begin
  inherited Paint;
{  If Visible Then
     FormShow(application);}
End;

procedure TAssistente.Desenha;
var CR: TRect;
begin
  {B.Canvas.Brush.Color := Application.HintColor;}
  DesenhaBalao;
end;

procedure TAssistente.DesenhaBalao;
var
  R: TRect;
  CCaption: array[0..255] of Char;
  FillRegion,ShadowRgn: HRgn;
  AP: array[0..2] of TPoint; { Pontos da seta }
  SP: array[0..2] of TPoint; { Pontos da sombra }
  X,Y, i, Dy: Integer;
begin
  case FHintDirection of
     hdUpRight:   begin
                  AP[0] := Point(10, Height - 15);
                  AP[1] := Point(20, Height - 15);
                  AP[2] := Point(0 , Height);
                  SP[0] := Point(12, Height - 15);
                  SP[1] := Point(25, Height - 15);
                  SP[2] := Point(12, Height);
                  end;

     hdUpLeft:   begin
                 AP[0] := Point(Width -SHADOW_WIDTH -20, Height - 15);
                 AP[1] := Point(Width -SHADOW_WIDTH -10, Height - 15);
                 AP[2] := Point(Width -SHADOW_WIDTH    , Height     );
                 SP[0] := Point(Width -SHADOW_WIDTH -27, Height - 15);
                 SP[1] := Point(Width -SHADOW_WIDTH -5 , Height - 15);
                 SP[2] := Point(Width -SHADOW_WIDTH    , Height     );
                 end;

     hdDownRight:   begin
                    AP[0] := Point(10, 15);
                    AP[1] := Point(20, 15);
                    AP[2] := Point(0 ,  0);

                    { for hdDownXXX, SP not used now }
                    SP[0] := Point(12, Height - 15);
                    SP[1] := Point(25, Height - 15);
                    SP[2] := Point(12, Height     );
                    end;

     hdDownLeft:   begin
                   AP[0] := Point(Width -SHADOW_WIDTH - 20 ,15);
                   AP[1] := Point(Width -SHADOW_WIDTH - 10 ,15);
                   AP[2] := Point(Width -SHADOW_WIDTH      , 0);

                   { for hdDownXXX, SP not used now }
                   SP[0] := Point(12, Height - 15);
                   SP[1] := Point(25, Height - 15);
                   SP[2] := Point(12, Height     );
                   end;
  end;

  { Draw Shadow of the Hint Rect}
  if (FHintDirection <= hdUpLeft) then
     begin
     ShadowRgn := CreateRoundRectRgn(10, 8, Width, Height - 9, 8, 8);

     { 8 is for RoundRect's corner }
     for X := Width - SHADOW_WIDTH - 8 to Width do
        for Y := 8 to Height-14 do
           if (Odd(X) = Odd(Y)) and PtInRegion(ShadowRgn, X, Y) then
              B.Canvas.Pixels[X,Y] := clBlack;

     for X := 10 to Width do
        for Y := Height - 14 to Height - 9 do
           if (Odd(X) = Odd(Y)) and PtInRegion(ShadowRgn,X,Y) then
               B.Canvas.Pixels[X,Y] := clBlack;
     end
  else           { for hdDownXXX }
     begin
     ShadowRgn := CreateRoundRectRgn(10, 23, Width, Height - 2, 8, 8);
     for X := Width - SHADOW_WIDTH - 8 to Width do
        for Y := 23 to Height - 8 do
            if (Odd(X) = Odd(Y)) and PtInRegion(ShadowRgn, X, Y) then
                B.Canvas.Pixels[X,Y] := clBlack;

     for X := 10 to Width do
        for Y := Height - 8 to Height - 2 do
            if (Odd(X) = Odd(Y)) and PtInRegion(ShadowRgn, X, Y) then
               B.Canvas.Pixels[X,Y] := clBlack;
     end;

  DeleteObject(ShadowRgn);

  { Desenha a sombra da seta }
  if (FHintDirection <= hdUpLeft) then
     begin
     ShadowRgn := CreatePolygonRgn(SP, 3, WINDING);
     for X := SP[0].X to SP[1].X do
        for Y := SP[0].Y to SP[2].Y do
           if (Odd(X) = Odd(Y)) and PtInRegion(ShadowRgn, X, Y) then
              B.Canvas.Pixels[X,Y] := clBlack;
     DeleteObject(ShadowRgn);
     end;

  { Draw HintRect }
  B.Canvas.Pen.Color := clBlack;
  B.Canvas.Pen.Style := psSolid;
  B.Canvas.Brush.Color := $00A2FFFF; {Amarelo claro}
  Canvas.Brush.Color := $00A2FFFF;

  B.Canvas.Brush.Style := bsSolid;
  if (FHintDirection <= hdUpLeft) then
     B.Canvas.RoundRect(0, 0, Width-SHADOW_WIDTH,Height-14, 9, 9)
  else
     B.Canvas.RoundRect(0, AddNum , Width-SHADOW_WIDTH, Height-14+6, 9, 9);

  { Desenha a seta }
  B.Canvas.Pen.Color := Application.HintColor;
  B.Canvas.MoveTo(AP[0].X, AP[0].Y);
  B.Canvas.LineTo(AP[1].X, AP[1].Y);
  B.Canvas.Pen.Color := clBlack;
  FillRegion := CreatePolygonRgn(AP, 3, WINDING);
  FillRgn(B.Canvas.Handle, FillRegion, B.Canvas.Brush.Handle);
  DeleteObject(FillRegion);
  B.Canvas.LineTo(AP[2].X,AP[2].Y);
  B.Canvas.LineTo(AP[0].X,AP[0].Y);

  { SetBkMode faz o desenho do texte ser transparente }
  SetBkMode(B.Canvas.Handle, TRANSPARENT);
  B.Canvas.Font.Name := 'MS Sans Serif';
  B.Canvas.Font.Size := 10;
  Canvas.Font.Assign(B.Canvas.Font);

  DrawText(B.Canvas);

  {...}
end;

Procedure TAssistente.GetTextLenght(SL: TStrings; Var Size: TSize; Var Dy: Integer);
var i, Tam: Integer;
Begin
  Size.cx := 0; Size.cy := 0;
  Dy := Canvas.TextHeight(SL[0]);
  {Inc(Dy, Dy div 2);}
  For i := 0 to SL.Count-1 do
    Begin
    Tam := Canvas.TextWidth(SL[i]);
    If Tam > Size.cx Then Size.cx := Tam;
    Inc(Size.cy, Dy);
    End;
End;

Procedure TAssistente.Active(L, T: Integer);
var tmpWidth, tmpHeight: Integer;
    Rect: TRect;
    Size: TSize;
    p: PChar;
begin
   Hide;
   GetTextLenght(Lines, Size, FTextDY);
   TimerTimer(nil);
   FHintDirection := hdUpRight;

   B.Free;
   B := TBitMap.Create; {Fundo da janela}

   Rect.Left   := L;
   Rect.Top    := T;

   If Title.Width > Size.cx Then Size.cx := Title.Width;
   Sep1.Width := Size.cx;
   Edit.Width := Size.cx;

   Rect.Right  := L + Size.cx + 8;
   Rect.Bottom := T + TopText + Size.cy;

   Left := Rect.Left;
   Top  := Rect.Top;

   SelectProperHintDirection(Rect);
   FHintDirection := SelectHintDirection;

   Inc(Rect.Right, 10 + SHADOW_WIDTH);
   Inc(Rect.Bottom, 20);
   if (FHintDirection >= hdDownRight) then Inc(Rect.Bottom, 9);

   If ShowEditBox Then
      Inc(Rect.Bottom, Edit.Height + btnOk.Height + 24);

   { para expandir o retangulo }
   tmpWidth    := Rect.Right - Rect.Left;
   tmpHeight   := Rect.Bottom - Rect.Top;
   Rect.Left   := ShowPos.X;
   Rect.Top    := ShowPos.Y;
   Rect.Right  := Rect.Left + tmpWidth;
   Rect.Bottom := Rect.Top + tmpHeight;
   BoundsRect  := Rect;

   B.Width  := tmpWidth;
   B.Height := tmpHeight;

  { R é para a saída do texto }
  TextRect := ClientRect;
  Inc(TextRect.Left  , 8);
  Inc(TextRect.Top   , TopText);
  TextRect.Right  := TextRect.Left + Size.cx;
  TextRect.Bottom := TextRect.Top + Size.cy;

  if FHintDirection >= hdDownRight then AddNum := 15 Else AddNum := 0;

  if (FHintDirection = hdUpRight) or  (FHintDirection = hdUpLeft) then
     Dec(TextRect.Bottom, 8);

  Inc(TextRect.Top, AddNum);

  {Posicionamento dos componentes}
  Sep1.Top := TextRect.Top - 6;
  Title.Top := Sep1.Top - Title.Height - 4;

  If ShowEditBox Then
    Begin
    Edit.Top := TextRect.Bottom + 10;
    if (FHintDirection < hdDownRight) then Edit.Top := Edit.Top + 2
                                      else Edit.Top := Edit.Top + 10;
    Sep2.Top := Edit.Top + Edit.Height + 8;
    btnOk.Top := Sep2.Top + 8;
    Sep2.Width := Sep1.Width;
    Edit.Visible := True;
    btnOk.Visible := True;
    Sep2.Visible := True;
    End;

  Inc(TextRect.Bottom, AddNum);
  if (FHintDirection < hdDownRight) then Inc(TextRect.Bottom, 5);
  Show;
end;

procedure TAssistente.FormShow(Sender: TObject);
begin
  SetActiveWindow(Handle);
  Timer.Enabled := True;

  {Copia o fundo da tela que está sob a janela}
  ScreenDC := GetDC(0);
  BitBlt(B.Canvas.Handle, 0,0,Width, Height,
         ScreenDC, Left, top, SRCCOPY);

  SetWindowPos(Handle, HWND_TOPMOST, left, Top, 0,0,
               SWP_SHOWWINDOW or SWP_NOSIZE);


  {Todo desenho subsequente é feito sobre o fundo recortado}
  Desenha;

  {Copia o desenho para o canvas da janela}
  BitBlt(Canvas.Handle, 0, 0, Width,Height, B.Canvas.Handle, 0, 0, SRCCOPY);
  ReleaseDC(0, ScreenDC);
End;

procedure TAssistente.CheckUpLeft(Spot: TPoint);
var
   Width,Height: Integer;
begin
   Dec(Spot.Y,N_PIXELS);
   Width:=UpRect.Right-UpRect.Left;
   Height:=UpRect.Bottom-UpRect.Top;
   SelectHintDirection:=hdUpLeft;
   if (Spot.X+SHADOW_WIDTH-Width)<0 then
      begin
         Inc(Spot.Y,N_PIXELS);{back tp original}
         CheckUpRight(Spot);
         Exit;
      end;
   if (Spot.Y-Height)<0 then
      begin
         Inc(Spot.Y,N_PIXELS);
         CheckDownLeft(Spot);
         Exit;
      end;
   ShowPos.X:=Spot.X+SHADOW_WIDTH-Width;
   ShowPos.Y:=Spot.Y-Height;
end;

procedure TAssistente.CheckUpRight(Spot: TPoint);
var Width, Height: Integer;
begin
   Dec(Spot.Y,N_PIXELS);
   Width:=UpRect.Right-UpRect.Left;
   Height:=UpRect.Bottom-UpRect.Top;
   SelectHintDirection:=hdUpRight;
   if (Spot.X+Width)>Screen.Width then
      begin
         Inc(Spot.Y,N_PIXELS);
         CheckUpLeft(Spot);
         Exit;
      end;
   if (Spot.Y-Height)<0 then
      begin
         Inc(Spot.Y,N_PIXELS);
         CheckDownRight(Spot);
         Exit;
      end;
   ShowPos.X := Spot.X;
   ShowPos.Y := Spot.Y-Height;
end;

procedure TAssistente.CheckDownRight(Spot: TPoint);
var Width, Height: Integer;
begin
   Inc(Spot.Y,N_PIXELS*3);
   Width:=DownRect.Right-DownRect.Left;
   Height:=DownRect.Bottom-DownRect.Top;
   SelectHintDirection:=hdDownRight;
   if (Spot.X+Width)>Screen.Width then
      begin
         Dec(Spot.Y,N_PIXELS*3);
         CheckDownLeft(Spot);
         Exit;
      end;
   if (Spot.Y+Height)>Screen.Height then
      begin
         Dec(Spot.Y,N_PIXELS*3);
         CheckUpRight(Spot);
         Exit;
      end;
   ShowPos.X:=Spot.X;
   ShowPos.Y:=Spot.Y;
end;

procedure TAssistente.CheckDownLeft(Spot:TPoint);
var Width,Height:Integer;
begin
   Inc(Spot.Y,N_PIXELS*3);
   Width:=DownRect.Right-DownRect.Left;
   Height:=DownRect.Bottom-DownRect.Top;
   SelectHintDirection:=hdDownLeft;
   if (Spot.X+SHADOW_WIDTH-Width)<0 then
      begin
         Dec(Spot.Y,N_PIXELS*3);
         CheckDownRight(Spot);
         Exit;
      end;
   if (Spot.Y+Height)>Screen.Height then
      begin
         Dec(Spot.Y,N_PIXELS*3);
         CheckUpLeft(Spot);
         Exit;
      end;
   ShowPos.X:=Spot.X+SHADOW_WIDTH-Width;
   ShowPos.Y:=Spot.Y;
end;

procedure TAssistente.SelectProperHintDirection(ARect: TRect);
var
   Spot:TPoint;
   OldHintDirection,SendHintDirection:THintDirection;
   HintControl:TControl;
begin
   If UseCursorPos Then
      GetCursorPos(Spot)
   Else
      Begin
      Spot.x := Left;
      Spot.y := Top;
      End;

   HintCOntrol:=FindDragTarget(Spot,True);
   Inc(ARect.Right,10+SHADOW_WIDTH);
   Inc(ARect.Bottom,20);
   UpRect:=ARect;
   Inc(ARect.Bottom,9);
   DownRect:=ARect;

   case FHintDirection of
      hdUpRight:    CheckUpRight   (Spot);
      hdUpLeft:     CheckUpLeft    (Spot);
      hdDownRight:  CheckDownRight (Spot);
      hdDownLeft:   CheckDownLeft  (Spot);
      end;
end;


procedure TAssistente.TimerTimer(Sender: TObject);
var H: HWnd;
begin
  H := GetActiveWindow;
  If (H = 0) or (H <> Handle) Then Hide;
end;

procedure TAssistente.FormDestroy(Sender: TObject);
begin
  If Assigned(B) Then B.Free;
  Lines.Free;
end;

procedure TAssistente.FormCreate(Sender: TObject);
begin
  UseCursorPos := False;
  ShowEditBox  := False;
  Lines := TStringList.Create;
end;

procedure TAssistente.btnOkClick(Sender: TObject);
begin
  Close;
end;

procedure TAssistente.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := False;
end;

Initialization
  Assistente := TAssistente.Create(Application);

end.

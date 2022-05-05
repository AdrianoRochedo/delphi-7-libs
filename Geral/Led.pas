unit Led;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TProf = (tpRaised, tpLowered, tpNone);
  TPos  = (tpHorizontal, tpVertical);

  TLed = class(TGraphicControl)
  private
    Acesas      : LongInt;
    Baixo       : LongInt;
    Medio       : LongInt;
    Alto        : LongInt;
    FWidthBar   : Byte;     {Largura de uma barra}
    FBevelOuter : TProf;    {Tipo de profundidade}
    FValue      : LongInt;  {Porcentagem de leds acesos}
    FNLeds      : Integer;  {Número de barras}
    FSpaceBar   : Byte;     {Espaço entre as barras}
    FAuto       : Boolean;  {Número de barras automáticas se for o caso}
    FKind       : TPos;     {Horizontal ou vertical}
    FMax        : LongInt;
    SizeW       : Integer;
    SizeH       : Integer;

    Procedure WMSize      (var Messagem : TWMSize); message WM_Size;
    Procedure SetValue    (Valor : LongInt);
    Procedure SetWidthBar (Valor : Byte);
    Procedure SetBevel    (Value : TProf);
    Procedure SetNLeds    (Value : Integer);
    Procedure SetSpaceBar (Value : Byte);
    Procedure SetMax      (Value : LongInt);
    Procedure FSetKind    (Value : TPos);
    {Procedure SetMin     (Value : Integer);}
  protected
    Procedure Paint ;  Override;
    Procedure DesenhaLedsHoriz(Baixo, Medio, Alto : Integer);
    Procedure DesenhaLedsVert (Baixo, Medio, Alto : Integer);
    Procedure Atualiza;
  public
    Constructor Create(MeuComp : TComponent); Override;
  published
    Property Align;
    Property Color;
    Property Enabled;
    Property ParentColor;
    Property ParentShowHint;
    Property ShowHint;
    Property Visible;
    Property WidthBar   : Byte    Read FWidthBar   Write SetWidthBar Default 3;
    Property Value      : LongInt Read FValue      Write SetValue Default 65;
    Property BevelOuter : TProf   Read FBevelOuter Write SetBevel Default tpLowered;
    Property Max        : LongInt Read FMax        Write SetMax Default 100;
    Property NLeds      : Integer Read FNLeds      Write SetNLeds;
    Property SpaceBar   : Byte    Read FSpaceBar   Write SetSpaceBar Default 2;
    Property Auto       : Boolean Read FAuto       Write FAuto Default True;
    Property Kind       : TPos    Read FKind       Write FSetKind Default tpHorizontal;

    Property DragCursor;
    Property DragMode;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
 end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AD-2', [TLed]);
end;

Constructor TLed.Create(MeuComp : TComponent);
Begin
  Inherited Create(MeuComp);
  Width       := 104;
  Height      := 20;
  FMax        := 100;
  FValue      := 65;
  FWidthBar   := 3;
  FSpaceBar   := 2;
  SizeW       := -1;
  SizeH       := -1;
  FAuto       := True;
  FKind       := tpHorizontal;
  Color       := clBlack;
  FBevelOuter := tpLowered;
  Atualiza;
  Acesas := Trunc(FValue * FNLeds / FMax);
End;

Procedure TLed.DesenhaLedsHoriz(Baixo, Medio, Alto : Integer);
Var i : Integer;
    Inicio : Integer;

Label Apagadas;

Begin
  Inicio := 3;
  With Canvas Do Begin

      Brush.Color := clRed;
      For i := 1 to Baixo do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(Inicio, 4, Inicio+FWidthBar, Height-4));
        Inc(Inicio, FWidthBar + FSpaceBar);
      End;

      Brush.Color := clYellow;
      For i := Baixo+1 to Medio do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(Inicio, 4, Inicio+FWidthBar, Height-4));
        Inc(Inicio, FWidthBar + FSpaceBar);
      End;

      Brush.Color := clLime;
      For i := Medio+1 to Alto do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(Inicio, 4, Inicio+FWidthBar, Height-4));
        Inc(Inicio, FWidthBar + FSpaceBar);
      End;

    {Desenha leds apagados se existem}
    Apagadas :
       Brush.Color := clMaroon;
       For i := Acesas+1 to Alto do Begin
         FillRect(Rect(Inicio, 4, Inicio+FWidthBar, Height-4));
         Inc(Inicio, FWidthBar + FSpaceBar);
       End;

  End;
End;

Procedure TLed.DesenhaLedsVert(Baixo, Medio, Alto : Integer);
Var i : Integer;
    Inicio : Integer;

Label Apagadas;

Begin
  Inicio := Height - 3;
  With Canvas Do Begin

      Brush.Color := clRed;
      For i := 1 to Baixo do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(4, Inicio, Width-4, Inicio-FWidthBar));
        Dec(Inicio, FWidthBar + FSpaceBar);
      End;

      Brush.Color := clYellow;
      For i := Baixo+1 to Medio do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(4, Inicio, Width-4, Inicio-FWidthBar));
        Dec(Inicio, FWidthBar + FSpaceBar);
      End;

      Brush.Color := clLime;
      For i := Medio+1 to Alto do Begin
        If i > Acesas Then Goto Apagadas;
        FillRect(Rect(4, Inicio, Width-4, Inicio-FWidthBar));
        Dec(Inicio, FWidthBar + FSpaceBar);
      End;

    {Desenha leds apagados se existem}
    Apagadas :
       Brush.Color := clMaroon;
       For i := Acesas+1 to Alto do Begin
         FillRect(Rect(4, Inicio, Width-4, Inicio-FWidthBar));
         Dec(Inicio, FWidthBar + FSpaceBar);
       End;

  End;
End;

Procedure TLed.Paint;
var  R: TRect;
Begin
  with Canvas do Begin
    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(Rect(2,2,Width-2, Height-2));
    R := Rect(0,0,Width-1,Height-1);
    With R Do Begin
      Case FBevelOuter of
        tpRaised  :
          Begin
            Pen.Color := clBtnHighLight;
            MoveTo(Left,Bottom);
            LineTo(Left,Top);
            LineTo(Right,Top);

            Pen.Color := clBtnShadow;
            MoveTo(Right,Top);
            LineTo(Right,Bottom);
            LineTo(Left,Bottom);
          End;
        tpLowered :
          Begin
            Pen.Color := clBtnShadow;
            MoveTo(Left ,Bottom);
            LineTo(Left ,Top);
            LineTo(Right,Top);

            Pen.Color := clBtnHighLight;
            MoveTo(Right,Top);
            LineTo(Right,Bottom);
            LineTo(Left ,Bottom);
          End;
        tpNone    :
          Begin
            {Não faz nada, entendeu!}
          End;
      End; {Case}

    End; {With R}
  End; {With Canvas}

  {Simula uma mensagem WM_SIZE}
  If (SizeW <> Width) or (SizeH <> Height)Then Begin
    SizeW := Width;
    SizeH := Height;
    Atualiza;
  End;


  If FKind = tpHorizontal Then
    DesenhaLedsHoriz(Baixo,Medio+Baixo,Alto+Medio+Baixo)
  Else
    DesenhaLedsVert(Baixo,Medio+Baixo,Alto+Medio+Baixo);

End;

Procedure TLed.Atualiza;
var AreaUtil : Integer;
Begin
  {Atualiza valores}
  If FKind = tpHorizontal Then
    AreaUtil := Width - 2
  Else
    AreaUtil := Height - 2;

  If FAuto Then
    FNLeds := Trunc(AreaUtil / (FWidthBar + FSpaceBar))
  Else
    FWidthBar := Trunc(AreaUtil / FNLeds - FSpaceBar);

  Baixo  := Trunc(FNLeds * 0.2); {20%}
  Medio  := Trunc(FNLeds * 0.3); {30%}
  Alto   := FNLeds - Baixo - Medio; {45% para mais}
End;

Procedure TLed.WMSize(var Messagem : TWMSize);
Begin
  MessageBeep(0);
  Inherited;
End;

{
Procedure TLed.WMMouseMove(var Messagem : TWMMouseMove);
Begin
  If MouseCaptured Then Begin
  End;
End;
}

Procedure TLed.SetValue(Valor : LongInt);
Begin
  If (Valor <= FMax) and (Valor <> FValue) Then Begin
    FValue := Valor;
    {Cálculo do número de leds acesos}
    {Cuidado com a ordem das contas, senão dará estouro de inteiro}
    Acesas := Trunc(FNLeds * ( FValue  / FMax ));

    If FKind = tpHorizontal Then
      DesenhaLedsHoriz(Baixo,Medio+Baixo,Alto+Medio+Baixo)
    Else
      DesenhaLedsVert(Baixo,Medio+Baixo,Alto+Medio+Baixo);

    {RePaint;}
  End;
End;

Procedure TLed.SetWidthBar(Valor : Byte);
Begin
  If Valor <> FWidthBar Then Begin
    FWidthBar := Valor;
    Atualiza;
    RePaint;
  End;
End;

Procedure TLed.SetBevel(Value : TProf);
Begin
  If Value <> FBevelOuter Then Begin
    FBevelOuter := Value;
    RePaint;
  End;
End;

Procedure TLed.SetNLeds(Value : Integer);
Begin
  If (Value <> FNLeds) and (Value >= 5) Then Begin
    FNLeds := Value;
    Atualiza;
    RePaint;
  End;
End;

Procedure TLed.SetSpaceBar (Value : Byte);
Begin
  If Value <> FSpaceBar Then Begin
    FSpaceBar := Value;
    Atualiza;
    RePaint;
  End;
End;

{
Procedure TLed.SetMin(Value : Integer);
Begin
  If Value <> FMin Then Begin
    FMin := Value;
    RePaint;
  End;
End;
}

Procedure TLed.SetMax(Value : LongInt);
Begin
  If Value <> FMax Then Begin
    FMax := Value;
    Atualiza;
    RePaint;
  End;
End;

Procedure TLed.FSetKind(Value : TPos);
var Aux : Integer;
Begin
  If Value <> FKind Then Begin
    FKind := Value;

    {Troca Larguara por Altura}
    If (csDesigning in ComponentState) Then Begin
      Aux    := Width;
      Width  := Height;
      Height := Aux;
    End;

    Atualiza;
    ReFresh;
  End;
End;

end.

unit GraphicUtils;

interface
uses Windows, Classes, Graphics, IniFiles, jPeg, SysUtilsEx,
     {$ifdef MSXML}
     MSXML4,
     XML_Utils,
     {$endif}
     ExtCTRLs;


{$ifdef MSXML}
  type TXML_Font = class(TFont)
  public
    procedure ToXML(Buffer: TStrings; Ident: Integer); overload;
    procedure ToXML(const tagName: string; Buffer: TStrings; Ident: Integer); overload;

    procedure FromXML(node: IXMLDomNode); overload;
    procedure FromXML(nodeList: IXMLDomNodeList); overload;
  end;
{$endif}


  // Escolhe uma cor dependendo do indice entrado
  // A partir de um certo indice as cores sao rotativas
  Function SelectColor(i: Word; BlackWhite: Boolean = False): TColor;

{ Rotina para, a partir de uma reta r formada por dois pontos P1 e P2, se construa um
quadrado com centro em P2, e que se considere um novo sistema xoy de coordenadas, conforme
documentado no papel,Achando- se, neste novo sistema, as coordenadas dos vértices de um
triângulo A1 A2 A3 , no qual A1 jaz entre P1 e P2, pela intersecção da reta r com uma dada
circunferência de raio l/2 contida no quadrado de TamLado l que fora construído.}

{entrada: TamLado, P1(xp1,yp1) e P2(xp2,yp2)
  saída: A1(xa1,ya1), A2(xa2,ya2) e A3(xa3,ya3)  }

   { |----------------------------------------------------------->x
     |(0,0)
     |
     |
     |              K1(0,0).---------.-----------.K2(TamLado,0) _____ x'
     |                     |        A2          |
     |                     |                    |
     |                     |                    |
     |                     |                    |
     |                     |         .          |
     |                     |      P2(xp2,yp2)   |
     |                     .A1                . |
     |                     |                 A3 |
     |              .      |                    |
     |        P1(xp1,yp1)  .--------------------.
     |                     K3(0,TamLado)            K4(TamLado,TamLado)
      y                    |
                           |
                           y'
}

  type
    TTriangle = record
                  P1, P2, P3: TPoint;
                end;

  Function  ObtemTriangulo(TamLado: Integer; P1, P2: TPoint): TTriangle;
  Function  DistanciaDeUmPonto_aUmaReta(Ponto, PR1, PR2: TPoint): Integer;
  Function  CentroDaReta(const P1, P2: TPoint): TPoint;
  Function  DistanciaEntre2Pontos(P1, P2: TPoint): Integer;
  function  ObterPontoNaReta(P1, P2: TPoint; Dist_P2: integer): TPoint;

  // Desenha uma seta entre dois pontos
  // Tam eh o tamanho de cada haste do simbolo que representa a seta --->---
  // Dist_P2 representa a distancia do simbolo do segundo ponto
  procedure DesenharSeta(Canvas: TCanvas;
                         P1, P2: TPoint;
                         Tam: Integer = 10;
                         Dist_P2: Integer = 0);

  // Salva e recupera um bitmap em um arquivo ini
  procedure SalvarBitmapEmArquivo(Bitmap: TBitmap; Arquivo: TCustomIniFile; const Secao: String);
  function  LerBitmapDoArquivo(Arquivo: TCustomIniFile; const Secao: String): TBitmap;

  // Converte uma cor para uma string Hexadecimal no formato RGB
  function GetRGBColorAsString(Color: TColor): String;

  // obtem as propriedades da font como string
  function GetFontAsString(Font: TFont): String;

  // Le uma imagem jpeg de um recurso
  procedure LoadImage(const jpgID: string; AnImage: TImage);

implementation
uses stSTRs, SysUtils;

Function SelectColor(i: Word; BlackWhite: Boolean = False): TColor;
var xx: Integer;
begin
  // Se "BlackWhite = true" entao a as cores branca e preta poderao ser escolhidas
  if BlackWhite then xx := 14 else xx := 12;

  // Se xx = 12 a variacao sera de 0 a 11
  // Se xx = 14 a variacao sera de 0 a 13
  // Nao importando o valor de i
  case (i mod xx) of
    00: Result := clRed;
    01: Result := clGreen;
    02: Result := clBlue;
    03: Result := clYellow;
    04: Result := clAqua;
    05: Result := clTeal; 
    06: Result := clFuchsia;
    07: Result := clMaroon;
    08: Result := clNavy;
    09: Result := clPurple;
    10: Result := clOlive;
    11: Result := clLime;
    12: Result := clBlack;
    13: Result := clWhite;
    end;
end;

Function ObtemTriangulo(TamLado: Integer; P1, P2: TPoint): TTriangle;
var
  xa1,ya1,xa2,ya2,xa3,ya3,xp1,yp1,xp2,yp2,alfa,beta,gama,theta1,theta2,
  theta3,v1,v2,u1,u2,d1,d2,k1,k2,k3,m1,m2,t1,t2,t3,xk1,yk1,a2i,a2j,a3i,a3j,
  b2i,b2j,b3i,b3j,x1,y1, x2, y2:real;

  procedure AtribuiResultado;
  begin
    Result.P1 := Point(Round(xa1), Round(ya1));
    Result.P2 := Point(Round(xa2), Round(ya2));
    Result.P3 := Point(Round(xa3), Round(ya3));
  end;

begin
  xp1 := P1.x; yp1 := P1.y;
  xp2 := P2.x; yp2 := P2.y;

  xk1:=(2*xp2-TamLado)/2 ; yk1:=(2*yp2-TamLado)/2 ;{coordenadas xk1 eyk1 no sistema antigo}

  alfa:=yp1-yp2;
  beta:=xp2-xp1;

  if alfa=0 then
     begin
     xa1:=0; ya1:=TamLado/2;
     xa2:=(3*TamLado)/2; ya2:=(TamLado*(2-sqrt(3)))/4;
     xa3:=(3*TamLado)/2; ya3:=(TamLado*(2+sqrt(3)))/4;
     AtribuiResultado;
     exit;
     end; {fim da execução}

  if beta=0 then
     begin
     xa1:=TamLado/2; ya1:=0;
     xa2:=(TamLado*(2+sqrt(3)))/4; ya2:=(3*TamLado)/2;
     xa3:=(TamLado*(2-sqrt(3)))/4; ya3:=(3*TamLado)/2;
     AtribuiResultado;
     exit;
     end; {fim da execução}

  if ((alfa<>0)and(beta<>0))then
     begin
     gama:=(xp1-xk1)*(yp2-yk1)-(xp2-xk1)*(yp1-yk1);
     theta1:=4*(sqr(alfa) + sqr(beta));
     theta2:=4*(2*beta*gama+alfa*TamLado*beta-sqr(alfa)*TamLado);
     theta3:=4*sqr(gama) + 4*alfa*TamLado*gama + sqr(alfa)*sqr(TamLado);

     v1:=(-theta2+sqrt(sqr(theta2) - 4*theta1*theta3)) / (2*theta1);
     v2:=(-theta2-sqrt(sqr(theta2) - 4*theta1*theta3)) / (2*theta1);

     u1:=(-beta*v1-gama)/alfa;
     u2:=(-beta*v2-gama)/alfa;

     x1:=x1-xk1 ; y1:=y1-yk1;
     d1:=sqrt(sqr(x1-u1) + sqr(y1-v1));
     d2:=sqrt(sqr(x2-u2) + sqr(y2-v2));

     if (d1>d2) then
       begin
       xa1:=u2;
       ya1:=v2;
       end
     else
       begin
       xa1:=u1;
       ya1:=v2;
       end;                 {com isso acham-se as coordenadas de A1}

     m1:=(4*alfa*beta+sqrt(3)* (sqr(alfa) + sqr(beta)))/(sqr(alfa)-3*sqr(beta));

     k1:=1+m1*m1;
     k2:=2*m1*(ya1-m1*xa1)-TamLado*(1+m1);
     k3:=sqr(ya1-m1*xa1) - TamLado*(ya1-m1*xa1) + sqr(TamLado)/4;

     a2i:=(-k2+sqrt(sqr(k2) - 4*k1*k3))/(2*k1);
     a2j:=(-k2-sqrt(sqr(k2) - 4*k1*k3))/(2*k1);

     b2i:=m1*a2i+ya1-m1*xa1;
     b2j:=m1*a2j+ya1-m1*xa1;

     d1:=sqrt(sqr(a2i-xa1) + sqr(b2i-ya1));
     d2:=sqrt(sqr(a2j-xa1) + sqr(b2j-ya1));

     if (d1>d2) then
       begin
       xa2:=a2i;
       ya2:=b2i;
       end
     else
       begin
       xa2:=a2j;
       ya2:=b2j;
       end;             {com isso acham-se as coordenadas de A2}

     m2:=(4*alfa*beta+sqrt(3)*(sqr(alfa) + sqr(beta)))/(3*sqr(beta)-sqr(alfa));

     t1:=1+ sqr(m2);
     t2:=2*m2*(ya1-m2*xa1)-TamLado*(1+m2);
     t3:=(ya1-m2*xa1)*(ya1-m2*xa1)-TamLado*(ya1-m2*xa1)+(TamLado*TamLado)/4;

     a3i:=(-t2+sqrt(sqr(t2)-4*t1*t3))/(2*t1);
     a3j:=(-t2-sqrt(sqr(t2)-4*t1*t3))/(2*t1);

     b3i:=m2*a3i+ya1-m2*xa1;
     b3j:=m2*a3j+ya1-m2*xa1;


    d1:=sqrt(sqr(a3i-xa1) + sqr(b3i-ya1));
    d2:=sqrt(sqr(a3j-xa1) + sqr(b3j-ya1));

    if d1>d2 then
      begin
      xa3:=a3i;
      ya3:=b3i;
      end
    else
      begin
      xa3:=a3j;
      ya3:=b3j;
      end;
    end;                  {com isso acham-se as coordenadas de A3}

  AtribuiResultado;
end;

Function DistanciaEntre2Pontos(P1, P2: TPoint): Integer;
begin
  Result := Trunc(sqrt(sqr(p1.x - p2.x) + sqr(p1.y - p2.y)));
end;

Function DistanciaDeUmPonto_aUmaReta(Ponto, PR1, PR2: TPoint): Integer;
var a, b, c: Integer;
begin
  a := PR1.Y - PR2.Y;
  b := PR2.X - PR1.X;
  c := PR1.X * PR2.Y - PR2.X * PR1.Y;
  Result := ABS(Trunc((a*Ponto.X + b*Ponto.Y + c) / SQRT(a*a + b*b)));
end;

procedure DesenharSeta(Canvas: TCanvas; P1, P2: TPoint; Tam: Integer = 10; Dist_P2: Integer = 0);
var M, T, Q: TPoint;
    mr, r2: Real;
    sen_q, cos_q, x2barra, y2barra, xmlinha, ymlinha,
    xmbarra, ymbarra, x2linha, y2linha: Real;
begin
  Q.x := p2.x;
  Q.y := p2.y;

  if P2.x = P1.x then
     begin
     r2 := Tam * sqrt(2)/2;
     if P2.y > P1.y then
        begin
        p2.y := p2.y - Dist_p2;

        t.x := Trunc(P1.x - r2);
        t.y := Trunc(P2.y - r2);

        m.x := Trunc(P1.x + r2);
        m.y := t.y;
        end
     else
        begin
        p2.y := p2.y + Dist_p2;

        t.x := Trunc(P1.x - r2);
        t.y := Trunc(P2.y + r2);

        m.x := Trunc(P1.x + r2);
        m.y := t.y;
        end
     end
  else
     begin
     mr := (p2.y - p1.y) / (p2.x - p1.x);   {tangente do ângulo _q }

     { seno e cosseno do ângulo de rotação _q }
     r2 := sqrt(1+mr*mr);

     sen_q := mr / r2;
     cos_q := 1 / r2;

     if p2.x < p1.x then
        begin
        sen_q := -sen_q;
        cos_q := -cos_q;
        end;

     {translação de sistema,para que a nova origem colida com P1 }
     x2barra := p2.x - p1.x;
     y2barra := p2.y - p1.y;

     {rotação do sistema que já foi transladado: rotação de _q radianos }
     r2 := sqrt(2)/2;
     x2linha := x2barra*cos_q + y2barra*sen_q - Dist_P2;
     y2linha := y2barra*cos_q - x2barra*sen_q;

     x2barra := x2linha*cos_q - y2linha*sen_q;
     y2barra := x2linha*sen_q + y2linha*cos_q;

     p2.x := trunc(x2barra + p1.x);
     p2.y := trunc(y2barra + p1.y);

     xmlinha := x2linha - Tam*r2;
     ymlinha := Tam*r2;

     xmbarra := xmlinha*cos_q - ymlinha*sen_q;
     ymbarra := xmlinha*sen_q + ymlinha*cos_q;

     {valores de um dos pontos procurados:}
     m.x := Trunc(xmbarra + p1.x);
     m.y := Trunc(ymbarra + p1.y);

     {..................................................................}

     xmlinha := x2linha - Tam*r2;
     ymlinha := -Tam*r2;

     xmbarra := xmlinha*cos_q - ymlinha*sen_q;
     ymbarra := xmlinha*sen_q + ymlinha*cos_q;

     {valores do outro ponto procurado:}
     t.x := Trunc(xmbarra + p1.x);
     t.y := Trunc(ymbarra + p1.y);
     end;

  // Desenha a reta
  Canvas.MoveTo(p1.x, p1.y);
  Canvas.LineTo(p2.x, p2.y);

  // Desenha um dos lados da seta
  Canvas.LineTo(T.x, T.y);

  // Volta para a ponta da reta
  Canvas.MoveTo(p2.x, p2.y);

  // Desenha o outro lado da reta
  Canvas.LineTo(M.x, M.y);

  // Desenha a continuacao da reta
  if Dist_P2 > 0 then
     begin
     Canvas.MoveTo(p2.x, p2.y);
     Canvas.LineTo(Q.x, Q.y);
     end;
end;

procedure SalvarBitmapEmArquivo(Bitmap: TBitmap; Arquivo: TCustomIniFile; const Secao: String);
var i, j: integer;
    s   : String;
begin
  Arquivo.WriteInteger(Secao, 'Largura', Bitmap.Width);
  Arquivo.WriteInteger(Secao, 'Altura', Bitmap.Height);

  for i := 0 to Bitmap.Height-1 do
    begin
    s := '';
    for j := 0 to Bitmap.Width-1 do s := s + IntToHex(Bitmap.Canvas.Pixels[j, i], 6);
    Arquivo.WriteString(Secao, intToStr(i+1), s);
    end;
end;

function LerBitmapDoArquivo(Arquivo: TCustomIniFile; const Secao: String): TBitmap;
var L, A  : Integer;
    i, j  : Integer;
    k     : Integer;
    Linha : String;    // Linha que contém os códigos
    s     : String[7]; // cod. Hexadecimal que forma a cor de cada byte do bitmap
    Cor   : Longint;
begin
  Result := nil;
  L := Arquivo.ReadInteger(Secao, 'Largura', 0);
  A := Arquivo.ReadInteger(Secao, 'Altura', 0);

  if (A = 0) or (L = 0) then exit;
  Result := TBitmap.Create;
  Result.Height := A; Result.Width := L;

  for i := 0 to A-1 do
    begin
    s := '$';
    k := -1;
    Linha := Arquivo.ReadString(Secao, intToStr(i+1), '');
    if (L * 6) = Length(Linha) then
       for j := 1 to (L * 6) do
         begin
         s := s + Linha[j];
         if (j mod 6) = 0 then
            begin
            inc(k);
            if str2LongS(s, Cor) then
               Result.Canvas.Pixels[k, i] := Cor
            else
               Result.Canvas.Pixels[k, i] := clBlack;
            s := '$';
            end;
         end;
    end;
end;

Function CentroDaReta(const P1, P2: TPoint): TPoint;
var Dx, Dy: Integer;
begin
  Dx := abs(P1.x - P2.x) div 2;
  Dy := abs(P1.y - P2.y) div 2;

  // Calcula a coordenada x
  if P1.x < P2.x then Result.x := P1.x + Dx else
  if P1.x > P2.x then Result.x := P1.x - Dx else
  if P1.x = P2.x then Result.x := P1.x;

  // Calcula a coordenada y
  if P1.y < P2.y then Result.y := P1.y + Dy else
  if P1.y > P2.y then Result.y := P1.y - Dy else
  if P1.y = P2.y then Result.y := P1.y;
end;

function GetRGBColorAsString(Color: TColor): String;
var c: Longint;
begin
  c := ColorToRGB(Color);
  Result := intToHex(getRValue(c), 2) +
            intToHex(getGValue(c), 2) +
            intToHex(getBValue(c), 2);
end;

function GetFontAsString(Font: TFont): String;
begin
  Result := Font.Name + ' (' + IntToStr(Font.Size) + ') - ';
  if Font.Style = [] then Result := Result + 'Normal' else
  if Font.Style = [fsItalic, fsBold] then Result := Result + 'Itálico Negrito' else
  if fsItalic in Font.Style then Result := Result + 'Itálico' else
  if fsBold in Font.Style then Result := Result + 'Negrito';
end;

// Le uma imagem em jpeg de um recurso
procedure LoadImage(const jpgID: string; AnImage: TImage);
var Rs: TResourceStream;                    // Resource Stream
    jpi: TJpegImage;                        // Jpeg Image
begin
  jpi := TJpegImage.Create;                 // Create the Jpeg
  Rs := TResourceStream.Create(hInstance,   // Load the Image
                               jpgID,
                               RT_RCDATA);
  try
    Jpi.LoadFromStream(Rs);                 // Load from the resource string
    AnImage.Picture.Bitmap.Assign(jpi);     // Assign to the image control
  finally
    jpi.Free;                               // Free the Jpeg
    Rs.Free;                                // Free the resource stream
  end;
end;

function ObterPontoNaReta(P1, P2: TPoint; Dist_P2: integer): TPoint;
var Q: TPoint;
    mr, r2: Real;
    sen_q, cos_q, x2barra, y2barra,
    x2linha, y2linha: Real;
begin
  Q.x := p2.x;
  Q.y := p2.y;

  if P2.x = P1.x then
     begin
     result.x := P2.x;
     if P2.y > P1.y then
        result.y := p2.y - Dist_p2
     else
        result.y := p2.y + Dist_p2
     end
  else
     begin
     mr := (p2.y - p1.y) / (p2.x - p1.x);   {tangente do ângulo _q }

     { seno e cosseno do ângulo de rotação _q }
     r2 := sqrt(1 + mr*mr);

     sen_q := mr / r2;
     cos_q := 1 / r2;

     if p2.x < p1.x then
        begin
        sen_q := -sen_q;
        cos_q := -cos_q;
        end;

     {translação de sistema,para que a nova origem colida com P1 }
     x2barra := p2.x - p1.x;
     y2barra := p2.y - p1.y;

     {rotação do sistema que já foi transladado: rotação de _q radianos }
     x2linha := x2barra*cos_q + y2barra*sen_q - Dist_P2;
     y2linha := y2barra*cos_q - x2barra*sen_q;

     x2barra := x2linha*cos_q - y2linha*sen_q;
     y2barra := x2linha*sen_q + y2linha*cos_q;

     result.x := trunc(x2barra + p1.x);
     result.y := trunc(y2barra + p1.y);
     end;
end;

{$ifdef MSXML}

{ TXML_Font }

procedure TXML_Font.FromXML(node: IXMLDomNode);
begin
  self.FromXML(node.childNodes);
end;

procedure TXML_Font.FromXML(nodeList: IXMLDomNodeList);
var no: IXMLDomNode;
    n, s : string;
begin
  self.Style := [];

  no := NodeList.nextNode();
  while (no <> nil) do
    begin
    n := LowerCase(no.nodeName);
    s := LowerCase(no.Text);

    if n = 'name' then self.Name := s;
    if n = 'size' then self.Size := toInt(s);
    if n = 'color' then self.Color := toInt(s);
    if n = 'style' then
       begin
       if System.Pos('italic'   , s) > 0 then self.Style := self.Style + [fsItalic];
       if System.Pos('bold'     , s) > 0 then self.Style := self.Style + [fsBold];
       if System.Pos('underline', s) > 0 then self.Style := self.Style + [fsUnderline];
       if System.Pos('strikeout', s) > 0 then self.Style := self.Style + [fsStrikeOut];
       end;

    // go to the next node
    no := NodeList.nextNode();
    end;
end;

procedure TXML_Font.ToXML(Buffer: TStrings; Ident: Integer);
var x: TXML_Writer;
    s: string;
begin
  x := TXML_Writer.Create(Buffer, Ident);

  x.Write('Name', self.Name);
  x.Write('Size', self.Size);
  x.Write('Color', self.Color);

  s := '';
  if fsItalic    in self.Style then s := s + 'italic '    else
  if fsBold      in self.Style then s := s + 'bold '      else
  if fsUnderline in self.Style then s := s + 'underline ' else
  if fsStrikeOut in self.Style then s := s + 'strikeout ' ;
  if s <> '' then
     begin
     SysUtilsEx.DeleteLastChar(s);
     x.Write('Style', s);
     end;

  x.Free();
end;

procedure TXML_Font.ToXML(const tagName: string; Buffer: TStrings; Ident: Integer);
var s: string;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<' + tagName + '>');
    self.toXML(Buffer, Ident + 2);
  Buffer.Add(s + '</' + tagName + '>');
end;

{$endif MSXML}

end.

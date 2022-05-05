unit XML_Utils_Graphics;

interface
uses Classes,
     SysUtils,
     Graphics,
     STStrS,
     MSXML4,
     XML_Utils;

  procedure SaveBitmapToXML(Bitmap: TBitmap; Buffer: TStrings; Ident: integer);
  function LoadBitmapFromXML(no: IXMLDomNode): TBitmap;

implementation

  procedure SaveBitmapToXML(Bitmap: TBitmap; Buffer: TStrings; Ident: integer);
  var i, j: integer;
      s   : String;
      x   : TXML_Writer;
  begin
    x := TXML_Writer.Create(Buffer, Ident);

    x.BeginTag('bitmap', ['width', 'heigth'], [Bitmap.Width, Bitmap.Height]);
      x.BeginIdent();

      for i := 0 to Bitmap.Height-1 do
        begin
        s := '';
        for j := 0 to Bitmap.Width-1 do
          s := s + IntToHex(Bitmap.Canvas.Pixels[j, i], 6);
        x.Write('L', s, false);
        end;

      x.EndIdent();
    x.EndTag('bitmap');

    x.Free();
  end;

  function LoadBitmapFromXML(no: IXMLDomNode): TBitmap;
  var L, A  : Integer;
      i, j  : Integer;
      k     : Integer;
      Linha : String;    // Linha que contém os códigos
      s     : String[7]; // cod. Hexadecimal que forma a cor de cada byte do bitmap
      Cor   : Longint;
  begin
    Result := nil;
    if no.nodeName <> 'bitmap' then Exit;

    L := no.attributes.item[0].nodeValue;
    A := no.attributes.item[1].nodeValue;

    if (A = 0) or (L = 0) then exit;

    Result := TBitmap.Create();
    Result.Height := A;
    Result.Width := L;

    L := L * 6; // 6 caracteres por pixel

    for i := 0 to no.childNodes.length-1 do
      begin
      s := '$';
      k := -1;
      Linha := no.childNodes.item[i].text;
      if L = Length(Linha) then
         for j := 1 to L do
           begin
           s := s + Linha[j];
           if (j mod 6) = 0 then
              begin
              inc(k);
              if STStrS.str2LongS(s, Cor) then
                 Result.Canvas.Pixels[k, i] := Cor
              else
                 Result.Canvas.Pixels[k, i] := clBlack;
              s := '$';
              end;
           end;
      end;
  end;

end.

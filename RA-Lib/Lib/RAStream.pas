{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov, Roman Tkachev

       components  : none
       description : stream routines

       authors     : Andrei Prygounkov, Roman Tkachev
       e-mail      : black@infa.ru, whiteman@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAStream;

interface

uses Classes, Graphics;

  procedure TextSaveToStream(Stream : TStream; Text : string);
  function  TextLoadFromStream(Stream : TStream) : string;
  procedure StringSaveToStream  (Stream : TStream; S : string);
  function  StringLoadFromStream(Stream : TStream) : string;
  procedure IntSaveToStream     (Stream : TStream; int : integer);
  function  IntLoadFromStream   (Stream : TStream) : integer;
  function  WordLoadFromStream(Stream : TStream) : Word;
  procedure WordSaveToStream(Stream : TStream; int : Word);
  function  LongLoadFromStream(Stream : TStream) : Longint;
  procedure LongSaveToStream(Stream : TStream; int : Longint);
  function  ExtendedLoadFromStream(Stream : TStream) : Extended;
  procedure ExtendedSaveToStream(Stream : TStream; Ext : Extended);
  procedure BoolSaveToStream    (Stream : TStream; bool : boolean);
  function  BoolLoadFromStream  (Stream : TStream) : boolean;
  procedure FontSaveToStream    (Stream : TStream; Font : TFont);
  procedure FontLoadFromStream  (Stream : TStream; Font : TFont);
  procedure BrushSaveToStream   (Stream : TStream; Brush : TBrush);
  procedure BrushLoadFromStream (Stream : TStream; Brush : TBrush);

  function  StringLoadFromStreamDef(Stream : TStream; Def : string) : string;
  function  IntLoadFromStreamDef   (Stream : TStream; Def : integer) : integer;
  function  BoolLoadFromStreamDef  (Stream : TStream; Def : boolean) : boolean;

implementation

procedure TextSaveToStream(Stream : TStream; Text : string);
begin
  Stream.WriteBuffer(Pointer(Text)^, Length(Text));
end;

function TextLoadFromStream(Stream : TStream) : string;
var
  Size : integer;
begin
  Size := Stream.Size - Stream.Position;
  SetString(Result, nil, Size);
  Stream.Read(Pointer(Result)^, Size);
end;

procedure StringSaveToStream(Stream : TStream; S : string);
var
  L : integer;
  P : PChar;
begin
  L := Length(S);
  Stream.WriteBuffer(L, sizeof(L));
  P := PChar(S);
  Stream.WriteBuffer(P^, L);
end;

function StringLoadFromStream(Stream : TStream) : string;
var
  L : integer;
  P : PChar;
begin
  Stream.ReadBuffer(L, sizeof(L));
  SetLength(Result, L);
  P := PChar(Result);
  Stream.ReadBuffer(P^, L);
end;

procedure IntSaveToStream(Stream : TStream; int : integer);
begin
  Stream.WriteBuffer(int, sizeof(int));
end;

function IntLoadFromStream(Stream : TStream) : integer;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure WordSaveToStream(Stream : TStream; int : Word);
begin
  Stream.WriteBuffer(int, sizeof(int));
end;

function WordLoadFromStream(Stream : TStream) : Word;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure LongSaveToStream(Stream : TStream; int : Longint);
begin
  Stream.WriteBuffer(int, sizeof(int));
end;

function LongLoadFromStream(Stream : TStream) : Longint;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure ExtendedSaveToStream(Stream : TStream; Ext : Extended);
begin
  Stream.WriteBuffer(Ext, sizeof(Ext));
end;

function ExtendedLoadFromStream(Stream : TStream) : Extended;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure BoolSaveToStream(Stream : TStream; bool : boolean);
var
  B : integer;
begin
  B := integer(bool);
  Stream.WriteBuffer(B, sizeof(B));
end;

function BoolLoadFromStream(Stream : TStream) : boolean;
var
  B : integer;
begin
  Stream.ReadBuffer(B, sizeof(B));
  Result := boolean(B);
end;

procedure ColorSaveToStream(Stream : TStream; Color : TColor);
begin
  Stream.WriteBuffer(Color, sizeof(Color));
end;

function ColorLoadFromStream(Stream : TStream) : TColor;
begin
  Stream.ReadBuffer(Result, sizeof(Result));
end;

procedure FontSaveToStream(Stream : TStream; Font : TFont);
var
  Style : TFontStyles;
begin
  StringSaveToStream(Stream, Font.Name);
  ColorSaveToStream(Stream, Font.Color);
  Style := Font.Style;
  Stream.WriteBuffer(Style, sizeof(Style));
  IntSaveToStream(Stream, Font.Size);
end;

procedure FontLoadFromStream(Stream : TStream; Font : TFont);
var
  Style : TFontStyles;
begin
  Font.Name := StringLoadFromStream(Stream);
  Font.Color := ColorLoadFromStream(Stream);
  Stream.ReadBuffer(Style, sizeof(Style));
  Font.Style := Style;
  Font.Size := IntLoadFromStream(Stream);
end;

procedure BrushSaveToStream(Stream : TStream; Brush : TBrush);
//...
var
  Style : TBrushStyle;
begin
  ColorSaveToStream(Stream, Brush.Color);
  Style := Brush.Style;
  Stream.WriteBuffer(Style, sizeof(Style));
end;

procedure BrushLoadFromStream(Stream : TStream; Brush : TBrush);
//...
var
  Style : TBrushStyle;
begin
  Brush.Color := ColorLoadFromStream(Stream);
  Stream.ReadBuffer(Style, sizeof(Style));
  Brush.Style := Style;
end;

function StringLoadFromStreamDef(Stream : TStream; Def : string) : string;
begin
  if Stream <> nil then Result := StringLoadFromStream(Stream)
                   else Result := Def;
end;

function IntLoadFromStreamDef(Stream : TStream; Def : integer) : integer;
begin
  if Stream <> nil then Result := IntLoadFromStream(Stream)
                   else Result := Def;
end;

function BoolLoadFromStreamDef(Stream : TStream; Def : boolean) : boolean;
begin
  if Stream <> nil then Result := BoolLoadFromStream(Stream)
                   else Result := Def;
end;


end.

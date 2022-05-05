unit HTML_Utils;

interface
uses classes, Graphics, SysUtils;

type
  THTML_Doc = class
  private
    FBI : String;
    FALC: TColor;
    FTC : TColor;
    FLC : TColor;
    FVLC: TColor;
    FBC : TColor;
    FTitle: String;
  public
    constructor Create;

    procedure BeginDoc(Text: TStrings; out LineIndex: Integer);
    procedure EndDoc(Text: TStrings);

    property Title              : String read FTitle write FTitle;
    property Background_Image   : String read FBI    write FBI;
    property Background_Color   : TColor read FBC    write FBC;
    property Text_Color         : TColor read FTC    write FTC;
    property Link_Color         : TColor read FLC    write FLC;
    property Visited_Link_Color : TColor read FVLC   write FVLC;
    property Active_Link_Color  : TColor read FALC   write FALC;
  end;

implementation
uses GraphicUtils;

{ THTML_Doc }

constructor THTML_Doc.Create;
begin
  inherited;

  FALC := -1;
  FTC  := -1;
  FLC  := -1;
  FVLC := -1;
  FBC  := -1;
end;

procedure THTML_Doc.BeginDoc(Text: TStrings; out LineIndex: Integer);
begin
  LineIndex := 0;
  Text.Insert(LineIndex, '<html>'); inc(LineIndex);
  Text.Insert(LineIndex, '<head>'); inc(LineIndex);
  Text.Insert(LineIndex, '  <title>'); inc(LineIndex);
  Text.Insert(LineIndex, '    ' + FTitle); inc(LineIndex);
  Text.Insert(LineIndex, '  </title>'); inc(LineIndex);
  Text.Insert(LineIndex, '</head>'); inc(LineIndex);

  Text.Insert(LineIndex, '<body'); inc(LineIndex);
  if Background_Image <> '' then
     begin
     Text.Insert(LineIndex, Format('  background= "%s"', [Background_Image]));
     inc(LineIndex);
     end;

  if Background_Color <> -1 then
     begin
     Text.Insert(LineIndex, Format('  bgcolor= "#%s"', [GetRGBColor(Background_Color)]));
     inc(LineIndex);
     end;

  if Text_Color <> -1 then
     begin
     Text.Insert(LineIndex, Format('  text= "#%s"', [GetRGBColor(Text_Color)]));
     inc(LineIndex);
     end;

  if Link_Color <> -1 then
     begin
     Text.Insert(LineIndex, Format('  link= "#%s"', [GetRGBColor(Link_Color)]));
     inc(LineIndex);
     end;

  if Visited_Link_Color <> -1 then
     begin
     Text.Insert(LineIndex, Format('  vlink= "#%s"', [GetRGBColor(Visited_Link_Color)]));
     inc(LineIndex);
     end;

  if Active_Link_Color <> -1 then
     begin
     Text.Insert(LineIndex, Format('  alink="#%s"', [GetRGBColor(Active_Link_Color)]));
     inc(LineIndex);
     end;

  Text.Insert(LineIndex, '>');
  inc(LineIndex);
end;

procedure THTML_Doc.EndDoc(Text: TStrings);
begin
  Text.Add('</body>');
  Text.Add('</HTML>');
end;

end.

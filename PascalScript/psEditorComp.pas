{***********************************************************
       Copyright (C) 2000 Rochedo

       component   : TScriptPascalEditor
       programer   : rochedo
       e-mail      : rochedo@ufpel.tche.br
************************************************************}

{$INCLUDE ps.INC}

unit psEditorComp;

interface
uses SysUtils, Classes, Graphics,
     psEditorBaseComp, 
     psParser;

const

  { Max_Line - maximum line numbers, what editor scan for comments }
  Max_Line = 20000;

type
  TSymbolColor = class(TPersistent)
  private
    FStyle: TFontStyles;
    FForeColor: TColor;
    FBackColor: TColor;
  public
    procedure SetColor(const ForeColor, BackColor: TColor; const Style: TFontStyles);
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TFontStyles read FStyle write FStyle;
    property ForeColor: TColor read FForeColor write FForeColor;
    property BackColor: TColor read FBackColor write FBackColor;
  end;

  TColors = class(TPersistent)
  private
    FComment: TSymbolColor;
    FNumber: TSymbolColor;
    FString: TSymbolColor;
    FSymbol: TSymbolColor;
    FReserved: TSymbolColor;
    FIdentifer: TSymbolColor;
    FPreproc: TSymbolColor;
    FFunctionCall: TSymbolColor;
    FDeclaration: TSymbolColor;
    FStatement: TSymbolColor;
    FPlainText: TSymbolColor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Comment      : TSymbolColor read FComment      write FComment;
    property Number       : TSymbolColor read FNumber       write FNumber;
    property Strings      : TSymbolColor read FString       write FString;
    property Symbol       : TSymbolColor read FSymbol       write FSymbol;
    property Reserved     : TSymbolColor read FReserved     write FReserved;
    property Identifer    : TSymbolColor read FIdentifer    write FIdentifer;
    property Preproc      : TSymbolColor read FPreproc      write FPreproc;
    property FunctionCall : TSymbolColor read FFunctionCall write FFunctionCall;
    property Declaration  : TSymbolColor read FDeclaration  write FDeclaration;
    property Statement    : TSymbolColor read FStatement    write FStatement;
    property PlainText    : TSymbolColor read FPlainText    write FPlainText;
  end;

  TOnReservedWord = procedure(Sender: TObject; Token: string;
    var Reserved: Boolean) of object;

  TScriptPascalEditor = class(TBaseEditor)
  private
    Parser: TIParser;
    FColors: TColors;
    FLine: string;
    FLineNum: Integer;
    FLong: byte;
    FLongTokens: Boolean;
    FLongDesc: array[0..Max_Line] of byte;
    FOnReservedWord: TOnReservedWord;
    procedure RescanLong;
    procedure CheckInLong;
    function FindLongEnd: integer;
  protected
    procedure Loaded; override;
    procedure GetAttr(Line, ColBeg, ColEnd: integer); override;
    procedure TextModified(Pos: integer; Action: TModifiedAction; Text: string);
      override;
    function GetReservedWord(const Token: string; var Reserved: Boolean):
      Boolean; virtual;
    function UserReservedWords: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Colors         : TColors read FColors write FColors;
    property LongTokens     : Boolean read FLongTokens write FLongTokens default True;
    property OnReservedWord : TOnReservedWord read FOnReservedWord write FOnReservedWord;
  end;

implementation
uses SysUtilsEx,
     Math;

procedure TSymbolColor.SetColor(const ForeColor, BackColor: TColor; const Style:
  TFontStyles);
begin
  FForeColor := ForeColor;
  FBackColor := BackColor;
  FStyle := Style;
end;

procedure TSymbolColor.Assign(Source: TPersistent);
begin
  if Source is TSymbolColor then
  begin
    FForeColor := TSymbolColor(Source).FForeColor;
    FBackColor := TSymbolColor(Source).FBackColor;
    FStyle     := TSymbolColor(Source).FStyle;
  end
  else
    inherited Assign(Source);
end;

constructor TColors.Create;
begin
  FComment := TSymbolColor.Create;
  FNumber := TSymbolColor.Create;
  FString := TSymbolColor.Create;
  FSymbol := TSymbolColor.Create;
  FReserved := TSymbolColor.Create;
  FStatement := TSymbolColor.Create;
  FIdentifer := TSymbolColor.Create;
  FPreproc := TSymbolColor.Create;
  FFunctionCall := TSymbolColor.Create;
  FDeclaration := TSymbolColor.Create;
  FPlainText := TSymbolColor.Create;
  FComment.SetColor(clOlive, clWindow, [fsItalic]);
  FNumber.SetColor(clNavy, clWindow, []);
  FString.SetColor(clPurple, clWindow, []);
  FSymbol.SetColor(clBlue, clWindow, []);
  FReserved.SetColor(clWindowText, clWindow, [fsBold]);
  FStatement.SetColor(clWindowText, clWindow, [fsBold]);
  FIdentifer.SetColor(clWindowText, clWindow, []);
  FPreproc.SetColor(clGreen, clWindow, []);
  FFunctionCall.SetColor(clWindowText, clWindow, []);
  FDeclaration.SetColor(clWindowText, clWindow, []);
  FPlainText.SetColor(clWindowText, clWindow, []);
end;

destructor TColors.Destroy;
begin
  FComment.Free;
  FNumber.Free;
  FString.Free;
  FSymbol.Free;
  FReserved.Free;
  FStatement.Free;
  FIdentifer.Free;
  FPreproc.Free;
  FFunctionCall.Free;
  FDeclaration.Free;
  FPlainText.Free;
  inherited Destroy;
end; { Destroy }

procedure TColors.Assign(Source: TPersistent);
begin
  if Source is TColors then
  begin
    FComment     .Assign(TColors(Source).FComment     );
    FNumber      .Assign(TColors(Source).FNumber      );
    FString      .Assign(TColors(Source).FString      );
    FSymbol      .Assign(TColors(Source).FSymbol      );
    FReserved    .Assign(TColors(Source).FReserved    );
    FStatement   .Assign(TColors(Source).FStatement   );
    FIdentifer   .Assign(TColors(Source).FIdentifer   );
    FPreproc     .Assign(TColors(Source).FPreproc     );
    FFunctionCall.Assign(TColors(Source).FFunctionCall);
    FDeclaration .Assign(TColors(Source).FDeclaration );
    FPlainText   .Assign(TColors(Source).FPlainText   );
  end
  else
    inherited Assign(Source);
end;


{ TScriptPascalEditor }
constructor TScriptPascalEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parser := TIParser.Create;
  Parser.ReturnComments := True;
  FColors := TColors.Create;
  FLongTokens := True;
end; { Create }

destructor TScriptPascalEditor.Destroy;
begin
  Parser.Free;
  FColors.Free;
  inherited Destroy;
end; { Destroy }

procedure TScriptPascalEditor.Loaded;
begin
  inherited Loaded;
  RescanLong;
end;

procedure TScriptPascalEditor.GetAttr(Line, ColBeg, ColEnd: integer);
var
  Token: string;
  i: integer;
const
  Symbols = [',', ':', ';', '.', '(', ')', '=', '+',
    '-', '/', '<', '>', '%', '*', '~', '''', '\', '{', '}', '#'];
const
  DelphiKeyWords: string =
    '  program  for  to  downto  step  createobject  ' +
    'procedure  function  ' +
    'integer  boolean  real  string  object  ' +
    'begin  end  if  then  else  ' +
    'while  do  var  or  and  not  div  mod  exit  ';

  function IsDelphiKeyWord(St: string): boolean;
  begin
    Result := System.Pos(' ' + ANSILowerCase(St) + ' ', DelphiKeyWords) <> 0;
  end;

  function IsComment(St: string): boolean;
  var
    LS: integer;
  begin
    LS := Length(St);
    Result := ((LS > 0) and (St[1] = '{')) or
      ((LS > 1) and ((St[1] = '/') and (St[2] = '/')));
  end;

  function IsStringConstant(St: string): boolean;
  var
    LS: integer;
  begin
    LS := Length(St);
    Result := (LS > 0) and ((St[1] = '''') or (St[1] = '"'));
  end;

  procedure SetColor(Color: TSymbolColor);
  var
    i: integer;
  begin
    for i := Parser.PosBeg[0] + 1 to Parser.PosEnd[0] do
    begin
      LineAttrs[i].FC := Color.ForeColor;
      LineAttrs[i].BC := Color.BackColor;
      LineAttrs[i].Style := Color.Style;
    end;
  end;

  function NextSymbol: string;
  var
    i: Integer;
  begin
    i := 0;
    while (Parser.PCPos[i] <> #0) and (Parser.PCPos[i] in [' ', #9, #13, #10]) do
      inc(i);
    Result := Parser.PCPos[i];
  end;

var
  F: boolean;
  S: string;
  C: TSymbolColor;
  Reserved: Boolean;
  PrevToken: string;
  N: Integer;
begin
  S := Lines[Line];
  FLine := S;
  FLineNum := Line;
  Parser.pcProgram := PChar(S);
  Parser.pcPos := Parser.pcProgram;
  CheckInLong;

  C := FColors.FPlainText;

  if (FLong <> 0) then
     begin
     Parser.pcPos := Parser.pcProgram + FindLongEnd + 1;
     C := FColors.FComment;
     end;

  LineAttrs[1].FC := C.ForeColor;
  LineAttrs[1].Style := C.Style;
  LineAttrs[1].BC := C.BackColor;

  N := Math.Min(Max_X, Length(S));

  for i := 1 to N do
    Move(LineAttrs[1], LineAttrs[i], sizeof(LineAttrs[1]));

  if Length(S) < Max_X then
    begin
    LineAttrs[N+1].FC := Font.Color;
    LineAttrs[N+1].Style := Font.Style;
    LineAttrs[N+1].BC := Color;
    for i := N+1 to Max_X do
      Move(LineAttrs[N+1], LineAttrs[i], sizeof(LineAttrs[1]));
    end;

  try
    PrevToken := '';
    Token := Parser.Token;
    while Token <> '' do
      begin
      F := true;
      if UserReservedWords and GetReservedWord(Token, Reserved) then
         begin
         if Reserved then
            SetColor(FColors.FReserved);
         end
      else
         if IsDelphiKeyWord(Token) then
            SetColor(FColors.FReserved)
         else
            F := false;

      if F then
        {Ok}
      else if IsComment(Token) then
        SetColor(FColors.FComment)
      else if IsStringConstant(Token) then
        SetColor(FColors.FString)
      else if (Length(Token) = 1) and (Token[1] in Symbols) then
        SetColor(FColors.FSymbol)
      else if IsIntConstant(Token) or IsRealConstant(Token) then
        SetColor(FColors.FNumber)
      else
        SetColor(FColors.FIdentifer);

      PrevToken := Token;
      Token := Parser.Token;
    end;
  except

  end;
end; { GetAttr }

procedure TScriptPascalEditor.CheckInLong;
begin
  if not FLongTokens then
     begin
     FLong := 0;
     Exit;
     end;

  if FLineNum <= High(FLongDesc) then
     FLong := FLongDesc[FLineNum]
  else
     FLong := 0;
end;

procedure TScriptPascalEditor.RescanLong;
var
  iLine: integer;
  P, F: PChar;
  S: string;
  i, i1, L1: Integer;
begin
  if not FLongTokens then
     begin
     FLong := 0;
     Exit;
     end;

  if Lines.Count = 0 then Exit;

  FLong := 0;
  iLine := 0;
  FillChar(FLongDesc, sizeof(FLongDesc), 0);

  while iLine < Lines.Count - 1 do { Iterate }
    begin
    { only real programer can write loop on 5 pages }

    S := Lines[iLine];
    P := PChar(S);
    F := P;
    L1 := Length(S);
    i := 1;
    while i <= L1 do
      begin
          case FLong of
            0: //  not in comment
              case S[i] of
                '{':
                  begin
                    P := StrScan(F + i, '}');
                    if P = nil then
                    begin
                      FLong := 1;
                      Break;
                    end
                    else
                      i := P - F + 1;
                  end;

                '(':
                  if {S[i + 1]} F[i] = '*' then
                  begin
                    FLong := 2;
                    P := StrScan(F + i + 2, ')');
                    if P = nil then
                      Break
                    else
                    begin
                      if P[-1] = '*' then
                        FLong := 0;
                      i := P - F + 1;
                    end;
                  end;

                '''':
                  begin
                    P := StrScan(F + i + 1, '''');
                    if P <> nil then
                    begin
                      i1 := P - F;
                      if P[1] <> '''' then
                        i := i1
                      else
                        { ?? }
                    end
                    else
                      i := L1 + 1;
                  end;
              end; { case }

            1:
              begin //  {
                P := StrScan(F + i - 1, '}');
                if P <> nil then
                begin
                  FLong := 0;
                  i := P - F + 1;
                end
                else
                  i := L1 + 1;
              end;

            2:
              begin //  (*
                P := StrScan(F + i, ')');
                if P = nil then
                  Break
                else
                begin
                  if P[-1] = '*' then
                    FLong := 0;
                  i := P - F + 1;
                end;
              end;
          end; { case FLong }

      inc(i);
    end; { while i <= L1 }
    inc(iLine);
    FLongDesc[iLine] := FLong;
  end; { iLine < Lines.Count - 1 }
end;

function TScriptPascalEditor.FindLongEnd: integer;
var
  P, F: PChar;
begin
  P := PChar(FLine);
  Result := Length(FLine);
  case FLong of
    1:
      begin
        P := StrScan(P, '}');
        if P <> nil then
          Result := P - PChar(FLine);
      end;

    2:
      begin
        F := P;
        while true do
        begin
          F := StrScan(F, '*');
          if F = nil then Exit;
          if F[1] = ')' then break;
          inc(F);
        end;
        P := F + 1;
        Result := P - PChar(FLine);
      end;
  end;
end;

procedure TScriptPascalEditor.TextModified(Pos: integer; Action: TModifiedAction; Text: string);
var
  S: string;
begin
  if not FLongTokens then Exit;
  S := '{}*()/'#13;
  if HasAnyChar(S, Text) then
     begin
     RescanLong;
     Invalidate;
     end;
end; { TextChanged }

function TScriptPascalEditor.GetReservedWord(const Token: string; var Reserved:
  Boolean): Boolean;
begin
  Result := Assigned(FOnReservedWord);
  if Result then
     begin
     Reserved := False;
     FOnReservedWord(Self, Token, Reserved);
     end
end;

function TScriptPascalEditor.UserReservedWords: boolean;
begin
  Result := Assigned(FOnReservedWord);
end;

end.


unit MPSConversor;

interface
uses classes, Contnrs, SysUtils, SysUtilsEx, wsParser, dialogs;

const
  cSTEPS = 4;

type
  TMPSConversor = class;

  TLine = {private} class
  private
    Pos: integer;
    MPSConversor: TMPSConversor;
    constructor Create(MPSConversor: TMPSConversor; Pos: integer);
    function toString(): string; virtual;
  end;

  TID = record
          Pos: integer;
          Value: string;
        end;

  TEquation = {private} class(TLine)
  private
    Error: string;
    IDs: array of TID;
    Name: string;
    EquationType: char;
    Value: string;
    NeedSearchID: boolean;
    IDsCountTemp: integer;

    constructor Create(MPSConversor: TMPSConversor; Pos: integer; const Name, Equation: string);
    procedure Analyze(const Equation: string);
    function HasID(IndexID: integer; out Value: string): boolean;
    function toString(): string; override;
    function IDsCount(): integer;
    procedure Reset();
  end;

  TComment = {private} class(TLine)
  private
    Comment: string;
    constructor Create(Pos: integer; const Comment: string);
    function toString(): string; override;
  end;

  TLines = {private} class(TObjectList)
  private
    function getLine(i: integer): TLine;
  private
    property Line[i: integer] : TLine read getLine; default;
  end;

  TParser = class(TAbstractParser)
  private
    constructor Create();
    function Automaton (const Expr: String;
                          var Pos: Integer;
                          var Token: String): Boolean; override;
  end;

  // delegates
  TProgressEvent = procedure (ProgressID, Value: integer; const Text: string) of object;

  TMPSConversor = class
  private
    FStop: boolean;
    Lines: TLines;
    GlobalIDs: TStrings;
    SortedGlobalIDs: TStrings;
    FOnProgress: TProgressEvent;
    function GlobalIDPos(const id: string): integer;
    procedure WriteMPS(outText: TStrings);
    procedure DoProgress(ProgressID, Value, Max: integer; const Text: string = '');
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Convert(const inText, outText: TStrings);
    procedure Stop();
    property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TMPSConversor }

constructor TMPSConversor.Create();
begin
  inherited Create();
  Lines := TLines.Create(true);
  GlobalIDs := TStringList.Create();
  
  SortedGlobalIDs := TStringList.Create();
  TStringList(SortedGlobalIDs).Sorted := true;
end;

destructor TMPSConversor.Destroy();
begin
  Lines.Free();
  GlobalIDs.Free();
  SortedGlobalIDs.Free();
  inherited;
end;

procedure TMPSConversor.WriteMPS(outText: TStrings);
var i, k: Integer;
    L: TLine;
    E: TEquation;
    ID, Value: string;
begin
  if Lines.Count > 0 then
     if Lines[0] is TComment then
        outText.Add(SysUtilsEx.LeftStr('NAME', 14) + TComment(Lines[0]).Comment )
     else
        outText.Add(SysUtilsEx.LeftStr('NAME', 14) + 'SEM_NOME');

  // Secao ROWS
  DoProgress(1, 2, cSTEPS, 'Gerando a Seção "ROWS" ...');
  outText.Add('ROWS');
  for i := 0 to Lines.Count-1 do
    begin
    L := Lines[i];
    if L is TEquation then
       begin
       E := TEquation(L);
       outText.Add(' ' + E.EquationType + '  ' + E.Name);
       end;
    DoProgress(2, i, Lines.Count-1);
    if FStop then Exit;
    end;

  // Inicializa as equacoes para a busca dos IDs
  for i := 0 to Lines.Count-1 do
    if Lines[i] is TEquation then
       TEquation(Lines[i]).Reset();

  // Secao COLUMNS
  DoProgress(1, 3, cSTEPS, 'Gerando a Seção "COLUMNS" ...');
  DoProgress(2, 0, 100);
  outText.Add('COLUMNS');
  for k := 0 to GlobalIDs.Count-1 do
    begin
    DoProgress(2, k, GlobalIDs.Count-1);
    for i := 0 to Lines.Count-1 do
      begin
      L := Lines[i];
      if L is TEquation then
         begin
         E := TEquation(L);
         if E.NeedSearchID and E.HasID(k, {out} Value) then
            begin
            outText.Add(SysUtilsEx.LeftStr('    ' + GlobalIDs[k], 14) +
                        SysUtilsEx.LeftStr(E.Name, 10) + Value);
            end;
         end
      if FStop then Exit;
      end;
    end;

  // Secao RHS
  DoProgress(1, 4, cSTEPS, 'Gerando a Seção "RHS" ...');
  outText.Add('RHS');
  for i := 0 to Lines.Count-1 do
    begin
    L := Lines[i];
    if L is TEquation then
       begin
       E := TEquation(L);
       if E.Value <> '' then
          outText.Add(SysUtilsEx.LeftStr('    RHS1', 14) +
                      SysUtilsEx.LeftStr(E.Name, 10) + E.Value);
       end;
    DoProgress(2, i, Lines.Count-1);
    if FStop then Exit;
    end;

  // FIM
  outText.Add('ENDATA');
end;

procedure TMPSConversor.Convert(const inText, outText: TStrings);

   procedure AddComment(PosLine: integer; Line: string);
   begin
     System.Delete(Line, 1, 1);
     Lines.Add(TComment.Create(PosLine, AllTrim(Line)));
   end;

   procedure AnalyzeEquationLine(PosLine: integer; Line: string);
   var s1, s2: string;
       i: integer;
   begin
     i := System.Pos(' ', Line);
     s1 := System.Copy(Line, 1, i-1);
     s2 := SysUtilsEx.Copy(Line, i+1, Length(Line), true);

     if (s1 = 'MIN') or (s1 = 'MAX') then
        begin
        // Funcao Objetivo
        Lines.Add( TEquation.Create(self, PosLine, 'FO', s2) );
        end
     else
        if (System.Pos(')', s1) > 0) then
           begin
           // Equacoes gerais
           Lines.Add(
             TEquation.Create(self, PosLine, LeftStringOf(s1, ')'), s2));
           end;
   end;

   procedure Analyze(PosLine: integer);
   var Line: string;
   begin
     Line := UpperCase(Alltrim(inText[PosLine]));
     if (Line <> '') and
        (Line <> 'ST') and
        (Line <> 'END') then
        begin
        // Comentarios
        if Line[1] = '!' then
           AddComment(PosLine, Line)
        else
           AnalyzeEquationLine(PosLine, Line);
        end;
   end;

var i: Integer;
begin
  outText.Clear();
  Lines.Clear();
  GlobalIDs.Clear();
  SortedGlobalIDs.Clear();
  FStop := false;

  DoProgress(1, 1, cSTEPS, 'Analisando as Equações ...');
  for i := 0 to inText.Count-1 do
    begin
    Analyze(i);
    DoProgress(2, i, inText.Count-1);
    if FStop then Exit;
    end;

  outText.BeginUpdate();
  try
    WriteMPS(outText);
  finally
    outText.EndUpdate();
  end;
end;

function TMPSConversor.GlobalIDPos(const id: string): integer;
begin
  result := SortedGlobalIDs.IndexOf(id);
  if result = -1 then
     begin
     result := GlobalIDs.Add(id);
     SortedGlobalIDs.Add(id);
     end;
end;

procedure TMPSConversor.DoProgress(ProgressID, Value, Max: integer; const Text: string);
begin
  if assigned(FOnProgress) then
     FOnProgress(ProgressID, Trunc(Value * 100 / Max), Text);
end;

procedure TMPSConversor.Stop();
begin
  FStop := true;
end;

{ TLine }

constructor TLine.Create(MPSConversor: TMPSConversor; Pos: integer);
begin
  inherited Create();
  self.Pos := Pos;
  self.MPSConversor := MPSConversor;
end;

function TLine.toString(): string;
begin
  result := '';
end;

{ TComment }

constructor TComment.Create(Pos: integer; const Comment: string);
begin
  inherited Create(nil, Pos);
  self.Comment := Comment;
end;

function TComment.toString(): string;
begin
  result := Comment;
end;

{ TLines }

function TLines.getLine(i: integer): TLine;
begin
  result := TLine(self.Items[i]);
end;

{ TEquation }

procedure TEquation.Analyze(const Equation: string);
var p: TParser;

    procedure ERROR();
    begin
      raise Exception.Create('Expressão inválida');
    end;

    procedure getProps(i: integer; t: TToken; out id: string;
                                              out coef: string;
                                              out sinal: char);
    var t0, t1: TToken;
    begin
      id := t.AsString;
      if i = 0 then
         begin
         sinal := '+';
         coef  := '1';
         end
      else
         if i = 1 then
            begin
            t0 := p.Token[0];
            if t0.TokenType = ttAritOperator then
               begin
               sinal := t0.AsString[1];
               coef  := '1';
               end else
            if t0.TokenType in [ttInteger, ttFloat] then
               begin
               sinal := '+';
               coef  := t0.AsString;
               end
            else
               ERROR()
            end
         else
            begin
            t1 := p.Token[i-1];
            t0 := p.Token[i-2];
            if t1.TokenType = ttAritOperator then
               begin
               if t0.TokenType = ttAritOperator then ERROR();
               sinal := t1.AsString[1];
               coef  := '1';
               end else
            if t1.TokenType in [ttInteger, ttFloat] then
               begin
               coef := t1.AsString;
               if t0.TokenType = ttAritOperator then
                  sinal := t0.AsString[1]
               else
                  ERROR()
               end
            else
               ERROR()
            end;
    end; // getProps

var id, coef: string;
    sinal: char;
    i, k, tc: integer;
    t: TToken;
begin
  p := TParser.Create();
  try
    p.Text.Add(Equation);
    p.Scan();

    // Extracao dos coeficientes, tipo e valor da equacao ...

    setLength(IDs, p.Variables.Count);
    k := 0;
    tc := p.TokensCount-1;

    for i := 0 to tc-1 do
      begin
      t := p.Token[i];
      if t.TokenType = ttVariable then
         begin
         getProps(i, t, {out} id, {out} coef, {out} sinal);

         IDs[k].Pos := MPSConversor.GlobalIDPos(id);
         IDs[k].Value := sinal + coef;
         inc(k);
         end
      else
         if t.TokenType = ttLogicOperator then
            begin
            // Obtencao do tipo da equacao
            if t.AsString = '='  then self.EquationType := 'E' else
            if t.AsString = '<=' then self.EquationType := 'L' else
            if t.AsString = '>=' then self.EquationType := 'G';

            // Obtencao do valor da equacao.
            // O operador logico somente pode ser o penultimo ou
            // anti-penultimo token.
            if (i = tc-2) then
               self.Value := '+' + p.Token[i+1].AsString
            else
               if (i = tc-3) then
                  self.Value := p.Token[i+1].AsString + p.Token[i+2].AsString
               else
                  ERROR();
            end;
      end;

  finally
    p.Free();
  end;
end;

constructor TEquation.Create(MPSConversor: TMPSConversor; Pos: integer; const Name, Equation: string);
begin
  inherited Create(MPSConversor, Pos);
  self.Name := Name;
  self.EquationType := 'N';
  try
    Analyze(Equation);
  except
    On E: Exception do
       Error := E.Message;
  end;
end;

function TEquation.HasID(IndexID: integer; out Value: string): boolean;
var i: Integer;
begin
  result := false;
  for i := 0 to High(IDs) do
    if IDs[i].Pos = IndexID then
       begin
       result := true;
       Value := IDs[i].Value;
       inc(IDsCountTemp);
       if IDsCountTemp = self.IDsCount() then
          self.NeedSearchID := false;
       break;
       end;
end;

function TEquation.IDsCount(): integer;
begin
  result := System.Length(IDs);
end;

procedure TEquation.Reset();
begin
  IDsCountTemp := 0;
  NeedSearchID := true;
end;

function TEquation.toString: string;
begin
  result := self.Name;
  if self.Error <> '' then
     result := result + ') ' + self.Error;
end;

{ TParser }

function TParser.Automaton(const Expr: String; var Pos: Integer; var Token: String): Boolean;
type
  TState = (sSTART, sEND1, sEND2, sNUMBER, sFLOAT_NUM,
            sEND_FLOAT_NUM, sID, sOPER, sERROR);
var
  p: integer;
  State: TState;
Begin
  Token := '';
  p := Pos-1;
  Result := false;
  state := sSTART;
  Repeat
    inc(p);
    case state of
      sSTART: begin
              case expr[p] of
                ' '                     : {Nada};
                #0                      : state := sEND1;
                'A'..'Z', 'a'..'z', '_' : state := sID;
                '0'..'9'                : state := sNUMBER;
                '+', '-', ';', '='      : state := sEND1;
                '<', '>'                : state := sOPER;
                else
                   state := sERROR;
                end;
              if Expr[p] <> ' ' then Token := Expr[p];
              end;

      // Formacao de um identificador
      sID: case expr[p] of
             'A'..'Z', 'a'..'z', '0'..'9', '_': Token := Token + Expr[p];
             else
                state := sEND2;
           end;

      // Formacao de um numero inteiro ou
      // inicio de um numero em ponto flutuante
      sNUMBER: case Expr[p] of
                 '0'..'9' : Token := Token + Expr[p];
                 '.'      : begin
                            Token := Token + Expr[p];
                            state := sFLOAT_NUM;
                            end;
                 else
                    state := sEND2;
               end;

       // Depois de um ponto eh necessario pelo menos um digito
       sFLOAT_NUM: case Expr[p] of
                     '0'..'9': begin
                               Token := Token + Expr[p];
                               state := sEND_FLOAT_NUM;
                               end;
                     else
                        state := sERROR; // Erro. Ex: 2.
                   end;

       // Formacao final de um numero em ponto flutuante
       sEND_FLOAT_NUM: case Expr[p] of
                         '0'..'9' : Token := Token + Expr[p];
                         else
                            state := sEND2;
                       end;

       // Estado final onde devolvemos o ultimo caracter lido
       sEND2: begin
              pos := p-1;
              Result := True;
              exit;
              end;

       // Estado final onde consumimos o ultimo caracter lido
       sEND1: begin
              pos := p;
              Result := True;
              exit;
              end;

       // Formacao dos elementos ">=", "<=", "<" ou ">"
       sOPER: case Expr[p] of
                '=': begin
                     Token := Token + Expr[p];
                     state := sEND1;
                     end;
                else
                   state := sEND2;
              end;

      sERROR: begin
              Pos := p;
               Raise Exception.CreateFmt(
                 'Elemento não reconhecido na posição %d: "%s"',
                  [Pos-1, Expr]);
              end;
      end;
  until false;
end; { TParser.Automaton }

constructor TParser.Create();
begin
  inherited Create();
  AritOpers.Text := '+'#13'-';
  LogicOpers.Text := '='#13'<='#13'>=';
end;

end.

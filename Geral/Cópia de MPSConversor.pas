unit MPSConversor;

interface
uses classes, Contnrs, SysUtils, SysUtilsEx, wsParser, dialogs, Forms;

const
  cSTEPS = 5;

type
  TMPSConversor = class;

  TLine = {private} class
  private
    Pos: integer;
    MPSConversor: TMPSConversor;
    constructor Create(MPSConversor: TMPSConversor; Pos: integer);
    function toString(): string; virtual;
  end;

  // Utilizado para identificar os IDs de cada equacao
  TID = record
          Pos: integer;
          Value: string;
        end;

  TIDType = (idtUP,    //  upper bound        -->   (0 <=) x <= b
             idtLO,    //  lower bound        -->   b <= x (< +inf)
             idtMI,    //  lower bound -inf   -->   -inf < x (<= 0)
             idtPL,    //  upper bound +inf   -->   (0 <=) x < +inf
             idtFX,    //  fixed variable     -->   x = b
             idtFR,    //  free variable      -->   -inf < x < +inf
             idtBV,    //  binary variable    -->   x = 0  or  1
             idtLI,    //  integer variable   -->   b <= x (< +inf)
             idtUI,    //  integer variable   -->   (0 <=) x <= b
             idtSC     //  semi-cont variable -->   x = 0  or  l <= x <= b
             );

  // Utilizado para armazenar as propriedades de cada ID (Limites e tipos)
  TID_Props = {private} class
  private
    IDType: set of TIDType; // Default = []
    UP: string;
    LO: string;
    FX: string;
    LI: string;
    UI: string;
    SC: string;
    // guardar as equacoes onde ele aparece
    constructor Create();
    procedure WriteBounds(outText: TStrings; const ID: string);
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

  TObjectiveFunction = {private} class(TEquation)
  private
    constructor Create(MPSConversor: TMPSConversor; var Pos: integer);
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

  TMPSConversor = class{(TThread)}
  private
    Lines: TLines;

    // Utilizado para guardar os IDs na ordem em que eles aparecem no texto
    GlobalIDs: TStrings;

    // Utilizado para otimizar a busca de IDs
    // ATENCAO: SortedGlobalIDs.Objects[] é utilizado de maneira diferente
    // nas duas partes do algoritmo.
    SortedGlobalIDs: TStrings;

    FOnProgress: TProgressEvent;
    FinText: TStrings;
    FoutText: TStrings;
    FProgressID: integer;
    FProgressValue: Integer;
    FProgressText: string;
    FPMCounter: integer;

    // Indica se o analisador esta analisando texto depois do END
    AfterEND: boolean;

    function GlobalIDPos(const id: string): integer;
    procedure WriteMPS(outText: TStrings);
    procedure DoInternalProgress();
    procedure DoProgress(ProgressID, Value, Max: integer; const Text: string = '');
    procedure Convert(const inText, outText: TStrings);
  public
    Terminated : Boolean;

    constructor Create();
    destructor Destroy(); override;
    procedure Execute(); {override;}

    property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
    property inText : TStrings read FinText write FinText;
    property outText : TStrings read FoutText write FoutText;
  end;

implementation

{ TMPSConversor }

constructor TMPSConversor.Create();
begin
  inherited Create{(true)};
  
  Lines := TLines.Create(false);
  GlobalIDs := TStringList.Create();

  SortedGlobalIDs := TStringList.Create();
  TStringList(SortedGlobalIDs).Sorted := true;
end;

destructor TMPSConversor.Destroy();
var i: Integer;
begin
  for i := 0 to Lines.Count-1 do
    if Lines[i] <> nil then
       Lines[i].Free();
  Lines.Free();

  GlobalIDs.Free();

  // SortedGlobalIDs pode armazenar as props de cada ID
  if AfterEND then
     for i := 0 to SortedGlobalIDs.Count-1 do
       if SortedGlobalIDs.Objects[i] <> nil then
          SortedGlobalIDs.Objects[i].Free();
  SortedGlobalIDs.Free();

  inherited Destroy();
end;

procedure TMPSConversor.WriteMPS(outText: TStrings);
var i, ii, k, kk: Integer;
    L: TLine;
    E: TEquation;
    ID, Value: string;
    p: TID_Props;
    b: boolean;
    x: real;
begin
  // Obtem o nome do script
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
       E.Reset();
       end;
    DoProgress(2, i, Lines.Count-1);
    if Terminated then Exit;
    end;

  // Secao COLUMNS
  // Percorre as variaveis na ordem que elas aparecem no texto
  DoProgress(1, 3, cSTEPS, 'Gerando a Seção "COLUMNS" ...');
  DoProgress(2, 0, 100);
  outText.Add('COLUMNS');
  for k := 0 to GlobalIDs.Count-1 do
    begin
    DoProgress(2, k, GlobalIDs.Count-1);

    // Obtem este ID
    ID := GlobalIDs[k];

    // Marca o ID como INTEGER se ele for uma variavel deste tipo
    ii := SortedGlobalIDs.IndexOf(ID);
    if (ii <> -1) and (SortedGlobalIDs.Objects[ii] <> nil) then
       begin
       p := TID_Props ( SortedGlobalIDs.Objects[ii] );
       if (idtUI in p.IDType) or (idtLI in p.IDType) or (idtSC in p.IDType) then
          outText.Add(SysUtilsEx.LeftStr('    INTEGER', 14) +
                      SysUtilsEx.LeftStr(#39'MARKER'#39, 10) +
                      SysUtilsEx.LeftStr(' ', 15) + #39'INTORG'#39)
       else
          p := nil;
       end
    else
       p := nil;

    // Percorre todas as equacoes buscando o ID selecionado
    for i := 0 to Lines.Count-1 do
      begin
      L := Lines[i];
      if L is TEquation then
         begin
         E := TEquation(L);

         // Este trexo de codigo considera que um ID pode aparecer mais de uma
         // vez na equacao. Para cada ID eh gerado uma linha no MPS.
         (*
         //if E.NeedSearchID and E.HasID(k, {out} Value) then
         for kk := 0 to E.IDsCount-1 do
           if E.IDs[kk].Pos = k then
              begin
              outText.Add(SysUtilsEx.LeftStr('    ' + ID, 14) +
                          SysUtilsEx.LeftStr(E.Name, 10) + E.IDs[kk].Value);
              end;
         *)

         // Se um ID aparecer mais de uma vez, eh feito o somatorio e
         // apenas uma linha no MPS é gerado.
         b := false;
         for kk := 0 to E.IDsCount-1 do
           if E.IDs[kk].Pos = k then
              begin
              if b then x := x + toFloat(E.IDs[kk].Value) else x := toFloat(E.IDs[kk].Value);
              b := true;
              end;

         // Se existir pelo menos um ID eh gerado uma linha no arquivo MPS
         if b then
            outText.Add(SysUtilsEx.LeftStr('    ' + ID, 14) +
                        SysUtilsEx.LeftStr(E.Name, 10) + toString(x, 5));

         end; // L is TEquation
      if Terminated then Exit;
      end; // for i (equacoes)

    if p <> nil then
       outText.Add(SysUtilsEx.LeftStr('    INTEGER', 14) +
                   SysUtilsEx.LeftStr(#39'MARKER'#39, 10) +
                   SysUtilsEx.LeftStr(' ', 15) + #39'INTEND'#39);
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
    if Terminated then Exit;
    end;

  // Secao BOUNDS
  DoProgress(1, 5, cSTEPS, 'Gerando a Seção "BOUNDS ...');
  outText.Add('BOUNDS');
  for i := 0 to SortedGlobalIDs.Count-1 do
    begin
    if SortedGlobalIDs.Objects[i] <> nil then
       begin
       ID := SortedGlobalIDs[i]; // Nome da variável
       p := TID_Props ( SortedGlobalIDs.Objects[i] );
       p.WriteBounds(outText, ID);
       end;
    DoProgress(2, i, SortedGlobalIDs.Count-1);
    if Terminated then Exit;
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

   procedure AnalyzeIDTypes(var PosLine: integer; Line: string);
   var sl : TStrings;
       id : string;
       s  : string;
       i  : integer;
       p  : TID_Props;
   begin
     sl := nil;
     Split(Line, sl, [' ']);
     try
       if sl.Count >= 2 then
          begin
          id := sl[1];
          i := SortedGlobalIDs.IndexOf(id);

          // Verifica se eh um ID valido
          if i > -1 then
             begin
             // Verifica se ele ja possui propriedades associadas a este ID
             // senao cria um container para elas.
             if SortedGlobalIDs.Objects[i] = nil then
                SortedGlobalIDs.Objects[i] := TID_Props.Create();

             // Obtem as props.
             p := TID_Props ( SortedGlobalIDs.Objects[i] );

             // atribuicao das props.
             s := sl[0];

             if (s = 'UI') then
                begin
                include(p.IDType, idtUI);
                if sl.Count > 2 then p.UI := sl[2];
                end
             else

             if (s = 'LI') then
                begin
                include(p.IDType, idtLI);
                if sl.Count > 2 then p.LI := sl[2];
                end
             else

             if (s = 'BV') then
                include(p.IDType, idtBV)
             else

             if (s = 'SC') then
                begin
                include(p.IDType, idtSC);
                if sl.Count > 2 then p.SC := sl[2];
                end
             else

             if (s = 'SUB') or (s = 'UP') then
                begin
                include(p.IDType, idtUP);
                if sl.Count > 2 then p.UP := sl[2];
                end
             else

             if (s = 'SLB') or (s = 'LO') then
                begin
                include(p.IDType, idtLO);
                if sl.Count > 2 then p.LO := sl[2];
                end
             else

             if (s = 'MI') then
                include(p.IDType, idtMI)
             else

             if (s = 'PL') then
                include(p.IDType, idtPL)
             else

             if (s = 'FX') then
                begin
                include(p.IDType, idtFX);
                if sl.Count > 2 then p.FX := sl[2];
                end
             else

             if (s = 'FR') then
                include(p.IDType, idtFR)

             else
                outText.Add('Palavra chave desconhecida: ' + s);
             end
          else
             outText.Add('Identificador Desconhecido: ' + id);
          end
       else
          outText.Add('Comando Inválido: ' + Line);
     finally
       sl.Free;
     end;
   end;

   procedure AnalyzeEquationLine(var PosLine: integer; Line: string);
   var s1, s2: string;
       i: integer;
   begin
     if (Line = 'MIN') or (Line = 'MAX') then
        begin
        // Funcao Objetivo - Pode estar contida em mais de uma linha a partir da
        // linha seguinte a esta.
        Lines.Add( TObjectiveFunction.Create(self, PosLine) );
        end
     else
        begin
        i := System.Pos(')', Line);
        s1 := Copy(Line, 1, i-1, true);
        s2 := Copy(Line, i+1, Length(Line), true);
        Lines.Add( TEquation.Create(self, PosLine, s1, s2) );
        end;
   end;

   procedure Analyze(var PosLine: integer);
   var Line: string;
       i   : integer;
   begin
     Line := UpperCase(Alltrim(inText[PosLine]));
     if (Line <> '') and (Line <> 'ST') then
        if (Line = 'END') then
           begin
           AfterEND := true;

           // Limpa Objects para ser utilizado no bloco de análise dos tipos dos IDs
           // Esta limpeza somente deverá ocorrer uma vez.
           for i := 0 to SortedGlobalIDs.Count-1 do SortedGlobalIDs.Objects[i] := nil;
           end
        else
           begin
           // Comentarios
           if Line[1] = '!' then
              AddComment(PosLine, Line)
           else
              if AfterEND then
                 AnalyzeIDTypes({var} PosLine, Line)
              else
                 AnalyzeEquationLine({var} PosLine, Line);
           end;
   end;

var i, j: Integer;
begin
  outText.Clear();
  Lines.Clear();
  GlobalIDs.Clear();
  SortedGlobalIDs.Clear();      

  AfterEND := false;
  DoProgress(1, 1, cSTEPS, 'Analisando as Equações ...');

  i := 0;
  j := inText.Count;
  while i < j do
    begin
    Analyze(i);
    DoProgress(2, i, j-1);
    if Terminated then Exit;
    inc(i);
    end;

  if outText.Count = 0 then
     begin
     outText.BeginUpdate();
     try
       WriteMPS(outText);
       finally
       outText.EndUpdate();
       end;
     end;
end; // Convert

// Retorna a localizacao do ID sempre na lista global
// Utiliza a lista ordenada para otimizar a busca de um ID.
// Esta funcao eh somente utilizada na primeira parte do algoritmo, apos
// a definicao de todas as equacoes esta rotina nao eh mais necessaria e
// SortedGlobalIDs.Objects[] será utilizado para outros fins.
function TMPSConversor.GlobalIDPos(const id: string): integer;
begin
  result := SortedGlobalIDs.IndexOf(id);
  if result = -1 then
     begin
     result := GlobalIDs.Add(id);

     // Salva o ID e sua posicao na lista global para ser utilizado temporariamente
     // nesta mesma funcao em consecutivas chamadas de localizacao de IDs
     SortedGlobalIDs.AddObject(id, pointer(result));
     end
  else
     // retorna a posicao armazenada temporariamente
     result := integer(SortedGlobalIDs.Objects[result]);
end;

procedure TMPSConversor.DoProgress(ProgressID, Value, Max: integer; const Text: string);
begin
  if assigned(FOnProgress) then
     begin
     FProgressID := ProgressID;
     FProgressValue := Trunc(Value * 100 / Max);
     FProgressText := Text;
     DoInternalProgress();
     inc(FPMCounter);
     if (FPMCounter mod 200) = 0 then
        Application.ProcessMessages();
     end;
end;

procedure TMPSConversor.Execute();
begin
  Terminated := false;
  FPMCounter := 0;
  Convert(FinText, FoutText);
end;

procedure TMPSConversor.DoInternalProgress();
begin
  FOnProgress(FProgressID, FProgressValue, FProgressText);
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

    tc := p.TokensCount-1;

    // Se as variaveis aparecem nas equacoes apenas uma vez entao podemos usar
    // o codigo a seguir ...
    // setLength(IDs, p.Variables.Count);

    // Senao, temos que contar quantos tokens do tipo "Variavel" aparecem na equacao ...
    k := 0;
    for i := 0 to tc-1 do
      if p.Token[i].TokenType = ttVariable then inc(k);
    setLength(IDs, k);

    // Obtem as informacaoes para cada ID
    k := 0;
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

{ TID_Props }

constructor TID_Props.Create();
begin
  inherited Create();
  IDType := [];
end;

procedure TID_Props.WriteBounds(outText: TStrings; const ID: string);

   procedure Write(const BoundType, BoundValue: string);
   begin
     outText.Add(' ' + BoundType + ' ' +
                 SysUtilsEx.LeftStr('BOUNDS', 10) +
                 SysUtilsEx.LeftStr(ID, 10) +
                 BoundValue);
   end;

begin

  if idtLO in IDType then Write('LO', LO);
  if idtUP in IDType then Write('UP', UP);
  if idtMI in IDType then Write('MI', '');
  if idtPL in IDType then Write('PL', '');
  if idtUI in IDType then Write('UI', UI);
  if idtLI in IDType then Write('LI', LI);
  if idtSC in IDType then Write('SC', SC);
  if idtFX in IDType then Write('FX', FX);
  if idtFR in IDType then Write('FR', '');
  if idtBV in IDType then Write('BV', '');
end;

{ TObjectiveFunction }

// A funcao objetivo devera estar declarada apos as palavras chaves "MIN" ou "MAX"
// Formato da equacao:
// [ NAME) ] EQUACAO
// EQUACAO pode estar escrita em varias linhas
// Ex:
//     FO) X + Y +
//         A + B - 2.5C
// Se NAME nao for declarado FO sera dado a funcao objetivo
// Se NAME existir ele devera estar escrito na 1, linha da equacao
constructor TObjectiveFunction.Create(MPSConversor: TMPSConversor; var Pos: integer);
var Text: TStrings;
    IniPos, i: integer;
    s: string;
    Equation: string;
    PriLinha: boolean;
    Name: string;
begin
  Equation := '';
  Text := MPSConversor.inText;

  PriLinha := true;
  IniPos := Pos;
  Name := 'FO';

  repeat
    inc(Pos);
    s := UpperCase(AllTrim(Text[Pos]));

    if s = '' then
       Continue;

    if ( s = 'ST' ) or
       ( s = 'SUBJECT TO' ) or
       ( s = 'END') or
       ( s[1] = '!' ) or
       ( Pos = Text.Count-1 ) then
       break
    else
       begin
       // Obtem o nome da equacao se ele existir
       // Este nome somente podera aparecer na primeira linha da equacao
       if PriLinha then
          begin
          PriLinha := false;
          i := System.Pos(')', s);
          if i > 0 then
             begin
             Name := Copy(s, 1, i-1, true);
             s := Copy(s, i+1, Length(s), false);
             end;
          end;

       Equation := Equation + ' ' + s;
       end;
  until false;

  // Chama a base com a equacao ajustada
  inherited Create(MPSConversor, IniPos, Name, Equation);

  // Volta uma posicao
  Dec(Pos);
end;

end.

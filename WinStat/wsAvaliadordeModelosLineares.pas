Unit wsAvaliadorDeModelosLineares;

{
DATA DA �LTIMA ATUALIZA��O ............................. 22/01/1999
HIST�RICO
  22/10/1999 .................. Rochedo
    Na avali��o de modelos a fun��o I(express�o) que possibilitaria a introdu��o de
    vari�veis como express�es num�ricas foi alterada para ser reconhecida no formato
    [express�o]. Para isto foi necess�rio a modifica��o do Analisador L�xico que reconhece
    tudo o que est� estre de '[' e ']' como um identificador.
}

{
Unidade totalmente reformulada por Adriano Rochedo em 05/11/1998.

Gram�tica:

    Objetivo:
              Refletem, alocam e encadeiam os n�s da �rvore de express�es de
              acordo com a gram�tica abaixo.

              Gram�tica:

                S  -> E;
                E  -> FE�
                E� -> +FE� | -FE� | -*FE� | -/FE� | 0
                F  -> GF�
                F� -> *GF� | 0
                G  -> HG�
                G� -> /HG� | 0
                H  -> IH�
                H� -> .IH� | 0
                I  -> id | (E) | fun��o(E)

}


Interface
Uses Classes,
     wsGLib,
     wsMatrix,
     wsTabelaDeSimbolos,
     wsMasterModel;

CONST
  chInt     = '.';
  chCross   = '*';
  chNested  = '/';
  chDelete  = '-';
  chInclude = '+';

{ Funcao que avalia a expressao e devolve o modelo expandido }
Procedure EvalModel(Expr: String;
                    var Terms, VarList: TStrings;
                    DS: TwsDataSet{ = nil};
                    SortTerms: Boolean{ = true});

Implementation
uses wsListaDeTermos,
     SysUtils,
     SysUtilsEx,
     wsConstTypes,
     wsRotinasUteis,
     wsExceptions,
     wsAvaliadorDeExpressoes;

Const
    MaxFunc     = 2;

Type
    TFun    = array [1..maxfunc] of string[10];

Const
    TAMtab_fun  = 2;
    TAMTAB      = 200;
    AMAI        = 65;
    ZMAI        = 90;
    AMIN        = 97;
    ZMIN        = 122;
    OPERADOR     : array [1..7] of string[2] = (chDelete, chInclude, chCross, chNested, chInt, '-/', '-*');
    LETRAS       : TCharSet = ['A'..'Z', 'a'..'z'];
    TERMINADOR   : TCharSet = [';'];
    OUTROS       : TCharSet = ['(', ')', '[', ']'];
    TAB_FUN      : TFun     = ('POLY', 'I');

Type
    TipoToken  = (ttIdent, ttOper, ttFunc, ttOpen, ttClose, ttTerm);
    TipoElemen = (fator, variavel, nul);
    testagio   = (sintatico, avaliador);
    tcontraste = (helm, polort, usuario, desvio, anula);
    ttipo      = (numerica, quantitativa, qualitativa);

{ ----------------- Exce��es ---------------------}
  EwsModel = Class(Exception);

{ ----------------- Pilha ------------------------------------- }
    TStack = class(TList)
      procedure push(p: TwsTermlist);
      function pop: TwsTermlist;
    end;


    {Implementa��o da estrutura do token}
    Ptoken = ^Ttoken;
    Ttoken =  record
                Pos  : Integer;   {Posi��o do primeiro caracter do Token na String de entrada}
                Tipo : TipoToken; {Tipo do token (Ident, Oper, etc)}
                Simb : String[2]; {Usado quando o token for um operador}
                Ind  : Integer;   {�ndice das tabelas de (S�mbolos, Const., Fun��es)}
                Prev : Ptoken;    {Ponteiro para o n� esquerdo da �rvore}
                Next : Ptoken;    {Ponteiro para o n� direito da �rvore}
              end;


var
    Tab_Sim      : TStrings;            { Tabela de simbolos (Variaveis) }
    ArrayPToken  : TList;               { Lista de Tokens }
    stack        : TStack;              { Pilha auxiliar das opera��es}
    Term         : TwsTerm;             { LIsta dos termos retornados}

{ ------------------------- Rotinas Auxiliares -------------------------------- }

procedure TStack.push(p: TwsTermlist);
begin
  add(p);
end; { tstack.push }

function TStack.Pop: TwsTermlist;
begin
  Result := last;
  Remove(last);
end; { tstack.pop }

procedure Init;
begin
  Tab_Sim     := TStringList.Create;
  ArrayPToken := TList.Create;
  Stack       := TStack.Create;
end; { Init }

Procedure LiberaMem;
Var Pt : PToken;
    i  : Integer;
Begin
  {libera os tokens alocados}
  For i := 0 to ArrayPToken.Count-1 Do
    Begin
    Pt := ArrayPToken.Items[i];
    Dispose(Pt);
    End;

  If ArrayPToken.Count > -1 Then ArrayPToken.Clear;
  ArrayPToken.Free;
  Stack.Free;
End; {LiberaMem}

{ -----------------------------------------------------------------------------
       Analise Lexica
  -----------------------------------------------------------------------------}


{    -------------- Procedure An�lise_L�xica ----------------------------------

  Objetivo:
            Efetua a An�lise L�xica de uma Express�o.

  Descri��o:
            Procedimento que recebe uma string e a quebra em tokens que s�o
            alocados na mem�ria e apontados por uma tlist. Esta tlist �
            devolvida junto com as tabelas de s�mbolos e constantes.

  Entradas:
           expressao   -> String que contem a express�o a ser analisada
           tabfun      -> Tabela que cont�m as fun��es que podem aparecer na
                          express�o.

  Sa�das:
           tabsim      -> Tabela que cont�m os s�mbolos contidos na express�o.
           arrayptoken -> Lista que aponta para os tokens alocados na mem�ria.
           Erro        -> Erro encontrado na etapa.
}

{ ------------------------- Fun��o L�xico -----------------------------------

    Objetivo:
              Retorna o pr�ximo token da express�o.

    Descri��o:
              Esta fun��o implementa o l�xico por demanda. Ela obt�m o token,
              aloca-o no heap e o inclui na tlist de tokens, retornado a posi��o
              em que o token acabou.

    Entradas:
             exp -> Express�o que cont�m os tokens.

    Sa�das:
             pos    -> posi��o em que o token acabou.
             lexico -> 0 se sucesso.
                       1 se erro.
}
procedure analise_lexica(Const Expressao: String; var TabSim: TStrings; TabFun: TFun;
                         ArrayPToken: TList; Var Erro: Word);
var
  Pos    :Integer;  { Posi��o do inicio do pr�ximo token }

  function lexico(Const exp: string; var pos: integer): byte;
  const
          ZERO = 48;                     { C�digo ASCII do n�mero zero }
          NOVE = 57;                     { C�digo ASCII do n�mero nove }
  var
      posinic     : Integer;            { Posi��o inicial do token }
      tip         : Tipotoken;          { Tipo do token }
      pont_token  : Ptoken;             { Ponteiro ao token alocado }
      token       : String;             { Token obtido }

  {   ----------------- Fun��o Obt�m_Token ------------------------------------

      Objetivo:
                Obt�m o pr�ximo token da string.

      Descri��o:
                Esta fun��o recebe uma string e devolve o pr�ximo token a partir
                da posi��o passada. O token � ent�o devolvido. Foi implementada
                com o aut�mato finito cuja tabela de transi��o de estados pode
                ser vista acima.

      Entradas:
                exp ->  String que cont�m a espress�o.
                pos ->  Posi��o onde deve come�ar o token.

      Sa�das:
                tok ->  Token montado.
                obtem_token  -> 0 se sucesso;
                                1 se erro.

  }
function Obtem_Token(Exp: String; Var Pos: Integer; var Tok: String; out gIdentificador: Boolean): Byte;
var p,                               { Posi��o do caracter a ser lido }
    state  :byte;                    { Estado do Aut�mato Finito }
    i : integer;
    c : char;
begin
  p := pos - 1;
  state := 0;
  gIdentificador := False;
  repeat
    inc(p);
//    i := length(exp);
//    c := exp[p];
    if p <= length(exp) then
       exp[p] := upcase(exp[p]);
    case state of
      0:  begin
          case exp[p] of
            ' '                     : begin  end;
            'A'..'Z', '_'           : state := 1;
            '['                     : begin state := 2; Continue; end;
            chInclude, '(', ')',';' : state := 6;
            chInt                   : state := 7;
            chNested                : state := 10;
            chCross                 : state := 11;
            chDelete                : state := 13;
            else                      state := 16; {Erro}
            end;
          if exp[p] <> ' ' then tok := exp[p];
          end;

      1  :case exp[p] of
              'A'..'Z', '0'..'9', '_'  :tok := tok + exp[p];
              else
                 state := 5;
              end;

      2  :case exp[p] of
             ']'    : begin
                      state := 6;
                      gIdentificador := True;
                      end ;

             else     begin
                      tok := tok + exp[p];
                      state := 2;
                      end;
             end;
{
       3  :case exp[p] of
             '0'..'9'            :begin
                                  tok := tok + exp[p];
                                  state := 4;
                                  end;
             else
                state := 16;
             end;

       4  :case exp[p] of
             '0'..'9'            :tok := tok + exp[p];
             else
                state := 5;
             end;
}
       5  :begin
           pos := p - 1;
           obtem_token := 0;
           exit;
           end;

       6  :begin
           pos := p;
           obtem_token := 0;
           exit;
          end;

       7  :case exp[p] of
             chInt  :begin
                   tok := tok + exp[p];
                   state := 6;
                   end;
             else
                state := 5;
             end;
{
       8  :case exp[p] of
             '#'  :begin
                   tok := tok + exp[p];
                   state := 6;
                   end;
             chNested  :begin
                   tok := tok + exp[p];
                   state := 6;
                   end;
             else
                state := 5;
             end;

       9  :case exp[p] of
             '@'  :begin
                   tok := tok + exp[p];
                   state := 6;
                   end;
             else
                state := 5;
             end;
}
       10  :case exp[p] of
              chNested  :begin
                    tok := exp[p];
                    state := 6;
                    end;
              else
                 state := 5;
              end;

       11  :case exp[p] of
              chCross  :begin
                    tok := tok + exp[p];
                    state := 6;
                   end;
              else
                 state := 5;
              end;
{
       12  :case exp[p] of
              '>'  :begin
                    tok := exp[p];
                    state := 6;
                    end;
              '='  :begin
                    tok := exp[p];
                    state := 6;
                    end;
              else
                 state := 5;
              end;
}
        13  :case exp[p] of
               chNested, chCross :
                    begin
                    tok := tok + exp[p];
                    state := 6;
                    end;
               else
                  state := 5;
               end;
{
        14  :case exp[p] of
               '<'  :begin
                     tok:=tok+exp[p];
                     state:=6;
                     end;
               '='  :begin
                     tok:=exp[p];
                     state:=6;
                     end;
               else
                 state:=5;
               end;

        15  :case exp[p] of
               '='  :begin
                       tok := tok + exp[p];
                       state := 6;
                   end;
              else
                state:=5;
              end;
}
      16  :begin
             pos := p;
             obtem_token := 1;
             exit;
           end;
      end;
  until FALSE;
end; { obtem_token }

  { ----------------------- Fun��o �_Fun��o --------------------------------

    Objetivo:
              Verifica se o token passado � uma fun��o.

    Descri��o:
              Recebe uma string que � o token e verifica se ele est� contido no
              conjunto de fun��es.

    Entradas:
             token   ->  String que cont�m o token.
             tab_fun ->  Tabela que cont�m o conjunto de fun��es.

    Sa�das:
             efuncao ->  TRUE se token � uma fun��o;
                         FALSE caso contr�rio.
  }
  function efuncao(token: string; tab_fun: tfun): boolean;
  var i: integer;
  begin
    efuncao := FALSE;
    for i := 1 to TAMtab_fun do
      if token = tab_fun[i] then
         begin
         efuncao := TRUE;
         break;
         end;
  end; { efuncao }


  { -------------------- Fun��o IndFunc ---------------------------------------

    Objetivo:
              Devolve o �ndice da fun��o passada.

    Descri��o:
              Recebe em token a fun��o cujo �ndice ser� retornado pela fun��o.

    Entradas:
              token -> Cont�m a fun��o.

    Sa�das:
              indfunc -> �ndice da fun��o;
                         0 se n�o for.
  }
  function IndFunc(Const token: string): integer;
  begin
    for Result := 1 to MAXFUNC do
      if token = tabfun[Result] then exit;

    result := 0;
  end; { indfunc }


  { -------------------- Fun��o AlocaToken ------------------------------------

    Objetivo:
              Aloca um token na mem�ria.

    Descri��o:
              O token passado como par�metro � alocado no heap junto com a sua
              posi��o na string original e o seu tipo.

    Entradas:
              posi  -> Posi��o inicial do token na string.
              t     -> Tipo do token.
              token -> Label do token.

    Sa�das:
              alocatoken -> Ponteiro ao token criado.
  }
  function alocatoken(posi: integer; t: tipotoken;
                      const token: string; Expressao: Boolean): pToken;
    var
        nodo  :ptoken;                { Registro na mem�ria que cont�m o token }

    { insere um s�mbolo na tabela de s�mbolos, usa o objeto TList}
    function Insere_Sim(const simbol : string):integer;
      Var
        Ind  :Integer;
      Begin
        Ind := Tab_Sim.IndexOf(Simbol);
        if Ind = -1 then
           begin
           if Expressao then
              Tab_Sim.AddObject(Simbol, Pointer(-1)) // significa que o identificador � uma express�o
           else
              Tab_Sim.Add(Simbol);

           Result := Tab_Sim.Count-1;
           end
        else
           Result := ind;
      End; { Insere_Sim }

    begin
      new(nodo);
      with nodo^ do
        begin
        case t of
            ttident  :begin
                      simb := '';
                      ind  := insere_sim(token);
                      end;

            ttterm   :simb := ';';
            ttoper   :simb := token;
            ttfunc   :ind  := IndFunc(token);
          end;
        pos  := posi;
        tipo := t;
        next := nil;
        prev := nil;
        end;
      alocatoken := nodo;
    end; { alocatoken }


  { ---------------------------- Fun��o Tipo ----------------------------------

    Objetivo:
              Retorna o tipo do token.

    Descri��o:
              Verifica o tipo do token atrav�s dos caracteres do label do token.

    Entradas:
              token -> String que cont�m o label do token.

    Sa�das:
              tipo -> Tipo do token.
  }
  function tipo(const token: string):tipotoken;
  begin
    if efuncao(token,tab_fun) then
       tipo := ttfunc
    else
       if ((ord(token[1]) >= AMAI) and (ord(token[1]) <= ZMAI)) or
          ((ord(token[1]) >= AMIN) and (ord(token[1]) <= ZMIN)) then
           tipo := ttident
       else
          case token[1] of
            '('  :tipo := ttopen;
            ')'  :tipo := ttclose;
            else
              if token[1] = ';' then
                 tipo := ttterm
              else
                 tipo := ttoper;
            end;
  end; { tipotoken }

  // Vari�vel usada somente nos casos de tokens delimitados por '[' e ']'
  // Olhar documenta��o no cabe�alho da unidade
  var gIdentificador: Boolean; // Rochedo 22/01/1999
  begin  {Lexico}
    posinic := pos;
    token := '';
    if obtem_token(expressao, pos, token, gIdentificador) = 0 then
       begin
       if gIdentificador then
          tip := ttIdent
       else
          tip := tipo(token);

       pont_token := alocatoken(posinic, tip, token, gIdentificador);
       ArrayPToken.Add(pont_token);
       Result := 0;
       end
    else
       Result := 1;
  end; { Lexico }

begin {Analise_Lexica}
  pos := 1;
  while pos <= length(expressao) do
    If lexico(expressao, pos) <> 0 Then
       Raise EwsModel.CreateFmt('Erro no modelo < %s >'#13 +
                                'Posi��o: %d',
                                [System.Copy(Expressao, 1, Length(Expressao) - 1),
                                 pos - 1])
end; { analise_lexica }


{ -----------------------------------------------------------------------------
                     Fun��o de Percorrimento da �rvore
  -----------------------------------------------------------------------------}



{ -------------------------- Fun��o Percorre ----------------------------------

  Objetivo:
            Faz a an�lise sint�tica e a s�ntese da express�o. Na an�lise
            sint�tica, � utilizada a gram�tica mostrada adiante.

  Descri��o:
            Recebe a tlist criada pelo l�xico.
            Se sint�tico  :Retorna Verdadeiro ou Falso de acordo com a constru��o
                          da express�o.
            Se avaliador  :Gera a lista correspondente na tstringlist lista.

 Entradas:
            estagio     :Estagio de analise - se sintatica ou avaliacao.
            arrayptoken :Tlist que contem os tokens obtidos na analise l�xica.
            tab_sim     :Tabela de s�mbolos encontrados na express�o de entrada.
            tabfun      :Tabela de fun��es.


  Sa�das:
            Erro        : Codigo de Erro. 0 se sucesso.


  Alteracoes:

             25/10/96  :Inclusao de Erro como variavel de saida.
}
function percorre(estagio: TEstagio;
                  arrayptoken: TList;
                  var tab_sim: TStrings;
                  var tabfun: TFun;
                  Var Erro: Word;
                  STerms: Boolean): boolean;

  var
      indptoken  :integer;             { �ndice da tlist de tokens }
      listatemp,
      oper1,
      oper2      :TwsTermList;

  procedure opera1(Const operad: string; oper1, oper2: TStrings);
  var
    i,j  :integer;
    s    :string;
    del  :boolean;

    function op(Const oper: string):byte;
      begin
        if oper = chInclude then
           op := 1
        else if oper = chInt then
           op := 2
        else if oper = chCross then
           op := 3
        else if oper = chDelete then
           op := 4
        else if oper = chNested then
           op := 5
        else if oper = '-*' then
           op := 6
        else if oper = '-/' then
           op := 7;
      end; { op }


  function Variaveis(Const termo: String; s: string):string;
  Const DelChar: TCharSet = [chInt];
  var i    : Cardinal;
      tok  : string;

    function pertence(Const s1,s2: string): boolean;
    begin
      Result := (System.pos(s1, s2) <> 0);
    end; { pertence }

    begin
      i := 1;
      tok := ' ';
      while tok <> '' do
        begin
        tok := strtoken(termo, i, DelChar);
        if tok <> '' then
           if not pertence(tok, s) then
              if s = '' then
                 s := tok
              else
                 s := s + chInt + tok
        end;

      Result := s;
    end; { variaveis }

  procedure Acerta(var Lista: TwsTermList);
  var   i,j,k          : integer;
        s,s1,s2, saux  : string;
        apaga          : boolean;
        Marcado        : TList;

    function Ordem(Const s: string): integer;
    var i: Integer;
    begin
      Result := 0;
      for i := 1 to Length(s) do
        if s[i] = chInt then Inc(Result);
      inc(Result);
    end; { ordem }

    procedure troca;
    begin
       Term := TwsTerm.Create;  {07/01/1998 by Rochedo}
       lista.insertObject(i, Lista[j], TObject(Term));
       Term.Alias := Format('_Term%d_', [i]);
       TwsTerm(Lista.Objects[j+1]).Free;
       lista.delete(j+1);
     end; { troca }

{ ----------- Retira vari�veis repetidas de termos ----------- }

begin
  for i := 0 to lista.count-1 do
    begin
    saux := '';
    s1 := '';
    s := lista[i];
    for j := 1 to length(s) do
      if s[j] <> chInt then
         s1 := s1 + s[j]
      else
         begin
         apaga := FALSE;
         s2 := '';
         for k := j + 1 to length(s) do
           if s[k] <> chInt then
              s2 := s2 + s[k]
           else
              begin
              if s1 = s2 then apaga := TRUE;
              s2 := '';
              end;
         if s1 = s2 then apaga := TRUE;
         if not apaga then
            if saux = '' then
               saux := s1
            else
               saux := saux + chInt + s1;
         s1 := '';
         end;

    if saux = '' then
       saux := s1
    else
       saux := saux + chInt + s1;

    lista.strings[i] := saux;
    end; {For}

    { ---------------- Retira termos repetidos --------------- }

    Marcado := TList.Create;
    For i := 0 to lista.Count-1 do Marcado.Add(Pointer(0));

    For i := 0 to lista.Count-2 do
      Begin
      If Longint(Marcado[i]) = 1 Then Continue;
      For j := i+1 To lista.Count-1 do
        If CompareText(lista.Strings[i], lista.Strings[j]) = 0 Then
           Marcado[j] := Pointer(1);
      End;

    i := 0;
    While i < lista.Count do
      begin
      If Longint(Marcado[i]) = 1 Then
         begin
         lista.Term[i].Free;
         marcado.Delete(i);
         lista.Delete(i);
         continue;
         end;
      inc(i);
      end;

    Marcado.Free;

    { ------------------ Ordena --------------- }
    if STerms then
      for i := 0 to lista.count-1 do
        for j := i to lista.count-1 do
          if ordem(lista[i])>ordem(lista[j]) then Troca;

  end; { acerta }

  function margem(Const s1, s2: string): boolean;
  begin
    Result := (System.pos(s1, s2) <> 0)
  end; { margem }

  begin
    case op(operad) of
      1: {+}
        begin
        listatemp := TwsTermList.create;
        while oper1.count <> 0 do
          begin
          listatemp.addTerm(oper1.strings[0], TwsTerm.Create);
          oper1.delete(0);
          end;

        while oper2.count <> 0 do
          begin
          listatemp.addTerm(oper2.strings[0], TwsTerm.Create);
          oper2.delete(0);
          end;
        end;

      2: {.}
        begin
        listatemp := TwsTermList.create;
        for i := 0 to oper1.count-1 do
          for j := 0 to oper2.count-1 do
            listatemp.addTerm(oper1.strings[i] + chInt + oper2.strings[j], TwsTerm.Create);
        end;

      3: {*}
        begin
        listatemp := TwsTermList.create;
        for i := 0 to oper1.count-1 do
          for j := 0 to oper2.count-1 do
            begin
            listatemp.addTerm(oper1.strings[i], TwsTerm.Create);
            listatemp.addTerm(oper2.strings[j], TwsTerm.Create);
            listatemp.addTerm(oper1.strings[i] + chInt + oper2.strings[j], TwsTerm.Create);
            end;
        end;

      4: {-}
        begin
        listatemp := TwsTermList.Create;
        i := 0;
        while (i <= oper1.count-1) do
          begin
          j := 0;
          del := FALSE;
          while (j<=oper2.count-1) do
            begin
            if oper1.strings[i] = oper2.strings[j] then del := TRUE;
            inc(j);
            end;
          if not del then
             listatemp.addTerm(oper1.strings[i], TwsTerm.Create);
          inc(i);
          end;
        end;

      5: {/}
        begin
        listatemp := TwsTermList.Create;
        s := '';
        for i := 0 to oper1.count-1 do
          begin
          listatemp.addTerm(oper1.strings[i], TwsTerm.Create);
          s := variaveis(oper1.strings[i], s);
          end;
        for j := 0 to oper2.count-1 do
          listatemp.addTerm(s + chInt + oper2.strings[j], TwsTerm.Create);
        end;

      6: {-*}
        begin
        listatemp := TwsTermList.Create;
        i := 0;
        while (i <= oper1.count-1) do
          begin
          del := FALSE;
          for j := 0 to oper2.count-1 do
            if margem(oper2.strings[j], oper1.strings[i]) then del := TRUE;
          if not del then
             listatemp.addTerm(oper1[i], TwsTerm.Create);
          inc(i);
          end;
        end;

      7: {-/}
        begin
        listatemp := TwsTermList.Create;
        i := 0;
        while (i <= oper1.count-1) do
          begin
          del := FALSE;
          for j := 0 to oper2.count-1 do
            if (margem(oper2.strings[j], oper1.strings[i])) and
               (oper1.strings[i] <> oper2.strings[j]) then del := TRUE;

          if not del then
             listatemp.addTerm(oper1[i], TwsTerm.Create);

          inc(i);
          end;
        end;
      end; {Case}

    oper1.clear;
    oper2.clear;
    oper1.free;
    oper2.free;
    acerta(listatemp);
    stack.push(listatemp);
  end; { opera1 }

function opera2(i: byte; p: pointer): TStringList;
begin
  case i of
    0  :begin
        end;

    1  :begin
        end;
    end;
end; { opera2 }

  { --------------------- Fun��es que implementam a gram�tica ------------------

    Objetivo:
              Refletem, alocam e encadeiam os n�s da �rvore de express�es de
              acordo com a gram�tica abaixo.

              Gram�tica:

                S  -> E;
                E  -> FE�
                E� -> +FE� | -FE� | -*FE� | -/FE� | 0
                F  -> GF�
                F� -> *GF� | 0
                G  -> HG�
                G� -> /HG� | 0
                H  -> IH�
                H� -> .IH� | 0
                I  -> id | (E) | fun��o(E)


    Descri��o:
              O procedimentos recebem o �ndice da tlist e o devolvem junto com
              o ponteiro do n� e se houve erro.

    Entradas:
              p    -> Ponteiro ao n� criado.
              indi -> �ndice na tlist.
              erro -> Tipo de erro encontrado na an�lise;
                      0 se sucesso.

    Sa�das:
            a pr�pria fun��o -> TRUE se sucesso;
                                FALSE se erro;
  }
  function e (var indi:integer):boolean;forward;
  function el(var indi:integer):boolean;forward;
  function f (var indi:integer):boolean;forward;
  function fl(var indi:integer):boolean;forward;
  function g (var indi:integer):boolean;forward;
  function gl(var indi:integer):boolean;forward;
  function h (var indi:integer):boolean;forward;
  function hl(var indi:integer):boolean;forward;
  function i (var indi:integer):boolean;forward;


  function el(var indi: integer): boolean;
  var sind: integer;
  begin
    Result := FALSE;
    If Erro <> 0 Then Exit;

    sind := indi;
    inc(indi);
    with ptoken(arrayptoken[indi])^ do
      if (simb = chInclude) or ( simb = chDelete) or ( simb = '-/') or ( simb = '-*') then
         case estagio of
{$B+}
           sintatico  :if f(indi) and el(indi) then
                          Result := TRUE
                       else
                          ind := sind;
{$B-}
           avaliador  :  begin
                         f(indi);
                         oper2 := stack.pop;
                         oper1 := stack.pop;
                         opera1(simb, oper1, oper2);
                         el(indi);
                         Result := TRUE;
                         end;
           end
        else
           begin
           Result := TRUE;
           indi := sind;
           end;
  end;  { el }

  function fl(var indi: integer): boolean;
  var sind: integer;
  begin
    Result := FALSE;
    If Erro <> 0 Then Exit;

    sind := indi;
    inc(indi);
    with ptoken(arrayptoken[indi])^ do
      if simb = chCross then
         case estagio of
{$B+}
           sintatico  :if g(indi) and fl(indi) then
                          Result := TRUE
                       else
                          ind := sind;
{$B-}
           avaliador  :  begin
                         g(indi);
                         oper2 := stack.pop;
                         oper1 := stack.pop;
                         opera1(simb, oper1, oper2);
                         fl(indi);
                         Result := TRUE;
                         end;
           end
        else
           begin
           Result := TRUE;
           indi := sind;
           end;
  end;  { fl }

  function gl(var indi: integer): boolean;
  var sind: Integer;
  begin
    Result := FALSE;
    If Erro <> 0 Then Exit;

    sind := indi;
    inc(indi);
    with ptoken(arrayptoken[indi])^ do
      if simb = chNested then
         case estagio of
{$B+}
           sintatico  :if h(indi) and gl(indi) then
                          Result := TRUE
                       else
                          ind := sind;
{$B-}
           avaliador  :  begin
                         h(indi);
                         oper2 := stack.pop;
                         oper1 := stack.pop;
                         opera1(simb, oper1, oper2);
                         gl(indi);
                         Result := TRUE;
                         end;
           end
       else
          begin
          Result := TRUE;
          indi := sind;
          end;
  end;  { gl }

  function hl(var indi: integer): boolean;
  var sind: integer;
  begin
    Result := FALSE;
    If Erro <> 0 Then Exit;

    sind := indi;
    inc(indi);
    with ptoken(arrayptoken[indi])^ do
      if simb = chInt then
         case estagio of
{$B+}
           sintatico  :if i(indi) and hl(indi) then
                          Result := TRUE
                       else
                          ind := sind;
{$B-}
           avaliador  :  begin
                         i(indi);
                         oper2 := stack.pop;
                         oper1 := stack.pop;
                         opera1(simb, oper1, oper2);
                         hl(indi);
                         Result := TRUE;
                         end;
           end
       else
           begin
           Result := TRUE;
           indi := sind;
           end;
  end;  { hl }

{$B+}
  function e(var indi: integer): boolean;
  begin
    Result := f(indi) and el(indi);
  end; { e }

  function f(var indi:integer):boolean;
  begin
    Result := g(indi) and fl(indi)
  end; { f }

  function g(var indi:integer):boolean;
  begin
    Result := h(indi) and gl(indi)
  end; { g }

  function h(var indi:integer):boolean;
  begin
    Result := i(indi) and hl(indi)
  end; { h }
{$B-}

  function i(var indi:integer):boolean;
  var sind  : integer;
      s     : string;
      p     : pToken;
  begin
    Result := FALSE;
    If Erro <> 0 Then Exit;

    sind := indi;
    inc(indi);
    with ptoken(arrayptoken[indi])^ do
      begin
      case tipo of
        ttterm: Result := estagio = sintatico;

        ttident:
          begin
          case estagio of
             sintatico: Result := TRUE;
             avaliador:
               begin
               listatemp := TwsTermlist.Create;
               listatemp.addTerm(
                  tab_sim.Strings[ptoken(arrayptoken[indi])^.ind], TwsTerm.Create);
               stack.push(listatemp);
               end;
             end;
          exit;
          end;

        ttfunc:
          begin
          case estagio of
            sintatico:
              begin
              inc(indi);

              if ptoken(arrayptoken[indi])^.tipo = ttopen then
                 if e(indi) then
                    begin
                    inc(indi);
                    If ptoken(arrayptoken[indi])^.tipo = ttclose Then
                       Result := True
                    Else
                       Erro := ENExpected_Close; { If Token = ) }
                    end
              else
                 Erro := ENExpected_Open;

              exit;
              end;

            avaliador:
              begin
              listatemp := TwsTermlist.Create;
              s := tab_fun[ind] + '(';
              inc(indi, 2);

              while ptoken(arrayptoken[indi])^.tipo <> ttclose do
                begin
                p := ptoken(arrayptoken[indi]);
                if p^.tipo = ttident then
                   s := s + tab_sim.strings[p^.ind]
                else
                   if p^.tipo = ttoper then
                      s := s + p^.simb;
                inc(indi);
                end;

              s := s + ')';
              inc(indi);
              listatemp.addTerm(s, TwsTerm.Create);
              stack.push(listatemp);
              end;
            end;
          end; {func}

        ttopen:
          case estagio of
            sintatico:  begin
                        if e(indi) then inc(indi);
                        If Erro = 0 Then
                           if ptoken(arrayptoken[indi])^.tipo = ttclose then
                              Result := TRUE
                           Else
                              Erro := ENExpected_Close
                        Else
                           Erro := ENIdentExpectedAfterOpen;
                           
                        exit;
                        end;

            avaliador:  begin
                        e(indi);
                        inc(indi);
                        exit;
                        end;

            end;
        end; { case }
      end;  { with }

    Erro := ENIdentExpected;
    indi := sind;
  end; { i }

  function s(var indi:integer): boolean;
  begin
    Result := FALSE;
    case estagio of
      sintatico:  if e(indi) then
                     begin
                     inc(indi);
                     If ptoken(arrayptoken.items[indi])^.simb = ';' Then
                        Result := True
                     Else
                        Erro := ENOperatorExpected;
                     end;

      avaliador:  e(indi);
      end;
  end; { s }

  begin
    Erro := 0;
    Result := FALSE;
    indptoken := -1;
    case estagio of
      sintatico  : Result := s(indptoken);

      avaliador  : Begin
                   s(indptoken);
                   Result := True;
                   End;
      end;
  end; { Percorre }

Procedure EvalModel(Expr: String;
                    var Terms, VarList: TStrings;
                    DS: TwsDataSet{ = nil};
                    SortTerms: Boolean{ = true});

var NomesVar: TStringList; // Vari�vel auxiliar que ajuda a identificar s�mbolos repetidos
    Eval: TAvaliator;   // Faz a verifica��o das express�es

  procedure AnalisaVariavel(const s: String);
  var i         : Integer;
      NomeVar   : String;
      Expressao : String;
  begin
    i := System.Pos(':=', s);
    If i = 0 Then
       Raise Exception.Create(
         'Falta o Operador de Atribui��o < := > na Express�o: ' + s);

    NomeVar   := AllTrim(System.Copy(s, 1, i-1));
    Expressao := System.Copy(s, i+2, Length(s));

    If Not SysUtilsEx.IsValidIdent(NomeVar) Then
       Raise Exception.CreateFmt('< %s > n�o � uma Vari�vel V�lida', [NomeVar]);

    Try
      NomesVar.Add(NomeVar);
    Except
      Raise Exception.Create('Vari�vel redefinida: ' + NomeVar);
    End;

    if DS <> NIL then // testa as express�es
       Eval.Expression := Expressao;
  end;

var Erro: Word;
    i: Integer;
Begin                                     
  If Expr = '' Then
     Raise EwsModel.Create('Modelo n�o especificado');

  NomesVar := TStringList.Create;
  NomesVar.Sorted := True;
  NomesVar.Duplicates := dupError;

  if DS <> NIL then
     begin
     Eval := TAvaliator.Create;
     for i := 1 to DS.nCols do
       Eval.TabVar.AddFloat(DS.Struct.Col[i].Name, 0);
     end;

  Init;
  Expr := Expr + ';';
  Erro := 0;
  Try
    Analise_Lexica(Expr, Tab_Sim, Tab_Fun, ArrayPToken, Erro);
    If Erro = 0 Then
       If Percorre(Sintatico,ArrayPToken,Tab_Sim,Tab_Fun,Erro,SortTerms) Then
          If Erro = 0 Then
             Begin
             Percorre(Avaliador,ArrayPToken,Tab_Sim,Tab_Fun,Erro,SortTerms);
             If Erro = 0 Then
                Begin
                Terms   := Stack.Pop;
                VarList := Tab_Sim;
                for i := 0 to VarList.Count - 1 do
                  if VarList.Objects[i] = Pointer(-1) then
                     AnalisaVariavel(VarList[i]); {Rochedo, 25/01/1999}
                End;
             End
          Else {Avaliador}
             ShowException(Erro, '')
       Else {Sintatico}
          ShowException(Erro, '');
  Finally
    if DS <> NIL then Eval.Free;
    NomesVar.Free;
    LiberaMem;
  End;
End; {EvalModel}

end.

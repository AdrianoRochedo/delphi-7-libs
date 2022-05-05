unit Lib_xxx;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses drBase;

  procedure API(Lib: TLib);

type
  TVec_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
    class procedure am_InvVec(Const Func_Name: String; Stack: TexeStack);
    class procedure am_ProdEscVec(Const Func_Name: String; Stack: TexeStack);
    class procedure am_ProdVec(Const Func_Name: String; Stack: TexeStack);
    class procedure am_SubVec(Const Func_Name: String; Stack: TexeStack);
    class procedure am_SumVec(Const Func_Name: String; Stack: TexeStack);
    class procedure am_TranspVec(Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_wsVec = class(nil)
    procedure AddMethods; override;
    class procedure am_FindGT(Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindGTE(Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindLT(Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindLTE(Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindMinMean(Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindMinSun(Const Func_Name: String; Stack: TexeStack);
    class procedure am_Max(Const Func_Name: String; Stack: TexeStack);
    class procedure am_Min(Const Func_Name: String; Stack: TexeStack);
    class procedure am_PolyFit(Const Func_Name: String; Stack: TexeStack);
    class procedure am_REC(Const Func_Name: String; Stack: TexeStack);
  end;

implementation

  procedure API(Lib: TLib);
  begin
    TVec_Functions.AddFunctionsIn(Lib.Functions);
    Tps_wsVec.Create(TwsVec,
                     nil,
                     '',
                     Vetores,
                     [],
                     [],
                     [],
                     False,
                     Lib.Classes);
  end;

{ TVec_Functions }

class procedure TVec_Functions.am_InvVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(InvVec(TwsVec(Stack.AsObject(1))))
end;

class procedure TVec_Functions.am_ProdEscVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    ProdEscVec(
      Stack.AsFloat(1),
      TwsVec(Stack.AsObject(2))))
end;

class procedure TVec_Functions.am_ProdVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    ProdVec(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2))))
end;

class procedure TVec_Functions.am_SubVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    SubVec(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2))))
end;

class procedure TVec_Functions.am_SumVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    SumVec(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2))))
end;

class procedure TVec_Functions.am_TranspVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TranspVec(TwsVec(Stack.AsObject(1))))
end;

class procedure TVec_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('InvVec',
        'Inverte cada elemento de um vetor, isto é, realiza a operação 1/x para cada elemento.',
        'Vetores',
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsDFVec,
        am_InvVec);

    Add('ProdEscVec',
        'Realiza o produto entre um escalar e um vetor',
        'Vetores',
        [pvtReal, pvtObject],
        [nil, TwsVec],
        [False, True],
        pvtObject,
        TwsDFVec,
        am_ProdEscVec);

    Add('ProdVec',
        'Multiplica dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [False, False],
        pvtObject,
        TwsDFVec,
        am_ProdVec);

    Add('SubVec',
        'Subtrai dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [False, False],
        pvtObject,
        TwsDFVec,
        am_SubVec);

    Add('SumVec',
        'Soma dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [False, False],
        pvtObject,
        TwsDFVec,
        am_SumVec);

    Add('TranspVec',
        'Transforma um vetor linha em um vetor coluna ou vice-versa',
        'Vetores',
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsDFVec,
        am_TranspVec);

    end;
end;

{ Tps_wsVec }

class procedure Tps_wsVec.am_FindGT(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindGT(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindGTE(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindGTE(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindLT(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindLT(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindLTE(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindLTE(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindMinMean(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindMinMean(Stack.AsInteger(1)))
end;

class procedure Tps_wsVec.am_FindMinSun(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.FindMinSun(Stack.AsInteger(1)))
end;

class procedure Tps_wsVec.am_Max(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(1));
  Stack.PushFloat(o.Max)
end;

class procedure Tps_wsVec.am_Min(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(1));
  Stack.PushFloat(o.Min)
end;

class procedure Tps_wsVec.am_PolyFit(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(4));
  Stack.PushObject(
    o.PolyFit(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2)),
      Stack.AsInteger(3)));
end;

class procedure Tps_wsVec.am_REC(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.AsObject(2));
  Stack.PushObject(o.REC(TwsVec(Stack.AsObject(1))))
end;

procedure Tps_wsVec.AddMethods;
begin
  with Procs do
    begin
    end;

  with Functions do
    begin
    Add('FindGT',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são maiores que uma valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindGT);

    Add('FindGTE',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são maiores ou iguais a um valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindGTE);

    Add('FindLT',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são menores que uma valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindLT);

    Add('FindLTE',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são menores ou iguais a um valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindLTE);

    Add('FindMinMean',
        'Procura uma sequência contínua de um vetor linha, cuja média dos elementos seja mínima. '#13 +
        'Retorna um vetor com as n posições do vetor original.',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindMinMean);

    Add('FindMinSun',
        'Procura uma sequência contínua de um vetor linha, cuja soma dos elementos seja mínima. '#13 +
        'Retorna um vetor com as n posições do vetor original.',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TwsSIVec,
        am_FindMinSun);

    Add('Max',
        'Retorna o maior valor encontrado no vetor',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        am_Max);

    Add('Min',
        'Retorna o menor valor encontrado no vetor',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        am_Min);

    Add('PolyFit',
        'Realiza uma regressão polinomial a um conjunto de pontos.'#13 +
        'Sintaxe: PolyFit(X, Y, n)'#13 +
        '  - X: Vetor com as abcissas dos pontos'#13 +
        '  - Y: Vetor com as ordenadas dos pontos'#13 +
        '  - n: Grau do polinômio'#13 +
        ''#13 +
        'Retorna um vetor com n elementos, sendo que os n-1 elementos contém os coeficientes e '#13 +
        'o último valor contém a correlação obtida no ajuste.',
        '',
        [pvtObject, pvtObject, pvtInteger],
        [TwsVec, TwsVec, nil],
        [True, True, False],
        pvtObject,
        TwsDFVec,
        am_PolyFit);

    Add('REC',
        'Dado um vetor com índices, é retornado um vetor com os valores que '#13 +
        'correspondem a estes índices.',
        '',
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsDFVec,
        am_REC);

    end;
end;

end.

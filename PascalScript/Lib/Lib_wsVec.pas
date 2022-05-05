{ ultima versao }
unit Lib_wsVec;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Classes,
     SysUtils,
     OutPut,
     wsVec;

const
  cCatVetores = 'WinStat (vetores)';

type
  TVector_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
    class procedure am_ProdEscVec (Const Func_Name: String; Stack: TexeStack);
    class procedure am_ProdVec    (Const Func_Name: String; Stack: TexeStack);
    class procedure am_SubVec     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_SumVec     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_TranspVec  (Const Func_Name: String; Stack: TexeStack);
  end;

  TVector_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn(Procs: TProcList); override;
  end;

  // Classes Abstrata
  Tps_wsVec = class(TpsClass)
  public
    procedure AddMethods; override;

    // ---------------procedimentos
    class procedure amVecPrint     (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSetData   (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecAdd       (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecDelete    (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSort      (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecFill      (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSaveToFile(Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSetLen    (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecCopy      (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecCopyEx    (const Func_Name: String; Stack: TexeStack);
    class procedure amVecMinMax    (Const Func_Name: String; Stack: TexeStack);

 (*
    class procedure amVecLocate    (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecVarMean   (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSumsAndSquares    (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecMeanAndStdDev     (Const Func_Name: String; Stack: TexeStack);
 *)

    class procedure amVecGetData     (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecIsMissValue (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecGetLen      (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecGetAsInteger(Const Func_Name: String; Stack: TexeStack);
    class procedure amVecGetName     (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSetName(const Func_Name: String; Stack: TexeStack);

    class procedure amVecAccum       (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecInv         (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecMean        (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecTotal       (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSumOfSq     (Const Func_Name: String; Stack: TexeStack);

    class procedure am_FindGT      (Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindGTE     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindLT      (Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindLTE     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindMinMean (Const Func_Name: String; Stack: TexeStack);
    class procedure am_FindMinSun  (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Max         (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Min         (Const Func_Name: String; Stack: TexeStack);
    class procedure am_REC         (Const Func_Name: String; Stack: TexeStack);
(*
    {05/07/00}
    class procedure amVecMinValue      (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecMaxValue      (Const Func_Name: String; Stack: TexeStack);
    {06/07/00}
    class procedure amVecSum           (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecSumOfSquares  (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecStdDev        (Const Func_Name: String; Stack: TexeStack);
*)
  end;


  Tps_wsSFVec = class(Tps_wsVec)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
  end;


  Tps_wsDFVec = class(Tps_wsVec)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
  end;


  Tps_wsSIVec = class(Tps_wsVec)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
  end;


  Tps_wsLIVec = class(Tps_wsVec)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
  end;


{ TVector_Functions }

class procedure TVector_Functions.am_ProdEscVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    VecScalarProd(TwsVec(Stack.AsObject(2)), Stack.AsFloat(1), True)
  )
end;

class procedure TVector_Functions.am_ProdVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    VecProd(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2)),
      True))
end;

class procedure TVector_Functions.am_SubVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    VecSub(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2)),
      True))
end;

class procedure TVector_Functions.am_SumVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    VecSum(
      TwsVec(Stack.AsObject(1)),
      TwsVec(Stack.AsObject(2)),
      True))
end;

class procedure TVector_Functions.am_TranspVec(Const Func_Name: String; Stack: TexeStack);
begin
//  Stack.PushObject(TranspVec(TwsVec(Stack.AsObject(1))))
end;

class procedure TVector_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('ProdEscVec',
        'Realiza o produto entre um escalar e um vetor',
        'Vetores',
        [pvtReal, pvtObject],
        [nil, TwsVec],
        [False, True],
        pvtObject,
        TwsVec,
        am_ProdEscVec);

    Add('VecProd',
        'Multiplica dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [true, true],
        pvtObject,
        TwsVec,
        am_ProdVec);

    Add('VecSub',
        'Subtrai dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [true, true],
        pvtObject,
        TwsVec,
        am_SubVec);

    Add('VecSum',
        'Soma dois vetores de comprimentos iguais',
        'Vetores',
        [pvtObject, pvtObject],
        [TwsVec, TwsVec],
        [true, true],
        pvtObject,
        TwsVec,
        am_SumVec);
{
    Add('TranspVec',
        'Transforma um vetor linha em um vetor coluna ou vice-versa',
        'Vetores',
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsVec,
        am_TranspVec);
}
    end;
end;

{ TVector_Procs }

class procedure TVector_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    end;
end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TVector_Functions.AddFunctionsIn(Lib.Functions);
  TVector_Procs.AddProcsIn(Lib.Procs);

  Tps_wsVec.Create(TwsVec,
                   nil,
                   'Classe base para as classes de vetores',
                   cCatVetores,
                   [], [], [],
                   False,
                   Lib.Classes);

  Tps_wsSFVec.Create(TwsSFVec,
                     TwsVec,
                     'Encapsula um vetor de N elementos com índice inicial em 1'#13 +
                     'Armazena números reais de 4 bytes',
                     cCatVetores,
                     [pvtInteger], [nil], [False],
                     True,
                     Lib.Classes);

  Tps_wsDFVec.Create(TwsDFVec,
                     TwsVec,
                     'Encapsula um vetor de N elementos com índice inicial em 1'#13 +
                     'Armazena números reais de 8 bytes',
                     cCatVetores,
                     [pvtInteger], [nil], [False],
                     True,
                     Lib.Classes);

  Tps_wsSIVec.Create(TwsSIVec,
                     TwsVec,
                     'Encapsula um vetor de N elementos com índice inicial em 1'#13 +
                     'Armazena números inteiros de 2 bytes',
                     cCatVetores,
                     [pvtInteger], [nil], [False],
                     True,
                     Lib.Classes);

  Tps_wsLIVec.Create(TwsLIVec,
                     TwsVec,
                     'Encapsula um vetor de N elementos com índice inicial em 1'#13 +
                     'Armazena números inteiros de 4 bytes',
                     cCatVetores,
                     [pvtInteger], [nil], [False],
                     True,
                     Lib.Classes);
end;

{ Tps_wsVec }

procedure Tps_wsVec.AddMethods;
begin
  with Procs do
    begin
    Add('Print',
        'Mostra um vetor em uma saída padrão (OutPut)'#13 +
        'Parâmetros: OutPut',
        '',
        [pvtObject],      // OutPut
        [TOutPut  ],      // Subtipo dos parâmetros se forem objetos
        [True     ],      // Referência
        pvtNull   ,       // Procedimentos não possuem retorno
        TObject   ,       // Tipo da classe caso o resultado seja Object
        amVecPrint);      // Endereço do método de acesso

    Add('Set',
        'Atribui um valor a um elemento de um vetor'#13 +
        'Parâmetros: Índice do elemento, Valor',
        '',
        [pvtInteger, pvtReal],
        [nil       , nil    ],
        [False     , False  ],
        pvtNull    ,
        TObject    ,
        amVecSetData);

    Add('Add',
        'Adiciona um elemento no vetor',
        '',
        [pvtReal   ],
        [nil       ],
        [False     ],
        pvtNull    ,
        TObject    ,
        amVecAdd);

    Add('Delete',
        'Elimina N valores do vetor a partir de um índice'#13 +
        'Parâmetros: Índice Inicial, Quantidade',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil    ],
        [False     , False  ],
        pvtNull        ,
        TObject    ,
        amVecDelete);

    Add('Fill',
        'Preenche o vetor com um valor específico',
        '',
        [pvtReal  ],
        [nil],
        [False    ],
        pvtNull   ,
        TObject   ,
        amVecFill);

    Add('MinMax',
        'Retorna os valores mínimo e máximo do vetor '#13 +
        'Parâmetros:'#13 +
        '  Min: Variável que receberá o valor mínimo'#13 +
        '  Max: Variável que receberá o valor máximo',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil    ],
        [True      , True   ],
        pvtNull    ,
        TObject    ,
        amVecMinMax);

{
    Add('SaveToFile',
        'Grava o objeto no arquivo especificado  '#13 +
        'Parâmetros: Nome do arquivo onde o objeto será armazenado ',
        '',
        [pvtString  ],
        [nil],
        [False    ],
        pvtNull   ,
        TObject   ,
        amVecSavetoFile);
}
    Add('SetLen',
        'Especifica um novo tamanho para o vetor',
        '',
        [pvtInteger],
        [nil],
        [False     ],
        pvtNull   ,
        TObject   ,
        amVecSetLen);

(* <<<
    Add('Locate',
        'Retorna uma posição no vetor relativa ao um valor especificado'#13 +
        'Parâmetros: x: Valor a pesquisar ; Ind: Índice relativo ao valor pesquisado ',
        '',
        [pvtReal  , pvtInteger],
        [nil      , nil       ],
        [False    , True      ],
        pvtNull   ,
        TObject   ,
        amVecLocate);

    Add('VarMean',
        'Determina a media dos valores do vetor especificado '#13 +
        'Parâmetros: n: Número de valores válidos',
        '',
        [pvtReal  ,pvtReal     ,pvtInteger],
        [nil      ,nil         ,nil       ],
        [True     ,True        ,True      ],
        pvtNull   ,
        TObject   ,
        amVecVarMean);

      {06/07/00}
      Add('SumsAndSquares',
        '',
        '',
        [pvtReal  ,pvtReal     ],
        [nil      ,nil         ],
        [True     ,True        ],
        pvtNull   ,
        TObject   ,
        amVecSumsAndSquares);

      Add('MeanAndStdDev',
        'Calcula Média e Desvio Padrão ',
        '',
        [pvtReal  ,pvtReal ],
        [nil      ,nil     ],
        [True     ,True    ],
        pvtNull   ,
        TObject   ,
        amVecMeanAndStdDev);
*)
    end;
{ ---------FUNCTIONS-----------}

  with Functions do
    begin
    Add('Copy',
        'Retorna uma cópia do vetor',
        '',
        [],
        [],
        [],
        pvtObject,
        TwsVec,
        amVecCopy);

    Add('CopyEx',
        'Retorna uma cópia parcial do vetor',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil       ],
        [false     , false     ],
        pvtObject,
        TwsVec,
        amVecCopyEx);

    Add('Get',
        'Retorna o valor armazenado em um elemento do vetor'#13 +
        'Parâmetros: Índice do elemento',
        '',
        [pvtInteger],
        [nil       ],
        [False     ],
        pvtReal    ,
        TObject    ,
        amVecGetData);

   Add('IsMissValue',
       'Verifica se um valor é válido'#13 +
       'Parâmetros: Índice, Valor (Saída)',
        '',
        [pvtInteger, pvtReal],
        [nil       ,nil     ],
        [False     ,True    ],
        pvtBoolean ,
        TObject    ,
        amVecIsMissValue);

    Add('GetLen',
        'Obtém o tamanho atual do vetor',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject   ,
        amVecGetLen);

    Add('GetAsInteger',
        'Obtém um valor como um inteiro'#13 +
        'Parâmetros: Índice',
        '',
        [pvtInteger ],
        [nil        ],
        [False      ],
        pvtInteger   ,
        TObject      ,
        amVecGetAsInteger);

    Add('GetName',
        'Retorna o nome do vetor',
        '',
        [],
        [],
        [],
        pvtString  ,
        TObject    ,
        amVecGetName);

     Add('Mean',
        'Obtém a média dos valores armazenados'#13 +
        'parametro: Número de valores válidos (Saída)',
        '',
        [pvtInteger ],
        [nil        ],
        [True       ],
        pvtReal   ,
        TObject    ,
        amVecMean);

     Add('Sort',
         'Ordena o vetor em ordem crescente ou decrescente'#13 +
         'Parâmetros:'#13 +
         '  NewVec: TRUE = Cria um novo vetor / FALSE = Ordena o próprio vetor'#13 +
         '  Order: TRUE = Crescente / FALSE = Decrescente',
         '',
         [pvtBoolean, pvtBoolean],
         [nil       , nil       ],
         [false     , false     ],
         pvtObject  ,
         TwsVec     ,
         amVecSort);

      Add('Sum',
        'Determina o somatório dos valores armazenados'#13 +
        'Parâmetros: Número de valores válidos (Saída)',
        '',
        [pvtInteger ],
        [nil        ],
        [True       ],
        pvtReal    ,
        TObject    ,
        amVecTotal);

    Add('SumOfSq',
        'Determina a "soma de quadrados" dos valores armazenados'#13 +
        'parametro: Número de valores válidos (Saída)',
        '',
        [pvtInteger ],
        [nil        ],
        [True       ],
        pvtReal    ,
        TObject    ,
        amVecSumOfSq);

    Add('Accum',
        'Retorna um vetor com os valores acumulados até a posição considerada'#13 +
        'Parâmetro:'#13 +
        '  NewVec: Se True um novo vetor é criado para armazenar os resultados, senão'#13 +
        '          os valores acumulados substituem os valores originais',
        '',
        [pvtBoolean ],
        [nil        ],
        [False      ],
        pvtObject   ,
        TwsVec      ,
        amVecAccum);

    Add('Inv',
        'Retorna o inverso de um vetor, isto é, 1/x'#13 +
        'Parâmetro:'#13 +
        '  NewVec: Se True um novo vetor é criado para armazenar os resultados, senão'#13 +
        '          o proprio vetor será invertido',
        '',
        [pvtBoolean ],
        [nil        ],
        [False      ],
        pvtObject   ,
        TwsVec      ,
        amVecAccum);

    Add('FindGT',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são maiores que uma valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        am_FindGT);

    Add('FindGTE',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são maiores ou iguais a um valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        am_FindGTE);

    Add('FindLT',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são menores que uma valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        am_FindLT);

    Add('FindLTE',
        'Retorna um vetor com os índices dos elementos cujos valores'#13 +
        'são menores ou iguais a um valor pré-determinado.',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        am_FindLTE);

    Add('FindMinMean',
        'Procura uma sequência contínua de um vetor linha, cuja média dos elementos seja mínima. '#13 +
        'Retorna um vetor com as n posições do vetor original.',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        am_FindMinMean);

    Add('FindMinSun',
        'Procura uma sequência contínua de um vetor linha, cuja soma dos elementos seja mínima. '#13 +
        'Retorna um vetor com as n posições do vetor original.',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TwsVec,
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

    Add('REC',
        'Dado um vetor com índices, é retornado um vetor com os valores que '#13 +
        'correspondem a estes índices.',
        '',
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsVec,
        am_REC);
(*
    Add('MinValue',
        'Retorna o menor valor armazenado',
        '',
        [],
        [],
        [],
        pvtReal   ,
        TObject   ,
        amVecMinValue);

    Add('MaxValue',
        'Retorna o maior valor armazenado',
        '',
        [],
        [],
        [],
        pvtReal   ,
        TObject   ,
        amVecMaxValue);

      Add('StdDev',
        'Retorna o Desvio Padrão dos valores armazenados',
        '',
        [],
        [],
        [],
        pvtReal    ,
        TObject    ,
        amVecStdDev);

     Add('Variance',
        'Retorna a Variância dos valores armazenados',
        '',
        [],
        [],
        [],
        pvtReal    ,
        TObject    ,
        amVecVariance);
*)
    end;
end;

class procedure Tps_wsVec.amVecGetData(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(TwsVec(Stack.getSelf).Data[Stack.AsInteger(1)]);
end;

class procedure Tps_wsVec.amVecPrint(const Func_Name: String; Stack: TexeStack);
var V: TwsVec;
begin
  V := TwsVec(Stack.getSelf);
  V.Print(TOutPut(Stack.AsObject(1)).Text);
end;

class procedure Tps_wsVec.amVecSetData(const Func_Name: String; Stack: TexeStack);
begin
  with Stack do
    TwsVec(getSelf).Data[AsInteger(1)] := AsFloat(2);
end;

class procedure Tps_wsVec.amVecAdd(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).Add(Stack.AsFloat(1));
end;

class procedure Tps_wsVec.amVecDelete(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).Delete(Stack.AsInteger(1), Stack.AsInteger(2));
end;

class procedure Tps_wsVec.amVecSort(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TwsVec(Stack.getSelf).Sort(Stack.AsBoolean(1), Stack.AsBoolean(2))
  );
end;

class procedure Tps_wsVec.amVecIsMissValue(const Func_Name: String; Stack: TexeStack);
var b: Boolean;
    r: Double;
begin
  b := TwsVec(Stack.getSelf).IsMissValue(Stack.AsInteger(1), r);
  Stack.AsReferency(2).Value := r;
  Stack.PushBoolean(b);
end;

class procedure Tps_wsVec.amVecFill(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).Fill(Stack.AsFloat(1));
end;

class procedure Tps_wsVec.amVecSaveToFile(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).SaveToFile(Stack.AsString(1));
end;

{------------------------------amVecLocate-------------------------------------}
(* <<<
class procedure Tps_wsVec.amvecLocate(const Func_Name: String; Stack: TexeStack);
var i: Integer;
begin
  TwsVec(Stack.AsObject(3)).Locate(Stack.AsFloat(1), i);
  Stack.AsReferency(2).Value := i;
end;
*)
{------------------------------amVecMinMax-------------------------------------}

class procedure Tps_wsVec.amvecMinMax(const Func_Name: String; Stack: TexeStack);
var Min, Max: Double;
begin
   TwsVec(Stack.getSelf).MinMax(Min, Max);
   Stack.AsReferency(1).Value := Min;
   Stack.AsReferency(2).Value := Max;
end;

(* <<<
{------------------------------amVecVarMean------------------------------------}
class procedure Tps_wsVec.amvecVarMean(const Func_Name: String; Stack: TexeStack);
var r   : Integer;
    s,t : Real;
begin
   TwsVec(Stack.getSelf).VarMean(r, s, t);
   Stack.AsReferency(1).Value := r;
   Stack.AsReferency(2).Value := s;
   Stack.AsReferency(3).Value := t;
end;
*)

class procedure Tps_wsVec.amVecMean(const Func_Name: String; Stack: TexeStack);
var r: Real;
    i: Integer;
begin
  r := TwsVec(Stack.getSelf).Mean(i);
  Stack.AsReferency(1).Value := i;
  Stack.PushFloat(r);
end;

class procedure Tps_wsVec.amVecTotal(const Func_Name: String; Stack: TexeStack);
var r: Real;
    i: Integer;
begin
  r := TwsVec(Stack.getSelf).Total(i);
  Stack.AsReferency(1).Value := i;
  Stack.PushFloat(r);
end;

class procedure Tps_wsVec.amVecSumOfSq(const Func_Name: String; Stack: TexeStack);
var r: Real;
    i: Integer;
begin
  r := TwsVec(Stack.getSelf).SumOfSq(i);
  Stack.AsReferency(1).Value := i;
  Stack.PushFloat(r);
end;

class procedure Tps_wsVec.amVecGetLen(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TwsVec(Stack.getSelf).Len);
end;

class procedure Tps_wsVec.amVecSetLen(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).Len := Stack.AsInteger(1);
end;

class procedure Tps_wsVec.amVecGetAsInteger(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TwsVec(Stack.getSelf).AsInteger[Stack.AsInteger(1)]);
end;

class procedure Tps_wsVec.amVecGetName(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(TwsVec(Stack.getSelf).Name);
end;

class procedure Tps_wsVec.amVecSetName(const Func_Name: String; Stack: TexeStack);
begin
  TwsVec(Stack.getSelf).Name := Stack.AsString(1);
end;

(* <<<
{--------------------------amVecSumsAndSquares------------------------------------}
class procedure Tps_wsVec.amVecSumsAndSquares(const Func_Name: String; Stack: TexeStack);
var r1, r2 : Integer;
begin
   TwsVec(Stack.getSelf).SumsAndSquares(r1, r2);
   Stack.AsReferency(1).Value := r1;
   Stack.AsReferency(2).Value := r2;
end;
{----------------------------amVecMeanAndStdDev------------------------------------}
class procedure Tps_wsVec.amVecMeanAndStdDev(const Func_Name: String; Stack: TexeStack);
begin
  {falta fazer}
end;

{--------------------------amVecMean------------------------------------}
class procedure Tps_wsVec.amVecMean(const Func_Name: String; Stack: TexeStack);
var v: TwsVec;
begin
  v := TwsVec(Stack.getSelf);
  Stack.PushFloat( v.Mean(v.DataArray) );
end;

{--------------------------amVecMinValue------------------------------------}
class procedure Tps_wsVec.amVecMinValue(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( TwsVec(Stack.getSelf).MinValue );
end;

{--------------------------amVecMaxValue------------------------------------}
class procedure Tps_wsVec.amVecMaxValue(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( TwsVec(Stack.getSelf).MaxValue );
end;
*)

class procedure Tps_wsVec.amVecAccum(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsVec(Stack.getSelf).Accum(Stack.AsBoolean(1)));
end;

class procedure Tps_wsVec.amVecCopy(const Func_Name: String; Stack: TexeStack);
var v: TwsVec;
begin
  v := TwsVec(Stack.getSelf);
  Stack.PushObject( v.Copy(1, v.Len) );
end;

class procedure Tps_wsVec.amVecCopyEx(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject( TwsVec(Stack.getSelf).Copy(Stack.asInteger(1), Stack.asInteger(2)) );
end;

class procedure Tps_wsVec.am_FindGT(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindGT(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindGTE(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindGTE(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindLT(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindLT(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindLTE(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindLTE(Stack.AsFloat(1)))
end;

class procedure Tps_wsVec.am_FindMinMean(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindMinMean(Stack.AsInteger(1)))
end;

class procedure Tps_wsVec.am_FindMinSun(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.FindMinSun(Stack.AsInteger(1)))
end;

class procedure Tps_wsVec.am_Max(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushFloat(o.Max)
end;

class procedure Tps_wsVec.am_Min(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushFloat(o.Min)
end;

class procedure Tps_wsVec.am_REC(Const Func_Name: String; Stack: TexeStack);
var o: TwsVec;
begin
  o := TwsVec(Stack.getSelf);
  Stack.PushObject(o.REC(TwsVec(Stack.AsObject(1))))
end;

class procedure Tps_wsVec.amVecInv(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsVec(Stack.getSelf).Inv(Stack.AsBoolean(1)));
end;

{ Tps_wsSFVec }

procedure Tps_wsSFVec.AddMethods;
begin
  // ...
end;

function Tps_wsSFVec.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TwsSFVec.Create(Stack.AsInteger(1));
end;

{ Tps_wsDFVec }

procedure Tps_wsDFVec.AddMethods;
begin
  // ...
end;

function Tps_wsDFVec.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TwsDFVec.Create(Stack.AsInteger(1));
end;

{ Tps_wsSIVec }

procedure Tps_wsSIVec.AddMethods;
begin
  // ...
end;

function Tps_wsSIVec.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TwsSIVec.Create(Stack.AsInteger(1));
end;

{ Tps_wsLIVec }

procedure Tps_wsLIVec.AddMethods;
begin
  //...
end;

function Tps_wsLIVec.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TwsLIVec.Create(Stack.AsInteger(1));
end;

end.

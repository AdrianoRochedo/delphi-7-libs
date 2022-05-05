unit Lib_wsMatrix;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Classes,
     SysUtils,
     OutPut,
     Lib_Windows,
     wsVec,
     wsMatrix;

const
  cCatMatrizes = 'WinStat (matrizes)';

type
  TMatrix_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
  end;

  TMatrix_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn(Procs: TProcList); override;
  end;

  // Classes Abstrata
  Tps_wsMatrix = class(TpsClass)
  public
    procedure AddMethods; override;

    // gerenciamento dos dados
    class procedure amMatrixPrint   (Const Func_Name: String; Stack: TexeStack);
    class procedure amMatrixSetData (Const Func_Name: String; Stack: TexeStack);
    class procedure amMatrixGetData (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetRow        (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetRow        (Const Func_Name: String; Stack: TexeStack);
    //class procedure amSetPrecision  (Const Func_Name: String; Stack: TexeStack);
    //class procedure amGetPrecision  (Const Func_Name: String; Stack: TexeStack);
    //class procedure amCopy          (Const Func_Name: String; Stack: TexeStack);
    class procedure amIsMissValue   (Const Func_Name: String; Stack: TexeStack);
    class procedure amSaveToFile    (Const Func_Name: String; Stack: TexeStack);
    class procedure amLoadFromFile  (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetAsString   (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetAsstring   (Const Func_Name: String; Stack: TexeStack);

    // procedimentos matem�ticos
    class procedure amMatrixPower   (Const Func_Name: String; Stack: TexeStack);
    class procedure amInv           (Const Func_Name: String; Stack: TexeStack);
    class procedure amTranspose     (Const Func_Name: String; Stack: TexeStack);
    class procedure amMult          (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecMult       (Const Func_Name: String; Stack: TexeStack);
    class procedure amVecKronecker  (Const Func_Name: String; Stack: TexeStack);
    class procedure amKronecker     (Const Func_Name: String; Stack: TexeStack);
    class procedure amColKronecker  (Const Func_Name: String; Stack: TexeStack);
    class procedure amHorKronecker  (Const Func_Name: String; Stack: TexeStack);
    class procedure amTranspMul1    (Const Func_Name: String; Stack: TexeStack);

    //class procedure amSubMatrix     (Const Func_Name: String; Stack: TexeStack);
    //class procedure amDesStat       (Const Func_Name: String; Stack: TexeStack);
    //class procedure amRowReduc      (Const Func_Name: String; Stack: TexeStack);
    //class procedure amColReduc      (Const Func_Name: String; Stack: TexeStack);
    //class procedure amRowColReduc   (Const Func_Name: String; Stack: TexeStack);

    // gerenciamento da estrutura
    class procedure amDeleteCol     (Const Func_Name: String; Stack: TexeStack);
    class procedure amMadd          (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetNRows      (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetNCols      (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetNRows      (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetNCols      (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetRowName    (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetColName    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetRowName    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetColName    (Const Func_Name: String; Stack: TexeStack);
    class procedure amMInsert       (Const Func_Name: String; Stack: TexeStack);
    class procedure amMDelete       (Const Func_Name: String; Stack: TexeStack);
    class procedure amExchange      (Const Func_Name: String; Stack: TexeStack);
    //class procedure amSubSymMat     (Const Func_Name: String; Stack: TexeStack);
    //class procedure amSubIndex      (Const Func_Name: String; Stack: TexeStack);

    class procedure amGetMLab       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetMLab       (Const Func_Name: String; Stack: TexeStack);
    //class procedure amPerc          (Const Func_Name: String; Stack: TexeStack);
    //class procedure amMatOrderStat  (Const Func_Name: String; Stack: TexeStack);
    //class procedure amToVec         (Const Func_Name: String; Stack: TexeStack);
    //class procedure amRankTie       (Const Func_Name: String; Stack: TexeStack);
    //class procedure amRank          (Const Func_Name: String; Stack: TexeStack);
    //class procedure amColVecConcat  (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_wsGeneral = class(Tps_wsMatrix)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;

    class procedure amSortRows (Const Func_Name: String; Stack: TexeStack);
{
    class procedure amBalanc   (Const Func_Name: String; Stack: TexeStack);
    class procedure amAppGiv   (Const Func_Name: String; Stack: TexeStack);

    class procedure amMakeGiv  (Const Func_Name: String; Stack: TexeStack);
    class procedure amAppGiv   (Const Func_Name: String; Stack: TexeStack);
    class procedure amSolve    (Const Func_Name: String; Stack: TexeStack);
    class procedure amHFTCov   (Const Func_Name: String; Stack: TexeStack);
    class procedure amHFTICov  (Const Func_Name: String; Stack: TexeStack);
    class procedure amRevSweep (Const Func_Name: String; Stack: TexeStack);
    class procedure amMatOrder (Const Func_Name: String; Stack: TexeStack);
    class procedure amBackSubst(Const Func_Name: String; Stack: TexeStack);

   //Fun��es
    class procedure amLUDet       (Const Func_Name: String; Stack: TexeStack);
    class procedure amMoorePenrose(Const Func_Name: String; Stack: TexeStack);
    class procedure amKeyMeans    (Const Func_Name: String; Stack: TexeStack);
    class procedure amColConst    (Const Func_Name: String; Stack: TexeStack);
    class procedure amLUToInv     (Const Func_Name: String; Stack: TexeStack);
    class procedure amHesQR       (Const Func_Name: String; Stack: TexeStack);
}
  end;
{
 Tps_wsSymmetric = class(Tps_wsMatrix)
 public
    procedure AddMethods; override;
    class procedure amSweepApp   (Const Func_Name: String; Stack: TexeStack);
    class procedure amSweep      (Const Func_Name: String; Stack: TexeStack);
    class procedure amCorrMat    (Const Func_Name: String; Stack: TexeStack);
  end;
}
  { TMatrix_Functions }

class procedure TMatrix_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    end;
end;

{ TMatrix_Procs }

class procedure TMatrix_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    end;
end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TMatrix_Functions.AddFunctionsIn(Lib.Functions);
  TMatrix_Procs.AddProcsIn(Lib.Procs);

  Tps_wsMatrix.Create(TwsMatrix,
                      nil,
                      'Classe base para as matrizes',
                      cCatMatrizes,
                      [], [], [],
                      False,
                      Lib.Classes);

  Tps_wsGeneral.Create(TwsGeneral,
                       TwsMatrix,
                       'Encapsula uma matriz Geral N x N',
                       cCatMatrizes,
                       [pvtInteger, pvtInteger],
                       [nil       , nil       ],
                       [False     , False     ],
                       True,
                       Lib.Classes);
end;

{ Tps_wsMatrix }

procedure Tps_wsMatrix.AddMethods;
begin
  with Procs do
    begin
    Add('Print',
        'Mostra uma matriz em uma sa�da (OutPut)'#13 +
        'Par�metros: OutPut',
        '',
        [pvtObject],
        [TOutPut  ],
        [True     ],
        pvtNull   ,
        TObject   ,
        amMatrixPrint);

    Add('Set',
        'Atribui um valor a uma C�lula de uma matriz'#13 +
        'Par�metros: Linha, Coluna, Valor',
        '',
        [pvtInteger, pvtInteger, pvtReal],
        [nil       , nil       , nil    ],
        [False     , False     , False  ],
        pvtNull   ,
        TObject   ,
        amMatrixSetData);

    Add('DeleteCol',
        'Remove uma coluna',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amDeleteCol);

    Add('Add',
        'Insere uma linha no final da matriz',
        cCatMatrizes,
        [pvtObject],
        [TwsVec],
        [True],
        pvtNull,
        TObject,
        amMadd);

      Add('Inv',
        'Inverte a matriz atrav�s do m�todo de Gauss-Jordan com pivoteamento total',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [true],
        pvtNull,
        TObject,
        amInv);

    Add('SetNRows',
        '',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetNrows);

    Add('SetNCols',
        '',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetNCols);
{
    Add('SetPrecision',
        '',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetPrecision);
}
     Add('SetAsString',
        '',
        cCatMatrizes,
        [pvtInteger, pvtInteger, pvtString],
        [nil, nil, nil],
        [False, False, False],
        pvtNull,
        TObject,
        amSetAsString);

    Add('SetRow',
        '',
        cCatMatrizes,
        [pvtInteger, pvtObject],
        [nil, TwsVec],
        [False, True],
        pvtNull,
        TObject,
        amSetRow);

   Add('SaveToFile',
        'Salva os dados de uma metrix em um arquivo',
        cCatMatrizes,
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSaveToFile);

   Add('SetMLab',
        '',
        cCatMatrizes,
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetMLab);

    Add('SetRowName',
        'Atribui um nome � linha de uma metriz',
        cCatMatrizes,
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amSetRowName);

    Add('SetColName',
        'Atribui um nome � coluna especificada de uma metriz',
        cCatMatrizes,
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amSetColName);

    Add('Insert',
        'Insere uma linha na posi��o especificada'#13 +
        'Par�metros'#13 +
        '  - Pos : Posi��o que ser� ocupada pela nova linha'#13 +
        '  - L   : Linha que ser� inserida',
        cCatMatrizes,
        [pvtInteger, pvtObject],
        [nil, TwsVec],
        [False, True],
        pvtNull,
        TObject,
        amMInsert);

    Add('Delete',
        'Elimina uma linha especificada pelo �ndice',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amMDelete);

    Add('DeleteCol',
        '',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amDeleteCol);

    Add('Add',
        'Concatena uma linha � matriz',
        cCatMatrizes,
        [pvtObject],
        [TwsVec],
        [True],
        pvtNull,
        TObject,
        amMadd);

    Add('Exchange',
        'Troca duas linhas entre si'#13 +
        '  Par�metros'#13 +
        '    i, j: �ndices das linhas que ser�o trocadas',
        cCatMatrizes,
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amExchange);
    end;

  with Functions do
    begin
    Add('Get',
        'Retorna o valor armazenado em uma C�lula'#13 +
        'Par�metros: Linha, Coluna',
        cCatMatrizes,
        [pvtInteger, pvtInteger],
        [nil       , nil],
        [False     , False],
        pvtReal    ,
        TObject    ,
        amMatrixGetData);

    Add('Power',
        'Respons�vel em multiplicar "n" vezes a Matriz A por ela mesma (pot�ncia de matriz)'#13 +
        'Par�metros: Pot�ncia (inteiro)',
        '',
        [pvtInteger], // Potencia
        [nil       ],
        [False     ],
        pvtObject  ,
        TwsGeneral ,
        amMatrixPower);

     Add('GetNRows',
        '',
        cCatMatrizes,
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetNRows);

    Add('GetNCols',
        '',
        cCatMatrizes,
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetNCols);
{
    Add('GetPrecision',
        'Retorna r�tulo da matriz ',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amGetPrecision);
}
    Add('IsMissValue',
        '',
        cCatMatrizes,
        [pvtInteger, pvtInteger, pvtReal],
        [nil, nil, nil],
        [False, False, True],
        pvtBoolean,
        TObject,
        amIsMissValue);

    Add('Transpose',
        'Retorna a transposta da matriz que chama o m�todo'#13 +
        'Sempre retorna uma matriz geral com a transposta',
        cCatMatrizes,
        [],
        [],
        [],
        pvtObject,
        TwsMatrix,
        amTranspose);

    Add('Mult',
        ' Metodo para o produto de matrizes. Utilizado quando a matriz que chama o metodo � geral,'#13 +
        '    sim�trica, Vandermonde ou Toeplitz'#13 +
        '  Par�metros'#13 +
        '    B: Matriz para o (pos) produto com a matriz que chama o m�todo'#13 +
        '    ErrCode: C�digo de erro. O produto somente sera poss�vel (ErrCode retorna 0) se o'#13 +
        '      n�mero de colunas de quem chama � igual ao n�mero de linhas de B; caso contr�rio'#13 +
        '      ErrCode retorna a constante NImprDim (Dimens�es impr�prias para a opera��o) e a'#13 +
        '      fun��o retorna nil'#13 +
        '  Retorno'#13 +
        '    Retorna sempre uma matriz geral',
        cCatMatrizes,
        [pvtObject, pvtInteger],
        [TwsMatrix, nil],
        [True, True],
        pvtObject,
        TwsMatrix,
        amMult);

    Add('VecMult',
        'Fazer o produto de uma matriz por um vetor'#13 +
        ' Par�matros'#13 +
        '   A: Vetor para o produto'#13 +
        '   PreMult: True se o vetor premultiplicar a matriz; False caso contrario'#13 +
        '   ErrCode: C�digo de erro. Retorna 0 se o produto for poss�vel e NImprDim (dimens�es'#13 +
        '     impr�prias para a opera��o) se a) n�mero de componentes do vetor diferente do n�mero'#13 +
        '     de linhas da matriz (PreMult=True); b) n�mero de colunas da matriz diferente do n�mero'#13 +
        '     de componentes do vetor (PreMult=False)'#13 +
        '   Retorno'#13 +
        '     Retorna sempre um vetor',
        cCatMatrizes,
        [pvtObject, pvtBoolean, pvtInteger],
        [TwsVec, nil, nil],
        [True, False, True],
        pvtObject,
        TwsDFVec,
        amVecMult);

{
    Add('SubMatrix',
        ' Gera uma submatriz da matriz que chama o m�todo (A)'#13 +
        '  Par�metros:'#13 +
        '    l : Linha inicial para submatriz'#13 +
        '    mb: Linha final'#13 +
        '    k : Coluna inicial para submatriz'#13 +
        '    nb: Coluna final'#13 +
        '  Retorno'#13 +
        '    Sempre uma matriz geral',
        cCatMatrizes,
        [pvtInteger, pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil, nil],
        [True, True, True, True],
        pvtObject,
        TwsGeneral,
        amSubMatrix);

    Add('DesStat',
        'Obtem estatisticas descritivas para as colunas especificadas.'#13 +
        '  Par�metros'#13 +
        '    Col: �ndices das colunas para as quais ser�o calculadas as estat�sticas'#13 +
        '    Stat: Estat�sticas desejadas. Os valores em Stat e as correspondentes estatisticas sao:'#13 +
        '      0.  Media'#13 +
        '      1.  Variancia'#13 +
        '      2.  Desvio padrao'#13 +
        '      3.  Total'#13 +
        '      4.  Minimo'#13 +
        '      5.  Maximo'#13 +
        '      6.  Numero de valores'#13 +
        '      7.  Numero de valores validos'#13 +
        '      8.  Erro padrao da media'#13 +
        '      9.  Amplitude'#13 +
        '      10. Coeficiente de variacao'#13 +
        '      11. Assimetria'#13 +
        '      12. Curtose'#13 +
        '      13. Soma de quadrados nao corrigida'#13 +
        '      14. Soma de quadrados corrigida'#13 +
        '  Retorno'#13 +
        '    Matriz com as estat�sticas desejadas nas colunas e as colunas nas linhas',
        cCatMatrizes,
        [pvtObject, pvtObject],
        [TwsLIVec, TwsLIVec],
        [True, True],
        pvtObject,
        TwsGeneral,
        amDesStat);
}
     Add('GetAsString',
        '',
        cCatMatrizes,
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtString,
        TObject,
        amGetAsString);

    Add('GetRow',
        '',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TwsVec,
        amGetRow);

     Add('VecKronecker',
        'Obt�m o produto de Kronecker de uma matriz por um vetor'#13 +
        '  Par�metros'#13 +
        '    x: Vetor para o produto'#13 +
        '  Retorno'#13 +
        '    Se a matriz que chama � de ordem m x n e o vetor � de ordem k ent�o o retorno � uma'#13 +
        '    matriz geral de ordem m x nk'#13 +
        '  Valores perdidos'#13 +
        '    Produto que envolve valor(es) perdidos retorna valor perdido',
        cCatMatrizes,
        [pvtObject],
        [TwsVec],
        [True],
        pvtObject,
        TwsMatrix,
        amVecKronecker);

    Add('Kronecker',
        ' Retorna o produto de Kronecker entre duas matrizes'#13 +
        '  Par�metros'#13 +
        '    X: Matriz com a qual ser� feito o produto de Kronecker'#13 +
        '  Retorno'#13 +
        '    Se A � a matriz m x n que chama o m�todo e X uma matriz p x q este m�todo retorna uma'#13 +
        '    matriz geral mp x nq com o produto de Kronecker'#13 +
        '  Valores perdidos'#13 +
        '    N�o trata',
        cCatMatrizes,
        [pvtObject],
        [TwsMatrix],
        [True],
        pvtObject,
        TwsMatrix,
        amKronecker);

    Add('ColKronecker',
        'Retorna o produto de Kronecker entre as colunas especificadas'#13 +
        '  Par�metros'#13 +
        '    X: matriz para o produto'#13 +
        '    i,j: �ndices das colunas da matriz e de X, respectivamente'#13 +
        '  Retorno'#13 +
        '    Se a matriz que chama � de ordem m x n e X � de ordem p x q, retorna um vetor (TwsDFVec)'#13 +
        '    de ordem mp, que corresponde ao produto de Kronecker entre a coluna i da matriz e coluna'#13 +
        '    j de X'#13 +
        '  Valores perdidos'#13 +
        '    Produtos envolvendo valores perdidos retornam valores perdidos',
        cCatMatrizes,
        [pvtObject, pvtInteger, pvtInteger],
        [TwsMatrix, nil, nil],
        [True, False, False],
        pvtObject,
        TwsVec,
        amColKronecker);

    Add('HorKronecker',
        '   Faz produto de Kronecker linha por linha'#13 +
        '  Par�metros'#13 +
        '    X: Matriz para realiza��o do produto'#13 +
        '    ErrCode: C�digo de erro. Retorna 0 se o n�mero de linhas da matriz que chama � igual ao'#13 +
        '    n�mero de linhas de X; NImprDim (dimens�es impr�prias para a opera��o) caso contr�rio'#13 +
        '  Retorno'#13 +
        '    Se A (com dimens�o m x n) � a matriz que chama e X � de ordem p x q retorna uma matriz'#13 +
        '    geral de dimens�o m x nq'#13 +
        '  Valores perdidos'#13 +
        '    Produto envolvendo valores perdidos retornam valores perdidos.',
        cCatMatrizes,
        [pvtObject, pvtInteger],
        [TwsMatrix, nil],
        [True, True],
        pvtObject,
        TwsMatrix,
        amHorKronecker);

    Add('TranspMul1',
        ' Faz o produto da matriz (geral, sim�trica, Vandermonde ou Toeplitz) pela transposta da'#13 +
        '    outra sem que a transposi��o seja explicitamente realizada'#13 +
        '  Par�metros'#13 +
        '    B: Matriz que ser� transposta para o produto'#13 +
        '    ErrCode: C�digo de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contr�rio'#13 +
        '  Retorno'#13 +
        '    Se A � a matriz que chama o m�todo, retorna uma matriz geral com o produt AB'''#13 +
        '  Valores perdidos'#13 +
        '    N�o trata',
        cCatMatrizes,
        [pvtObject, pvtInteger],
        [TwsMatrix, nil],
        [True, True],
        pvtObject,
        TwsMatrix,
        amTranspMul1);

{
    Add('SubIndex',
        ' Obtem uma submatriz com os elementos especificados atrav�s dos vetores R e C'#13 +
        '  Par�metros'#13 +
        '    R: �ndices das linhas dos elementos'#13 +
        '    C: �ndices das colunas'#13 +
        '  Retorno'#13 +
        '    Retorna sempre uma matriz geral',
        cCatMatrizes,
        [pvtObject, pvtObject],
        [TwsLIVec, TwsLIVec],
        [True, True],
        pvtObject,
        TwsGeneral,
        amSubIndex);

    Add('RowReduc',
        'Reduz as linhas para uma, nas colunas especificadas por C, dependendo do'#13 +
        '  operador Op. Os operadores sao:'#13 +
        '  + soma'#13 +
        '  . m�dia'#13 +
        '  # soma de quadrados'#13 +
        '  > m�ximo'#13 +
        '  < m�nimo',
        cCatMatrizes,
        [pvtObject, pvtString],
        [TwsLIVec, nil],
        [True, False],
        pvtObject,
        TwsGeneral,
        amRowReduc);

    Add('ColReduc',
        ' Reduz as colunas para uma, nas linhas especificadas por R, dependendo do'#13 +
        '  operador Ch.',
        cCatMatrizes,
        [pvtObject, pvtString],
        [TwsLIVec, nil],
        [True, False],
        pvtObject,
        TwsGeneral,
        amColReduc);

    Add('RowColReduc',
        ' Inicialmente reduz as linhas segundo o operador ChRow e posteriormente reduz as'#13 +
        '  colunas segundo o operador ChCol.',
        cCatMatrizes,
        [pvtString, pvtString],
        [nil, nil],
        [False, False],
        pvtReal,
        TObject,
        amRowColReduc);
}
     Add('LoadFromFile',
        '',
        '',
        [pvtString],
        [nil],
        [False],
        pvtObject,
        TwsMatrix,
        amLoadFromFile);

     Add('GetMLab',
        '',
        cCatMatrizes,
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetMLab);

    Add('GetRowName',
        ' Retorna o nome da linha especificada'#13 +
        '  Par�metros'#13 +
        '    i: �ndice da linha para obten��o do nome',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amGetRowName);

    Add('GetColName',
        'Retorna o nome da coluna de �ndice especificado'#13 +
        '  i: �ndice da coluna',
        cCatMatrizes,
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amGetColName);
{
    Add('Perc',
        'Obtem o percentil de uma coluna da matriz'#13 +
        '  Pa�metros'#13 +
        '    f: Valor do percentil desejado.'#13 +
        '    Col: �ndice da coluna para obten��o do percentil',
        cCatMatrizes,
        [pvtReal, pvtInteger],
        [nil, nil],
        [True, False],
        pvtReal,
        TObject,
        amPerc);

    Add('MatOrderStat',
        ' Obtem estatisticas de ordem das colunas especificadas'#13 +
        '  Par�metros'#13 +
        '    Col: �ndices das colunas desejadas'#13 +
        '    Stat: Estat�sticas de ordem se deseja calcular. Os c�digos s�o os seguintes:'#13 +
        '      Ind  Perc         Nome'#13 +
        '        0:    0   0%  Minimo'#13 +
        '        1:  .01   1%   Perc1'#13 +
        '        2:  .05   5%   Perc5'#13 +
        '        3:  .10  10%  Perc10'#13 +
        '        4:  .25  25%      Q1'#13 +
        '        5:  .50  50% Mediana'#13 +
        '        6:  .75  75%      Q3'#13 +
        '        7:    1 100%  Maximo'#13 +
        '        8:            Amplit'#13 +
        '        9:           QAmplit'#13 +
        '       10:  .90 90%   Perc90'#13 +
        '       11:  .95 95%   Perc95'#13 +
        '       12:  .99 99%   Perc99'#13 +
        '       13:            Cerca Inferior'#13 +
        '       14:            Cerca Superior'#13 +
        '       15:            val < cerca inf'#13 +
        '       16:            val > cerca sup',
        cCatMatrizes,
        [pvtObject, pvtObject],
        [TwsLIVec, TwsLIVec],
        [True, True],
        pvtObject,
        TwsGeneral,
        amMatOrderStat);

    Add('ToVec',
        'Transforma a matriz num vetor copiando, sequencialmente por linhas, os seus valores num vetor',
        cCatMatrizes,
        [],
        [],
        [],
        pvtObject,
        TwsVec,
        amToVec);

    Add('RankTie',
        'Obtem uma matriz com postos com empates dos elementos'#13 +
        '  Par�metros'#13 +
        '    NewMat: Se True, o resultado retorna numa nova matriz; caso contr�rio, substitui a'#13 +
        '    pr�pria matriz',
        cCatMatrizes,
        [pvtBoolean],
        [nil],
        [False],
        pvtObject,
        TwsMatrix,
        amRankTie);

    Add('Rank',
        'Obtem uma matriz com postos dos elementos'#13 +
        '  Par�metros'#13 +
        '    NewMat: Se True, o resultado retorna numa nova matriz; caso contr�rio, substitui a'#13 +
        '      pr�pria matriz'#13 +
        '    Ascd: True se a ordem ser� ascendente; false caso contr�rio',
        cCatMatrizes,
        [pvtBoolean, pvtBoolean],
        [nil, nil],
        [False, False],
        pvtObject,
        TwsMatrix,
        amRank);

    Add('CopyCol',
        'Retorna um vetor com os valores da coluna especificada.'#13 +
        '  Par�metros'#13 +
        '    Col: Coluna a ser copiada no vetor'#13 +
        '    IMiss: True se inclui os valores perdidos na copia; False caso contrario',
        cCatMatrizes,
        [pvtInteger, pvtBoolean],
        [nil, nil],
        [False, False],
        pvtObject,
        TwsVec,
        amCopyCol);

    Add('ColVecConcat',
        'Concatena por coluna um vetor com uma matriz. Responde ao operador ||.'#13 +
        '  Par�metros'#13 +
        '    x:       Vetor a concatenar.'#13 +
        '    VFirst:  Se true, ent�o o vetor vai primeiro e as colunas da matriz depois.'#13 +
        '    ErrCode: Retorna zero se a concatena��o foi feita com sucesso; NImprDim se o n�mero'#13 +
        '             de elementos do vetor for diferente do n�mero de linhas da matriz.',
        cCatMatrizes,
        [pvtObject, pvtBoolean, pvtInteger],
        [TwsVec, nil, nil],
        [True, False, True],
        pvtObject,
        TwsGeneral,
        amColVecConcat);
}
    end;
end;

class procedure Tps_wsMatrix.amMatrixGetData(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TwsMatrix(Stack.AsObject(3)).Data[Stack.AsInteger(1), Stack.AsInteger(2)]);
end;

class procedure Tps_wsMatrix.amMatrixPower(const Func_Name: String; Stack: TexeStack);
var Erro: Word;
begin
  Stack.PushObject( MatPower(TwsMatrix(Stack.AsObject(2)), Stack.AsInteger(1), Erro) );
  if Erro <> 0 then Raise
     Exception.Create('Potencia��o de Matrizes: Dimens�es Impr�prias');
end;

class procedure Tps_wsMatrix.amMatrixPrint(const Func_Name: String; Stack: TexeStack);
var M: TwsMatrix;
begin
  M := TwsMatrix(Stack.AsObject(2));
  //M.OutPut := TOutPut(Stack.AsObject(1));
  M.Print(TOutPut(Stack.AsObject(1)).Text);
end;

class procedure Tps_wsMatrix.amMatrixSetData(const Func_Name: String; Stack: TexeStack);
begin
  with Stack do
    TwsMatrix(AsObject(4)).Data[AsInteger(1), AsInteger(2)] := AsFloat(3);
end;

class procedure Tps_wsMatrix.amInv(Const Func_Name: String; Stack: TexeStack);
var EC: Word;
begin
   TwsMatrix(Stack.AsObject(2)).Inv(EC);
   Stack.AsReferency(1).Value := EC;
end;

class procedure Tps_wsMatrix.amSetNrows(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).NRows:= (Stack.AsInteger(1));
end;

class procedure Tps_wsMatrix.amSetNCols(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).NCols := (Stack.AsInteger(1));
end;
{
class procedure Tps_wsMatrix.amSetPrecision(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).Precision := Stack.AsInteger(1);
end;
}
class procedure Tps_wsMatrix.amGetNRows(Const Func_Name: String; Stack: TexeStack);
begin
   Stack.PushInteger(TwsMatrix(Stack.AsObject(1)).NRows);
end;

class procedure Tps_wsMatrix.amGetNCols(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TwsMatrix(Stack.AsObject(1)).NCols);
end;
{
class procedure Tps_wsMatrix.amGetPrecision(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TwsMatrix(Stack.AsObject(2)).Precision(Stack.AsInteger(1)));
end;
}
class procedure Tps_wsMatrix.amSetAsstring(Const Func_Name: String; Stack: TexeStack);
begin
   TwsMatrix(Stack.AsObject(4)).AsString[Stack.AsInteger(1), Stack.AsInteger(2)] := Stack.AsString(3);
end;

class procedure Tps_wsMatrix.amSetRow(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(3)).Row[Stack.AsInteger(1)] := TwsVec(Stack.AsObject(2));
end;

class procedure Tps_wsMatrix.amGetAsString(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(
    TwsMatrix(Stack.AsObject(4)).AsString[Stack.AsInteger(1), Stack.AsInteger(2)]);
end;

class procedure Tps_wsMatrix.amGetRow(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(2)).Row[Stack.AsInteger(1)]);
end;

class procedure Tps_wsMatrix.amIsMissValue(Const Func_Name: String; Stack: TexeStack);
var r : Double;
    V : TVariable;
begin
  v := Stack.AsReferency(3);
  Stack.PushBoolean(TwsMatrix(Stack.AsObject(4)).IsMissValue(Stack.AsInteger(1), Stack.AsInteger(2), r));
  v.Value := r;
end;

class procedure Tps_wsMatrix.amTranspose(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(1)).Transpose);
end;

class procedure Tps_wsMatrix.amMult(Const Func_Name: String; Stack: TexeStack);
var a : word;
    V : TVariable;
begin
  v := Stack.AsReferency(2);
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).Mult(TwsMatrix(Stack.AsObject(1)), a));
  V.Value := a;
end;

class procedure Tps_wsMatrix.amVecMult(Const Func_Name: String; Stack: TexeStack);
var a : word;
    V : TVariable;
begin
  V := Stack.AsReferency(3);
  Stack.PushObject(TwsMatrix(Stack.AsObject(4)).VecMult(TwsVec(Stack.AsObject(1)), Stack.AsBoolean(2), a));
  V.Value := a;
end;

class procedure Tps_wsMatrix.amDeleteCol(Const Func_Name: String; Stack: TexeStack);
begin
   TwsMatrix(Stack.AsObject(2)).DeleteCol(Stack.AsInteger(1));
end;

class procedure Tps_wsMatrix.amVecKronecker(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(2)).VecKronecker(TwsVec(Stack.AsObject(1))));
end;

class procedure Tps_wsMatrix.amKronecker(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(2)).Kronecker(TwsMatrix(Stack.AsObject(1))));
end;

class procedure Tps_wsMatrix.amColKronecker(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(4)).
    ColKronecker(TwsMatrix(Stack.AsObject(1)), Stack.AsInteger(2), Stack.AsInteger(3)));
end;

class procedure Tps_wsMatrix.amHorKronecker(Const Func_Name: String; Stack: TexeStack);
var r:word;
   aVar:Tvariable;
begin
  aVAr := Stack.AsReferency(2);
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).HorKronecker(TwsMatrix(Stack.AsObject(1)), r));
  aVar.VAlue := r;
end;

class procedure Tps_wsMatrix.amTranspMul1(Const Func_Name: String; Stack: TexeStack);
var r:word;
   aVar:Tvariable;
begin
  aVar := Stack.AsReferency(2);
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).TranspMul1(TwsMatrix(Stack.AsObject(1)), r));
  aVar.Value := r;
end;

{
class procedure Tps_wsMatrix.amSubMatrix(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(5)).SubMatrix(Stack.AsInteger(1),
                Stack.AsInteger(2),Stack.AsInteger(3),Stack.AsInteger(4)));
end;

class procedure Tps_wsMatrix.amSubSymMat(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).SubSymMat(Stack.AsInteger(1),Stack.AsInteger(2)));
end;

class procedure Tps_wsMatrix.amSubIndex(Const Func_Name: String; Stack: TexeStack);
begin
   Stack.PushObject(TwsMatrix(Stack.AsObject(3)).SubIndex(Stack.AsObject(1),Stack.AsObject(2)));
end;

class procedure Tps_wsMatrix.amRowReduc(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).RowReduc(Stack.AsObject(1),Stack.AsString(2)));
end;

class procedure Tps_wsMatrix.amColReduc(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).ColReduc(Stack.AsObject(1),Stack.AsString(2)));
end;

class procedure Tps_wsMatrix.amRowColReduc(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(TwsMatrix(Stack.AsObject(3)).RowColReduc(Stack.AsString(1),Stack.AsString(2)));
end;
}
class procedure Tps_wsMatrix.amSaveToFile(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).SaveToFile(Stack.AsString(1));
end;

class procedure Tps_wsMatrix.amLoadFromFile(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(2)).LoadFromFile(Stack.AsString(1)));
end;

class procedure Tps_wsMatrix.amSetMLab(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).MLab := Stack.AsString(1);
end;

class procedure Tps_wsMatrix.amSetRowName(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(3)).RowName[Stack.AsInteger(1)] := Stack.AsString(2);
end;

class procedure Tps_wsMatrix.amSetColName(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(3)).ColName[Stack.AsInteger(1)] := Stack.AsString(2);
end;

class procedure Tps_wsMatrix.amMInsert(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(3)).MInsert(Stack.AsInteger(1), TwsVec(Stack.AsObject(2)));
end;

class procedure Tps_wsMatrix.amMDelete(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(2)).MDelete(Stack.AsInteger(1));
end;

class procedure Tps_wsMatrix.amMadd(Const Func_Name: String; Stack: TexeStack);
begin
   TwsMatrix(Stack.AsObject(2)).MAdd(TwsVec(Stack.AsObject(1)));
end;

class procedure Tps_wsMatrix.amExchange(Const Func_Name: String; Stack: TexeStack);
begin
  TwsMatrix(Stack.AsObject(3)).Exchange(Stack.AsInteger(1),Stack.AsInteger(2));
end;
{
class procedure Tps_wsMatrix.amCopy(Const Func_Name: String; Stack: TexeStack);
var r : TwsMatrix;
begin
  TwsMatrix(Stack.AsObject(3)).Copy(Stack.AsObject(1), r);
  Stack.AsReferency(2).Value := Integer(r);
end;
}
class procedure Tps_wsMatrix.amGetMLab(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(TwsMatrix(Stack.AsObject(1)).MLab);
end;

class procedure Tps_wsMatrix.amGetRowName(Const Func_Name: String; Stack: TexeStack);
begin
   Stack.PushString(TwsMatrix(Stack.AsObject(2)).RowName[Stack.AsInteger(1)]);
end;

class procedure Tps_wsMatrix.amGetColName(Const Func_Name: String; Stack: TexeStack);
begin
    Stack.PushString(TwsMatrix(Stack.AsObject(2)).ColName[Stack.AsInteger(1)]);
end;
{
class procedure Tps_wsMatrix.amPerc(Const Func_Name: String; Stack: TexeStack);
var r    : Real;
    aVar : Tvariable;
begin
  Stack.PushFloat(TwsMatrix(Stack.AsObject(3)).Perc(r,Stack.AsInteger(2)));
  aVar       := Stack.AsReferency(2);
  aVAr.value := r;
end;

class procedure Tps_wsMatrix.amMatOrderStat(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).MatOrderStat(Stack.AsObject(1),
                                                Stack.AsObject(2)));
end;

class procedure Tps_wsMatrix.amToVec(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(1)).ToVec);
end;

class procedure Tps_wsMatrix.amRankTie(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(2)).RankTie(Stack.AsBoolean(1)));
end;

class procedure Tps_wsMatrix.amRank(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).Rank(Stack.AsBoolean(1),Stack.AsBoolean(2)));
end;

class procedure Tps_wsMatrix.amCopyCol(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(3)).CopyCol(Stack.AsInteger(1),Stack.AsBoolean(2)));
end;

class procedure Tps_wsMatrix.amColVecConcat(Const Func_Name: String; Stack: TexeStack);
var r : Integer;
  aVar : Tvariable;
begin
  Stack.PushObject(TwsMatrix(Stack.AsObject(4)).CopyCol(Stack.AsObject(1),Stack.AsBoolean(2),r));
  aVar := Stack.AsReferency(3);
  aVar.value := r;
end;
}

{ Tps_wsGeneral }

class procedure Tps_wsGeneral.amSortRows(const Func_Name: String; Stack: TexeStack);
begin
  TwsGeneral(Stack.AsObject(3)).SortRows(TwsLIVec(Stack.AsObject(1)),
                                         TwsLIVec(Stack.AsObject(2)));
end;

procedure Tps_wsGeneral.AddMethods;
begin
  with Procs do
    begin
    Add('SortRows',
        'Ordena as linhas da matriz, em ordem ascendente ou descendente, '#13 +
        'utilizando as colunas indicadas como chaves.'#13 +
        'Par�metros:'#13 +
        '  - Column: Vetor que indica os �ndices das colunas que funcionar�o como chaves de ordena��o.'#13 +
        '  - Ascend: Vetor de mesma dimens�o de Column que indica se para a coluna correspondente'#13 +
        '            a ordena��o ser� ascendente ou descendente. Se para uma coluna a posi��o Ascend'#13 +
        '            tiver 0 (zero) na posi��o correspondente a ordem ser� descendente; ascendente se tiver'#13 +
        '            um valor diferente de zero.',
        cCatMatrizes,
        [pvtObject, pvtObject],
        [TwsLIVec, TwsLIVec],
        [True, True],
        pvtNull,
        TObject,
        amSortRows);
    end;

  with Functions do
    begin
    end;
end;

function Tps_wsGeneral.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TwsGeneral.Create(Stack.AsInteger(1), Stack.AsInteger(2));
end;

end.

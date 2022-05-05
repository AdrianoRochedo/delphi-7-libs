unit Lib_Math;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Math;

type
  TMath_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn (Functions: TFunctionList); override;

    class procedure amSQR       (Const Func_Name: String; Stack: TexeStack);
    class procedure amPOWER     (Const Func_Name: String; Stack: TexeStack);
    class procedure amArcCos    (Const Func_Name: String; Stack: TexeStack);
    class procedure amArcSin    (Const Func_Name: String; Stack: TexeStack); 
    class procedure amArcTan2   (Const Func_Name: String; Stack: TexeStack); 
    class procedure amTan       (Const Func_Name: String; Stack: TexeStack); 
    class procedure amCoTan     (Const Func_Name: String; Stack: TexeStack); 
    class procedure amHypot     (Const Func_Name: String; Stack: TexeStack); 
    class procedure amDegtoRad  (Const Func_Name: String; Stack: TexeStack); 
    class procedure amRadtoDeg  (Const Func_Name: String; Stack: TexeStack); 
    class procedure amGradtoRad (Const Func_Name: String; Stack: TexeStack); 
    class procedure amRadtoGrad (Const Func_Name: String; Stack: TexeStack); 
    class procedure amCycletoRad(Const Func_Name: String; Stack: TexeStack); 
    class procedure amRadtoCycle(Const Func_Name: String; Stack: TexeStack); 
    class procedure amCosh      (Const Func_Name: String; Stack: TexeStack); 
    class procedure amSinh      (Const Func_Name: String; Stack: TexeStack); 
    class procedure amTanh      (Const Func_Name: String; Stack: TexeStack); 
    class procedure amArcCosh   (Const Func_Name: String; Stack: TexeStack);
    class procedure amArcSinh   (Const Func_Name: String; Stack: TexeStack); 
    class procedure amArcTanh   (Const Func_Name: String; Stack: TexeStack); 
    class procedure amLnXP1     (Const Func_Name: String; Stack: TexeStack); 
    class procedure amLog10     (Const Func_Name: String; Stack: TexeStack); 
    class procedure amLog2      (Const Func_Name: String; Stack: TexeStack); 
    class procedure amLogN      (Const Func_Name: String; Stack: TexeStack); 
    class procedure amIntPower  (Const Func_Name: String; Stack: TexeStack); 
    class procedure amLdexp     (Const Func_Name: String; Stack: TexeStack); 
    class procedure amMin       (Const Func_Name: String; Stack: TexeStack); 
    class procedure amMax       (Const Func_Name: String; Stack: TexeStack); 
    class procedure amTrunc     (Const Func_Name: String; Stack: TexeStack);
    class procedure amRound     (Const Func_Name: String; Stack: TexeStack);
    class procedure amABS       (Const Func_Name: String; Stack: TexeStack);
  end;

  TMath_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn (Procs: TProcList); override;
    class procedure amSinCos (Const Func_Name: String; Stack: TexeStack);
  end;

{ TMath_Functions }

type
  Tpr = ^Real;

class procedure TMath_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('SQR',
        'Calcula o quadrado de um número',
        cCatMatematica,
        [pvtReal],
        [nil],
        [False],
        pvtReal,
        TObject,
        amSQR);

    Add('Power',
        'Eleva um número a outro'#13 +
        'Parâmetros: Valor a ser elevado, Potência',
        cCatMatematica,
        [pvtReal, pvtReal],
        [nil, nil],
        [False, False],
        pvtReal,
        TObject,
        amPOWER);

    Add('ArcCos',
        'Calcula o Arco-Cosseno de um número',
        cCatMatematica,
        [pvtReal   ],
        [nil       ],
        [False     ],
        pvtReal     ,
        TObject     ,
        amArcCos);

    Add('ArcSin',
        'Calcula o Arco-Seno de um número',
        cCatMatematica,
        [pvtReal   ],
        [nil       ],
        [False     ],
        pvtReal     ,
        TObject     ,
        amArcSin);
{
    Add('ArcTan2',
        '',
        cCatMatematica,
        [pvtReal   ,pvtReal],
        [nil       ,nil    ],
        [False     ,False  ],
        pvtReal    ,
        TObject     ,
        amArcTan2);
}
    Add('Tan',
        'Calcula o Tangente de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amTan);

    Add('CoTan',
        'Calcula a Co-Tangente de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amCoTan);

    Add('Hypot',
        'Calcula o tamanho da Hipotenusa de um triângulo',
        cCatMatematica,
        [pvtReal  ,pvtReal],
        [nil      ,nil    ],
        [False    ,False  ],
        pvtReal   ,
        TObject   ,
        amHypot);

    Add('DegToRad',
        'Converte graus para radianos',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amDegToRad);

    Add('RadToDeg',
        'Converte radianos para graus',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amRadToDeg);
{
    Add('GradToRad',
        '',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amGradToRad);

    Add('RadToGrad',
        '',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amRadToGrad);

     Add('CycleToRad',
        '',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amCycleToRad);

      Add('RadToCycle',
        '',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amRadToCycle);
}
    Add('Cosh',
        'Calcula o Cosseno Hiperbólico de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amCosh);

    Add('Sinh',
        'Calcula o Seno Hiperbólico de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amSinh);

    Add('Tanh',
        'Calcula a Tangente Hiperbólica de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amTanh);

    Add('ArcCosh',
        'Calcula o Arco-Cosseno Hiperbólico de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amArcCosh);

    Add('ArcSinh',
        'Calcula o Arco-Seno Hiperbólico de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amRadToGrad);

    Add('ArcTanh',
        'Calcula o Arco-Tangente Hiperbólico de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amRadToGrad);

    Add('LnXP1',
        'Retorna o Logarítmo Natural de (X+1).'#13 +
        'Utilize LnXP1 quando o número for um valor próximo a 0',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amLnXP1);

    Add('Log10',
        'Retorna o logarítimo de base 10 de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amLog10);

    Add('Log2',
        'Retorna o logarítimo de base 2 de um número',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtReal   ,
        TObject   ,
        amLog2);

    Add('LogN',
        'Retorna o logarítimo de base N de um número'#13 +
        'Parâmetros: Base, Valor',
        cCatMatematica,
        [pvtReal  ,pvtReal],
        [nil      ,nil    ],
        [False    ,False  ],
        pvtReal   ,
        TObject   ,
        amLogN);

    Add('IntPower',
        'Calcula a potenciação integral de um número'#13 +
        'Parâmetros: Base, Expoente',
        cCatMatematica,
        [pvtReal  ,pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtReal   ,
        TObject   ,
        amIntPower);

    Add('Ldexp',
        'Calcula X * 2 ** P ',
        cCatMatematica,
        [pvtReal  , pvtInteger ],
        [nil      , nil        ],
        [False    , False      ],
        pvtReal   ,
        TObject   ,
        amLdexp);

    Add('Min',
        'Retorna o menor de dois números',
        cCatMatematica,
        [pvtReal  ,pvtReal],
        [nil      ,nil    ],
        [False    ,False  ],
        pvtReal   ,
        TObject   ,
        amMin);

    Add('Max',
        'Retorna o maior de dois números',
        cCatMatematica,
        [pvtReal  ,pvtReal],
        [nil      ,nil    ],
        [False    ,False  ],
        pvtReal   ,
        TObject   ,
        amMax);

    Add('ABS',
        'Retorna o valor absoluto de um número',
        cCatMatematica,
        [pvtReal],
        [nil    ],
        [False  ],
        pvtReal  ,
        TObject  ,
        amABS);

    Add('Trunc',
        'Realiza o truncamento de um número real, retornando um inteiro',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtInteger ,
        TObject    ,
        amTrunc);

    Add('Round',
        'Arredonda de um número real para um inteiro',
        cCatMatematica,
        [pvtReal  ],
        [nil      ],
        [False    ],
        pvtInteger ,
        TObject    ,
        amRound);
    end;
end;

class procedure TMath_Functions.amPOWER(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( POWER(Stack.AsFloat(1), Stack.AsFloat(2)) );
end;

class procedure TMath_Functions.amSQR(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( SQR(Stack.AsFloat(1)) );
end;

{17/07/00}
class procedure TMath_Functions.amArcCos(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( ArcCos(Stack.AsFloat(1)) );
end;

class procedure TMath_Functions.amArcSin(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( ArcSin(Stack.AsFloat(1)) );
end;

class procedure TMath_Functions.amArcTan2(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( ArcTan2(Stack.AsFloat(1), Stack.AsFloat(2)) );
end;

class procedure TMath_Functions.amTan(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Tan(Stack.AsFloat(1)) );
end;

class procedure TMath_Functions.amCoTan(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(CoTan(Stack.AsFloat(1)) );
end;

class procedure TMath_Functions.amHypot(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( Hypot(Stack.AsFloat(1), Stack.AsFloat(2)));
end;

class procedure TMath_Functions.amDegToRad(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(DegToRad(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amRadToDeg(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(RadToDeg(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amGradToRad(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(GradToRad(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amRadToGrad(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(RadToGrad(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amCycleToRad(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(CycleToRad(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amRadToCycle(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(RadToCycle(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amCosh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Cosh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amSinh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Sinh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amTanh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Tanh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amArcCosh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(ArcCosh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amArcSinh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(ArcSinh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amArcTanh(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(ArcTanh(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amLnXP1(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(LnXP1(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amLog10(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Log10(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amLog2(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(Log2(Stack.AsFloat(1)));
end;

class procedure TMath_Functions.amLogN(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( LogN(Stack.AsFloat(1), Stack.AsFloat(2)));
end;

class procedure TMath_Functions.amIntPower(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( IntPower(Stack.AsFloat(1), Stack.AsInteger(2)));
end;

class procedure TMath_Functions.amLdexp(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( Ldexp(Stack.AsFloat(1), Stack.AsInteger(2)));
end;

class procedure TMath_Functions.amMin(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( Min(Stack.AsFloat(1), Stack.AsFloat(2)));
end;

class procedure TMath_Functions.amMax(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( Max(Stack.AsFloat(1), Stack.AsFloat(2)));
end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TMath_Functions.AddFunctionsIn(Lib.Functions);
  TMath_Procs.AddProcsIn(Lib.Procs);
end;

class procedure TMath_Functions.amABS(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( abs(Stack.AsFloat(1) ));
end;

class procedure TMath_Functions.amTrunc(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( Trunc(Stack.AsFloat(1) ));
end;

class procedure TMath_Functions.amRound(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( Round(Stack.AsFloat(1) ));
end;

{ TMath_Procs }

class procedure TMath_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    Add('SinCos',
      'Retorna o Seno e o Cosseno de um número',
      cCatMatematica,
      [pvtReal  , pvtReal, pvtReal],
      [nil      , nil    , nil    ],
      [False    , True   , True   ],
      pvtNull   ,
      TObject   ,
      amSinCos);
    end;
end;

class procedure TMath_Procs.amSinCos(const Func_Name: String; Stack:TexeStack);
var r1, r2: Extended;
begin
  SinCos(Stack.AsFloat(1), r1, r2);
  Stack.AsReferency(2).Value := r1;
  Stack.AsReferency(3).Value := r2;
end;
end.

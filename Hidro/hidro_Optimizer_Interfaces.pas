unit hidro_Optimizer_Interfaces;

interface
uses SysUtilsEx;

type
  IOptimizable = interface;

  // Base para os otimizadores
  TOptimizator = class
    procedure Optimize(obj: IOptimizable); virtual; abstract;
    procedure Stop(); virtual; abstract;
    procedure ShowObjectiveFunction(aLeft, aTop: Integer); virtual; abstract;
    procedure ShowParamsManager(aLeft, aTop: Integer); virtual; abstract;
  end;

  // Utilizado em objetos que possuam propriedades otimizáveis.
  //   PropName - Nome da propriedade
  //   i1 e i2  - Índices opcionais a serem utilizados em props. indexadas.
  IOptimizableParameter = interface
    ['{4D4E33A6-71F7-4C58-A673-44614F592893}']
    function op_getValue(const PropName: string; i1, i2: Integer): real;
    procedure op_setValue(const PropName: string; i1, i2: Integer; const r: real);
  end;

  // Torna um objeto ápto a ser otimizado
  IOptimizable = interface
    ['{65C71F41-D064-4D2F-8973-7992D3A661DF}']
    procedure o_setOptimizator(obj: TOptimizator);
    procedure o_visualUpdate();
    procedure o_beginOptimization();
    procedure o_endOptimization();
    function  o_getObjetiveFunctionValue(): real;
  end;

implementation

end.

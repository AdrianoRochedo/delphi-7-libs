unit Optimizer_Base;

interface
uses SysUtils,
     WinUtils,
     Graphics,
     Dialogs,
     Classes,
     Contnrs,
     Math,
     Lists,
     SysUtilsEx,
     Optimizer_Interfaces,
     Optimizer_Frame_ParManager,
     Optimizer_Form_Parameter,
     Optimizer_Form_ParsManager,
     Optimizer_Form_ObjectiveFunctionViewer;

type

  OptimizerException = class(Exception);

  // Representa a ligação entre um parâmetro e uma propriedade de algum objeto
  TLink = class
  private
    FOptimizable : IOptimizableParameter;
    FPropName    : string;
    Fi1          : integer; // utilizado em propriedades indexadas
    Fi2          : integer; // utilizado em propriedades indexadas
  public
    // interface para o parâmetro a ser otimizado
    property Optimizable : IOptimizableParameter read FOptimizable write FOptimizable;

    // Nome da propriedade do objeto a ser otimizada
    property PropName : string read FPropName write FPropName;

    // 1. índice opcional, caso a propriedade seja indexada
    property i1 : integer read Fi1 write Fi1;

    // 2. índice opcional
    property i2 : integer read Fi2 write Fi2;
  end;

  TParameter = class
  private
    FParForm      : TfoParameter;
    FManagerFrame : TfrParManager;
    FMin          : Real;
    FCurrentValue : Real;
    FStep         : Real;
    FValue        : Real;
    FTolerance    : Real;
    FMax          : Real;
    FName         : String;
    FLink         : TLink;
    FOF_Status    : Char;

    procedure SetMax(const Value: Real);
    procedure SetMin(const Value: Real);
    procedure SetName(const Value: String);
    procedure SetStep(const Value: Real);
    procedure SetTolerance(const Value: Real);
    procedure SetValue(const Value: Real);
    function  GetValue: Real;
    procedure SetOF_Status(const Value: Char);
    procedure SetColor(const Value: TColor);
    destructor Destroy; override;
  public
    constructor Create(const aName      : String;
                       const aMin       : Real;
                       const aMax       : Real;
                       const aStep      : Real;
                       const aTolerance : Real); overload;

    constructor Create(const aName: string;
                       const aMin, aMax: real); overload;

    // Cria a ligação com a variável a ser otimizada
    procedure setLink(aObj: TObject; const aPropName: string; i1, i2: Integer);

    // Mostra uma janela que mostra as propriedades do Parâmetro
    procedure Show(Left, Top: Integer);

    // Link de conecção para as variaveis a serem otimizadas
    property Link: TLink read FLink;

    // Nome do parâmetro. Ex: "Volume do Res. Queimado"
    property Name : String read FName write SetName;

    // interface para o valor da propriedade do objeto ligado a este parâmetro
    property Value : Real read GetValue write SetValue;

    // Limite inferior
    property Min : Real read FMin write SetMin;

    // Limite superior
    property Max : Real read FMax write SetMax;

    // Passo para incremento
    property Step : Real read FStep write SetStep;

    // Tolerância
    property Tolerance : Real read FTolerance write SetTolerance;

    // Informa se houve melhoria/piora no valor da função objetivo em relação a este par.
    property OF_Status : Char read FOF_Status write SetOF_Status;

    // Cor do Gauge
    property Color : TColor write SetColor;

    // Janela que mostra as propriedades do parâmetro para o usuário
    property ParForm : TfoParameter read FParForm write FParForm;

    // Frame que representa o parâmetro
    property ParFrame : TfrParManager read FManagerFrame;
  end;

  // Representa um conjunto de parametros
  TParameters = class
  private
    FList: TObjectList;
    FManagerForm: TfoParsManager;
    FActiveItem: Integer;
    FIndPos: integer;

    function getCount: Integer;
    function GetItem(i: Integer): TParameter;
    procedure SetActiveItem(const Value: Integer);
  public
    constructor Create(ManagerForm: TfoParsManager);
    destructor Destroy; override;

    function CreateParameter(): TParameter;
    function ParamByName(const Name: String): TParameter;
    procedure getMinMax(var Min: real; var Max: real);
    procedure Clear();

    function Add (Param: TParameter): Integer; overload;
    procedure Add (const Name: string; const Min, Max: real); overload;

    property Count            : Integer      read getCount;
    property Item[i: Integer] : TParameter   read GetItem; default;
    property ActiveItem       : Integer      read FActiveItem write SetActiveItem;

    property IndividualPos : integer read FIndPos write FIndPos;
  end;

  TOFViwers = array of TfoObjectiveFunctionViewer;

  // Base para os otimizadores
  TOptimizer = class
  private
    procedure setTolerance(const Value: real);
  protected
    FPars      : TParameters;
    FProject   : IOptimizable;
    FStop      : boolean;
    FTolerance : real;
    FErro      : string;

    // Guarda o numero de funcoes objetivo utilizadas pelo sistema
    // 1 por default
    FOFValuesCount : integer;

    // Guarda os valores calculados pelo usuario
    FOFValues : TDoubleList;

    // Visualizadores
    F_OF_Viwers  : TOFViwers;
    FParsManager : TfoParsManager;

    // Indica o numero de simulações que ja ocorreram
    // Deverá ser atualizado pelos descendentes
    FSimulationCount : integer;

    // Guarda o número de pontos calculados
    FOFCount : TIntArray;

    procedure RedimOF();
    procedure ProcessMessages(MessageID: integer);
    procedure UpdateOFValue(OFIndex: integer; const Value: real);
    procedure UpdateSimulationCount();

    destructor Destroy; override;

    procedure Start(); virtual;
    procedure Finish(); virtual;
    procedure Execute(); virtual;

    function CreateParsManager(): TfoParsManager; virtual; abstract;
  public
    constructor Create();

    // Inicia o processo de otimização
    procedure Optimize(obj: IOptimizable);

    // Termina o processo de otimização
    procedure Stop(); virtual;

    // Estabelece o valor de uma funcao objetivo 
    procedure setOFValue(OFIndex: integer; const Value: real);

    // Mostra o visualizador da função objetivo
    procedure Show_OFViwer(OFIndex: integer; aLeft, aTop: Integer);

    // Mostra o visualizador de parametros
    procedure Show_ParsManager(aLeft, aTop: Integer);

    // Numero de funcoes objetivos a serem otimizadas
    // Geralmente é apenas uma.
    property ObjectivesCount: integer read FOFValuesCount
                                     write FOFValuesCount;

    // Indica a tolerancia do sistema
    property Tolerance : real read FTolerance write setTolerance;

    // Indica o numero de simulações que ja ocorreram
    property SimulationCount : integer read FSimulationCount;

    // Representa os parametros a serem otimizados
    property Parameters : TParameters read FPars;

    // Visualizador da Função Objetivo
    property OF_Viwers : TOFViwers read F_OF_Viwers write F_OF_Viwers;

    // Gerenciador de Parâmetros
    property ParsManager : TfoParsManager read FParsManager write FParsManager;
  end;

  procedure OptimizerError(const PropName, ClassName: string);

implementation

procedure OptimizerError(const PropName, ClassName: string);
begin
  raise OptimizerException.CreateFmt(
    'Optimizer: Property name <%s> not found in Class <%s>', [PropName, ClassName]);
end;

{ TParameter }

constructor TParameter.Create(const aName: string; const aMin, aMax: real);
begin
  inherited Create();
  FName := aName;
  FMin := aMin;
  FMax := aMax;
end;

Constructor TParameter.Create(const aName      : String;
                              const aMin       : Real;
                              const aMax       : Real;
                              const aStep      : Real;
                              const aTolerance : Real);
begin
  inherited Create();
  FName := aName;
  FMin := aMin;
  FMax := aMax;
  FStep := aStep;
  FTolerance := aTolerance;
end;

destructor TParameter.Destroy();
begin
  FLink.Free;
  FParForm.Free;
  Inherited;
end;

procedure TParameter.SetValue(const Value: Real);
begin
  if FLink <> nil then
     FLink.Optimizable.op_SetValue(FLink.PropName, FLink.i1, FLink.i2, Value)
  else
     FValue := Value;

  if FParForm <> nil then
     FParForm.Value := Value;

  if FManagerFrame.Visible then
     FManagerFrame.Value := Value;
end;

function TParameter.GetValue(): Real;
begin
  if FLink <> nil then
     Result := FLink.Optimizable.op_GetValue(FLink.PropName, FLink.i1, FLink.i2)
  else
     Result := FValue;
end;

procedure TParameter.SetMax(const Value: Real);
begin
  FMax := Value;
  FManagerFrame.Max := Value;
  if (FParForm <> nil) then
     FParForm.Max := Value;
end;

procedure TParameter.SetMin(const Value: Real);
begin
  FMin := Value;
  FManagerFrame.Min := Value;
  if (FParForm <> nil) then
     FParForm.Min := Value;
end;

procedure TParameter.SetName(const Value: String);
begin
  FName := Value;
  FManagerFrame.ParName := Value;
  if (FParForm <> nil) then
     FParForm.ParName := Value;
end;

procedure TParameter.SetStep(const Value: Real);
begin
  FStep := Value;
  FManagerFrame.StepLen := Value;
  if (FParForm <> nil) then
     FParForm.Step := Value;
end;

procedure TParameter.SetTolerance(const Value: Real);
begin
  FTolerance := Value;
  if (FParForm <> nil) then
     FParForm.Tolerance := Value;
end;

procedure TParameter.Show(Left, Top: Integer);
begin
  if FParForm = nil then
     FParForm := TfoParameter.Create(Self);

  FParForm.Left  := Left;
  FParForm.Top   := Top;

  with FParForm do
    begin
    ParName    := FName;
    Value      := FCurrentValue;
    Min        := FMin;
    Max        := FMax;
    Step       := FStep;
    Tolerance  := FTolerance;
    Show;
    end;
end;

procedure TParameter.setLink(aObj: TObject; const aPropName: string; i1, i2: Integer);
var i: IOptimizableParameter;
begin
  if aObj.GetInterface(IOptimizableParameter, i) then
     begin
     if FLink = nil then FLink := TLink.Create;
     FLink.Optimizable := i;
     FLink.PropName := aPropName;
     FLink.i1 := i1;
     FLink.i2 := i2;
     end
  else
     raise Exception.CreateFmt(
       'a Classe <%s> não implementa a interface <IOptimizableParameter>', [aObj.ClassName]);
end;

procedure TParameter.SetOF_Status(const Value: Char);
begin
  FOF_Status := Value;
  FManagerFrame.OF_Status := Value;
  if (FParForm <> nil) then
     FParForm.OF_Status := Value;
end;

procedure TParameter.SetColor(const Value: TColor);
begin
  FManagerFrame.Color := Value;
end;

{ TParameters }

constructor TParameters.Create(ManagerForm: TfoParsManager);
begin
  inherited Create;
  FList := TObjectList.Create(true);
  FManagerForm := ManagerForm;
  FActiveItem := -1;
end;

destructor TParameters.Destroy();
begin
  FList.Free;
  inherited;
end;

procedure TParameters.Clear();
begin
  FList.Clear();
  FManagerForm.Clear();
  FActiveItem := -1;
end;

function TParameters.CreateParameter(): TParameter;
begin
  FManagerForm.Visible := False;
  Result := TParameter.Create();
  self.Add(Result);
end;

function TParameters.Add(Param: TParameter): Integer;
begin
  {$ifdef VERSAO_LIMITADA}
  if FList.Count >= 4 then
     begin
     MessageDLG('Versão Limitada - Somente 4 parâmetros podem ser otimizados',
                mtInformation, [mbOk], 0);
     Exit;
     end;
  {$endif}
  FList.Add(Param);
  Param.FManagerFrame := FManagerForm.CreateFrame(FList.Count);
  with Param.FManagerFrame do
    begin
    Tag := integer(Param);
    ParName := Param.Name;
    Min := Param.Min;
    Max := Param.Max;
    end;
end;

procedure TParameters.Add(const Name: string; const Min, Max: real);
var p: TParameter;
begin
  p := TParameter.Create(Name, Min, Max);
  Self.Add(p);
end;

function TParameters.getCount: Integer;
begin
  Result := FList.Count;
end;

function TParameters.GetItem(i: Integer): TParameter;
begin
  Result := TParameter(FList[i]);
end;

function TParameters.ParamByName(const Name: String): TParameter;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    begin
    Result := TParameter(FList[i]);
    if CompareText(Result.Name, Name) = 0 then Exit;
    end;

  Result := nil;
end;

procedure TParameters.SetActiveItem(const Value: Integer);
begin
  if Value <> FActiveItem then
     begin
     if FActiveItem > -1 then GetItem(FActiveItem).Color := clBlue;
     GetItem(Value).Color := clRed;
     FActiveItem := Value;
     end;
end;

procedure TParameters.getMinMax(var Min, Max: real);
var i: Integer;
    p: TParameter;
begin
  Min := Math.MaxDouble;
  Max := Math.MinDouble;
  for i := 0 to FList.Count-1 do
    begin
    p := getItem(i);
    if p.Min < Min then Min := p.Min;
    if p.Max > Max then Max := p.Max
    end;
end;

{ TOptimizer }


procedure TOptimizer.Start();
begin
  FStop := false;
  FSimulationCount := 0;
  SysUtilsEx.Fill(FOFCount, 0);

  FErro := '';
  try
    FProject.o_beginOptimization();
  except
    FErro := 'Erro na inicialização do processo de otimização';
  end;

  if FErro = '' then
     begin
     if FPars.Count = 0 then
        raise Exception.Create('Não há parâmetros a serem otimizados');

     FOFValues.Free();
     FOFValues := TDoubleList.Create(FOFValuesCount);

     WinUtils.StartWait();
     end;
end;

procedure TOptimizer.Execute();
begin
  // virtual
end;

procedure TOptimizer.Finish();
begin
  WinUtils.StopWait();
  FProject.o_endOptimization();
end;

procedure TOptimizer.Optimize(obj: IOptimizable);
begin
  FProject := obj;
  FProject.o_setOptimizer(self);
  try
    Start();
    if FErro = '' then
       Execute()
    else
       showMessage(FErro);
  finally
    Finish();
  end;
end;

procedure TOptimizer.Stop();
begin
  FStop := true;
end;

procedure TOptimizer.RedimOF();
var i: Integer;
begin
  // Destroi
  for i := 0 to High(F_OF_Viwers) do
    F_OF_Viwers[i].Free();

  // Redimensiona
  setLength(F_OF_Viwers, FOFValuesCount);
  setLength(FOFCount, FOFValuesCount);

  // Cria as janelas
  for i := 0 to FOFValuesCount-1 do
    F_OF_Viwers[i] := TfoObjectiveFunctionViewer.Create(self);
end;

procedure TOptimizer.Show_OFViwer(OFIndex: integer; aLeft, aTop: Integer);
begin
  if Length(F_OF_Viwers) <> FOFValuesCount then
     RedimOF();

  if OFIndex < FOFValuesCount then
     begin
     F_OF_Viwers[OFIndex].Left := aLeft;
     F_OF_Viwers[OFIndex].Top := aTop;
     F_OF_Viwers[OFIndex].Show();
     F_OF_Viwers[OFIndex].Clear();
     end;
end;

procedure TOptimizer.Show_ParsManager(aLeft, aTop: Integer);
begin
  FParsManager.Left := aLeft;
  FParsManager.Top := aTop;
  FParsManager.Show();
end;

constructor TOptimizer.Create();
begin
  inherited Create();
  FOFValuesCount := 1;
  FTolerance := 0.001;

  FParsManager := CreateParsManager();
  FPars := TParameters.Create(FParsManager);
end;

destructor TOptimizer.Destroy();
var i: Integer;
begin
  // Não mudar esta ordem de destruição
  FOFValues.Free();
  FPars.Free();
  FParsManager.Free();
  for i := 0 to High(F_OF_Viwers) do F_OF_Viwers[i].Free();
  inherited Destroy();
end;

procedure TOptimizer.ProcessMessages(MessageID: integer);
begin
  FProject.o_ProcessMessage(MessageID);
  SysUtilsEx.ProcessMessages();
end;

procedure TOptimizer.UpdateOFValue(OFIndex: integer; const Value: real);
begin
  if Length(F_OF_Viwers) <> FOFValuesCount then
     RedimOF();

  if OFIndex <= High(F_OF_Viwers) then
     begin
     inc(FOFCount[OFIndex]);
     if F_OF_Viwers[OFIndex].Visible then
        F_OF_Viwers[OFIndex].setValue(FOFCount[OFIndex], Value);
     end;
end;

procedure TOptimizer.UpdateSimulationCount();
begin
  inc(FSimulationCount);
  if FParsManager.Visible then
     FParsManager.SimulationCount := FSimulationCount;
end;

procedure TOptimizer.setTolerance(const Value: real);
begin
  if (Value < 0) or (Value > 1) then
     FTolerance := 0.001
  else
     FTolerance := Value;
end;

procedure TOptimizer.setOFValue(OFIndex: integer; const Value: real);
begin
  if (OFIndex < 0) or (OFIndex >= FOFValuesCount) then
     raise Exception.CreateFmt('Índice da função objetivo inválido: %d', [OFIndex])
  else
     FOFValues.AsDouble[OFIndex] := Value;
end;

end.

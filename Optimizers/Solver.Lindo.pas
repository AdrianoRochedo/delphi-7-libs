unit Solver.Lindo;

interface
uses Classes, Contnrs, SysUtils, SysUtilsEx, ExecFile;

type
  // Representa uma variavel otimizada
  TVariable = class
  private
    FName: string;
    FValue: real;
    FRC: real;
  public
    constructor Create(const Name: string; const Value, ReducedCost: real);

    property Name : string read FName;
    property Value : real read FValue;
    property ReducedCost : real read FRC;

    function ToString(): string;
  end;

  // Representa a lista das variaveis otimizadas
  TVariableList = class
  private
    FList: TObjectList;
    destructor Destroy(); override;
    function getCount: integer;
    function getVar(i: integer): TVariable;
  public
    constructor Create();

    procedure Clear();
    procedure Add(const Name: string; const Value, ReducedCost: real);

    property Count : integer read getCount;
    property Item[i: integer] : TVariable read getVar; default;
  end;

  TLindoOptimizer = class
  private
    FLindoPath: string;
    FScriptFile: string;
    FSolutionFile: string;

    // resultados
    FOFV: real;
    FVars: TVariableList;

    destructor Destroy(); override;
    function LoadSolutionFile(): boolean;
    procedure setLindoPath(const Value: string);
    procedure Error(const msg: string);
  public
    constructor Create();

    // Caminho onde esta o Lindo.exe
    property LindoPath : string read FLindoPath write setLindoPath;

    // Nome do arquivo onde se encontra o modelo a ser otimizado
    property ScriptFile : string read FScriptFile write FScriptFile;

    // Nome do arquivo onde a solucao sera gerada
    // Se nao existir a solucao sera criada no mesmo diretorio do arquivo do modelo
    // com o nome de "solution.txt"
    property SolutionFile : string read FSolutionFile write FSolutionFile;

    // Executa a otimizacao
    function Execute(): boolean;

    // resultados (somente se a execucao for um sucesso)

    // Valor da Funcao objetivo
    property OFV : real read FOFV;

    // Variaveis otimizadas
    property Variables : TVariableList read FVars;
  end;

implementation

{ TLindoOptimizer }

procedure TLindoOptimizer.Error(const msg: string);
begin
  raise Exception.Create('TLindoOptimizer:'#13 + msg);
end;

function TLindoOptimizer.LoadSolutionFile(): boolean;
var sl: TStrings;
     i: integer;

     function LoadVariables(): boolean;
     var n: string;
         v, c: real;
     begin
       result := true;
       if Locate('VARIABLE        VALUE', sl, i, true) then
          begin
          inc(i);
          repeat
            n := Copy(sl[i], 01, 10, true); // Nome
            v := toFloat(Copy(sl[i], 18, 10, true)); // Valor
            c := toFloat(Copy(sl[i], 30, 16, true)); // Custo reduzido
            FVars.Add(n, v, c);
            inc(i);
            until (sl.Count = i) or (sl[i] = '');
          end
       else
          result := false;
     end;

begin
  i := 0;
  result := true;
  FVars.Clear();

  sl := LoadTextFile(FSolutionFile);
  try
    if Locate('OBJECTIVE FUNCTION VALUE', sl, i, true) then
       begin
       inc(i, 2);
       FOFV := toFloat(Copy(sl[i], 8, 16, true));
       inc(i, 2);
       result := LoadVariables();
       end
    else
       result := false;
  finally
    sl.Free();
  end;
end;

function TLindoOptimizer.Execute(): boolean;
var Exec: TExecFile;
    inFile, outFile: string;
begin
  result := false;

  if FLindoPath = '' then
     Error('Lindo path not defined')
  else
     SysUtils.SetCurrentDir(FLindoPath);

  if FScriptFile = '' then
     Error('Script not defined');

  if not SysUtils.FileExists(FScriptFile) then
     Error(Format('Model <%s> not exist', [FScriptFile]));

  inFile := SysUtils.ExtractShortPathName(FScriptFile);

  if (FSolutionFile = '') or (not SysUtils.FileExists(FSolutionFile)) then
     begin
     outFile := SysUtils.ExtractFilePath(inFile) + 'Solution.txt';
     FSolutionFile := SysUtils.ExtractFilePath(FScriptFile) + 'Solution.txt';
     end
  else
     outFile := SysUtils.ExtractShortPathName(FSolutionFile);

(*
  // Cria o arquivo de sript que sera a entrada do lindo
  sl := TStringList.Create();
  sl.add('PAGE 0');
  sl.add('TAKE ' + inFile);
  sl.add('GO');
  sl.add('DIVERT ' + outFile);
  sl.add('RVRT');
  sl.add('QUIT');
  sl.SaveToFile(FLindoPath + 'script.txt');
  sl.Free();
*)

  // Apaga o arquivo de saida caso ele exista
  SysUtils.DeleteFile(outFile);

  // Executa o Lindo
  Exec := TExecFile.Create(nil);
  try
    Exec.CommandLine := ExtractShortPathName(FLindoPath + 'Rodar_Lindo.bat');
    Exec.Parameters := inFile + ' ' + outFile;
    Exec.Wait := true;
    Exec.WaitStyle := wSuspend;
    if Exec.Execute() then
       result := LoadSolutionFile();
  finally
    Exec.Free();
  end;
end;

procedure TLindoOptimizer.setLindoPath(const Value: string);
begin
  if SysUtils.DirectoryExists(Value) then
     begin
     FLindoPath := Value;
     if SysUtilsEx.LastChar(FLindoPath) <> '\' then
        FLindoPath := FLindoPath + '\';
     end
  else
     Error(Format('Directory <%s> not exist', [Value]));
end;

constructor TLindoOptimizer.Create();
begin
  inherited Create();
  FVars := TVariableList.Create();
end;

destructor TLindoOptimizer.Destroy();
begin
  FVars.Free();
  inherited;
end;

{ TVariable }

constructor TVariable.Create(const Name: string; const Value, ReducedCost: real);
begin
  inherited Create();
  FName := Name;
  FValue := Value;
  FRC := ReducedCost;
end;

function TVariable.ToString(): string;
begin
  result := Format('%s = %f (RC: %f)', [FName, FValue, FRC]);
end;

{ TVariableList }

procedure TVariableList.Add(const Name: string; const Value, ReducedCost: real);
begin
  FList.Add(
    TVariable.Create(Name, Value, ReducedCost));
end;

procedure TVariableList.Clear();
begin
  FList.Clear();
end;

constructor TVariableList.Create();
begin
  inherited Create();
  FList := TObjectList.Create(true);
end;

destructor TVariableList.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TVariableList.getCount(): integer;
begin
  result := FList.Count;
end;

function TVariableList.getVar(i: integer): TVariable;
begin
  result := TVariable(FList[i]);
end;

end.

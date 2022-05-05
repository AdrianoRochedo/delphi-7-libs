unit Lib_System;

interface
uses psBase;

  procedure API(Lib: TLib);

type
  TSystem_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn(Procs: TProcList); override;
    class procedure amFreeObject (Const Func_Name: String; Stack: TexeStack);
    class procedure amRandomize  (Const Func_Name: String; Stack: TexeStack);
    class procedure amPB_SetMin  (Const Func_Name: String; Stack: TexeStack);
    class procedure amPB_SetMax  (Const Func_Name: String; Stack: TexeStack);
    class procedure amPB_SetVal  (Const Func_Name: String; Stack: TexeStack);
    class procedure amPB_SetMes  (Const Func_Name: String; Stack: TexeStack);
  end;

  TSystem_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
    class procedure amObjectIsValid   (Const Func_Name: String; Stack: TexeStack);
    class procedure amRandom          (Const Func_Name: String; Stack: TexeStack);
    class procedure amToASCII         (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTickCount    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTime         (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTimeAsString (Const Func_Name: String; Stack: TexeStack);
    class procedure amTimeToString    (Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Classes,
     Windows,
     SysUtils,
     SysUtilsEx;

{ TSystem_Functions }

class procedure TSystem_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('ObjectIsValid',
        'Verifica se um objeto é válido',
        cCatSistema,
        [pvtObject], [TObject], [True], pvtBoolean, TObject, amObjectIsValid);

    Add('Random',
        'Gera um número aleatório x onde 0 >= x < 1 se o intervalo for igual a 0 e um' +
        'número aleatório x onde 0 >= x < intervalo se intervalo maior que 0',
        cCatSistema,
        [pvtInteger], [TObject], [false], pvtReal, TObject, amRandom);

    Add('toASCII',
        'Converte um código ASCII para um caracter',
        cCatConversao,
        [pvtInteger], [TObject], [false], pvtString, TObject, amToASCII);

    Add('GetTickCount',
        'Retorna o número de milisegundos acumulados deste o Windows foi iniciado',
        cCatSistema,
        [], [], [], pvtInteger, TObject, amGetTickCount);

    Add('GetTime',
        'Retorna um índice que indica a atual hora do sistema',
        cCatSistema,
        [], [], [], pvtReal, TObject, amGetTime);

    Add('GetTimeAsString',
        'Retorna um índice que indica a atual hora do sistema como string',
        cCatSistema,
        [], [], [], pvtString, TObject, amGetTimeAsString);

    Add('TimeToString',
        'Converte um índice que indica a atual hora do sistema para string',
        cCatConversao,
        [pvtReal], [TObject], [false], pvtString, TObject, amTimeToString);
    end;
end;

class procedure TSystem_Functions.amGetTickCount(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( Windows.GetTickCount() );
end;

class procedure TSystem_Functions.amGetTime(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat( SysUtils.Now );
end;

class procedure TSystem_Functions.amObjectIsValid(const Func_Name: String; Stack: TexeStack);
var b: Boolean;
    o: TObject;
begin
  o := Stack.AsObject(1);
  b := (o <> nil);
  if b then
     try
       o.InstanceSize; // somente para testar se o objeto é realmente válido
     except
       b := False;
     end;
  Stack.PushBoolean(b);
end;

class procedure TSystem_Functions.amRandom(const Func_Name: String; Stack: TexeStack);
var i: Integer;
begin
  i := Stack.AsInteger(1);
  if i > 0 then
     Stack.PushFloat(Random(i))
  else
     Stack.PushFloat(Random);
end;

class procedure TSystem_Functions.amGetTimeAsString(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( SysUtils.TimeToStr(SysUtils.Now) );
end;

class procedure TSystem_Functions.amTimeToString(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( SysUtils.TimeToStr( Stack.AsFloat(1) ) );
end;

class procedure TSystem_Functions.amToASCII(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( char(Stack.AsInteger(1)) );
end;

{ TSystem_Procs }

class procedure TSystem_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    Add('FreeObject',
        'Libera a memória de um Objeto',
        cCatSistema,
        [pvtObject], [TObject], [True], pvtNull, TObject, amFreeObject);

    Add('Randomize',
        'Inicia o gerenciador de números aleatórios',
        cCatSistema,
        [], [], [], pvtNull, TObject, amRandomize);

    Add('Progress_SetMin',
        'Estabelece o valor mínimo da barra de progresso global',
        cCatSistema,
        [pvtInteger], [TObject], [false], pvtNull, TObject, amPB_setMin);

    Add('Progress_SetMax',
        'Estabelece o valor máximo da barra de progresso global',
        cCatSistema,
        [pvtInteger], [TObject], [false], pvtNull, TObject, amPB_setMax);

    Add('Progress_SetValue',
        'Estabelece o valor atual da barra de progresso global',
        cCatSistema,
        [pvtInteger], [TObject], [false], pvtNull, TObject, amPB_setVal);

    Add('Progress_SetMessage',
        'Estabelece o texto da barra de progresso global',
        cCatSistema,
        [pvtString], [TObject], [false], pvtNull, TObject, amPB_setMes);
    end;
end;

class procedure TSystem_Procs.amFreeObject(const Func_Name: String; Stack: TexeStack);
var Ref: TVariable;
begin
  Ref := Stack.AsReferency(1);
  TObject(integer(Ref.Value)).Free;
  Ref.Value := 0;
end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TSystem_Functions.AddFunctionsIn(Lib.Functions);
  TSystem_Procs.AddProcsIn(Lib.Procs);
end;

class procedure TSystem_Procs.amPB_SetMax(const Func_Name: String; Stack: TexeStack);
var PB: IProgressBar;
begin
  PB := getGlobalProgressBar();
  if PB <> nil then
     PB.setMax(Stack.AsInteger(1));
end;

class procedure TSystem_Procs.amPB_SetMes(const Func_Name: String; Stack: TexeStack);
var PB: IProgressBar;
begin
  PB := getGlobalProgressBar();
  if PB <> nil then
     PB.setMessage(Stack.AsString(1));
end;

class procedure TSystem_Procs.amPB_SetMin(const Func_Name: String; Stack: TexeStack);
var PB: IProgressBar;
begin
  PB := getGlobalProgressBar();
  if PB <> nil then
     PB.setMin(Stack.AsInteger(1));
end;

class procedure TSystem_Procs.amPB_SetVal(const Func_Name: String; Stack: TexeStack);
var PB: IProgressBar;
begin
  PB := getGlobalProgressBar();
  if PB <> nil then
     PB.setValue(Stack.AsInteger(1));
end;

class procedure TSystem_Procs.amRandomize(const Func_Name: String; Stack: TexeStack);
begin
  Randomize();
end;

end.

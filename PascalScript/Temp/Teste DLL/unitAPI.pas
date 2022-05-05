unit UnitAPI;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses drPascal;

  procedure API(Lib: TLib);

type
  Txxx_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn(Procs: TProcList); override;
    class procedure amP1(Const Func_Name: String; Stack: TexeStack);
  end;

  Txxx_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
    class procedure amF1(Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Dialogs;

  procedure API(Lib: TLib);
  begin
    Txxx_Procs.AddProcsIn(Lib.Procs);
    Txxx_Functions.AddFunctionsIn(Lib.Functions);
  end;

{ Txxx_Procs }

class procedure Txxx_Procs.amP1(Const Func_Name: String; Stack: TexeStack);
begin
  ShowMessage('Funcionou - Chamada de procedimento');
end;

class procedure Txxx_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    Add('P1',
        '',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amP1);
    end;
end;

{ Txxx_Functions }

class procedure Txxx_Functions.amF1(Const Func_Name: String; Stack: TexeStack);
var r: Real;
begin
  r := Stack.AsFloat(1);
  Stack.PushFloat(r * r);
end;

class procedure Txxx_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('F1',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtReal,
        TObject,
        amF1);
    end;
end;

end.

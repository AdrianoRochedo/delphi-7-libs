unit Lib_VariableList;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses psBase;

  procedure API(Lib: TLib);

type
  Tps_VariableList = class(TpsClass)
    procedure AddMethods; override;
    class procedure am_Ler(Const Func_Name: String; Stack: TexeStack);
    class procedure am_NumVars(Const Func_Name: String; Stack: TexeStack);
    class procedure am_ObterValor(Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Solver.Classes;

  procedure API(Lib: TLib);
  begin
    Tps_VariableList.Create(TVariableList,
                            TObject,
                            'Implementa uma lista de variáveis',
                            'Sistema',
                            [],
                            [],
                            [],
                            False,
                            Lib.Classes);
  end;

{ Tps_VariableList }

class procedure Tps_VariableList.am_Ler(Const Func_Name: String; Stack: TexeStack);
var o: TVariableList;
begin
  o := TVariableList(Stack.AsObject(2));
  o.LoadFromFile(Stack.AsString(1))
end;

class procedure Tps_VariableList.am_NumVars(Const Func_Name: String; Stack: TexeStack);
var o: TVariableList;
begin
  o := TVariableList(Stack.AsObject(1));
  Stack.PushInteger(o.Count);
end;

class procedure Tps_VariableList.am_ObterValor(Const Func_Name: String; Stack: TexeStack);
var o: TVariableList;
begin
  o := TVariableList(Stack.AsObject(2));
  Stack.PushFloat(o.getValue(Stack.AsString(1)))
end;

procedure Tps_VariableList.AddMethods();
begin
  with Procs do
    begin
    Add('Ler',
        'Carrega um conjunto de variáveis de um arquivo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_Ler);
    end;

  with Functions do
    begin
    Add('NumVars',
        'Retorna o número de variáveis armazenadas',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_NumVars);

    Add('ObterValor',
        'Retorna o valor da variável informado seu nome',
        '',
        [pvtString],
        [nil],
        [False],
        pvtReal,
        TObject,
        am_ObterValor);
    end;
end;

end.

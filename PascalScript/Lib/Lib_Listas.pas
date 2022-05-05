unit Lib_Listas;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses psBase;

  procedure API(Lib: TLib);

type
  Tps_ListaDeBooleanos = class(TpsClass)
    procedure AddMethods; override;
    function CreateObject(Stack: TexeStack): TObject; override;

    class procedure amAdicionar      (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemover        (Const Func_Name: String; Stack: TexeStack);
    class procedure amAtribui        (Const Func_Name: String; Stack: TexeStack);
    class procedure amNumDeElementos (Const Func_Name: String; Stack: TexeStack);
    class procedure amObtem          (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_ListaDeInteiros = class(TpsClass)
    procedure AddMethods; override;
    function CreateObject(Stack: TexeStack): TObject; override;

    class procedure amAdicionar          (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemover            (Const Func_Name: String; Stack: TexeStack);
    class procedure amLimpar             (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemoverValor       (Const Func_Name: String; Stack: TexeStack);
    class procedure amAtribui            (Const Func_Name: String; Stack: TexeStack);
    class procedure amAtribuiComoString  (Const Func_Name: String; Stack: TexeStack);
    class procedure amNumDeElementos     (Const Func_Name: String; Stack: TexeStack);
    class procedure amValorExiste        (Const Func_Name: String; Stack: TexeStack);
    class procedure amObtem              (Const Func_Name: String; Stack: TexeStack);
    class procedure amObtemComoString    (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_ListaDeReais = class(TpsClass)
    procedure AddMethods; override;
    function CreateObject(Stack: TexeStack): TObject; override;

    class procedure amAdicionar         (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemover           (Const Func_Name: String; Stack: TexeStack);
    class procedure amAtribui           (Const Func_Name: String; Stack: TexeStack);
    class procedure amAtribuiComoString (Const Func_Name: String; Stack: TexeStack);
    class procedure amNumDeElementos    (Const Func_Name: String; Stack: TexeStack);
    class procedure amObtem             (Const Func_Name: String; Stack: TexeStack);
    class procedure amObtemComoString   (Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Lists;

  procedure API(Lib: TLib);
  begin
    Tps_ListaDeBooleanos.Create(TBooleanList,
                                nil,
                                'Encapsula uma lista de valores booleanos',
                                'Listas',
                                [], [], [],
                                True,
                                Lib.Classes);

    Tps_ListaDeInteiros.Create(TIntegerList,
                               nil,
                               'Encapsula uma lista de valores Inteiros',
                               'Listas',
                               [], [], [],
                               True,
                               Lib.Classes);

    Tps_ListaDeReais.Create(TDoubleList,
                            nil,
                            'Encapsula uma lista de valores Reais (ponto flutuante)',
                            'Listas',
                            [], [], [],
                            True,
                            Lib.Classes);
  end;

{ Tps_ListaDeBooleanos }

class procedure Tps_ListaDeBooleanos.amAdicionar(Const Func_Name: String; Stack: TexeStack);
var o: TBooleanList;
begin
  o := TBooleanList(Stack.AsObject(2)); // self
  o.Add(Stack.AsBoolean(1));
end;

class procedure Tps_ListaDeBooleanos.amRemover(Const Func_Name: String; Stack: TexeStack);
var o: TBooleanList;
begin
  o := TBooleanList(Stack.AsObject(2)); // self
  o.Delete(Stack.AsInteger(1));
end;

class procedure Tps_ListaDeBooleanos.amAtribui(Const Func_Name: String; Stack: TexeStack);
var o: TBooleanList;
begin
  o := TBooleanList(Stack.AsObject(3)); // self
  o[Stack.AsInteger(1)] := Stack.AsBoolean(2);
end;

class procedure Tps_ListaDeBooleanos.amNumDeElementos(Const Func_Name: String; Stack: TexeStack);
var o: TBooleanList;
begin
  o := TBooleanList(Stack.AsObject(1)); // self
  Stack.PushInteger(o.Count);
end;

class procedure Tps_ListaDeBooleanos.amObtem(Const Func_Name: String; Stack: TexeStack);
var o: TBooleanList;
begin
  o := TBooleanList(Stack.AsObject(2)); // self
  Stack.PushBoolean(o[Stack.AsInteger(1)]);
end;

procedure Tps_ListaDeBooleanos.AddMethods;
begin
  with Procs do
    begin
    Add('Add',
        'Adiciona um item Lógico (Verdadeiro/Falso) na lista',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        amAdicionar);

    Add('Remove',
        'Remove um item Lógico (Verdadeiro/Falso) da lista'#13#10 +
        'Parâmetro: Índice do Item',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amRemover);

    Add('Set',
        '',
        '',
        [pvtInteger, pvtBoolean],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amAtribui);
    end;

  with Functions do
    begin
    Add('Count',
        '',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amNumDeElementos);

    Add('Get',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtBoolean,
        TObject,
        amObtem);
    end;
end;

function Tps_ListaDeBooleanos.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TBooleanList.Create;
end;

{ Tps_ListaDeInteiros }

class procedure Tps_ListaDeInteiros.amAdicionar(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  o.Add(Stack.AsInteger(1));
end;

class procedure Tps_ListaDeInteiros.amRemover(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  o.Delete(Stack.AsInteger(1));
end;

class procedure Tps_ListaDeInteiros.amLimpar(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(1)); // self
  o.Clear;
end;

class procedure Tps_ListaDeInteiros.amRemoverValor(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  o.DeleteVal(Stack.AsInteger(1));
end;

class procedure Tps_ListaDeInteiros.amAtribui(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(3)); // self
  o[Stack.AsInteger(1)] := Stack.AsInteger(2);
end;

class procedure Tps_ListaDeInteiros.amAtribuiComoString(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(3)); // self
  o.AsString[Stack.AsInteger(1)] := Stack.AsString(2);
end;

class procedure Tps_ListaDeInteiros.amNumDeElementos(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(1)); // self
  Stack.PushInteger(o.Count);
end;

class procedure Tps_ListaDeInteiros.amValorExiste(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
    i: Integer;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  Stack.PushBoolean(o.Exists(Stack.AsInteger(1), i));
end;

class procedure Tps_ListaDeInteiros.amObtem(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  Stack.PushInteger(o[Stack.AsInteger(1)]);
end;

class procedure Tps_ListaDeInteiros.amObtemComoString(Const Func_Name: String; Stack: TexeStack);
var o: TIntegerList;
begin
  o := TIntegerList(Stack.AsObject(2)); // self
  Stack.PushString(o.AsString[Stack.AsInteger(1)]);
end;

procedure Tps_ListaDeInteiros.AddMethods;
begin
  with Procs do
    begin
    Add('Add',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amAdicionar);

    Add('Remove',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amRemover);

    Add('Clear',
        '',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amLimpar);

    Add('RemoveValue',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amRemoverValor);

    Add('Set',
        '',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amAtribui);

    Add('SetAsString',
        '',
        '',
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amAtribuiComoString);
    end;

  with Functions do
    begin
    Add('Count',
        '',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amNumDeElementos);

    Add('ValueExists',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtBoolean,
        TObject,
        amValorExiste);

    Add('Get',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amObtem);

    Add('GetAsString',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amObtemComoString);
    end;
end;

function Tps_ListaDeInteiros.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TIntegerList.Create;
end;

{ Tps_ListaDeReais }

class procedure Tps_ListaDeReais.amAdicionar(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(2)); // self
  o.Add(Stack.AsFloat(1));
end;

class procedure Tps_ListaDeReais.amRemover(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(2)); // self
  o.Delete(Stack.AsInteger(1));
end;

class procedure Tps_ListaDeReais.amAtribui(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(3)); // self
  o[Stack.AsInteger(1)] := Stack.AsFloat(2);
end;

class procedure Tps_ListaDeReais.amAtribuiComoString(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(3)); // self
  o.AsString[Stack.AsInteger(1)] := Stack.AsString(2);
end;

class procedure Tps_ListaDeReais.amNumDeElementos(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(1)); // self
  Stack.PushInteger(o.Count);
end;

class procedure Tps_ListaDeReais.amObtem(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(2)); // self
  Stack.PushFloat(o[Stack.AsInteger(1)]);
end;

class procedure Tps_ListaDeReais.amObtemComoString(Const Func_Name: String; Stack: TexeStack);
var o: TDoubleList;
begin
  o := TDoubleList(Stack.AsObject(2)); // self
  Stack.PushString(o.AsString[Stack.AsInteger(1)]);
end;

procedure Tps_ListaDeReais.AddMethods;
begin
  with Procs do
    begin
    Add('Add',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amAdicionar);

    Add('Remove',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amRemover);

    Add('Set',
        '',
        '',
        [pvtInteger, pvtReal],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amAtribui);

    Add('SetAsString',
        '',
        '',
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amAtribuiComoString);
    end;

  with Functions do
    begin
    Add('Count',
        '',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amNumDeElementos);

    Add('Get',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtReal,
        TObject,
        amObtem);

    Add('GetAsString',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amObtemComoString);
    end;
end;

function Tps_ListaDeReais.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TDoubleList.Create;
end;

end.

unit Lib_StringList;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses psBase;

  procedure API(Lib: TLib);

type
  Tps_StringList = class(TpsClass)
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
    class procedure amAppend(Const Func_Name: String; Stack: TexeStack);
    class procedure amAddStrings(Const Func_Name: String; Stack: TexeStack);
    class procedure amBeginUpdate(Const Func_Name: String; Stack: TexeStack);
    class procedure amClear(Const Func_Name: String; Stack: TexeStack);
    class procedure amDelete(Const Func_Name: String; Stack: TexeStack);
    class procedure amEndUpdate(Const Func_Name: String; Stack: TexeStack);
    class procedure amExchange(Const Func_Name: String; Stack: TexeStack);
    class procedure amInsert(Const Func_Name: String; Stack: TexeStack);
    class procedure amLoadFromFile(Const Func_Name: String; Stack: TexeStack);
    class procedure amSaveToFile(Const Func_Name: String; Stack: TexeStack);
    class procedure amSetCommaText(Const Func_Name: String; Stack: TexeStack);
    class procedure amPutObject(Const Func_Name: String; Stack: TexeStack);
    class procedure amSetValue(Const Func_Name: String; Stack: TexeStack);
    class procedure amPut(Const Func_Name: String; Stack: TexeStack);
    class procedure amSetTextStr(Const Func_Name: String; Stack: TexeStack);
    class procedure amSort(Const Func_Name: String; Stack: TexeStack);
    class procedure amSetDuplicates(Const Func_Name: String; Stack: TexeStack);
    class procedure amSetSorted(Const Func_Name: String; Stack: TexeStack);
    class procedure amAdd(Const Func_Name: String; Stack: TexeStack);
    class procedure amAddObject(Const Func_Name: String; Stack: TexeStack);
    class procedure amEquals(Const Func_Name: String; Stack: TexeStack);
    class procedure amIndexOf(Const Func_Name: String; Stack: TexeStack);
    class procedure amIndexOfName(Const Func_Name: String; Stack: TexeStack);
    class procedure amIndexOfObject(Const Func_Name: String; Stack: TexeStack);
    class procedure amObjectByString(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetCommaText(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetCount(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetName(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetObject(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetValue(Const Func_Name: String; Stack: TexeStack);
    class procedure amGet(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTextStr(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetDuplicates(Const Func_Name: String; Stack: TexeStack);
    class procedure amGetSorted(Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Classes, SysUtils;

  procedure API(Lib: TLib);
  begin
    Tps_StringList.Create(TStringList,
                          nil,
                          'Classe que encapsula uma lista de strings',
                          cCatListas,
                          [], [], [],
                          True,
                          Lib.Classes);
  end;

{ Tps_StringList }

function Tps_StringList.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TStringList.Create;
end;

class procedure Tps_StringList.amAppend(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.Append(Stack.AsString(1));
end;

class procedure Tps_StringList.amAddStrings(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.AddStrings(TStringList(Stack.AsObject(1)));
end;

class procedure Tps_StringList.amBeginUpdate(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  o.BeginUpdate;
end;

class procedure Tps_StringList.amClear(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  o.Clear;
end;

class procedure Tps_StringList.amDelete(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.Delete(Stack.AsInteger(1));
end;

class procedure Tps_StringList.amEndUpdate(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  o.EndUpdate;
end;

class procedure Tps_StringList.amExchange(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  o.Exchange(Stack.AsInteger(1), Stack.AsInteger(2))
end;

class procedure Tps_StringList.amInsert(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  o.Insert(Stack.AsInteger(1), Stack.AsString(2));
end;

class procedure Tps_StringList.amLoadFromFile(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.LoadFromFile(Stack.AsString(1));
end;

class procedure Tps_StringList.amSaveToFile(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.SaveToFile(Stack.AsString(1));
end;

class procedure Tps_StringList.amSetCommaText(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.CommaText := Stack.AsString(1);
end;

class procedure Tps_StringList.amPutObject(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  o.objects[Stack.AsInteger(1)] := Stack.AsObject(2);
end;

class procedure Tps_StringList.amSetValue(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  o.values[Stack.AsString(1)] := Stack.AsString(2);
end;

class procedure Tps_StringList.amPut(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  o.strings[Stack.AsInteger(1)] := Stack.AsString(2);
end;

class procedure Tps_StringList.amSetTextStr(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.text := Stack.AsString(1);
end;

class procedure Tps_StringList.amSort(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  o.Sort;
end;

class procedure Tps_StringList.amSetDuplicates(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
    i: Integer;
begin
  o := TStringList(Stack.AsObject(2)); // self
  i := Stack.AsInteger(1);

  if i = 0 then
     o.Duplicates := dupIgnore else
  if i = 1 then
     o.Duplicates := dupError
  else
     o.Duplicates := dupAccept;
end;

class procedure Tps_StringList.amSetSorted(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  o.Sorted := Stack.AsBoolean(1);
end;

class procedure Tps_StringList.amAdd(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushInteger(o.add(Stack.AsString(1)));
end;

class procedure Tps_StringList.amAddObject(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(3)); // self
  Stack.PushInteger(o.AddObject(Stack.AsString(1), Stack.AsObject(2)));
end;

class procedure Tps_StringList.amEquals(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushBoolean(o.Equals(TStringList(Stack.AsObject(1))));
end;

class procedure Tps_StringList.amIndexOf(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushInteger(o.IndexOf(Stack.AsString(1)));
end;

class procedure Tps_StringList.amIndexOfName(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushInteger(o.IndexOfName(Stack.AsString(1)));
end;

class procedure Tps_StringList.amIndexOfObject(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushInteger(o.IndexOfObject(Stack.AsObject(1)));
end;

class procedure Tps_StringList.amGetCommaText(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  Stack.PushString(o.CommaText);
end;

class procedure Tps_StringList.amGetCount(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  Stack.PushInteger(o.Count);
end;

class procedure Tps_StringList.amGetName(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushString(o.Names[Stack.AsInteger(1)]);
end;

class procedure Tps_StringList.amGetObject(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushObject(o.Objects[Stack.AsInteger(1)]);
end;

class procedure Tps_StringList.amGetValue(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushString(o.Values[Stack.AsString(1)]);
end;

class procedure Tps_StringList.amGet(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(2)); // self
  Stack.PushString(o.Strings[Stack.asInteger(1)]);
end;

class procedure Tps_StringList.amGetTextStr(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  Stack.PushString(o.Text);
end;

class procedure Tps_StringList.amGetDuplicates(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  Stack.PushInteger(byte(o.Duplicates));
end;

class procedure Tps_StringList.amGetSorted(Const Func_Name: String; Stack: TexeStack);
var o: TStringList;
begin
  o := TStringList(Stack.AsObject(1)); // self
  Stack.PushBoolean(o.Sorted);
end;

class procedure Tps_StringList.amObjectByString(const Func_Name: String; Stack: TexeStack);
var o: TStringList;
    i: integer;
begin
  o := TStringList(Stack.getSelf());
  i := o.IndexOf(Stack.AsString(1));
  if i <> -1 then
     Stack.PushObject(o.Objects[i])
  else
     Stack.PushObject(nil);
end;

procedure Tps_StringList.AddMethods;
begin
  with Procs do
    begin
    Add('Append',
        'Adiciona uma string à lista'#13 +
        'Parâmetros:'#13 +
        '  string',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amAppend);

    Add('AddStrings',
        'Adiciona um grupo de strings à lista'#13 +
        'Parâmetros'#13 +
        '  Grupo de strings: TStringList',
        '',
        [pvtObject],
        [TStringList],
        [True],
        pvtNull,
        TObject,
        amAddStrings);

    Add('BeginUpdate',
        'Habilita o objeto TStringlist na posição quando a lista de strings está sendo alterada',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amBeginUpdate);

    Add('Clear',
        'Esvazia a lista e seus objetos associados',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amClear);

    Add('Delete',
        'Apaga da lista uma string especificada pelo seu índice'#13 +
        'Parâmetros:'#13 +
        '  Índice: Integer',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amDelete);

    Add('EndUpdate',
        'Habilita o objeto TStringList para manter a posição ao final de uma alteração',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amEndUpdate);

    Add('Exchange',
        'Troca a posição, na lista, de duas strings especificadas pelo seu índice:'#13 +
        'Parâmetros:'#13 +
        '  Índice1, Índice2: Integer',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amExchange);

    Add('Insert',
        'Insere uma string na posição (Índice) especificada'#13 +
        'Parâmetros:'#13 +
        '  - Índice: Integer,'#13 +
        '  - string',
        '',
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amInsert);

    Add('LoadFromFile',
        'Carrega a lista com as linhas de texto do arquivo especificado'#13 +
        'Parâmetros:'#13 +
        '  - Nome do arquivo: String',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amLoadFromFile);

    Add('SaveToFile',
        'Salva as strings da lista em um arquivo'#13 +
        'Parâmetros:'#13 +
        '  - Nome do Arquivo: string',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSaveToFile);

    Add('SetCommaText',
        'A adicionar'#13 +
        'Parâmetros:'#13 +
        '  - String',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetCommaText);

    Add('PutObject',
        'Altera o objeto associado à string especificada pelo índice'#13 +
        'Parâmetros: '#13 +
        '  - Índice: Integer'#13 +
        '  - Objeto: TObject',
        '',
        [pvtInteger, pvtObject],
        [nil, TObject],
        [False, True],
        pvtNull,
        TObject,
        amPutObject);

    Add('SetValue',
        'Altera a parte "valor" de uma string na forma "Nome=Valor" especificada pelo nome'#13 +
        'Parâmetros:'#13 +
        '  - Nome, Valor: String',
        '',
        [pvtString, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amSetValue);

    Add('Put',
        'Altera a string da posição (índice) especificada'#13 +
        'Parâmetros'#13 +
        '  Índice; integer,'#13 +
        '  String'#13 +
        '  ',
        '',
        [pvtInteger, pvtString],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amPut);

    Add('SetTextStr',
        'Altera a string com do texto de todas as strings da lista.'#13 +
        'As strings são delimitadas por "retorno de carro" e "alimentação de linha"'#13 +
        'Parâmetros:'#13 +
        '  - Texto: String',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetTextStr);

    Add('Sort',
        'Classifica, organiza as strings da lista em ordem ascendente',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amSort);

    Add('SetDuplicates',
        'Especifica se a lista aceita strings duplicadas em uma lista ordenada'#13 +
        'Parâmetros'#13 +
        '  0 - Ignora tentativas de adição'#13 +
        '  1 - Gera uma exceção'#13 +
        '  2 - Aceita strings duplicadas',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetDuplicates);

    Add('SetSorted',
        'Especifica se a lista for automaticamente organizada'#13 +
        'Parâmetros:'#13 +
        '  AutoOrganização: Boolean'#13 +
        '  ',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetSorted);
    end;

  with Functions do
    begin
    Add('Add',
        'Adiciona uma string "S"  no final da lista'#13 +
        'Parâmetros:'#13 +
        '  S: string;'#13 +
        '  ',
        '',
        [pvtString],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amAdd);

    Add('AddObject',
        'Adiciona uma string  à lista e associa um objeto à string'#13 +
        'Parâmetros:'#13 +
        '  string, '#13 +
        '  Objeto: TObject',
        '',
        [pvtString, pvtObject],
        [nil, TObject],
        [False, true],
        pvtInteger,
        TObject,
        amAddObject);

    Add('Equals',
        'Compara a lista de strings com a de outro objeto TStringList e retorna "verdadeiro" se forem iguais'#13 +
        'Parâmetros:'#13 +
        '  Lista: TStringList',
        '',
        [pvtObject],
        [TStringList],
        [True],
        pvtBoolean,
        TObject,
        amEquals);

    Add('IndexOf',
        'Retorna a posição (índice) de uma string na lista'#13 +
        'Parâmetros:'#13 +
        '  - String',
        '',
        [pvtString],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amIndexOf);

    Add('IndexOfName',
        'Retorna a posição (índice) da primeira string da lista na forma "Nome=Valor", com o nome especificado.'#13 +
        'Parâmetros:'#13 +
        '  - Nome: String',
        '',
        [pvtString],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amIndexOfName);

    Add('IndexOfObject',
        'Retorna a posição (índice) da primeira string da lista associada com o objeto especificado'#13 +
        'Parâmetros:'#13 +
        '  - Objeto: TObject',
        '',
        [pvtObject],
        [TObject],
        [True],
        pvtInteger,
        TObject,
        amIndexOfObject);

    Add('ObjectByString',
        'Retorna o Objeto associado com a string'#13 +
        'Se a string não for encontrada "nil" é retornado.',
        '',
        [pvtString],
        [nil],
        [false],
        pvtObject,
        TObject,
        amObjectByString);

    Add('GetCommaText',
        'A adicionar',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetCommaText);

    Add('Count',
        'Retorna o número de strings na lista',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetCount);

    Add('GetName',
        'Retorna a parte nome de uma sring na forma "Nome = Valor"'#13 +
        'Parâmetros:'#13 +
        '  - Índice: Integer',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amGetName);

    Add('GetObject',
        'Retorna o objeto associado à string especificada pelo índice'#13 +
        'Parâmetros'#13 +
        '  - Indíce: Integer',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TObject,
        amGetObject);

    Add('GetValue',
        'Retorna a parte "Valor" de uma string na forma "Nome = valor" especificada pelo nome'#13 +
        'Parâmetros:'#13 +
        '  - Nome: string',
        '',
        [pvtString],
        [nil],
        [False],
        pvtString,
        TObject,
        amGetValue);

    Add('Get',
        'Retorna a string da posição (índice) especificada'#13 +
        'Parâmetros:'#13 +
        '  - Índice: Integer',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtString,
        TObject,
        amGet);

    Add('GetTextStr',
        'Retorna uma string com o texto de todas as strings da lista.'#13 +
        'As strings delimitadas por "retorno de carro" e "alimentação de linha"',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetTextStr);

    Add('GetDuplicates',
        'Olhar "SetDuplicates"',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetDuplicates);

    Add('GetSorted',
        'Retorna verdadeiro se a lista for automaticamente organizada',
        '',
        [],
        [],
        [],
        pvtBoolean,
        TObject,
        amGetSorted);
    end;
end;

end.

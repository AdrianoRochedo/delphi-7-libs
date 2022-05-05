{ ultima versao }
unit Lib_String;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Classes,
     SysUtils,
     SysUtilsEx;

type
  TString_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn (Functions: TFunctionList); override;

    class procedure amFloatToStr          (Const Func_Name: String; Stack: TexeStack);
    class procedure amIntToStr            (Const Func_Name: String; Stack: TexeStack);
    class procedure amBooleanToStr        (Const Func_Name: String; Stack: TexeStack);
    class procedure amCopy                (Const Func_Name: String; Stack: TexeStack);
    class procedure amPos                 (Const Func_Name: String; Stack: TexeStack);
    class procedure amLowerCase           (Const Func_Name: String; Stack: TexeStack);
    class procedure amUpperCase           (Const Func_Name: String; Stack: TexeStack);
    class procedure amCompareStr          (Const Func_Name: String; Stack: TexeStack);
    class procedure amCompareText         (Const Func_Name: String; Stack: TexeStack);
    class procedure amTrim                (Const Func_Name: String; Stack: TexeStack);
    class procedure amTrimLeft            (Const Func_Name: String; Stack: TexeStack);
    class procedure amTrimRight           (Const Func_Name: String; Stack: TexeStack);
    class procedure amQuotedStr           (Const Func_Name: String; Stack: TexeStack);
    class procedure amFormatFloat         (Const Func_Name: String; Stack: TexeStack);
    class procedure amStrToFloat          (Const Func_Name: String; Stack: TexeStack);
    class procedure amLTrimZeros          (Const Func_Name: String; Stack: TexeStack);
    class procedure amRightStr            (Const Func_Name: String; Stack: TexeStack);
    class procedure amRTrim               (Const Func_Name: String; Stack: TexeStack);
    class procedure amDeleteSubStr        (Const Func_Name: String; Stack: TexeStack);
    class procedure amReplaceSubStr       (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetSubString        (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemovesDuplicates   (Const Func_Name: String; Stack: TexeStack);
    class procedure amStrCenter           (Const Func_Name: String; Stack: TexeStack);
    class procedure amChangeChar          (Const Func_Name: String; Stack: TexeStack);
    class procedure amEncrypt             (Const Func_Name: String; Stack: TexeStack);
    class procedure amDecrypt             (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddCharL            (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddCharR            (Const Func_Name: String; Stack: TexeStack);
    class procedure amLeftStr             (Const Func_Name: String; Stack: TexeStack);
    class procedure amStrPadZeroL         (Const Func_Name: String; Stack: TexeStack);
    class procedure amDelSpace1           (Const Func_Name: String; Stack: TexeStack);
    class procedure amDelSpaces           (Const Func_Name: String; Stack: TexeStack);
    class procedure amLength              (Const Func_Name: String; Stack: TexeStack);
    class procedure amSplit               (Const Func_Name: String; Stack: TexeStack);
  end;

  TString_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn (Procs: TProcList); override;

    class procedure amDelete              (Const Func_Name: String; Stack: TexeStack);
    class procedure amInsert              (Const Func_Name: String; Stack: TexeStack);
    class procedure amStringToStrings     (Const Func_Name: String; Stack: TexeStack);
  end;

procedure API(Lib: TLib);
begin
  TString_Functions.AddFunctionsIn(Lib.Functions);
  TString_Procs.AddProcsIn(Lib.Procs);
end;

class procedure TString_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('FloatToStr',
        'Transforma um número Real em String.'#13+
        'Parâmetros: Valor e Número de casas decimais',
        cCatConversao,
        [pvtReal, pvtInteger],
        [nil, nil],
        [False, False],
        pvtString,
        TObject,
        amFloatToStr);

    Add('IntToStr',
        'Transforma um número Inteiro em String',
        cCatConversao,
        [pvtInteger], [nil], [False], pvtString, TObject, amIntToStr);

    Add('BooleanToStr',
        'Transforma um Booleano em String',
        cCatConversao,
        [pvtBoolean], [nil], [False], pvtString, TObject, amBooleanToStr);

    Add('Pos',
        'Retorna o índice do primeiro caracter da sub-string que ocorre na string passada'#13 +
        'Parâmetros: Sub-String, String',
        cCatString,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtInteger,
        TObject,
        amPos);

    Add('Length',
        'Retorna o tamanho de uma string'#13 +
        'Parâmetros: String',
        cCatString,
        [pvtString],
        [nil],
        [False],
        pvtInteger,
        TObject,
        amLength);

    Add('Copy',
        'Retorna uma sub-string de uma string'#13 +
        'Parâmetros: String, Posição Inicial, Quantidade',
        cCatString,
        [pvtString, pvtInteger, pvtInteger],
        [nil      , nil       , nil       ],
        [False    , False     , False     ],
        pvtString,
        TObject,
        amCopy);

    Add('LowerCase',
        'Converte todos os caracteres de uma string para minúsculo',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amLowerCase);

    Add('UpperCase',
        'Converte todos os caracteres de uma string para maiúsculo',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amUpperCase);

    Add('CompareStr',
        'Faz a comparação entre S1 e S2 diferenciando maiúsculas de minúsculas'#13 +
        'Retorno:'#13 +
        '  0 se forem iguais'#13 +
        '  Menor que 0 se S1 for menor que S2'#13 +
        '  Maior que 0 se S1 for maior que S2',
        cCatString,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtInteger,
        TObject,
        amCompareStr);

    Add('CompareText',
        'Faz a comparação entre S1 e S2 não diferenciando maiúsculas de minúsculas'#13 +
        'Retorno:'#13 +
        '  0 se forem iguais'#13 +
        '  Menor que 0 se S1 for menor que S2'#13 +
        '  Maior que 0 se S1 for maior que S2',
        cCatString,
        [pvtString,pvtString],
        [nil      ,nil      ],
        [False    ,False    ],
        pvtInteger,
        TObject,
        amCompareText);

    Add('Trim',
        'Remove os espaços em branco e os caracteres de controle a esquerda e a direita de uma string',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amTrim);

    Add('TrimLeft',
        'Remove os espaços em branco e os caracteres de controle a esquerda de uma string',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amTrimLeft);

    Add('TrimRight',
        'Remove os espaços em branco e os caracteres de controle a direita de uma string',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amTrimRight);

    Add('QuotedStr',
        'Retorna a string passada entre aspas',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amQuotedStr);

    Add('FormatFloat',
        'Formata um valor usando uma máscara específica',
        cCatString,
        [pvtString,pvtReal],
        [nil      ,nil    ],
        [False    ,False  ],
        pvtString,
        TObject,
        amFormatFloat);

    Add('StrToFloat',
        'Converte uma string em um valor real',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtReal,
        TObject,
        amStrToFloat);

    Add('LTrimZeros',
        'Remove os caracteres "0" a esquerda de uma string',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amLTrimZeros);

    Add('RightStr',
        'Retorna os N caracteres a direita de uma string',
        cCatString,
        [pvtString, pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtString,
        TObject,
        amRightStr);

    Add('RTrim',
        '',
        cCatString,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , false    ],
        pvtString,
        TObject,
        amRTrim);

    Add('DeleteSubStr',
        'Remove uma Sub-String que se encontra entre os caracteres de Inicio e de Fim'#13 +
        'Parâmetros: String, Caracter Inicial, Caracter Final',
        cCatString,
        [pvtString, pvtString, pvtString],
        [nil      , nil      , nil      ],
        [False    , False    , False    ],
        pvtString,
        TObject,
        amDeleteSubStr);

    Add('ReplaceSubStr',
        'Troca uma Sub-String por outra'#13 +
        'Parâmetros: String, Sub-String antiga, Sub-String nova',
        cCatString,
        [pvtString,pvtString,pvtString],
        [nil      ,nil      ,nil      ],
        [False    ,False    ,False    ],
        pvtString,
        TObject,
        amReplaceSubStr);

    Add('SubString',
        'Retorna a Sub-String que se encontra entre os caracteres de Inicio e de Fim'#13 +
        'Parâmetros: String, Caracter Inicial, Caracter Final',
        cCatString,
        [pvtString, pvtString, pvtString],
        [nil      , nil      , nil      ],
        [False    , False    , False    ],
        pvtString,
        TObject,
        amGetSubString);

    Add('Split',
        'Quebra uma string em pedaços'#13 +
        'Parâmetros: String, Caracter separador',
        cCatString,
        [pvtString, pvtString],
        [nil      , nil],
        [False    , False],
        pvtObject,
        TStringList,
        amSplit);

    Add('RemoveDuplicates',
        'Remove as Strings repetidas de uma Lista de Strings',
        cCatString,
        [pvtObject],
        [TStrings ],
        [True     ],
        pvtObject,
        TStrings,
        amRemovesDuplicates);

    Add('StrCenter',
        'Centraliza uma string em um determinado número de espaços',
        cCatString,
        [pvtString, pvtInteger],
        [nil      , nil       ],
        [False    , False     ],
        pvtString,
        TObject,
        amStrCenter);

    Add('ChangeChar',
        'Troca um caracter por outro'#13 +
        'Parâmetros: String, Caracter antigo, Caracter novo',
        cCatString,
        [pvtString, pvtString, pvtString  ],
        [nil      , nil      , nil        ],
        [False    , False    , False      ],
        pvtString,
        TObject,
        amChangeChar);

    Add('Encrypt',
        'Codifica um string usando uma determinada chave',
        cCatString,
        [pvtString,pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtString,
        TObject,
        amEncrypt);

    Add('Decrypt',
        'Decodifica um string usando uma determinada chave',
        cCatString,
        [pvtString,pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtString,
        TObject,
        amDecrypt);

    Add('AddCharL',
        'Adiciona um caracter N vezes a esquerda de uma string',
        cCatString,
        [pvtString,pvtString,pvtInteger],
        [nil      ,nil         ,nil          ],
        [False    ,False       ,False        ],
        pvtString,
        TObject,
        amAddCharL);

    Add('AddCharR',
        'Adiciona um caracter N vezes a direita de uma string',
        cCatString,
        [pvtString,pvtString,pvtInteger],
        [nil      ,nil         ,nil          ],
        [False    ,False       ,False        ],
        pvtString,
        TObject,
        amAddCharR);

    Add('LeftStr',
        'Retorna os N caracteres a esquerda de uma string',
        cCatString,
        [pvtString,pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtString,
        TObject,
        amLeftStr);

    Add('StrPadZeroL',
        'Adiciona "0"s a esquerda de uma string',
        cCatString,
        [pvtString,pvtInteger],
        [nil      ,nil       ],
        [False    ,False     ],
        pvtString,
        TObject,
        amStrPadZeroL);

    Add('DelSpace1',
        'Normaliza os espaços em branco de uma string'#13 +
        'Ex: "abc   def  gh" -->  "abc def gh"',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amDelSpace1);

    Add('DelSpaces',
        'Remove os espaços em branco de uma string'#13 +
        'Ex: "  ab c d ef " --> "abcdef"',
        cCatString,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amDelSpaces);
     end;
  end;

{--------------------------------------------------------------------------------------}
class procedure TString_Functions.amLowerCase(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(LowerCase(Stack.AsString(1)));
end;

class procedure TString_Functions.amUpperCase(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(UpperCase(Stack.AsString(1)));
end;

class procedure TString_Functions.amCompareStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(CompareStr(Stack.AsString(1), Stack.AsString(2)));
end;

class procedure TString_Functions.amCompareText(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(CompareText(Stack.AsString(1), Stack.AsString(2)));
end;

class procedure TString_Functions.amTrim(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(Trim(Stack.AsString(1)));
end;

class procedure TString_Functions.amTrimLeft(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(TrimLeft(Stack.AsString(1)));
end;

class procedure TString_Functions.amTrimRight(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(TrimRight(Stack.AsString(1)));
end;

class procedure TString_Functions.amQuotedStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(QuotedStr(Stack.AsString(1)));
end;

class procedure TString_Functions.amFormatFloat(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(FormatFloat(Stack.AsString(1),Stack.AsFloat(2)));
end;

class procedure TString_Functions.amStrToFloat(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(StrToFloat(Stack.AsString(1)));
end;

class procedure TString_Functions.amLTrimZeros(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(LTrimZeros(Stack.AsString(1)));
end;

class procedure TString_Functions.amRightStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(RightStr(Stack.AsString(1),Stack.AsInteger(2)));
end;

class procedure TString_Functions.amRTrim(Const Func_Name: String; Stack: TexeStack);
var s: String;
    c: TCharSet;
    i: Integer;
begin
  c := [];
  s := Stack.AsString(2);

  if s <> '' then
     for i := 1 to Length(s) do
        Include(c, s[i]);

  Stack.PushString(RTrim(Stack.AsString(1), c));
end;

class procedure TString_Functions.amDeleteSubStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(DeleteSubStr(Stack.AsString(1),Stack.AsString(2),Stack.AsString(3)));
end;

class procedure TString_Functions.amReplaceSubStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ReplaceSubStr(Stack.AsString(1),Stack.AsString(2),Stack.AsString(3)));
end;

class procedure TString_Functions.amGetSubString(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(SubString(Stack.AsString(1),Stack.AsString(2),Stack.AsString(3)));
end;

class procedure TString_Functions.amRemovesDuplicates(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    RemoveDuplicates(TStrings(Stack.AsObject(1)))
    );
end;

class procedure TString_Functions.amStrCenter(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(StrCenter(Stack.AsString(1),Stack.AsInteger(2)));
end;

class procedure TString_Functions.amChangeChar(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ChangeChar(Stack.AsString(1), Stack.AsChar(2), Stack.AsChar(3)));
end;

class procedure TString_Functions.amEncrypt(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(Encrypt(Stack.AsString(1), Stack.AsInteger(2)));
end;

class procedure TString_Functions.amDecrypt(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(Decrypt(Stack.AsString(1), Stack.AsInteger(2)));
end;

class procedure TString_Functions.amAddCharL(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(AddCharL(Stack.AsChar(1), Stack.AsString(2), Stack.AsInteger(3)));
end;

class procedure TString_Functions.amAddCharR(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(AddCharR(Stack.AsChar(1), Stack.AsString(2), Stack.AsInteger(3)));
end;

class procedure TString_Functions.amLeftStr(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(LeftStr(Stack.AsString(1),Stack.AsInteger(2)));
end;

class procedure TString_Functions.amStrPadZeroL(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(StrPadZeroL(Stack.AsString(1),Stack.AsInteger(2)));
end;

class procedure TString_Functions.amDelSpace1(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(DelSpacesToOne(Stack.AsString(1)));
end;

class procedure TString_Functions.amDelSpaces(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(DelSpaces(Stack.AsString(1)));
end;

class procedure TString_Functions.amPos(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(System.Pos(Stack.AsString(1), Stack.AsString(2)));
end;

class procedure TString_Functions.amCopy(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(System.Copy(Stack.AsString(1), Stack.AsInteger(2), Stack.AsInteger(3)));
end;

{ TString_Procs }

class procedure TString_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    Add('Delete',
      '',
      cCatString,
      [pvtString, pvtInteger, pvtInteger],
      [nil      , nil       , nil       ],
      [True     , False     , False     ],
      pvtNull,
      TObject,
      amDelete);

    Add('Insert',
      '',
      cCatString,
      [pvtString, pvtString, pvtInteger],
      [nil      , nil      , nil       ],
      [False    , True     , True      ],
      pvtNull,
      TObject,
      amInsert);

    Add('StringToStrings',
        'Quebra uma String em n pedaços de acordo com os separadores',
        cCatString,
        [pvtString, pvtObject, pvtString],
        [nil      , TStrings , nil      ],
        [False    , True     , False    ],
        pvtNull,
        TObject,
        amStringToStrings);
    end;
end;

class procedure TString_Procs.amDelete(Const Func_Name: String; Stack: TexeStack);
var s: String;
    v: Tvariable;
begin
  v := Stack.AsReferency(1);
  s := v.Value;
  Delete(s, Stack.AsInteger(2), Stack.AsInteger(3));
  v.Value := s;
end;

class procedure TString_Procs.amInsert(Const Func_Name: String; Stack: TexeStack);
var s: String;
    v: Tvariable;
begin
  v := Stack.AsReferency(2);
  s := v.Value;
  Insert(Stack.AsString(1), s, Stack.AsInteger(3));
  v.Value := s;
end;

class procedure TString_Procs.amStringToStrings(Const Func_Name: String; Stack: TexeStack);
var v: TVariable;
    s: String;
    c: TCharSet;
    o: TStrings;
    i: Integer;
begin
  c := [];
  v := Stack.AsReferency(2);
  s := Stack.AsString(3);

  if s <> '' then
     for i := 1 to Length(s) do
        Include(c, s[i]);

  o := TStrings(Integer(v.Value));
  Split(Stack.AsString(1), o, c);
  v.Value := Integer(o);
end;

class procedure TString_Functions.amFloatToStr(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( FloatToStrF(Stack.AsFloat(1), ffFixed, 15, Stack.AsInteger(2)) );
end;

class procedure TString_Functions.amBooleanToStr(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( BooleanToStr(Stack.AsBoolean(1)) );
end;

class procedure TString_Functions.amLength(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( System.Length(Stack.AsString(1)) );
end;

class procedure TString_Functions.amSplit(const Func_Name: String; Stack: TexeStack);
var s: string;
    c: char;
    SL: TStrings;
begin
  s := Stack.AsString(1);
  c := Stack.AsChar(2);
  SL := TStringList.Create();
  Split(s, SL, [c]);
  Stack.PushObject(SL);
end;

class procedure TString_Functions.amIntToStr(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( IntToStr(Stack.AsInteger(1)) );
end;

end.





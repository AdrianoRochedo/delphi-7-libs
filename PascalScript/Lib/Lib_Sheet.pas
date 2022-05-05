unit Lib_Sheet;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Classes,
     SysUtils,
     drPlanilha,
     Forms,
     wsVec;

type
  Tps_Planilha = class(TpsClass)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;

    // métodos de escrita
    class procedure amWrite            (const Func_Name: String; Stack: TexeStack);
    class procedure amWriteFloat       (const Func_Name: String; Stack: TexeStack);
    class procedure amWriteCenter      (const Func_Name: String; Stack: TexeStack);
    class procedure amWriteFloatCenter (const Func_Name: String; Stack: TexeStack);
    class procedure amWriteVecInCol    (const Func_Name: String; Stack: TexeStack);
    class procedure amWriteVecInRow    (const Func_Name: String; Stack: TexeStack);

    // métodos para obtenção de valores
    class procedure amGetText    (const Func_Name: String; Stack: TexeStack);
    class procedure amGetEntry   (const Func_Name: String; Stack: TexeStack);
    class procedure amGetFloat   (const Func_Name: String; Stack: TexeStack);
    class procedure am_ColToVec  (Const Func_Name: String; Stack: TexeStack);
    class procedure am_RowToVec  (Const Func_Name: String; Stack: TexeStack);
    class procedure am_ColToVec2 (Const Func_Name: String; Stack: TexeStack);
    class procedure am_RowToVec2 (Const Func_Name: String; Stack: TexeStack);

    // métodos de formatação
    class procedure amSetCellFont (const Func_Name: String; Stack: TexeStack);

    // clipboard
    class procedure amCut   (const Func_Name: String; Stack: TexeStack);
    class procedure amCopy  (const Func_Name: String; Stack: TexeStack);
    class procedure amPaste (const Func_Name: String; Stack: TexeStack);

    // métodos gerais
    class procedure amClear   (const Func_Name: String; Stack: TexeStack);
    class procedure amPrint   (const Func_Name: String; Stack: TexeStack);
    class procedure amShow    (const Func_Name: String; Stack: TexeStack);

    // leitura e salvamento
    class procedure amLoadFromFile (const Func_Name: String; Stack: TexeStack);
    class procedure amSaveToFile   (const Func_Name: String; Stack: TexeStack);

    // Número de Elementos

    class procedure amSetNRows   (const Func_Name: String; Stack: TexeStack);
    class procedure amSetNCols   (const Func_Name: String; Stack: TexeStack);
    class procedure amGetNRows   (const Func_Name: String; Stack: TexeStack);
    class procedure amGetNCols   (const Func_Name: String; Stack: TexeStack);
  end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  Tps_Planilha.Create(TPlanilha,
                      nil,
                      'Classe que encapsula a funcionalidade de uma Planilha',
                      cCatPlanilhas,
                      [], [], [],
                      True,
                      Lib.Classes);
end;

{ Tps_Planilha }

function Tps_Planilha.CreateObject(Stack: TexeStack): TObject;
var x: TPlanilha;
begin
  x := TPlanilha.Create;
  x.Prepare;
  Result := x;
end;

procedure Tps_Planilha.AddMethods;
begin
  with Procs do
    begin
    Add('Write',
        'Escreve um texto na célula[Linha, Coluna]'#13 +
        'Linha e Coluna começam em 1'#13 +
        'Parâmetros: Linha, Coluna, Texto',
        '',
        [pvtInteger, pvtInteger, pvtString],
        [nil       , nil       , nil      ],
        [False     , False     , False    ],
        pvtNull,
        TObject,
        amWrite);

    Add('WriteFloat',
        'Escreve um número na célula[Linha, Coluna]'#13 +
        'Linha e Coluna começam em 1'#13 +
        'Parâmetros: Linha, Coluna, Valor',
        '',
        [pvtInteger, pvtInteger, pvtReal],
        [nil       , nil       , nil     ],
        [False     , False     , False   ],
        pvtNull,
        TObject,
        amWriteFloat);

    Add('WriteCenter',
        'Escreve um texto centralizado na célula[Linha, Coluna]'#13 +
        'Linha e Coluna começam em 1'#13 +
        'Parâmetros: Linha, Coluna, Texto',
        '',
        [pvtInteger, pvtInteger, pvtString],
        [nil       , nil       , nil      ],
        [False     , False     , False    ],
        pvtNull,
        TObject,
        amWriteCenter);

    Add('WriteFloatCenter',
        'Escreve um número centralizado na célula[Linha, Coluna]'#13 +
        'Linha e Coluna começam em 1'#13 +
        'Parâmetros: Linha, Coluna, Valor',
        '',
        [pvtInteger, pvtInteger, pvtReal],
        [nil       , nil       , nil    ],
        [False     , False     , False  ],
        pvtNull,
        TObject,
        amWriteFloatCenter);

    Add('WriteVecInCol',
        'Escreve um vetor em uma coluna da planilha'#13 +
        'Parâmetros: Vetor, Coluna, Linha Inicial',
        '',
        [pvtObject, pvtInteger, pvtInteger],
        [TwsVec   , nil       , nil       ],
        [True     , False     , False     ],
        pvtNull,
        TObject,
        amWriteVecInCol);

    Add('WriteVecInRow',
        'Escreve um vetor em uma linha da planilha'#13 +
        'Parâmetros: Vetor, Linha, Coluna Inicial',
        '',
        [pvtObject, pvtInteger, pvtInteger],
        [TwsVec   , nil       , nil       ],
        [True     , False     , False     ],
        pvtNull,
        TObject,
        amWriteVecInRow);

    Add('SetCellFont',
        'Características do texto de uma célula[Linha, Coluna]'#13 +
        'Linha e Coluna começam em 1'#13 +
        'Parâmetros:'#13 +
        '  - Linha'#13 +
        '  - Coluna'#13 +
        '  - Nome da fonte'#13 +
        '  - Tamanho'#13 +
        '  - Cor'#13 +
        '  - Negrito'#13 +
        '  - Itálico'#13 +
        '  - Sublinhado',
        '',
        [pvtInteger, pvtInteger, pvtString, pvtInteger, pvtInteger, pvtBoolean, pvtBoolean, pvtBoolean],
        [nil       , nil       , nil      , nil       , nil       , nil       , nil       , nil       ],
        [False     , False     , False    , False     , False     , False     , False     , False     ],
        pvtNull,
        TObject,
        amSetCellFont);

    Add('Cut',
        'Recorta o conteúdo da planilha para a clipboard',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amCut);

    Add('Copy',
        'Copia o conteúdo da planilha para a clipboard',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amCopy);

    Add('Paste',
        'Cola o conteúdo da clipboard na planilha',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amPaste);

    Add('Clear',
        'Limpa o conteúdo da planilha',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amClear);

    Add('Print',
        'Imprime o conteúdo da planilha em uma impressora',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amPrint);

    Add('Cut',
        'Recorta o conteúdo da planilha',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amCut);

    Add('Show',
        'Mostra o Planilha',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amShow);

    Add('LoadFromFile',
        'Lê o conteúdo de um arquivo para a planilha'#13 +
        'Parâmetro: Nome do arquivo',
        '',
        [pvtString],
        [nil      ],
        [False    ],
        pvtNull,
        TObject,
        amLoadFromFile);

    Add('SaveToFile',
        'Salva o conteúdo da planilha em um arquivo'#13 +
        'Parâmetros: Nome do arquivo, Tipo do Arquivo ("TXT" ou "XLS")',
        '',
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtNull,
        TObject,
        amSaveToFile);

    Add('SetNRows',
        'Configura o número de linhas da Planilha.',
        '',
        [pvtInteger],
        [nil       ],
        [False     ],
        pvtNull,
        TObject,
        amSetNRows);

    Add('SetNCols',
        'Configura o número de Colunas da Planilha.',
        '',
        [pvtInteger],
        [nil       ],
        [False     ],
        pvtNull,
        TObject,
        amSetNCols);
    end;

  with Functions do
    begin
    Add('GetFloat',
        'Obtém o valor numérico de uma célula.'#13 +
        'Parâmetros: Linha e Coluna.',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil       ],
        [False     , False     ],
        pvtReal,
        TObject,
        amGetFloat);

    Add('GetEntry',
        'Obtém o texto entrado em uma célula.'#13 +
        'Parâmetros: Linha e Coluna.',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil       ],
        [False     , False     ],
        pvtString,
        TObject,
        amGetEntry);

    Add('GetText',
        'Obtém o texto visualizado em uma célula.'#13 +
        'Parâmetros: Linha e Coluna.',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil       ],
        [False     , False     ],
        pvtString,
        TObject,
        amGetText);

    Add('GetNRows',
        'Retorna o número de linhas da Planilha.',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetNRows);

    Add('GetNCols',
        'Retorna o número de colunas da Planilha.',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetNCols);

    Add('ColToVec',
        'Retorna um vetor com os dados da coluna da Planilha.'#13 +
        'Parâmetros: Coluna, Linha Inicial, Linha Final',
        '',
        [pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil],
        [False, False, False],
        pvtObject,
        TwsVec,
        am_ColToVec);

    Add('RowToVec',
        'Retorna um vetor com os dados de uma linha da Planilha.'#13 +
        'Parâmetros: Linha, Coluna Inicial, Coluna Final',
        '',
        [pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil],
        [False, False, False],
        pvtObject,
        TwsVec,
        am_RowToVec);

    Add('ColToVec2',
        'Retorna um vetor com os dados da coluna da Planilha.'#13 +
        'Parâmetros: Coluna, Linha Inicial',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtObject,
        TwsVec,
        am_ColToVec2);

    Add('RowToVec2',
        'Retorna um vetor com os dados de uma linha da Planilha.'#13 +
        'Parâmetros: Linha, Coluna Inicial',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtObject,
        TwsVec,
        am_RowToVec2);
    end;
end;

class procedure Tps_Planilha.amClear(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Clear;
end;

class procedure Tps_Planilha.amCopy(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Copy;
end;

class procedure Tps_Planilha.amCut(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Cut;
end;

class procedure Tps_Planilha.amLoadFromFile(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(2)).LoadFromFile(Stack.AsString(1));
end;

class procedure Tps_Planilha.amPaste(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Paste;
end;

class procedure Tps_Planilha.amPrint(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Print(False);
end;

class procedure Tps_Planilha.amSaveToFile(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(3)).SaveToFile(Stack.AsString(1), Stack.AsString(2));
end;

class procedure Tps_Planilha.amSetCellFont(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(9)).
    SetCellFont(Stack.AsInteger(1),   // Row
                Stack.AsInteger(2),   // Col
                Stack.AsString (3),   // FontName
                Stack.AsInteger(4),   // Size
                Stack.AsInteger(5),   // Color
                Stack.AsBoolean(6),   // Bold
                Stack.AsBoolean(7),   // Italic
                Stack.AsBoolean(8));  // Underline
end;

class procedure Tps_Planilha.amShow(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(1)).Show;
end;

class procedure Tps_Planilha.amWrite(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)).  // self
    Write(Stack.AsInteger (1),   // row
          Stack.AsInteger (2),   // col
          Stack.AsString  (3));  // texto
end;

class procedure Tps_Planilha.amWriteCenter(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)).       // self
    WriteCenter(Stack.AsInteger(1),   // row
                Stack.AsInteger(2),   // col
                Stack.AsString (3));  // texto
end;

class procedure Tps_Planilha.amWriteFloat(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)). // self
    Write(Stack.AsInteger(1),   // row
          Stack.AsInteger(2),   // col
          Stack.AsFloat  (3));  // valor
end;

class procedure Tps_Planilha.amWriteFloatCenter (const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)).       // self
    WriteCenter(Stack.AsInteger(1),   // row
                Stack.AsInteger(2),   // col
                Stack.AsFloat  (3));  // valor
end;

class procedure Tps_Planilha.amWriteVecInCol(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)).                 // self
    WriteVecInCol(TwsVec(Stack.AsObject (1)),   // vetor
                         Stack.AsInteger(2),    // col
                         Stack.AsInteger(3));   // linha inicial
end;

class procedure Tps_Planilha.amWriteVecInRow(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(4)).                 // self
    WriteVecInRow(TwsVec(Stack.AsObject(1)),    // vetor
                         Stack.AsInteger(2),    // linha
                         Stack.AsInteger(3));   // coluna inicial
end;

class procedure Tps_Planilha.amGetFloat(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TPlanilha(Stack.AsObject(3)).GetFloat(Stack.AsInteger(1), Stack.AsInteger(2)));
end;

class procedure Tps_Planilha.amGetEntry(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(
    TPlanilha(Stack.AsObject(3)).GetEntry(Stack.AsInteger(1), Stack.AsInteger(2)));
end;

class procedure Tps_Planilha.amGetText(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(
    TPlanilha(Stack.AsObject(3)).GetText(Stack.AsInteger(1), Stack.AsInteger(2)));
end;

class procedure Tps_Planilha.amGetNCols(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TPlanilha(Stack.AsObject(1)).nCols);
end;

class procedure Tps_Planilha.amGetNRows(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TPlanilha(Stack.AsObject(1)).nRows);
end;

class procedure Tps_Planilha.amSetNCols(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(2)).nCols := Stack.AsInteger(1);
end;

class procedure Tps_Planilha.amSetNRows(const Func_Name: String; Stack: TexeStack);
begin
  TPlanilha(Stack.AsObject(2)).nRows := Stack.AsInteger(1);
end;

class procedure Tps_Planilha.am_ColToVec(Const Func_Name: String; Stack: TexeStack);
var o: TPlanilha;
begin
  o := TPlanilha(Stack.AsObject(4));
  Stack.PushObject(o.ColToVec(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsInteger(3)))
end;

class procedure Tps_Planilha.am_RowToVec(Const Func_Name: String; Stack: TexeStack);
var o: TPlanilha;
begin
  o := TPlanilha(Stack.AsObject(4));              
  Stack.PushObject(o.RowToVec(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsInteger(3)))
end;

class procedure Tps_Planilha.am_ColToVec2(const Func_Name: String; Stack: TexeStack);
var o: TPlanilha;
begin
  o := TPlanilha(Stack.AsObject(3));
  Stack.PushObject(o.ColToVec(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure Tps_Planilha.am_RowToVec2(const Func_Name: String; Stack: TexeStack);
var o: TPlanilha;
begin
  o := TPlanilha(Stack.AsObject(3));
  Stack.PushObject(o.RowToVec(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

end.

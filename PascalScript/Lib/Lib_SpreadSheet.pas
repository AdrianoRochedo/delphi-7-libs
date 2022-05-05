unit Lib_SpreadSheet;

// Unidade gerada automaticamente pelo Programa <Gerador de Bibliotecas>

interface
uses psBase;

  procedure API(Lib: TLib);

type
  Tps_SpreadSheetBook = class(TpsClass)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;
    class procedure am_ActiveSheet         (Const Func_Name: String; Stack: TexeStack);
    class procedure am_getActiveSheetIndex (Const Func_Name: String; Stack: TexeStack);
    class procedure am_LoadFromFile        (Const Func_Name: String; Stack: TexeStack);
    class procedure am_NewSheet            (Const Func_Name: String; Stack: TexeStack);
    class procedure am_SaveToFile          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setActiveSheetIndex (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setCaption          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_SheetCount          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Show                (Const Func_Name: String; Stack: TexeStack);
    class procedure am_ShowModal           (Const Func_Name: String; Stack: TexeStack);
    class procedure am_getCaption          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setBounds           (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_SpreadSheetWrapper = class(TpsClass)
  public
    procedure AddMethods; override;
    class procedure am_ColCount          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_ColToVec          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Copy              (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Cut               (Const Func_Name: String; Stack: TexeStack);
    class procedure am_GetDisplayText    (Const Func_Name: String; Stack: TexeStack);
    class procedure am_GetFloat          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_GetInt            (Const Func_Name: String; Stack: TexeStack);
    class procedure am_GetText           (Const Func_Name: String; Stack: TexeStack);
    class procedure am_RowCount          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_Paste             (Const Func_Name: String; Stack: TexeStack);
    class procedure am_RowToVec          (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setCellFont       (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setColWidth       (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setShowHeaders    (Const Func_Name: String; Stack: TexeStack);
    class procedure am_writeFloat        (Const Func_Name: String; Stack: TexeStack);
    class procedure am_writeText         (Const Func_Name: String; Stack: TexeStack);
    class procedure am_writeCenterFloat  (Const Func_Name: String; Stack: TexeStack);
    class procedure am_writeCenterText   (Const Func_Name: String; Stack: TexeStack);
    class procedure am_WriteVecInCol     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_WriteVecInRow     (Const Func_Name: String; Stack: TexeStack);
    class procedure am_setCellBold       (Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses {$ifdef WinStat} wsVec, {$endif}
     SpreadSheetBook,
     SheetWrapper;

  procedure API(Lib: TLib);
  begin
    Tps_SpreadSheetBook.Create(
        TSpreadSheetBook,
        nil,
        'Planilha estilo Excel',
        cCatPlanilhas,
        [], [], [],
        True,
        Lib.Classes);

    Tps_SpreadSheetWrapper.Create(
        TSheet,
        nil,
        'Folha de uma planilha.'#13 +
        'Utilize o botão de função de seu "mouse" para ter acesso a um menu com opções',
        cCatPlanilhas,
        [], [], [],
        False,
        Lib.Classes);
  end;

{ Tps_SpreadSheetBook }

function Tps_SpreadSheetBook.CreateObject(Stack: TExeStack): TObject;
begin
  Result := TSpreadSheetBook.Create('', '')
end;

class procedure Tps_SpreadSheetBook.am_getCaption(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.getSelf());
  Stack.PushString(o.Caption)
end;

class procedure Tps_SpreadSheetBook.am_setBounds(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.getSelf());
  o.ParentForm.setBounds(Stack.AsInteger(1),
                         Stack.AsInteger(2),
                         Stack.AsInteger(3),
                         Stack.AsInteger(4))
end;

class procedure Tps_SpreadSheetBook.am_setCaption(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.getSelf());
  o.Caption := Stack.AsString(1);
end;

class procedure Tps_SpreadSheetBook.am_ActiveSheet(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(1));
  Stack.PushObject(o.ActiveSheet)
end;

class procedure Tps_SpreadSheetBook.am_getActiveSheetIndex(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(1));
  Stack.PushInteger(o.ActiveSheetIndex)
end;

class procedure Tps_SpreadSheetBook.am_LoadFromFile(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(2));
  o.LoadFromFile(Stack.AsString(1))
end;

class procedure Tps_SpreadSheetBook.am_NewSheet(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(2));
  o.NewSheet(Stack.AsString(1))
end;

class procedure Tps_SpreadSheetBook.am_SaveToFile(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(2));
  o.SaveToFile(Stack.AsString(1))
end;

class procedure Tps_SpreadSheetBook.am_setActiveSheetIndex(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(2));
  o.ActiveSheetIndex := Stack.AsInteger(1);
end;

class procedure Tps_SpreadSheetBook.am_SheetCount(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(1));
  Stack.PushInteger(o.SheetCount)
end;

class procedure Tps_SpreadSheetBook.am_Show(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(1));
  o.Show();
end;

class procedure Tps_SpreadSheetBook.am_ShowModal(Const Func_Name: String; Stack: TexeStack);
var o: TSpreadSheetBook;
begin
  o := TSpreadSheetBook(Stack.AsObject(1));
  o.ShowModal();
end;

procedure Tps_SpreadSheetBook.AddMethods;
begin
  with Procs do
    begin
    Add('setCaption',
        'Estabelece o título da janela',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setCaption);

    Add('setBounds',
        'Estabelece a posição e o tamanho da janela',
        '',
        [pvtInteger, pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil, nil],
        [False, False, False, False],
        pvtNull,
        TObject,
        am_setBounds);

    Add('LoadFromFile',
        'Faz a leitura de um arquivo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_LoadFromFile);

    Add('NewSheet',
        'Cria uma nova folha dentro da planilha'#13 +
        'Parametro: Nome da folha (string)',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_NewSheet);

    Add('SaveToFile',
        'Salva o conteúdo da planilha em arquivo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_SaveToFile);

    Add('setActiveSheetIndex',
        'Muda a folha ativa pelo seu índice',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setActiveSheetIndex);

    Add('Show',
        'Mostra a planilha',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_Show);

    Add('ShowModal',
        'Mostra a planilha e só retorna a execução após o fechamento da janela',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_ShowModal);

    end;

  with Functions do
    begin
    Add('ActiveSheet',
        'Retorna a folha ativa',
        '',
        [],
        [],
        [],
        pvtObject,
        TSheet,
        am_ActiveSheet);

    Add('getCaption',
        'Retorna o título da janela',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        am_getCaption);

    Add('getActiveSheetIndex',
        'Retorna o índice da folha corrente',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_getActiveSheetIndex);

    Add('SheetCount',
        'Retorna o número de folhas da planilha',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_SheetCount);
    end;
end;

{ Tps_SpreadSheetWrapper }

class procedure Tps_SpreadSheetWrapper.am_ColCount(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(1));
  Stack.PushInteger(o.ColCount)
end;

class procedure Tps_SpreadSheetWrapper.am_ColToVec(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  {$ifdef WinStat}
  o := TSheet(Stack.AsObject(4));
  Stack.PushObject(o.ColToVec(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsInteger(3)))
  {$endif}
end;

class procedure Tps_SpreadSheetWrapper.am_Copy(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(1));
  o.Copy();
end;

class procedure Tps_SpreadSheetWrapper.am_Cut(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(1));
  o.Cut();
end;

class procedure Tps_SpreadSheetWrapper.am_GetDisplayText(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(3));
  Stack.PushString(o.GetDisplayText(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure Tps_SpreadSheetWrapper.am_GetFloat(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(3));
  Stack.PushFloat(o.GetFloat(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure Tps_SpreadSheetWrapper.am_GetInt(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(3));
  Stack.PushInteger(o.GetInt(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure Tps_SpreadSheetWrapper.am_GetText(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(3));
  Stack.PushString(o.GetText(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure Tps_SpreadSheetWrapper.am_RowCount(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(1));
  Stack.PushInteger(o.RowCount)
end;

class procedure Tps_SpreadSheetWrapper.am_Paste(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(1));
  o.Paste();
end;

class procedure Tps_SpreadSheetWrapper.am_RowToVec(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  {$ifdef WinStat}
  o := TSheet(Stack.AsObject(4));
  Stack.PushObject(o.RowToVec(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsInteger(3)))
  {$endif}
end;

class procedure Tps_SpreadSheetWrapper.am_setCellFont(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(8));
  o.setCellFont(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsString(3), Stack.AsInteger(4), Stack.AsInteger(5), Stack.AsBoolean(6), Stack.AsBoolean(7))
end;

class procedure Tps_SpreadSheetWrapper.am_setColWidth(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(3));
  o.ColWidth[Stack.AsInteger(1)] := Stack.AsInteger(2);
end;

class procedure Tps_SpreadSheetWrapper.am_setShowHeaders(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(2));
  o.ShowHeaders := Stack.AsBoolean(1);
end;

class procedure Tps_SpreadSheetWrapper.am_writeFloat(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(5));
  o.write(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsFloat(3), Stack.AsInteger(4))
end;

class procedure Tps_SpreadSheetWrapper.am_writeText(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.AsObject(4));
  o.write(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsString(3))
end;

class procedure Tps_SpreadSheetWrapper.am_WriteVecInCol(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  {$ifdef WinStat}
  o := TSheet(Stack.AsObject(5));
  o.WriteVecInCol(TwsVec(Stack.AsObject(1)), Stack.AsInteger(2), Stack.AsInteger(3), Stack.AsInteger(4))
  {$endif}
end;

class procedure Tps_SpreadSheetWrapper.am_setCellBold(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.getSelf());
  o.BoldCell(Stack.AsInteger(1),
             Stack.AsInteger(2));
end;

class procedure Tps_SpreadSheetWrapper.am_WriteVecInRow(Const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  {$ifdef WinStat}
  o := TSheet(Stack.AsObject(5));
  o.WriteVecInRow(TwsVec(Stack.AsObject(1)), Stack.AsInteger(2), Stack.AsInteger(3), Stack.AsInteger(4))
  {$endif}
end;

procedure Tps_SpreadSheetWrapper.AddMethods;
begin
  with Procs do
    begin
    Add('Copy',
        'Copiar',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_Copy);

    Add('Cut',
        'Recortar',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_Cut);

    Add('Paste',
        'Colar',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        am_Paste);

    Add('setCellFont',
        'Muda a fonte de uma célula'#13 +
        'Se "aFontName" = '''' valerá a fonte corrente'#13 +
        'Se "aSize" = 0 valerá o tamanho corrente'#13 +
        'Parâmetros: '#13 +
        '  - Linha e Coluna'#13 +
        '  - Nome da Fonte'#13 +
        '  - Tamanho'#13 +
        '  - Cor'#13 +
        '  - Negrito (boolean)'#13 +
        '  - Itálico (boolean)',
        '',
        [pvtInteger, pvtInteger, pvtString, pvtInteger, pvtInteger, pvtBoolean, pvtBoolean],
        [nil, nil, nil, nil, nil, nil, nil],
        [False, False, False, False, False, False, False],
        pvtNull,
        TObject,
        am_setCellFont);

    Add('setColWidth',
        'Muda o tamanho da coluna'#13 +
        'Parâmetro: Índice da Coluna e Tamanho',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        am_setColWidth);

    Add('setShowHeaders',
        'Mostra ou não os cabeçalhos da folha corrente',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setShowHeaders);

    Add('writeFloat',
        'Escreve um valor em uma célula'#13 +
        'Parâmetros: Linha, Coluna, Valor e Numero de Casas Decimais',
        '',
        [pvtInteger, pvtInteger, pvtReal, pvtInteger],
        [nil, nil, nil, nil],
        [False, False, False, False],
        pvtNull,
        TObject,
        am_writeFloat);

    Add('writeText',
        'Escreve um texto em uma célula'#13 +
        'Parâmetros: Linha, Coluna e Texto',
        '',
        [pvtInteger, pvtInteger, pvtString],
        [nil, nil, nil],
        [False, False, False],
        pvtNull,
        TObject,
        am_writeText);

    Add('writeCenterFloat',
        'Escreve um valor centralizado em uma célula'#13 +
        'Parâmetros: Linha, Coluna, Valor e Numero de Casas Decimais',
        '',
        [pvtInteger, pvtInteger, pvtReal, pvtInteger],
        [nil, nil, nil, nil],
        [False, False, False, False],
        pvtNull,
        TObject,
        am_writeCenterFloat);

    Add('writeCenterText',
        'Escreve um texto centralizado em uma célula'#13 +
        'Parâmetros: Linha, Coluna e Texto',
        '',
        [pvtInteger, pvtInteger, pvtString],
        [nil, nil, nil],
        [False, False, False],
        pvtNull,
        TObject,
        am_writeCenterText);

    {$ifdef WinStat}
    Add('WriteVecInCol',
        'Escreve um vetor em uma coluna da planilha'#13 +
        'Parâmetros: Vetor, Coluna, Linha Inicial e Número de Casas Decimais',
        '',
        [pvtObject, pvtInteger, pvtInteger, pvtInteger],
        [TwsVec, nil, nil, nil],
        [True, False, False, False],
        pvtNull,
        TObject,
        am_WriteVecInCol);

    Add('WriteVecInRow',
        'Escreve um vetor em uma linha da planilha'#13 +
        'Parâmetros: Vetor, Linha, Coluna Inicial e Número de Casas Decimais',
        '',
        [pvtObject, pvtInteger, pvtInteger, pvtInteger],
        [TwsVec, nil, nil, nil],
        [True, False, False, False],
        pvtNull,
        TObject,
        am_WriteVecInRow);
     {$endif}

    Add('BoldCell',
        'Estabelece o atributo de negrito de uma célula'#13 +
        'Parâmetros: Linha, Coluna'#13 +
        '  ',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        am_setCellBold);
    end;

  with Functions do
    begin
    Add('ColCount',
        'Número de Colunas com valores válidos',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_ColCount);

    {$ifdef WinStat}
    Add('ColToVec',
        'Retorna um vetor formado pelas células de uma coluna'#13 +
        'Parâmetros: Coluna, Linha inicial e Linha final',
        '',
        [pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil],
        [False, False, False],
        pvtObject,
        TwsVec,
        am_ColToVec);
     {$endif}

    Add('GetDisplayText',
        'Retorna o texto mostrado em uma célula',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtString,
        TObject,
        am_GetDisplayText);

    Add('GetFloat',
        'Retorna o valor de uma célula',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtReal,
        TObject,
        am_GetFloat);

    Add('GetInt',
        'Retorna o valor inteiro de uma célula',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtInteger,
        TObject,
        am_GetInt);

    Add('GetText',
        'Retorna o texto de uma célula',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtString,
        TObject,
        am_GetText);

    Add('RowCount',
        'Número de Linhas com valores válidos',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_RowCount);

    {$ifdef WinStat}
    Add('RowToVec',
        'Retorna um vetor formado pelas células de uma linha'#13 +
        'Parâmetros: Linha, Coluna inicial e Coluna final',
        '',
        [pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil],
        [False, False, False],
        pvtObject,
        TwsVec,
        am_RowToVec);
    {$endif}
    end;
end;

class procedure Tps_SpreadSheetWrapper.am_writeCenterFloat(const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.getSelf());
  o.writeCenter(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsFloat(3), Stack.AsInteger(4))
end;

class procedure Tps_SpreadSheetWrapper.am_writeCenterText(const Func_Name: String; Stack: TexeStack);
var o: TSheet;
begin
  o := TSheet(Stack.getSelf());
  o.writeCenter(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsString(3));
end;

end.

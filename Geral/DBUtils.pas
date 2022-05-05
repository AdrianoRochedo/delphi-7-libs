unit DbUtils;

{
  Versão: 1.2

  HISTÓRICO: 21/11/2000
             Considerado vtAnsiString nas passagens de parâmetros CONST

             31/01/2001
             Inserida a rotina SQLSave()

             02/06/2001
             Inserida a rotina DateToSQLDate()

             15/07/2001
             Inserida a rotina "Overload" Pack(string)

             05/09/2001
             Atualizado o cabeçalho da rotina SQLSave
}

interface
uses WinTypes, WinProcs, SysUtils, Controls, Graphics, StdCtrls, DBLookup,
     Forms, Classes, Messages, Dialogs, DBTables, DB, BDE;

type
  TFormatFloatFunc = function(const f: Double): String;

  {Funções para Banco de dados}
  Function  DBSearchSTR(Table: TTable; Const Field, S: String; FromBegin: Boolean = True): Boolean; {07/03/1997}
  Function  DBSearchINT(Table: TTable; Const Field: String; I: Longint; FromBegin: Boolean = True): Boolean; {18/03/1997}
  Function  DBSearchDATE(Table: TDataSet; Const Field: String; Date: TDateTime; FromBegin: Boolean = True): Boolean; {21/05/2001}
  Function  SQLSelect(Const Alias, SQLText: String; Param: Array of Const): TQuery; {23/04/97}
  Procedure Attrib(T: TDBDataSet; Const Vals: Array of Const); {01/05/97}
  Function  GotoKey(T: TTable; Const Vals: Array of Const): Boolean; {25/09/1997}
  Function  DataSetSortedBSearch(DataSet: TDataSet; Value: Longint; Const FieldName: string): Boolean;
  Procedure Pack(Table: TTable); overload;
  Procedure Pack(const Table: String); overload;
  function  SQLSave(Query: TDataSet; const FileName: String): Boolean; {05/09/2001}

  // converte uma data no formato DD/MM/AAAA para MM/DD/AAAA
  function DateToSQLDate(const strDate: String): String;

  // Verifica se é um SQL que retorna um resultado
  function IsResultSetSQL(SQL: string): boolean;

  // Verifica se o campo é numérico
  function IsNumericType(FieldType: TFieldType): boolean;

  // Verifica se o campo é uma Data ou tempo
  function IsDateTimeType(FieldType: TFieldType): boolean;

  // Converte um Dataset para texto
  procedure SaveAsText(Dataset: TDataSet; const Filename: string; Formated: boolean);

implementation
uses FileCTRL,
     SysUtilsEx,
     FileUtils;

Const
  ccTipoInt        =  'O campo não é do tipo < Número Inteiro >';
  ccArquivoFechado =  'Arquivo não Aberto';

{
 Posiciona o ponteiro do arquivo no registro 'n' se o termo 'S' for encontrado
 no campo 'Field' e retorna TRUE, senão retornará FALSE e posicionado no último reg.
 OBS: 'Table' deverá estar Ativa.
 }
Function DBSearchSTR(Table: TTable; Const Field, S: String; FromBegin: Boolean): Boolean;
Var F: TField;
Begin
  Result := False;
  If Not Table.Active Then
     Raise Exception.Create(ccArquivoFechado);

  F := Table.FindField(Field);
  Try
    Screen.Cursor := crHourGlass;
    Table.DisableControls;
    Try
      With Table do
        Begin
        if FromBegin then First;
        While Not EOF do
          Begin
          If F.AsString = S Then
             Begin
             Result := True;
             Break;
             End;
          Next;
          End;
        End; {With}
    Finally
      Screen.Cursor := crDefault;
      Table.EnableControls;
    End;
  Except
    {ERRO: A função retornará falso}
  End;
End;

Function DBSearchINT(Table: TTable; Const Field: String; I: Longint; FromBegin: Boolean): Boolean;
Var FT : TFieldType;
    F: TField;
Begin
  Result := False;
  If Not Table.Active Then
     Raise Exception.Create(ccArquivoFechado);

  F := Table.FindField(Field);

  FT := F.DataType;
  If Not((FT = ftSmallInt) or (FT = ftInteger) or (FT = ftWord)) Then
     Raise Exception.Create(ccTipoInt);

  Try
    Try
      Screen.Cursor := crHourGlass;
      Table.DisableControls;
      With Table do
        Begin
        if FromBegin then First;
        While Not EOF do
          Begin
          If F.AsInteger = I Then
             Begin
             Result := True;
             Break;
             End;
          Next;
          End;
        End; {With}
    Finally
      Screen.Cursor := crDefault;
      Table.EnableControls;
    End;
  Except
    { ERRO: A função retornará Falso}
  End;
End;

Function SQLSelect(Const Alias, SQLText: String; Param: Array of Const): TQuery; {23/04/97}
var i: Integer;
Begin
  Result := TQuery.Create(Nil);
  Result.DatabaseName := Alias;
  Result.SQL.Add(SQLText);
  If (Param[0].vPointer <> Nil) Then
     For i := 0 to High(Param) do
        Case Param[i].vType of
          vtAnsiString : Result.Params[i].AsString  := string(Param[i].VAnsiString); // String longa
          vtString     : Result.Params[i].AsString  := Param[i].vString^; // String curta
          vtInteger    : Result.Params[i].AsInteger := Param[i].vInteger;
          End; {Case}

  Try
    Result.Open;
    If Result.RecordCount = 0 Then
       Raise Exception.Create('Nenhum Registro foi Encontrado');
  Except
     Result.Free;
     Result := Nil;
     Raise;
  End;
End; {SQLSelect}

{Rotina de Atribuição de valores para campos de um arquivo}
{Os Índices pares do array Vals são os nomes dos campos}
{Os Índices ímpares do array Vals são os respectivos valores}
Procedure Attrib(T: TDBDataSet; Const Vals: Array of Const);
Var i: Integer;
    s: String;
Begin
  i := 0;
  While i <= High(Vals) Do
    Begin
    // Nome do Campo
    Case Vals[i].vType of
      vtString     : s := Vals[i].vString^;
      vtAnsiString : s := string(Vals[i].VAnsiString);
      end;

    Case Vals[i+1].vType of
      vtString     : T.FieldByName(s).AsString  := Vals[i+1].vString^; // String curta
      vtAnsiString : T.FieldByName(s).AsString  := string(Vals[i+1].VAnsiString); // String longa
      vtInteger    : T.FieldByName(s).AsInteger := Vals[i+1].vInteger;
      vtChar       : T.FieldByName(s).AsString  := Vals[i+1].vChar;
      vtBoolean    : T.FieldByName(s).AsBoolean := Vals[i+1].vBoolean;
      vtExtended   : T.FieldByName(s).AsFloat   := Vals[i+1].vExtended^;
      End; {Case}

    Inc(i,2);
    End;
End; {Attrib}

{Faz uma busca em arquivos indexados}
{Os Índices pares do array Vals são os nomes das chaves}
{Os Índices ímpares do array Vals são os respectivos valores}
Function GotoKey(T: TTable; Const Vals: Array of Const): Boolean;
Var i: Integer;
    s: String;
Begin
  If Not T.Active Then Raise Exception.Create('Função <GoToKey>: Arquivo Fechado');
  i := 0;
  T.SetKey;
  While i <= High(Vals) Do
    Begin
    // Nome do Campo
    Case Vals[i].vType of
      vtString     : s := Vals[i].vString^;
      vtAnsiString : s := string(Vals[i].VAnsiString);
      end;

    // Valores
    Case Vals[i+1].vType of
      vtString     : T.FieldByName(s).AsString  := Vals[i+1].vString^; // String curta
      vtAnsiString : T.FieldByName(s).AsString  := string(Vals[i+1].VAnsiString); // String longa
      vtInteger    : T.FieldByName(s).AsInteger := Vals[i+1].vInteger;
      vtChar       : T.FieldByName(s).AsString  := Vals[i+1].vChar;
      vtBoolean    : T.FieldByName(s).AsBoolean := Vals[i+1].vBoolean;
      vtExtended   : T.FieldByName(s).AsFloat   := Vals[i+1].vExtended^;
      End; {Case}
    Inc(i,2);
    End; {While}
  Result := T.GotoKey;
End; {GoToKey}

procedure Pack(Table : TTable); {27/05/97}
var
  TBDesc : CRTblDesc;
  hDb: hDbiDb;
  TablePath: array[0..dbiMaxPathLen] of char;
begin
  FillChar(TBDesc,Sizeof(TBDesc),0);
  with TBDesc do
    begin
    StrPCopy(szTblName,Table.TableName);
    StrPCopy(szTblType,szParadox);
    bPack := True;
    end;
  hDb := nil;
  Check(DbiGetDirectory(Table.DBHandle, True, TablePath));
  Table.Close;
  Check(DbiOpenDatabase(nil, 'STANDARD', dbiReadWrite, dbiOpenExcl, nil, 0, nil, nil, hDb));
  Check(DbiSetDirectory(hDb, TablePath));
  Check(DBIDoRestructure(hDb, 1, @TBDesc, nil, nil, nil, False));
  Table.Open;
end;

Procedure Pack(const Table: String); overload;
var t: TTable;
begin
  t := TTable.Create(nil);
  t.TableName := Table;
  try
    t.Open;
    Pack(t);
  finally
    t.Free;
  end;
end;

function DataSetSortedBSearch(DataSet: TDataSet; Value: Longint; Const FieldName: string): Boolean;
var
  L, H, I: Longint;
  CurrentPos: Longint;
  CurrentValue: Longint;
  Field: TField;
begin
  Result := False;
  if DataSet = nil then Exit;
  Field := DataSet.FindField(FieldName);
  if Field = nil then Exit;
  DataSet.DisableControls;
  try
    L := 1;
    H := DataSet.RecordCount;
    I := (L + H) shr 1;
    DataSet.First;
    DataSet.MoveBy(I - 1);
    CurrentPos := I;
    CurrentValue := Field.AsInteger;

    while  (Value <> CurrentValue) and (L < H) do
      begin
      if Value < CurrentValue then H := I else L := I + 1;
      I := (L + H) shr 1;
      if I <> CurrentPos then DataSet.MoveBy(I - CurrentPos);
      CurrentPos := I;
      CurrentValue := Field.AsInteger;
      end; { while }

      Result := Value = CurrentValue;
  finally
    DataSet.EnableControls;
  end;
end;

function  SQLSave(Query: TDataSet; const FileName: String): Boolean;
var i: Integer;
    t: TTable;
begin
  Result := False;
  if DirectoryExists(ExtractFilePath(FileName)) then
     try
       Query.DisableControls;
       t := TTable.Create(nil);
       t.TableName := FileName;
       t.FieldDefs.Assign(Query.FieldDefs);
       t.CreateTable;
       if FileExists(FileName) then
          begin
          t.Open;
          Query.First;
          while not Query.EOF do
            begin
            t.Append;
            for i := 0 to Query.FieldCount-1 do
              t.Fields[i].Value := Query.Fields[i].Value;
            t.Post;
            Query.Next;
            end;
          Query.First;
          Result := True;
          end;
     finally
       t.Free;
       Query.EnableControls;
     end;
end;

{--------------------------------------------------------}

function DefaultFloatFunc(const f: Double): String;
begin
  Result := ' ' + FloatToStr(f) + ' ';
end;

Function DBSearchDATE(Table: TDataSet;
                      Const Field: String;
                      Date: TDateTime;
                      FromBegin: Boolean): Boolean; {21/05/2001}
Var F : TField;
Begin
  Result := False;
  If Not Table.Active Then
     Raise Exception.Create(ccArquivoFechado);

  F := Table.FindField(Field);

  If Not (F.DataType = ftDate) Then
     Raise Exception.Create('Campo não é do tipo Data');

  Try
    Try
      Screen.Cursor := crHourGlass;
      Table.DisableControls;
      With Table do
        Begin
        if FromBegin then First;
        While Not EOF do
          Begin
          If F.Value = Date Then
             Begin
             Result := True;
             Break;
             End;
          Next;
          End;
        End; {With}
    Finally
      Screen.Cursor := crDefault;
      Table.EnableControls;
    End;
  Except
    { ERRO: A função retornará Falso}
  End;
End;

// converte uma data no formato DD/MM/AAAA para MM/DD/AAAA
function DateToSQLDate(const strDate: String): String;
var i1, i2: Integer;
begin
  i1 := System.Pos('/', strDate);
  i2 := PosR('/', strDate);
  if (i1 = 0) or (i2 = 0) then
     Raise Exception.Create('Data Inválida: ' + strDate);

  // MM/DD/AAAA
  Result := System.Copy(strDate, i1+1, i2-i1-1) + '/' +
            System.Copy(strDate, 1, i1-1) + '/' +
            System.Copy(strDate, i2+1, 4);
end;

function IsResultSetSQL(SQL: string): boolean;
begin
  SQL := upperCase(SQL);
  result := (System.Pos('SELECT'#32, SQL) > 0) or
            (System.Pos('SELECT'#13, SQL) > 0) or
            (System.Pos('SHOW'#32  , SQL) > 0) or
            (System.Pos('SHOW'#13  , SQL) > 0);
end;

function IsNumericType(FieldType: TFieldType): boolean;
begin
  case FieldType of
    ftInteger, ftSmallint, ftWord, ftFloat, ftCurrency: result := true;
    else
       result := false;
    end;
end;

function IsDateTimeType(FieldType: TFieldType): boolean;
begin
  case FieldType of
    ftDateTime, ftDate, ftTime: result := true;
    else
       result := false;
    end;
end;

procedure SaveAsText(Dataset: TDataSet; const Filename: string; Formated: boolean);
var i: integer;
    c: integer;
    t: integer;
    u: integer;
    s: string;
    w: array of integer;
    x: TTextFileWriter;
begin
  // Cria o gerador de arquivos texto
  x := TTextFileWriter.Create(Filename, true);

  // Obtem o numero de campos da tabela
  c := Dataset.FieldCount;

  // Cabecalho
  s := '';
  if Formated then
     begin
     setLength(w, c);
     for i := 0 to c-1 do
       begin
       t := Dataset.Fields[i].DisplayWidth; // pega o tamanho do campo atual;
       u := Length(Dataset.Fields[i].FieldName);
       if u > t then w[i] := u + 1 else w[i] := t + 1;
       s := s + (Format('%-' + IntToStr(w[i]) + '.' + IntToStr(w[i]) + 's',
                      [Dataset.Fields[i].FieldName])) // preenche a string com os dados
       end
     end
  else
     begin
     for i := 0 to c-1 do
       s := s + Dataset.Fields[i].FieldName + ';';
     SysUtilsEx.DeleteLastChar(s);
     end;

  x.writeLN(s);

  // Dados
  Dataset.DisableControls();
  try
    Dataset.First();
    while not Dataset.EOF do
      begin
      // Forma o registro a ser salvo
      s := '';
      if Formated then
         for i := 0 to c-1 do
            s := s + (Format('%-' + IntToStr(w[i]) + '.' + IntToStr(w[i]) + 's',
                            [Dataset.Fields[i].AsString])) 
      else
         begin
         for i := 0 to c-1 do
           s := s + Dataset.Fields[i].AsString + ';';
         SysUtilsEx.DeleteLastChar(s);
         end;

      // Escreve o registro no arquivo
      x.writeLN(s);

      // Pula para o proximo registro
      Dataset.Next;
      end; // while
  finally
    Dataset.First();
    Dataset.EnableControls();
    x.Free();
  end;
end;

end.

unit Lib_Windows;

interface
uses psBase;

  procedure API(Lib: TLib);

type
  TWindows_Procs = class(TFunctionServices)
  public
    class procedure AddProcsIn(Procs: TProcList); override;
    class procedure amShowMessage     (Const Func_Name: String; Stack: TexeStack);
    class procedure amCreateException (Const Func_Name: String; Stack: TexeStack);
    class procedure amKeepRun         (Const Func_Name: String; Stack: TexeStack);
  end;

  TWindows_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn(Functions: TFunctionList); override;
    class procedure amGetColor    (Const Func_Name: String; Stack: TexeStack);
    class procedure amInputQuery  (Const Func_Name: String; Stack: TexeStack);
    class procedure amInputBox    (Const Func_Name: String; Stack: TexeStack);
    class procedure amQuestion    (Const Func_Name: String; Stack: TexeStack);
    class procedure amSelectDir   (Const Func_Name: String; Stack: TexeStack);

    class procedure amDirectoryExists  (Const Func_Name: String; Stack: TexeStack);
    class procedure amForceDirectories (Const Func_Name: String; Stack: TexeStack);
  end;

  TpsOutPut_Class = Class(TpsClass)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;

    class procedure amClear(Const Func_Name: String; Stack: TexeStack);
    class procedure amSave(Const Func_Name: String; Stack: TexeStack);

    class procedure amWarning(Const Func_Name: String; Stack: TexeStack);
    class procedure amWrite(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteLN(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteFormated(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteImage(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteCenter(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteLine(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteLink(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteSeparator(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteStrings(Const Func_Name: String; Stack: TexeStack);
    class procedure amWriteSpaces(Const Func_Name: String; Stack: TexeStack);

    class procedure amSetFileName(Const Func_Name: String; Stack: TexeStack);
    class procedure amShowOutPut(Const Func_Name: String; Stack: TexeStack);
  end;

implementation
uses Dialogs,
     Classes,
     SysUtils,
     Controls,
     OutPut,
     Graphics,
     teEngine,
     FolderUtils,
     FileUtils,
     Forms,
     FileCTRL,
     Windows,
     RunTimeForm;

{ TWindows_Functions }

class procedure TWindows_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('GetColor',
        'Obtém um código de cor',
        cCatSistema,
        [pvtString],
        [nil      ],
        [false    ],
        pvtInteger,
        TObject,
        amGetColor);

    Add('Question',
        'Mostra um diálogo com uma pergunta ao usuário.',
        cCatDialogo,
        [pvtString],
        [nil      ],
        [false    ],
        pvtBoolean,
        TObject,
        amQuestion);

    Add('InputBox',
        'Mostra um diálogo que permite ao usuário entrar uma string'#13 +
        'Parâmetros:'#13 +
        '  - aCaption : Texto da janela'#13 +
        '  - aPrompt  : Texto que fica em cima da caixa de edição'#13 +
        '  - Default  : Indica o texto inicial'#13 +
        'Retorna o texto digitado.',
        cCatDialogo,
        [pvtString, pvtString, pvtString],
        [nil      , nil      , nil      ],
        [false    , false    , false    ],
        pvtString,
        TObject,
        amInputBox);

    Add('InputQuery',
        'Mostra um diálogo que permite ao usuário entrar uma string'#13 +
        'Parâmetros:'#13 +
        '  - aCaption : Texto da janela'#13 +
        '  - aPrompt  : Texto que fica em cima da caixa de edição'#13 +
        '  - Value    : Parâmetro passado por referência, indica o texto inicial e final editado'#13 +
        'Retorno:'#13 +
        '  - Retorna "TRUE" ou "FALSE" dependendo se o usuário pressionar o botão "OK" ou "CANCELAR".',
        cCatDialogo,
        [pvtString, pvtString, pvtString],
        [nil      , nil      , nil      ],
        [false    , false    , true     ],
        pvtBoolean,
        TObject,
        amInputQuery);

    Add('SelectDirectory',
        'Permite ao usuário escolher um diretório'#13 +
        'Parâmetros:'#13 +
        '  - aCaption  : Texto da janela'#13 +
        '  - Root      : Diretório raiz inicial'#13 +
        '  - Directory : Parâmetro passado por referência, indica o diretório escolhido'#13 +
        'Retorno:'#13 +
        '  - Retorna "TRUE" ou "falso" dependendo se o usuário pressionar o botão "OK" ou "CANCELAR".',
        cCatDialogo,
        [pvtString, pvtString, pvtString],
        [nil      , nil      , nil      ],
        [false    , false    , true     ],
        pvtBoolean,
        TObject,
        amSelectDir);

    Add('DirectoryExists',
        'Verifica a existência de um diretório',
        cCatFile,
        [pvtString],
        [nil      ],
        [false    ],
        pvtBoolean,
        TObject,
        amDirectoryExists);

    Add('ForceDirectories',
        'Força a existência de um diretório',
        cCatFile,
        [pvtString],
        [nil      ],
        [false    ],
        pvtBoolean,
        TObject,
        amForceDirectories);
    end;
end;

class procedure TWindows_Functions.amDirectoryExists(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean(DirectoryExists(Stack.AsString(1)));
end;

class procedure TWindows_Functions.amForceDirectories(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean(ForceDirectories(Stack.AsString(1)));
end;

class procedure TWindows_Functions.amGetColor(const Func_Name: String; Stack: TexeStack);
var s: String;
    c: TColor;
begin
  s := Stack.AsString(1);

  if CompareText(s, 'TeeColor'  ) = 0 then c := clTeeColor else
  if CompareText(s, 'red'       ) = 0 then c := clRed      else
  if CompareText(s, 'green'     ) = 0 then c := clGreen    else
  if CompareText(s, 'blue'      ) = 0 then c := clBlue     else
  if CompareText(s, 'white'     ) = 0 then c := clWhite    else
  if CompareText(s, 'black'     ) = 0 then c := clBlack    else
  if CompareText(s, 'yellow'    ) = 0 then c := clYellow   else
  if CompareText(s, 'lime'      ) = 0 then c := clLime     else
  if CompareText(s, 'maroon'    ) = 0 then c := clMaroon   else
  if CompareText(s, 'fuchsia'   ) = 0 then c := clFuchsia  else
  if CompareText(s, 'gray'      ) = 0 then c := clGray     else
  if CompareText(s, 'aqua'      ) = 0 then c := clAqua     else
  if CompareText(s, 'olive'     ) = 0 then c := clOlive    else
  if CompareText(s, 'purple'    ) = 0 then c := clPurple   else
  if CompareText(s, 'silver'    ) = 0 then c := clSilver   else
  if CompareText(s, 'navy'      ) = 0 then c := clNavy     else
  if CompareText(s, 'teal'      ) = 0 then c := clTeal
  else
     c := clBlack;

  Stack.PushInteger(integer(c));
end;

class procedure TWindows_Functions.amInputBox(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(InputBox(Stack.AsString(1), Stack.AsString(2), Stack.AsString(3)));
end;

class procedure TWindows_Functions.amInputQuery(const Func_Name: String; Stack: TexeStack);
var b: Boolean;
    s1, s2, s3: String;
    v: TVariable;
begin
  s1 := Stack.AsString(1);
  s2 := Stack.AsString(2);
  v := Stack.AsReferency(3);
  s3 := v.Value;

  b := InputQuery(s1, s2, s3);
  v.Value := s3;

  Stack.PushBoolean(b);
end;

class procedure TWindows_Functions.amQuestion(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean(
    MessageDLG(Stack.AsString(1), mtConfirmation, [mbYes, mbNo], 0) = mrYes
  );
end;

class procedure TWindows_Functions.amSelectDir(const Func_Name: String; Stack: TexeStack);
var s: String;
    b: Boolean;
begin
  b := SelectDirectory(Stack.AsString(1), Stack.AsString(2), s);
  Stack.AsReferency(3).Value := s;
  Stack.PushBoolean(b);
end;

{ TWindows_Procs }

class procedure TWindows_Procs.AddProcsIn(Procs: TProcList);
begin
  with Procs do
    begin
    Add('ShowMessage',
        'Mostra uma mensagem em uma janela de diálogo',
        cCatDialogo,
        [pvtString], [nil], [False], pvtNull, TObject, amShowMessage);

    Add('CreateException',
        'Gera uma exceção com uma determinada mansagem.',
        cCatDialogo,
        [pvtString], [nil], [False], pvtNull, TObject, amCreateException);

    Add('KeepRun',
        'Permite que a execução de um Script funcione como uma aplicação, isto é, '#13 +
        'Mantém em atividade todas as janelas criadas dinamicamente.',
        cCatSistema,
        [], [], [], pvtNull, TObject, amKeepRun);
    end;
end;

class procedure TWindows_Procs.amCreateException(const Func_Name: String; Stack: TexeStack);
begin
  raise Exception.Create(Stack.AsString(1));
end;

class procedure TWindows_Procs.amKeepRun(const Func_Name: String; Stack: TexeStack);
begin
  if CompareText(Application.Title, 'PSRT') = 0 then
     begin
     Application.CreateForm(TRunTime, RunTime);
     Application.Run;
     end;
end;

class procedure TWindows_Procs.amShowMessage(const Func_Name: String; Stack: TexeStack);
begin
  ShowMessage(Stack.AsString(1));
end;

{ TpsOutPut_Class }

function TpsOutPut_Class.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TOutPut.Create;
end;

procedure TpsOutPut_Class.AddMethods;
begin
  with Procs do
    begin
    Add('Clear',
        '',
        '',
        [], [], [],
        pvtNull,
        TObject,
        amClear);

    Add('Save',
        '',
        '',
        [], [], [],
        pvtNull,
        TObject,
        amSave);

    Add('SetFileName',
        'Dir ao objeto qual o nome do arquivo a ser salvo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetFileName);

    Add('Write',
        'Escreve uma String na Saída (OutPut) indicada sem pular linha',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amWrite);

    Add('WriteLN',
        'Escreve uma String na Saída (OutPut) indicada e pula uma linha',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amWriteLN);
{ HTML
    Add('Warning',
        '',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amWarning);


    Add('WriteFormated',
        '',
        '',
        [pvtString, pvtObject, pvtInteger, pvtBoolean, pvtBoolean, pvtBoolean, pvtBoolean, pvtString],
        [nil, TColor, nil, nil, nil, nil, nil, nil],
        [False, True, False, False, False, False, False, False],
        pvtNull,
        TObject,
        amWriteFormated);

    Add('WriteImage',
        '',
        '',
        [pvtObject],
        [TGraphic],
        [True],
        pvtNull,
        TObject,
        amWriteImage);
}
    Add('WriteCenter',
        'Escreve uma string de forma centrada e pula uma linha'#13 +
        'Parâmetros: '#13 +
        '  - Texto (String)'#13 +
        '  - Tamanho da Linha (Inteiro)',
        '',
        [pvtString, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amWriteCenter);

    Add('WriteLine',
        'Pula Linhas no texto'#13 +
        'Parâmetro: Quantidade de linhas',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amWriteLine);

    Add('WriteStrings',
        'Mostra o conteúdo de uma lista de strings',
        '',
        [pvtObject],
        [TStringList],
        [True],
        pvtNull,
        TObject,
        amWriteStrings);

{ HTML
    Add('WriteLink',
        '',
        '',
        [pvtString, pvtString, pvtBoolean],
        [nil, nil, nil],
        [False, False, False],
        pvtNull,
        TObject,
        amWriteLink);

    Add('WriteSeparator',
        '',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amWriteSeparator);

    Add('WriteSpaces',
        '',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amWriteSpaces);
}
    Add('Show',
        'Mostra a Saída (OutPut) indicada',
        '',
        [], [], [], pvtNull, TObject, amShowOutPut);
    end;
end;

class procedure TpsOutPut_Class.amShowOutPut(const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.getSelf());
  O.FileName := WindowsTempDir() + 'PSC.txt';
  O.Save();
  O.Show();
end;

class procedure TpsOutPut_Class.amWrite(const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(2));
  O.DocType := dtTEXT;
  O.Write(Stack.AsString(1), False);
end;

class procedure TpsOutPut_Class.amWriteLN(const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(2));
  O.DocType := dtTEXT;
  O.Write(Stack.AsString(1), True);
end;

class procedure TpsOutPut_Class.amClear(Const Func_Name: String; Stack: TexeStack);
begin
  TOutPut(Stack.AsObject(1)).Clear;
end;

class procedure TpsOutPut_Class.amSave(Const Func_Name: String; Stack: TexeStack);
begin
  TOutPut(Stack.AsObject(1)).Save;
end;

class procedure TpsOutPut_Class.amWarning(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amWriteFormated(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amWriteImage(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amWriteCenter(Const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(3));
  O.WriteCenter(Stack.AsString(1), Stack.AsInteger(2));
end;

class procedure TpsOutPut_Class.amWriteLine(Const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(2));
  O.WriteLine(Stack.AsInteger(1));
end;

class procedure TpsOutPut_Class.amWriteLink(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amWriteSeparator(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amWriteSpaces(Const Func_Name: String; Stack: TexeStack);
begin
end;

class procedure TpsOutPut_Class.amSetFileName(Const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(2));
  O.FileName := Stack.AsString(1);
end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TWindows_Functions.AddFunctionsIn(Lib.Functions);
  TWindows_Procs.AddProcsIn(Lib.Procs);

  TpsOutPut_Class.Create(
    TOutPut,
    nil,
    'Saída Padrão',
    cCatSistema,
    [], [], [],
    True,
    Lib.Classes);
end;

class procedure TpsOutPut_Class.amWriteStrings(const Func_Name: String; Stack: TexeStack);
var O: TOutPut;
begin
  O := TOutPut(Stack.AsObject(2));
  O.Write(TStrings(Stack.AsObject(1)));
end;

end.

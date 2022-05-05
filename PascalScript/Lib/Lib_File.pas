{ ultima versao }
unit Lib_File;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses SysUtils,
     FolderUtils;

type
  TFile_Functions = class(TFunctionServices)
  public
    class procedure AddFunctionsIn (Functions: TFunctionList); override;

    class procedure amDeleteFile    (Const Func_Name: String; Stack: TexeStack);
    class procedure amRenameFile    (Const Func_Name: String; Stack: TexeStack);
    class procedure amChangeFileExt (Const Func_Name: String; Stack: TexeStack);
    class procedure amFileSearch    (Const Func_Name: String; Stack: TexeStack);
    class procedure amFileExists    (Const Func_Name: String; Stack: TexeStack);
    class procedure amDiskFree      (Const Func_Name: String; Stack: TexeStack);
    class procedure amDiskSize      (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetCurrentDir (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetCurrentDir (Const Func_Name: String; Stack: TexeStack);
    class procedure amRemoveDir     (Const Func_Name: String; Stack: TexeStack);
    class procedure amCreateDir     (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTempDir    (Const Func_Name: String; Stack: TexeStack);

    class procedure amExtractFilePath  (Const Func_Name: String; Stack: TexeStack);
    class procedure amExtractFileDir   (Const Func_Name: String; Stack: TexeStack);
    class procedure amExtractFileDrive (Const Func_Name: String; Stack: TexeStack);
    class procedure amExtractFileName  (Const Func_Name: String; Stack: TexeStack);
    class procedure amExtractFileExt   (Const Func_Name: String; Stack: TexeStack);
  end;

procedure API(Lib: TLib);
begin
  TFile_Functions.AddFunctionsIn(Lib.Functions);
end;

class procedure TFile_Functions.AddFunctionsIn(Functions: TFunctionList);
begin
  with Functions do
    begin
    Add('DeleteFile',
        'Remove um arquivo especificado pelo nome',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean,
        TObject,
        amDeleteFile);

    Add('RenameFile',
        'Renomeia um arquivo',
        cCatFile,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtBoolean,
        TObject,
        amRenameFile);

    Add('ChangeFileExt',
        'Troca a extensão de um arquivo',
        cCatFile,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtString,
        TObject,
        amChangeFileExt);

    Add('FileExists',
        'Verifica a existência de um arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean,
        TObject,
        amFileExists);

    Add('GetTempDir',
        'Retorna o diretório temporário do Sistema Operacional',
        cCatFile,
        [], [], [],
        pvtString,
        TObject,
        amGetTempDir);

{
    Add('FileSearch',
        'FileSearch searches for the file given by Name in the list of '#13+
        'directories given by DirList',
        cCatFile,
        [pvtString, pvtString],
        [nil      , nil      ],
        [False    , False    ],
        pvtString,
        TObject,
        amFileSearch);

    Add('DiskFree',
        'DiskFree returns the number of free bytes on the specified drive number'#13+
        'where 0 = Current, 1 = A, 2 = B, etc. DiskFree returns -1 if the drive '#13+
        'number is invalid',
        cCatFile,
        [pvtInteger],
        [nil       ],
        [False     ],
        pvtInteger,
        TObject,
        amDiskFree);

    Add('DiskSize',
        'DiskSize returns the size in bytes of the specified drive number',
        cCatFile,
        [pvtInteger],
        [nil      ],
        [False    ],
        pvtInteger,
        TObject,
        amDiskSize);
}
    Add('GetCurrentDir',
        'Retorna o diretório corrente',
        cCatFile,
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetCurrentDir);

    Add('SetCurrentDir',
        'Atribui um diretório corrente',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean,
        TObject,
        amSetCurrentDir);

    Add('CreateDir',
        'Cria um novo diretório',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean,
        TObject,
        amCreateDir);

    Add('RemoveDir',
        'Remove um diretório que esteja vazio',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtBoolean,
        TObject,
        amRemoveDir);

    Add('ExtractFilePath',
        'Retorna o caminho de um nome de arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amExtractFilePath);

    Add('ExtractFileDir',
        'Retorna o caminho de um nome de arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amExtractFileDir);

    Add('ExtractFileDrive',
        'Retorna o drive de um nome de arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amExtractFileDrive);

    Add('ExtractFileName',
        'Retorna o nome e a extensão de um nome de arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amExtractFileName);

    Add('ExtractFileExt',
        'Retorna a extensão de um nome de arquivo',
        cCatFile,
        [pvtString],
        [nil      ],
        [False    ],
        pvtString,
        TObject,
        amExtractFileExt);
    end;
end;

class procedure TFile_Functions.amDeleteFile(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean( DeleteFile(Stack.AsString(1)) );
end;

class procedure TFile_Functions.amRenameFile(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean( RenameFile(Stack.AsString(1),Stack.AsString(2)) );
end;

class procedure TFile_Functions.amChangeFileExt(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( ChangeFileExt(Stack.AsString(1),Stack.AsString(2)) );
end;

class procedure TFile_Functions.amFileSearch(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( FileSearch(Stack.AsString(1),Stack.AsString(2)) );
end;

class procedure TFile_Functions.amDiskFree(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( DiskFree(Stack.AsInteger(1)) );
end;

class procedure TFile_Functions.amDiskSize(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( DiskSize(Stack.AsInteger(1)) );
end;

class procedure TFile_Functions.amGetCurrentDir(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(GetCurrentDir);
end;

class procedure TFile_Functions.amSetCurrentDir(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean( SetCurrentDir(Stack.AsString(1)) );
end;

class procedure TFile_Functions.amCreateDir(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean( CreateDir(Stack.AsString(1)) );
end;

class procedure TFile_Functions.amRemoveDir(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean( RemoveDir(Stack.AsString(1)) );
end;

class procedure TFile_Functions.amExtractFilePath(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ExtractFilePath(Stack.AsString(1)));
end;

class procedure TFile_Functions.amExtractFileDir(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ExtractFileDir(Stack.AsString(1)));
end;

class procedure TFile_Functions.amExtractFileDrive(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ExtractFileDrive(Stack.AsString(1)));
end;

class procedure TFile_Functions.amExtractFileName(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ExtractFileName(Stack.AsString(1)));
end;

class procedure TFile_Functions.amExtractFileExt(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(ExtractFileExt(Stack.AsString(1)));
end;

class procedure TFile_Functions.amFileExists(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean(FileExists(Stack.AsString(1)));
end;

class procedure TFile_Functions.amGetTempDir(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString( FolderUtils.WindowsTempDir() );
end;

end.





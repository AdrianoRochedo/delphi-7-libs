unit FileUtils;

{
ATENÇÃO:
  - Para utilizar as rotinas de compactação de arquivos ligue a dir. de comp. "ZIP"
  - Para não utilizar formulários ligue a dir. "NoForms"
}

interface
uses Classes;

type
  TFileUtils = Class
  private
    FFilename: String;
    FTam: Integer;
    FData: TDateTime;
    FExt: String;
    FNC: String;
    FCam: String;
    FDrive: Char;
    FHandle: File;
    FAberto: Boolean;
    iDataSize: Integer;
    pData: Pointer;
    procedure SetFilename(const Value: String);
    procedure SetExt(const Value: String);
    procedure SetCam(const Value: String);
    procedure SetLongFilename(const Value: String);
    procedure SetDrive(const Value: Char);
    procedure SetProps(var Handle: File);
    function GetTam: Integer;
    function GetCam: String;
    function GetDate: TDateTime;
    function GetDescription: String;
    function GetVersion: String;
    function GetVersionString(Key: string): string;
  public
    constructor Create(const Nome: String);
    destructor Destroy; override;

    function  Open(ReadOnly: Boolean = True): Boolean;
    procedure Close;

    // Fecham o arquivo se ele está aberto e usam as informações de nome para executar as ações
    // As informações de nome são atualizados para o novo nome
    function Delete(): Boolean;
    function Rename (const NewName: String): Boolean;
    function Copy   (const TargetName: String): Boolean;
    function Move   (const TargetName: String): Boolean;

    function getRelativeName(const CaminhoBase: String): String;
    function FormatedSize: String;

    // Informações mantidas somente em memória, não afetam o arquivo se ele está aberto
    property LongFilename : String  read FNC        write SetLongFilename;
    property Filename     : String  read FFilename  write SetFilename;
    property Extension    : String  read FExt       write SetExt;
    property Path         : String  read GetCam     write SetCam;
    property Drive        : Char    read FDrive     write SetDrive;

    // Quando a informação de tamanho é requisitada,
    // o arquivo é aberto e mantido aberto
    property Size        : Integer   read GetTam;
    property Date        : TDateTime read GetDate;
    property Version     : String    read GetVersion;
    property Description : String    read GetDescription;
  end;

  // Retorna um nome para um arquivo temporário
  // Se Path = '' utiliza o diretório temporário do Windows
  function GetTempFile(Path, Prefix: String; Ext: String = ''): String;

  // Rotinas de criptografia simples
  procedure EncryptFile(Key: Word; const InFile: String; const OutFile: String = '');
  procedure DecryptFile(Key: Word; const InFile: String; const OutFile: String = '');

  // Rotinas de compactação
  {$ifdef ZIP}
  procedure ZipFile(const aFileName, aZipName: String);
  procedure UnZipFile(const aFileName: String; Dir: String = '');

  // Compacta (zip) e codifica se (key <> 0) um stream para arquivo
  procedure PackStreamToFile(Stream: TMemoryStream; const FileName: String; Key: Word = 0);

  // Decodifica se (key <> 0) e descompacta (unzip) um arquivo totalmente em memória
  function  UnPackFileToStream(const FileName: String; Key: Word = 0): TMemoryStream;
  {$endif}

  // Retorna o tamanho de um arquivo
  // O arquivo é aberto e fechado para obtenção do tamanho
  function FileSize(const Name: String): Integer;

  // Retorna as partes de um nome de arquivo
  procedure ProcessFileName(const EditText: string;
                            var Drive: Char;
                            var DirPart: string;
                            var FilePart: string);

  // Retorna todos os nomes dos arquivos a partir de um diretório
  function EnumerateFiles(const Exts: Array of String;
                          const Dir: String;
                          const SubDirs: Boolean): TStrings;

Type
  // Encapsula um arquivo texto para "log" de informações
  // Se o arquivo ja existe, a informação é adicionada no final
  TLogFile = class
  private
    FFileName: String;
  public
    constructor Create(const FileName: String);
    procedure AddDateTime();
    procedure Add(const LogMessage: String; WriteDateTime: Boolean = True);
    property FileName : String read FFileName;
  end;

  // Classe básica para uso de arquivos texto
  TTextFile = class
  private
    FFilename: string;
  protected
    FFile: System.TextFile;
  public
    constructor Create(const Filename: string);
    destructor Destroy(); override;
  end;

  // Gerador de arquivos texto
  TTextFileWriter = class(TTextFile)
  private
  public
    constructor Create(const Filename: string; NewFile: boolean);
    destructor Destroy(); override;

    // Escreve valores continualmente em uma linha
    procedure Write(const Text: string); overload;
    procedure Write(const Value: integer); overload;
    procedure Write(const Value: real; DecimalPlaces: byte); overload;
    procedure Write(const Value: char); overload;
    procedure Write(const Strings: TStrings); overload;
    procedure Write(const Text: string; const Pars: array of const); overload;

    // Escreve valores em uma nova linha
    procedure WriteLN(const Text: string); overload;
    procedure WriteLN(const Value: integer); overload;
    procedure WriteLN(const Value: real; DecimalPlaces: byte); overload;
    procedure WriteLN(const Value: char); overload;
    procedure WriteLN(const Strings: TStrings); overload;
    procedure WriteLN(const Text: string; const Pars: array of const); overload;
  end;

implementation
uses Windows,
     FileCtrl,
     SysUtils,
     SysUtilsEx,
     FolderUtils,

     {$ifdef ZIP}
     VCLUnZip,
     VCLZip,
     kpLIB,
     {$endif}

     STUTILS;

function TestAccessDir(var Path: String): Boolean;
var Code: Integer;
begin
  Code := FileGetAttr(Path);

  Result := (Code <> -1) and                            // é um diretório válido
            (FILE_ATTRIBUTE_READONLY and Code = 0) and  // não é ReadOnly
            (FILE_ATTRIBUTE_SYSTEM   and Code = 0);     // não é do Sistema

  if Path[Length(Path)] <> '\' then Path := Path + '\';
end;

// Retorna um nome para um arquivo temporário
// Se Path = '' utiliza o diretório temporário do Windows
function GetTempFile(Path, Prefix: String; Ext: String = ''): String;
Var i: Integer;
Begin
  If (Path = '') or not ForceDirectories(Path) Then
     Path := WindowsTempDir();

//  if TestAccessDir(Path) then
     begin
     if (Ext <> '') and (Ext[1] <> '.') then Ext := '.' + Ext;
     i := 1;
     Repeat
       Result := Path + Prefix + IntToStr(i) + Ext;
       inc(i);
       Until Not FileExists(Result);
     end // if
//  else
//     Result := '';
End;

procedure CodificaArquivo(Codificar: Boolean; Key: Word; const InFile, OutFile: String);
var Stream, OutStream: TStream;
    S: String;
    Size: Integer;
begin
  OutStream := nil;
  if (OutFile <> '') and (CompareText(OutFile, InFile) <> 0) then
     OutStream := TFileStream.Create(OutFile, fmCreate);
     
  try
    Stream := TFileStream.Create(InFile, fmOpenReadWrite);
    try
      Size := Stream.Size - Stream.Position;
      SetString(S, nil, Size);
      Stream.Read(Pointer(S)^, Size);

      if Codificar then
         S := Encrypt(S, Key)
      else
         S := Decrypt(S, Key);

      if OutStream = nil then
         begin
         Stream.Position := 0;
         Size := Length(S);
         if Stream.Size <> Size then Stream.Size := Size;
         Stream.WriteBuffer(Pointer(S)^, Size);
         end
      else
         OutStream.WriteBuffer(Pointer(S)^, Size);

      finally
      Stream.Free();
      end;
      
    finally
    if OutStream <> nil then OutStream.Free();
    end;
end;

procedure EncryptFile(Key: Word; const InFile: String; const OutFile: String = '');
begin
  CodificaArquivo(True, Key, InFile, OutFile);
end;

procedure DecryptFile(Key: Word; const InFile: String; const OutFile: String = '');
begin
  CodificaArquivo(False, Key, InFile, OutFile);
end;

{$ifdef ZIP}
procedure ZipFile(const aFileName, aZipName: String);
begin
 With TVCLZip.Create(nil) do
   try
     ZipName := aZipName;
     FilesList.Add(aFileName);
     PackLevel := 9;
     Zip;
   finally
     Free;
   end;
end;

procedure UnZipFile(const aFileName: String; Dir: String = '');
begin
  With TVCLUnZip.Create(nil) do
    try
      ZipName := aFileName;
      ReadZip;
      DoAll := True;

      if Dir <> '' then
         DestDir := Dir
      else
         DestDir := ExtractFilePath(aFileName);

      //RecreateDirs := False;
      RetainAttributes := True;
      Unzip;
    finally
      Free;
    end;
end;

procedure PackStreamToFile(Stream: TMemoryStream; const FileName: String; Key: Word = 0);
var z: TVCLZip;
begin
  z := TVCLZip.Create(nil);
  try
    z.ArchiveStream := TMemoryStream.Create;
    z.PackLevel := 9;
    z.ZipFromStream(Stream, FileName);

    if Key <> 0 then EncryptStream(TMemoryStream(z.ArchiveStream), Key); // Codifica em memória
    TMemoryStream(z.ArchiveStream).SaveToFile(FileName);
  finally
    z.Free;
  end;
end;

function UnPackFileToStream(const FileName: String; Key: Word = 0): TMemoryStream;
var z: TVCLUnZip;
    m: TMemoryStream;
begin
  z := TVCLUnZip.Create(nil);
  m := TMemoryStream.Create;
  try
    m.LoadFromFile(FileName); // Lê o arquivo criptografado e zipado
    if Key <> 0 then DecryptStream(m, Key); // Decodifica em memória

    z.ArchiveStream := TLFNFileStream.Create(m); // Compatibiliza a Stream com o formato do zip
    z.ReadZip; // Lê as informações do zip (stream na memória)

    Result := TMemoryStream.Create;
    z.UnZipToStream(Result, z.FullName[0]); // Descompacta o primeiro arquivo para Result
    Result.Position := 0; // Vai para o começo da Stream
  finally
    m.Free;
    z.Free;
  end;
end;
{$endif}

function FileSize(const Name: String): Integer;
var F: File;
begin
  try
    AssignFile(F, Name);
    FileMode := 0;  {read only }
    Reset(F, 1);
    Result := System.FileSize(F);
    CloseFile(F);
  except
    Result := -1;
  end;
end;

procedure ProcessFileName (const EditText: string; var Drive: Char;
                       var DirPart: string; var FilePart: string);
begin
  if Length(EditText) > 0 then
     begin
     Drive := EditText[1];
     DirPart := ExtractFilePath(EditText);
     FilePart := ExtractFileName(EditText);
     end
  else
     begin
     Drive := #0;
     DirPart := '';
     FilePart := '';
     end;
end;

var
  FFiltroExts: Array of String;

function FuncFiltro(const SR : TSearchRec) : Boolean;
var s: String;
    b: Boolean;
    i: Integer;
begin
  s := UpperCase(ExtractFileExt(SR.Name));

  if Length(FFiltroExts) > 0 then
     begin
     b := (FFiltroExts[0] = '*');
     if not b then
        for i := 0 to High(FFiltroExts) do
          if s = FFiltroExts[i] then
             begin
             b := True;
             Break;
             end;
     end
  else
     b := True;

  if b or (SR.Attr and faDirectory = faDirectory) then
     Result := True
  else
     Result := False;
end;

function EnumerateFiles(const Exts: Array of String;
                        const Dir: String;
                        const SubDirs: Boolean): TStrings;
var SL  : TStrings;
    i   : Integer;
begin
{$IOCHECKS OFF}
  SetLength(FFiltroExts, Length(Exts));
  for i := 0 to High(Exts) do
    FFiltroExts[i] := UpperCase(Exts[i]);

  SL := TStringList.Create;
  try
    Result := TStringList.Create;
    STUTILS.EnumerateFiles(Dir, SL, SubDirs, FuncFiltro);
    for i := 0 to SL.Count-1 do
      if FileExists(SL[i]) then
         Result.Add(SL[i])
  finally
    SL.Free;
  end;
{$IOCHECKS ON}
end;

{ TLogFile }

procedure TLogFile.Add(const LogMessage: String; WriteDateTime: Boolean = True);
var F: TextFile;
begin
  AssignFile(F, FFileName);
  Append(F);
  if WriteDateTime then
     WriteLN(F, DateTimeToStr(Now) + '|' + LogMessage)
  else
     WriteLN(F, LogMessage);
  Flush(F);
  CloseFile(F);
end;

procedure TLogFile.AddDateTime;
begin
  Add(DateTimeToStr(Now), False);
end;

constructor TLogFile.Create(const FileName: String);
begin
  inherited Create;
  FFileName := FileName;
  if not FileExists(FileName) then
     FileClose(FileCreate(FileName));
end;

{ TArquivo }

constructor TFileUtils.Create(const Nome: String);
begin
  Inherited Create;
  SetLongFilename(Nome);
end;

destructor TFileUtils.Destroy;
begin
  if FAberto Then Close;
  inherited;
end;

function TFileUtils.Delete: Boolean;
begin
  if FAberto Then Close;
  Result := DeleteFile(FNC);
end;

function TFileUtils.Move(const TargetName: String): Boolean;
begin
  if FAberto then Close;
  Result := Windows.MoveFile(pChar(FNC), pChar(TargetName));
  if Result then
     SetLongFilename(TargetName);
end;

function TFileUtils.Copy(const TargetName: String): Boolean;
begin
  if FAberto then Close;
  Result := Windows.CopyFile(pChar(FNC), pChar(TargetName), False);
  if Result then
     SetLongFilename(TargetName);
end;

function TFileUtils.Rename(const NewName: String): Boolean;
begin
  if FAberto Then Close;
  Result := RenameFile(FNC, NewName);
  if Result then
     SetLongFilename(NewName);
end;

procedure TFileUtils.SetCam(const Value: String);
begin
  if FAberto Then Close;
  FCam := Value;
  if LastChar(FCam) <> '\' then FCam := FCam + '\';
  SetLongFilename(FCam + FFilename);
end;

procedure TFileUtils.SetExt(const Value: String);
begin
  if FAberto Then Close;
  FExt := Value;
  FNC := ChangeFileExt(FNC, '.' + FExt);
  FFilename := ExtractFileName(FNC);
end;

procedure TFileUtils.SetFilename(const Value: String);
begin
  if FAberto Then Close;
  FFilename := Value;
  FNC := FCam + FFilename;
  FExt  := ExtractFileExt(FFilename);
  System.Delete(FExt, 1, 1);
end;

procedure TFileUtils.SetLongFilename(const Value: String);
begin
  if FAberto Then Close;
  FNC := Value;
  FExt := ExtractFileExt(Value);
  System.Delete(FExt, 1, 1);
  ProcessFileName(Value, FDrive, FCam, FFilename);
end;

procedure TFileUtils.SetDrive(const Value: Char);
begin
  if FAberto Then Close;
  FDrive := Value;
  if Length(FNC) > 0 then
     FNC[1] := Value;
end;

procedure TFileUtils.SetProps(var Handle: File);
var lpdwHandle: dWord;
begin
  FTam := System.FileSize(Handle);
  FData := FileDateToDateTime(FileAge(FNC));
  iDataSize := GetFileVersionInfoSize(PChar(FNC), lpdwHandle);
  if iDataSize > 0 then
    begin
    GetMem(pData, iDataSize);
    Win32Check(GetFileVersionInfo(PChar(FNC), 0, iDataSize, pData));
    end;
end;

function TFileUtils.Open(ReadOnly: Boolean): Boolean;
begin
  Result  := False;
  FTam := -1;
  if FAberto Then Close;
  if FileExists(FNC) then
     try
       AssignFile(FHandle, FNC);
       if ReadOnly then FileMode := 0;
       Reset(FHandle, 1);
       FAberto := True;
       SetProps(FHandle);
       Result := True;
     except
       // Nada
       Close;
     end;
end;

procedure TFileUtils.Close;
begin
  if iDataSize > 0 then
     begin
     FreeMem(pData, iDataSize);
     iDataSize := 0;
     end;
  CloseFile(FHandle);
  FAberto := False;
end;

function TFileUtils.GetTam: Integer;
begin
  if not FAberto then Open;
  Result := FTam;
end;

function TFileUtils.GetCam: String;
begin
  if LastChar(FCam) <> '\' then FCam := FCam + '\';
  Result := FCam;
end;

function TFileUtils.getRelativeName(const CaminhoBase: String): String;
begin
  Result := ExtractRelativePath(CaminhoBase, FNC);
end;

function TFileUtils.GetDate: TDateTime;
begin
  if not FAberto then Open;
  Result := FData;
end;

function TFileUtils.FormatedSize: String;
var x: Integer;
begin
  if not FAberto then Open;
  x := FTam;
  if (x < 1024) and (x > 0) then x := 1024;
  Result := IntToStr(x div 1024) + ' KB';  //80
end;

function TFileUtils.GetVersionString(Key: string): string;
var
  lpdwHandle: dWord;
  P: Pointer;
  S: string;
  Buffer: PChar;
  b: Boolean;
begin
  Result := '';
  if iDataSize > 0 then
     begin
     b := VerQueryValue(pData, '\VarFileInfo\Translation', P, lpdwHandle);
     if b then
        begin
        S := Format('\StringFileInfo\%.4x%.4x\%s',
                   [LoWord(Integer(P^)), HiWord(Integer(P^)), Key]);
        if VerQueryValue(pData, PChar(S), Pointer(Buffer), lpdwHandle) then
           Result := StrPas(Buffer);
        end;
     end;
end;

function TFileUtils.GetDescription: String;
begin
  if not FAberto then Open;
  Result := GetVersionString('FileDescription');
end;

function TFileUtils.GetVersion: String;
begin
  if not FAberto then Open;
  Result := GetVersionString('FileVersion');
end;

{ TTextFile }

constructor TTextFile.Create(const Filename: string);
begin
  inherited Create();
  AssignFile(FFile, Filename);
end;

destructor TTextFile.Destroy;
begin
  CloseFile(FFile);
  inherited;
end;

{ TTextFileWriter }

constructor TTextFileWriter.Create(const Filename: string; NewFile: boolean);
begin
  inherited Create(Filename);
  if NewFile then
     ReWrite(FFile)
  else
     Append(FFile);
end;

destructor TTextFileWriter.Destroy();
begin
  Flush(FFile);
  inherited Destroy();
end;

procedure TTextFileWriter.Write(const Text: string);
begin
  System.Write(FFile, Text);
end;

procedure TTextFileWriter.WriteLN(const Text: string);
begin
  System.WriteLN(FFile, Text);
end;

procedure TTextFileWriter.Write(const Value: integer);
begin
  System.Write(FFile, toString(Value));
end;

procedure TTextFileWriter.Write(const Value: real; DecimalPlaces: byte);
begin
  System.Write(FFile, toString(Value, DecimalPlaces));
end;

procedure TTextFileWriter.Write(const Value: char);
begin
  System.Write(FFile, Value);
end;

procedure TTextFileWriter.WriteLN(const Value: integer);
begin
  System.WriteLN(FFile, toString(Value));
end;

procedure TTextFileWriter.WriteLN(const Value: real; DecimalPlaces: byte);
begin
  System.WriteLN(FFile, toString(Value, DecimalPlaces));
end;

procedure TTextFileWriter.WriteLN(const Value: char);
begin
  System.WriteLN(FFile, Value);
end;

procedure TTextFileWriter.Write(const Text: string; const Pars: array of const);
begin
  System.Write(FFile, Format(Text, Pars));
end;

procedure TTextFileWriter.WriteLN(const Text: string; const Pars: array of const);
begin
  System.WriteLN(FFile, Format(Text, Pars));
end;

procedure TTextFileWriter.Write(const Strings: TStrings);
var i: Integer;
begin
  for i := 0 to Strings.Count-1 do
    System.Write(FFile, Strings[i]);
end;

procedure TTextFileWriter.WriteLN(const Strings: TStrings);
var i: Integer;
begin
  for i := 0 to Strings.Count-1 do
    System.WriteLN(FFile, Strings[i]);
end;

end.

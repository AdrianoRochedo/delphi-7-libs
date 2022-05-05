unit LanguageControl;

interface
uses classes;

type
  TGetLanguageMessage = Function (Index: Word): String;

  TLanguageManager = class
  private
    FLanguagesPath: String;
    FIDs: TStrings;
    FProcAddressList: TList;
    FGetLanguageMessage: TGetLanguageMessage;
  public
    constructor Create(const LanguagesPath: String);
    destructor Destroy; override;

    procedure Clear;
    procedure Load(const Language: String);

    function  GetMessage(ID_Index, MessageIndex: Word): String; overload;
    function  GetMessage(const ID: String; MessageIndex: Word): String; overload;

    property LanguagesPath : String read FLanguagesPath write FLanguagesPath;
    property IDs : TStrings read FIDs;
  end;

var
  LanguageManager: TLanguageManager;

implementation
uses Windows, Sysutils, FileUtils;

{ TLanguageManeger }

constructor TLanguageManager.Create(const LanguagesPath: String);
begin
  inherited Create;
  FLanguagesPath := LanguagesPath;
  FIDs := TStringList.Create;
  FProcAddressList := TList.Create;
end;

destructor TLanguageManager.Destroy;
begin
  Clear;
  FProcAddressList.Free;
  FIDs.Free;
  inherited;
end;

procedure TLanguageManager.Clear;
var i: Integer;
begin
  for i := 0 to FIDs.Count-1 do
    FreeLibrary(THandle(FIDs.Objects[i]));
  FIDs.Clear;
  FProcAddressList.Clear;
end;

function TLanguageManager.GetMessage(ID_Index, MessageIndex: Word): String;
begin
  @FGetLanguageMessage := FProcAddressList[ID_Index];
  Result := FGetLanguageMessage(MessageIndex);
end;

function TLanguageManager.GetMessage(const ID: String; MessageIndex: Word): String;
var i: Integer;
begin
  i := FIDs.IndexOf(ID);
  if i > -1 then
     Result := GetMessage(i, MessageIndex)
  else
     Result := 'Invalide Message ID';
end;

procedure TLanguageManager.Load(const Language: String);
var SL: TStrings;
    h: THandle;
    s: String;
    i: Integer;
begin
  SL := FileUtils.EnumerateFiles(['.' + Language], FLanguagesPath, False);
  try
    if SL.Count > 0 then
       begin
       Clear;
       for i := 0 to SL.Count-1 do
         begin
         h := LoadLibrary(pChar(SL[i]));
         s := ExtractFileName(SL[i]);
         s := Copy(s, 1, Pos('.', s)-1);
         FIDs.AddObject(s, pointer(h));
         FProcAddressList.Add(GetProcAddress(h, 'GetMessage'));
         end;
       end
    else
       raise Exception.Create('Idioma não Encontrado !');
  finally
    SL.Free;
  end;
end;

initialization
  LanguageManager := TLanguageManager.Create('');

finalization
  LanguageManager.Free;

end.

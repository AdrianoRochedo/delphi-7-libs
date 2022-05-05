unit Plugin;

interface
uses Windows, Classes, SysUtilsEx, Graphics;

type
  IObjectFactory = interface(SysUtilsEx.IInterface)
    function getName(): wideString;
    function CreateObject(const ClassName: wideString): TObject;
    function getBitmap(const ClassName: wideString): TBitmap;
  end;

  TPlugin = class
  private
    FFactory: IObjectFactory;
    FDllHandle: THandle;
  protected
    function getComment(): wideString; virtual;
    function getVersion(): wideString; virtual;
    function getFactory: IObjectFactory; virtual;
  public
    constructor Create(Factory: IObjectFactory);

    function ToString(): wideString; virtual;

    property DllHandle   : THandle        read FDllHandle write FDllHandle;
    property Description : wideString     read getComment;
    property Version     : wideString     read getVersion;
    property Factory     : IObjectFactory read getFactory;
  end;

  // Se o modulo exporta um Plugin, a funcao retornara uma instancia para ele
  function getPluginInstance(const PluginName: string): TPlugin;

implementation

{ TPlugin }

constructor TPlugin.Create(Factory: IObjectFactory);
begin
  inherited Create();
  FFactory := Factory;
end;

function TPlugin.getComment: wideString;
begin
  Result := '';
end;

function TPlugin.getFactory: IObjectFactory;
begin
  Result := FFactory;
end;

function TPlugin.getVersion: wideString;
begin
  Result := '1.0';
end;

function TPlugin.ToString: wideString;
begin
  Result := 'SysUtilsEx.TPlugin';
end;

// --------------------------------------------------------------------------

type
  TPluginProc = function() : TPlugin;

function getPluginInstance(const PluginName: string): TPlugin;
var getPlugin: TPluginProc;
    h: THandle;
begin
  Result := nil;
  h := LoadLibrary(pChar(PluginName));
  if h <> 0 then
     begin
     @getPlugin := Windows.GetProcAddress(h, 'getPlugin');
     if @getPlugin <> nil then
        begin
        Result := getPlugin();
        Result.DllHandle := h;
        end;
     end;
end;

end.

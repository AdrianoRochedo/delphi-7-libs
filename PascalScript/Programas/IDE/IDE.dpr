program IDE;

uses
  ShareMem,
  Forms,
  psEditor;

{$R *.RES}

begin
  Application.Initialize;
  RunScript([], ParamSTR(1), nil, [], nil);
end.

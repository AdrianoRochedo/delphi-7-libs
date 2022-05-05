unit Rochedo.Dialogs.EditorServices;

interface
uses Classes, Controls, Forms, SysUtils;

type
  IEditable = interface
    ['{54A7E64E-7233-4428-A209-B7D0BC421780}']
    function edit(obj: TObject): TModalResult;
  end;

  TEditorServices = class
  private
    FEditors: TStringList;
    constructor Create();
    destructor Destroy(); override;
  public
    function CreateEditor(const Name: string): IEditable;
    procedure registerEditor(const Name: string; EditorClass: TComponentClass);
    procedure unregisterEditor(const Name: string);
  end;

  function getEditorServices(): TEditorServices;

implementation

  var ES: TEditorServices = nil;

  function getEditorServices(): TEditorServices;
  begin
    if ES = nil then ES := TEditorServices.Create();
    result := ES;
  end;

{ TEditorService }

constructor TEditorServices.Create();
begin
  inherited Create();
  FEditors := TStringList.Create();
end;

function TEditorServices.CreateEditor(const Name: string): IEditable;
var f: TForm;
    c: TComponentClass;
    i: integer;
begin
  i := FEditors.IndexOf(Name);
  if i > -1 then
     begin
     c := TComponentClass( FEditors.Objects[i] );
     Application.CreateForm(c, f);
     if not f.GetInterface(IEditable, result) then
        raise Exception.CreateFmt('Class %s do not implements IEditable', [Name]);
     end
  else
     raise Exception.Create('Class not registred: ' + Name);
end;

destructor TEditorServices.Destroy();
begin
  FEditors.Free();
  inherited Destroy();
end;

procedure TEditorServices.registerEditor(const Name: string; EditorClass: TComponentClass);
begin
  if FEditors.IndexOf(Name) = -1 then
     FEditors.AddObject(Name, TObject(EditorClass))
  else
     raise Exception.Create('Class already registred: ' + Name);
end;

procedure TEditorServices.unregisterEditor(const Name: string);
var i: Integer;
begin
  i := FEditors.IndexOf(Name);
  if i > -1 then FEditors.Delete(i);
end;

initialization

finalization
  if ES <> nil then ES.Free();

end.

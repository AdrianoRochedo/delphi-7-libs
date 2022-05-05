unit Rochedo.Utils.Menu;

interface
uses Classes, ActnList, Menus;

type
  MenuUtils = class
  public
    class function CreateMenu(Action: TBasicAction): TMenuItem;
    class function CreateMenuItem(Root: TMenuItem; Text: String; Event: TNotifyEvent): TMenuItem; overload;
    class function CreateMenuItem(Root: TMenuItem; Action: TBasicAction): TMenuItem; overload;
    class function CreateSubMenus(Root: TMenuItem; Action: TBasicAction; CreateItem: boolean): TMenuItem;
  end;

implementation

{ MenuUtils }

class function MenuUtils.CreateMenu(Action: TBasicAction): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := TAction(Action).Caption;
  Result.Tag := TAction(Action).Tag;
  Result.Checked := TAction(Action).Checked;
  Result.OnClick := TAction(Action).OnExecute;
end;

class function MenuUtils.CreateMenuItem(Root: TMenuItem; Text: String; Event: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := Text;
  Result.OnClick := Event;
  Root.Add(Result);
end;

class function MenuUtils.CreateMenuItem(Root: TMenuItem; Action: TBasicAction): TMenuItem;
begin
  Result := CreateMenu(Action);
  Root.Add(Result);
end;

class function MenuUtils.CreateSubMenus(Root: TMenuItem; Action: TBasicAction; CreateItem: boolean): TMenuItem;
var i: Integer;
begin
  if CreateItem then
     begin
     Result := CreateMenu(Action);
     Root.Add(Result);
     end
  else
     Result := Root;

  for i := 0 to Action.ComponentCount-1 do
    CreateSubMenus(Result, TAction(Action.Components[i]), true);
end;

end.
